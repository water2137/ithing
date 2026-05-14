{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

import System.Environment (getArgs, lookupEnv)
import System.IO (hFlush, stdout, isEOF)
import qualified Control.Exception as E
import Control.Concurrent
import System.IO.Error (isEOFError)
import System.Console.Haskeline
import System.Directory (setCurrentDirectory)
import System.Process (system)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isSpace)
import Data.List (nub, (\\), intercalate, foldl')
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map

import qualified Data.IntMap.Strict as IM
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as MS

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (empty)

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Read (readCompileTimeFile)

-- Opaque type for C_CPU
data C_CPU

-- FFI Imports
foreign import ccall "c_init" c_init :: IO (Ptr C_CPU)
foreign import ccall "c_interrupt" c_interrupt :: Ptr C_CPU -> IO ()
foreign import ccall "mk_var" mk_var :: CInt -> IO (Ptr ())
foreign import ccall "mk_lam" mk_lam :: CInt -> Ptr () -> IO (Ptr ())
foreign import ccall "mk_app" mk_app :: Ptr () -> Ptr () -> IO (Ptr ())
foreign import ccall "c_register_global" c_register_global :: Ptr C_CPU -> CInt -> Ptr () -> IO ()
foreign import ccall "safe c_eval" c_eval :: Ptr C_CPU -> Ptr () -> IO CLong
foreign import ccall "c_get_tag" c_get_tag :: CLong -> IO CSize
foreign import ccall "c_get_var_idx" c_get_var_idx :: CLong -> IO CInt
foreign import ccall "c_get_app_f" c_get_app_f :: CLong -> IO CLong
foreign import ccall "c_get_app_a" c_get_app_a :: CLong -> IO CLong
foreign import ccall "c_get_lam_idx" c_get_lam_idx :: CLong -> IO CInt
foreign import ccall "safe c_decode" c_decode :: Ptr C_CPU -> CLong -> IO CString
foreign import ccall "safe c_quote" c_quote :: Ptr C_CPU -> CLong -> IO CString
foreign import ccall "free" c_free :: Ptr a -> IO ()

toCExpr :: Expr -> IO (Ptr ())
toCExpr (Var i) = mk_var (fromIntegral i)
toCExpr (Lam i b) = do
    cb <- toCExpr b
    mk_lam (fromIntegral i) cb
toCExpr (App f a) = do
    cf <- toCExpr f
    ca <- toCExpr a
    mk_app cf ca

fromCValue :: CLong -> IO Expr
fromCValue v = do
    tag <- c_get_tag v
    case tag of
        0 -> Var . fromIntegral <$> c_get_var_idx v
        1 -> App <$> (fromCValue =<< c_get_app_f v) <*> (fromCValue =<< c_get_app_a v)
        2 -> do
            idx <- c_get_lam_idx v
            return $ Var (fromIntegral $ 1000 + idx)
        _ -> error "Unknown tag from C"

licenseText :: String
licenseText = "Copyright (C) 2026  water2137\n\n\
\This program is free software; you can redistribute it and/or\n\
\modify it under the terms of the GNU General Public License\n\
\as published by the Free Software Foundation; either version 2\n\
\of the License, or (at your option) any later version.\n\n\
\This program is distributed in the hope that it will be useful,\n\
\but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
\MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
\GNU General Public License for more details.\n\n\
\You should have received a copy of the GNU General Public License\n\
\along with this program; if not, see\n\
\<https://www.gnu.org/licenses/>.\n\n\
\Source code for interprething may be obtained at\n\
\<https://github.com/water2137/ithing>\n"

stdlib :: String
stdlib = $(readCompileTimeFile "it-std.txt")

data Expr
    = Var !Int
    | Lam !Int Expr
    | App Expr Expr
    deriving (Eq)

instance Show Expr where
    show e = go e
      where
        go (Var i) = getInternedName i
        go (Lam i b) = "(\\" ++ getInternedName i ++ " -> " ++ go b ++ ")"
        go (App f a) = "(" ++ go f ++ " " ++ go a ++ ")"

-- Interning table
type Interner = MS.Map String Int
type ReverseInterner = IM.IntMap String
type InternState = (Int, Interner, ReverseInterner)

emptyIntern :: InternState
emptyIntern = (0, MS.empty, IM.empty)

intern :: String -> InternState -> (Int, InternState)
intern s (next, m, rm) = case MS.lookup s m of
    Just i -> (i, (next, m, rm))
    Nothing -> (next, (next + 1, MS.insert s next m, IM.insert next s rm))

{-# NOINLINE globalInterner #-}
globalInterner :: IORef InternState
globalInterner = unsafePerformIO $ newIORef emptyIntern

internStr :: String -> Int
internStr s = unsafePerformIO $ atomicModifyIORef' globalInterner (\st -> let (i, st') = intern s st in (st', i))

getInternedName :: Int -> String
getInternedName i
    | i >= 1000 = "lambda_" ++ show (i - 1000)
    | i < 0 = "x" ++ show (-(i + 1))
    | otherwise = unsafePerformIO $ do
        (_, _, rm) <- readIORef globalInterner
        case IM.lookup i rm of
            Just s -> return s
            Nothing -> return ("v" ++ show i)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme $ try $ do
    x <- p
    check x
  where
    p       = (:) <$> identStart <*> many identLetter
    check x = if x `elem` ["->", "\\", "="]
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x
    identStart  = alphaNumChar <|> oneOf "!#$%&*+./<=>?@^|-~_"
    identLetter = alphaNumChar <|> oneOf "!#$%&*+./<=>?@^|-~_'"

reservedOp :: String -> Parser ()
reservedOp name = lexeme $ try $ string name *> notFollowedBy (oneOf "!#$%&*+./<=>?@^|-~_'")

whiteSpace :: Parser ()
whiteSpace = sc

atom :: Parser Expr
atom = parens expr
   <|> lam
   <|> (try (Var . internStr <$> identifier <* notFollowedBy (reservedOp "=")))

lam :: Parser Expr
lam = do
    reservedOp "\\"
    vars <- some identifier
    reservedOp "->"
    body <- expr
    return $ foldr (\v b -> Lam (internStr v) b) body vars

app :: Parser Expr
app = do
    es <- some atom
    return $ foldl1 App es

expr :: Parser Expr
expr = app

assignment :: Parser (Int, Expr)
assignment = do
    name <- identifier
    reservedOp "="
    val <- expr
    return (internStr name, val)

fileParser :: Parser [(Int, Expr)]
fileParser = do
    whiteSpace
    defs <- many (try assignment)
    eof
    return defs

data ReplCmd = CmdAssign Int Expr | CmdExpr Expr | CmdLoad String | CmdSave String | CmdLoadStd | CmdClear | CmdCD String | CmdShell String | CmdShowHelp | CmdShowDefs | CmdShowLicense | CmdDebug Expr | CmdChurch Expr | CmdRedirect String | CmdStopRedirect | CmdNOP

replParser :: Parser ReplCmd
replParser = do
    whiteSpace
    (try (do char ':'; char 'l'; whiteSpace; path <- some (satisfy (not . isSpace)); whiteSpace; eof; return $ CmdLoad path)
     <|> try (do char ':'; char 's'; whiteSpace; path <- some (satisfy (not . isSpace)); whiteSpace; eof; return $ CmdSave path)
     <|> try (do char ':'; char 'r'; whiteSpace; path <- some (satisfy (not . isSpace)); whiteSpace; eof; return $ CmdRedirect path)
     <|> try (do char ':'; char 'r'; whiteSpace; eof; return CmdStopRedirect)
     <|> try (do char ':'; char 'S'; whiteSpace; eof; return CmdLoadStd)
     <|> try (do char ':'; char '!'; whiteSpace; cmd <- many anySingle; eof; return $ CmdShell cmd)
     <|> try (do char ':'; char 'c'; char 'd'; whiteSpace; path <- some (satisfy (not . isSpace)); whiteSpace; eof; return $ CmdCD path)
     <|> try (do char ':'; char 'c'; whiteSpace; eof; return CmdClear)
     <|> try (do char ':'; char 'h'; whiteSpace; eof; return CmdShowHelp)
     <|> try (do char ':'; char 'd'; whiteSpace; eof; return CmdShowDefs)
     <|> try (do char ':'; char 'L'; whiteSpace; eof; return CmdShowLicense)
     <|> try (do char ':'; char 'D'; whiteSpace; e <- expr; whiteSpace; eof; return $ CmdDebug e)
     <|> try (do char ':'; char 'C'; whiteSpace; e <- expr; whiteSpace; eof; return $ CmdChurch e)
     <|> try (do (i, v) <- assignment; whiteSpace; eof; return $ CmdAssign i v)
     <|> try (do e <- expr; whiteSpace; eof; return $ CmdExpr e)
     <|> (eof >> return CmdNOP))

introText :: String
introText = "interpretthing, Copyright (C) 2026 water2137\n\
\interpretthing comes with ABSOLUTELY NO WARRANTY; for details\n\
\about the license, type `:L`\n\
\`:h` for help\n"

helpText :: String
helpText = "builtins: :[!lsrShcdLDC|cd]\n\
\! [shell] - shell escape\n\
\l [file] - load definitions file\n\
\s [file] - save definitions to file\n\
\r [file] - redirect output to file (or no file to stop)\n\
\S - load standard library\n\
\c - clear definitions\n\
\h - help\n\
\d - dump definitions\n\
\L - license details\n\
\D [expr] - debug, shows every reduction step (AST interpreter)\n\
\C [expr] - church to human readable\n"

freeVars :: Expr -> Set.Set Int
freeVars (Var i) = Set.singleton i
freeVars (Lam i b) = Set.delete i (freeVars b)
freeVars (App f a) = Set.union (freeVars f) (freeVars a)

subst :: Int -> Expr -> Expr -> Expr
subst x s (Var y) | x == y = s
                  | otherwise = Var y
subst x s (Lam y b)
    | x == y = Lam y b
    | y `Set.member` freeVars s =
        let name = getInternedName y ++ "'"
            y' = internStr name
        in Lam y' (subst x s (subst y (Var y') b))
    | otherwise = Lam y (subst x s b)
subst x s (App f a) = App (subst x s f) (subst x s a)

step :: IM.IntMap Expr -> Expr -> Maybe Expr
step env (Var i) = IM.lookup i env
step env (App f a) =
    case f of
        Lam x b -> Just (subst x a b)
        _ -> case step env f of
                Just f' -> Just (App f' a)
                Nothing -> App f <$> step env a
step env (Lam i b) = Lam i <$> step env b

expand :: IM.IntMap Expr -> Expr -> Expr
expand globalEnv expr = go Set.empty MS.empty expr
  where
    go visited localEnv (Var i) = case MS.lookup i localEnv of
        Just v -> v
        Nothing -> case IM.lookup i globalEnv of
            Just v | not (Set.member i visited) -> go (Set.insert i visited) MS.empty v
            _ -> Var i
    go visited localEnv (Lam i b) =
        let name = getInternedName i ++ "'"
        in name `seq` 
           let i' = internStr name
           in Lam i' (go visited (MS.insert i (Var i') localEnv) b)
    go visited localEnv (App f a) = App (go visited localEnv f) (go visited localEnv a)

runREPL :: IO ()
runREPL = do
    cpu <- c_init
    putStrLn introText
    runInputT defaultSettings $ withInterrupt $ loop cpu IM.empty Nothing
  where
    evalWithInterrupt cpu act = do
        mvar <- newEmptyMVar
        tid <- forkIO $ (act >>= putMVar mvar) `E.catch` (\(e :: E.SomeException) -> return ())
        takeMVar mvar `E.onException` (c_interrupt cpu >> killThread tid)

    loop cpu currentEnv maybeRedir = handleInterrupt (loop cpu currentEnv maybeRedir) $ do
        minput <- getInputLine "> "
        case minput of
            Nothing -> return ()
            Just "" -> loop cpu currentEnv maybeRedir
            Just line -> do
                result <- withInterrupt $ liftIO $ E.try $ do
                    let putStrLn' s = case maybeRedir of
                                        Nothing -> putStrLn s
                                        Just path -> appendFile path (s ++ "\n")
                    let putStr' s = case maybeRedir of
                                        Nothing -> putStr s
                                        Just path -> appendFile path s
                    case parse replParser "<stdin>" line of
                        Left err -> putStr (errorBundlePretty err) >> return (Just (currentEnv, maybeRedir))
                        Right (CmdRedirect path) -> do
                            writeFile path ""
                            putStrLn $ "redirecting output to " ++ path
                            return (Just (currentEnv, Just path))
                        Right CmdStopRedirect -> do
                            putStrLn "stopped redirection"
                            return (Just (currentEnv, Nothing))
                        Right CmdShowDefs -> do
                            if IM.null currentEnv
                                then putStrLn' "nothing defined"
                                else mapM_ (\(i, expr) -> putStrLn' $ getInternedName i ++ " = " ++ show expr) (IM.toList currentEnv)
                            return (Just (currentEnv, maybeRedir))
                        Right CmdShowLicense -> do
                            putStr' licenseText
                            return (Just (currentEnv, maybeRedir))
                        Right (CmdShowHelp) -> do
                            putStrLn' helpText
                            return (Just (currentEnv, maybeRedir))
                        Right (CmdCD path) -> do
                            setCurrentDirectory path
                            return (Just (currentEnv, maybeRedir))
                        Right CmdClear -> do
                            putStrLn' "cleared all definitions"
                            return (Just (IM.empty, maybeRedir))
                        Right (CmdShell cmd) -> do
                            _ <- system cmd
                            return (Just (currentEnv, maybeRedir))
                        Right (CmdLoad path) -> do
                            content <- BL.readFile path
                            case parse fileParser path (BL.unpack content) of
                                Left err -> putStr (errorBundlePretty err) >> return (Just (currentEnv, maybeRedir))
                                Right defs -> do
                                    mapM_ (\(i, e) -> toCExpr e >>= c_register_global cpu (fromIntegral i)) defs
                                    putStrLn' $ "loaded " ++ show (length defs) ++ " definitions from " ++ path
                                    return (Just (IM.union currentEnv (IM.fromList defs), maybeRedir))
                        Right CmdLoadStd -> do
                            case parse fileParser "stdlib" stdlib of
                                Left err -> putStr (errorBundlePretty err) >> return (Just (currentEnv, maybeRedir))
                                Right defs -> do
                                    mapM_ (\(i, e) -> toCExpr e >>= c_register_global cpu (fromIntegral i)) defs
                                    putStrLn' $ "loaded " ++ show (length defs) ++ " definitions from standard library"
                                    return (Just (IM.union currentEnv (IM.fromList defs), maybeRedir))
                        Right (CmdAssign i val) -> do
                            toCExpr val >>= c_register_global cpu (fromIntegral i)
                            putStrLn' $ "defined " ++ getInternedName i
                            return (Just (IM.insert i val currentEnv, maybeRedir))

                        Right CmdNOP -> return (Just (currentEnv, maybeRedir))
                        Right (CmdDebug e) -> do
                            let loopSteps expr = do
                                    putStrLn' $ "-> " ++ show expr
                                    case step currentEnv expr of
                                        Nothing -> return ()
                                        Just expr' -> loopSteps expr'
                            loopSteps e
                            return (Just (currentEnv, maybeRedir))
                        Right (CmdChurch e) -> do
                            ce <- toCExpr e
                            out <- liftIO $ evalWithInterrupt cpu $ do
                                res <- c_eval cpu ce
                                cstr <- c_decode cpu res
                                s <- peekCString cstr
                                c_free cstr
                                return s
                            putStrLn' out
                            return (Just (currentEnv, maybeRedir))
                        Right (CmdExpr e) -> do
                            ce <- toCExpr e
                            out <- liftIO $ evalWithInterrupt cpu $ do
                                res <- c_eval cpu ce
                                cstr <- c_quote cpu res
                                s <- peekCString cstr
                                c_free cstr
                                return s
                            
                            noColor <- liftIO $ lookupEnv "NO_COLOR"
                            let (lb, rb) = case (noColor, maybeRedir) of
                                             (Just _, _) -> ("[", "]")
                                             (_, Just _) -> ("[", "]")
                                             _           -> ("\ESC[31m[\ESC[0m", "\ESC[31m]\ESC[0m")
                            
                            let expandedAst = expand currentEnv e
                            putStrLn' $ out ++ " " ++ lb ++ show expandedAst ++ rb
                            return (Just (currentEnv, maybeRedir))

                case result of
                    Left (e :: E.SomeException) -> do
                        liftIO $ putStrLn $ "error: " ++ show e
                        loop cpu currentEnv maybeRedir
                    Right (Just (nextEnv, nextRedir)) -> loop cpu nextEnv nextRedir
                    Right Nothing -> return ()

main :: IO ()
main = runREPL
