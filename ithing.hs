{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

import System.Environment (getArgs, lookupEnv)
import System.IO (hFlush, stdout, isEOF)
import qualified Control.Exception as E
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

import Read (readCompileTimeFile)

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

alphaEq :: Expr -> Expr -> Bool
alphaEq e1 e2 = go IM.empty IM.empty e1 e2
  where
    go m1 m2 (Var i) (Var j) =
        case (IM.lookup i m1, IM.lookup j m2) of
            (Just a, Just b) -> a == b
            (Nothing, Nothing) -> i == j
            _ -> False
    go m1 m2 (Lam i b1) (Lam j b2) =
        let n = IM.size m1
        in go (IM.insert i n m1) (IM.insert j n m2) b1 b2
    go m1 m2 (App f1 a1) (App f2 a2) =
        go m1 m2 f1 f2 && go m1 m2 a1 a2
    go _ _ _ _ = False

instance Eq Expr where
    (==) = alphaEq

instance Show Expr where
    show e = go e
      where
        go (Var i) = getInternedName i
        go (Lam i b) = "(\\" ++ getInternedName i ++ " -> " ++ go b ++ ")"
        go (App f a) = "(" ++ go f ++ " " ++ go a ++ ")"

data Value
    = VVar !Int
    | VApp !Value !Value
    | VLam !(Value -> Value)
    | VThunk !(IORef (Maybe Value)) !(IO Value)

force :: Value -> Value
force (VThunk r m) = unsafePerformIO $ do
    mv <- readIORef r
    case mv of
        Just v -> return v
        Nothing -> do
            v <- m
            writeIORef r (Just v)
            return v
force v = v

type Env = IM.IntMap Value

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

-- Global interner state
{-# NOINLINE globalInterner #-}
globalInterner :: IORef InternState
globalInterner = unsafePerformIO $ newIORef emptyIntern

internStr :: String -> Int
internStr s = unsafePerformIO $ atomicModifyIORef' globalInterner (\st -> let (i, st') = intern s st in (st', i))

getInternedName :: Int -> String
getInternedName i
    | i < 0 = "x" ++ show (-(i + 1))
    | otherwise = unsafePerformIO $ do
        (_, _, rm) <- readIORef globalInterner
        case IM.lookup i rm of
            Just s -> return s
            Nothing -> return ("v" ++ show i)

evalHOAS :: Env -> Expr -> Value
evalHOAS env (Var i) = case IM.lookup i env of
    Just v -> force v
    Nothing -> VVar i
evalHOAS env (Lam i b) = VLam $ \a -> evalHOAS (IM.insert i a env) b
evalHOAS env (App f a) = apply (evalHOAS env f) (evalHOAS env a)
  where
    apply f' arg = case force f' of
        VLam g -> g arg
        f'' -> VApp f'' arg

mkVEnv :: [(Int, Expr)] -> Env
mkVEnv defs =
    let globals = IM.fromList [ (i, mkThunk i v) | (i, v) <- defs ]
        mkThunk i v = unsafePerformIO $ do
            r <- newIORef Nothing
            return $ VThunk r (return $ evalHOAS globals v)
    in globals

quote :: Int -> Value -> Expr
quote l v = case force v of
    VVar i -> Var i
    VApp f a -> App (quote l f) (quote l a)
    VLam f ->
        let i = -l - 1
        in Lam i (quote (l + 1) (f (VVar i)))

normalize :: Env -> Expr -> Expr
normalize env e = quote 0 (evalHOAS env e)

expand :: Env -> Expr -> Expr
expand env (Var i) = case IM.lookup i env of
    Just v -> quote 0 v
    Nothing -> Var i
expand env (Lam i b) =
    let name = getInternedName i ++ "'"
        i' = internStr name
    in Lam i' (expand (IM.insert i (VVar i') env) b)
expand env (App f a) = App (expand env f) (expand env a)

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

step :: [(Int, Expr)] -> Expr -> Maybe Expr
step env (Var i) = lookup i env
step env (App f a) =
    case f of
        Lam x b -> Just (subst x a b)
        _ -> case step env f of
                Just f' -> Just (App f' a)
                Nothing -> App f <$> step env a
step env (Lam i b) = Lam i <$> step env b

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

isChurchNumeralVal :: Value -> Maybe Int
isChurchNumeralVal v = case force v of
    VLam f -> case force (f (VVar (-1))) of
        VLam g -> go 0 (g (VVar (-2)))
          where
            go !acc v' = case force v' of
                VVar (-2) -> Just acc
                VApp (VVar (-1)) arg -> go (acc + 1) arg
                _ -> Nothing
        _ -> Nothing
    _ -> Nothing

isChurchBoolVal :: Value -> Maybe Bool
isChurchBoolVal v = case force v of
    VLam f -> case force (f (VVar (-1))) of
        VLam g -> case force (g (VVar (-2))) of
            VVar i | i == -1 -> Just True
                   | i == -2 -> Just False
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

isChurchListVal :: Value -> Maybe [Value]
isChurchListVal v = case force v of
    VLam f -> case force (f (VVar (-1))) of
        VLam g -> go [] (g (VVar (-2)))
          where
            go !acc v' = case force v' of
                VVar (-2) -> Just (reverse acc)
                VApp (VApp (VVar (-1)) x) rest -> go (x:acc) rest
                _ -> Nothing
        _ -> Nothing
    _ -> Nothing

decodeVal :: Value -> String
decodeVal v
    | Just n <- isChurchNumeralVal v = show n
    | Just b <- isChurchBoolVal v = show b
    | Just l <- isChurchListVal v = "[" ++ intercalate ", " (map decodeVal l) ++ "]"
    | otherwise = show (quote 0 v)

runFile :: String -> IO ()
runFile filename = do
    bs <- BL.readFile filename
    let code = BL.unpack bs
    case parse fileParser filename code of
        Left err -> putStr (errorBundlePretty err)
        Right defs -> do
            let mainId = internStr "main"
            case lookup mainId defs of
                Nothing -> putStrLn "execution error: no main definition found in the script\ntry using :l in interactive mode instead"
                Just mainExpr -> do
                    let vEnv = mkVEnv (filter ((/= mainId) . fst) defs)
                    let res = evalHOAS vEnv mainExpr
                    putStrLn $ decodeVal res

introText :: String
introText = "interpretthing, Copyright (C)  2026 water2137\n\
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
\h - hmmm idk what this does\n\
\d - dump definitions\n\
\L - license details\n\
\D [expr] - debug, shows every reduction step\n\
\C [expr] - church to human readable\n\
\example code: a = \\x -> x x\n\
\              a a\n"

runREPL :: IO ()
runREPL = do
    putStrLn introText
    runInputT defaultSettings $ withInterrupt $ loop [] Nothing
  where
    loop currentEnv maybeRedir = handleInterrupt (loop currentEnv maybeRedir) $ do
        minput <- getInputLine "> "
        case minput of
            Nothing -> return () -- Ctrl-D
            Just "" -> loop currentEnv maybeRedir
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
                            if null currentEnv
                                then putStrLn' "nothing defined"
                                else mapM_ (\(i, expr) -> putStrLn' $ getInternedName i ++ " = " ++ show expr) (reverse currentEnv)
                            return (Just (currentEnv, maybeRedir))
                        Right CmdShowLicense -> do
                            putStr' licenseText
                            return (Just (currentEnv, maybeRedir))
                        Right (CmdShowHelp) -> do
                            putStrLn helpText
                            return (Just (currentEnv, maybeRedir))
                        Right (CmdCD path) -> do
                            setCurrentDirectory path
                            return (Just (currentEnv, maybeRedir))
                        Right CmdClear -> do
                            putStrLn "cleared all definitions"
                            return (Just ([], maybeRedir))
                        Right (CmdShell cmd) -> do
                            _ <- system cmd
                            return (Just (currentEnv, maybeRedir))
                        Right (CmdLoad path) -> do
                            content <- BL.readFile path
                            case parse fileParser path (BL.unpack content) of
                                Left err -> putStr (errorBundlePretty err) >> return (Just (currentEnv, maybeRedir))
                                Right defs -> do
                                    putStrLn' $ "loaded " ++ show (length defs) ++ " definitions from " ++ path
                                    return (Just (reverse defs ++ currentEnv, maybeRedir))
                        Right (CmdSave path) -> do
                            let content = concatMap (\(i, expr) -> getInternedName i ++ " = " ++ show expr ++ "\n") (reverse currentEnv)
                            writeFile path content
                            putStrLn' $ "saved " ++ show (length currentEnv) ++ " definitions to " ++ path
                            return (Just (currentEnv, maybeRedir))
                        Right CmdLoadStd -> do
                            case parse fileParser "stdlib" stdlib of
                                Left err -> putStr (errorBundlePretty err) >> return (Just (currentEnv, maybeRedir))
                                Right defs -> do
                                    putStrLn' $ "loaded " ++ show (length defs) ++ " definitions from standard library"
                                    return (Just (reverse defs ++ currentEnv, maybeRedir))
                        Right (CmdAssign i val) -> do
                            putStrLn' $ "defined " ++ getInternedName i
                            return (Just ((i, val) : currentEnv, maybeRedir))
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
                            let vEnv = mkVEnv (reverse currentEnv)
                            let valResult = evalHOAS vEnv e
                            putStrLn' $ decodeVal valResult
                            return (Just (currentEnv, maybeRedir))
                        Right (CmdExpr e) -> do
                            let vEnv = mkVEnv (reverse currentEnv)
                            let valResult = evalHOAS vEnv e
                            let normalizedResult = quote 0 valResult
                            let expandedAst = expand vEnv e
                            noColor <- liftIO $ lookupEnv "NO_COLOR"
                            let (lb, rb) = case (noColor, maybeRedir) of
                                             (Just _, _) -> ("[", "]")
                                             (_, Just _) -> ("[", "]")
                                             _           -> ("\ESC[31m[\ESC[0m", "\ESC[31m]\ESC[0m")
                            putStrLn' $ show normalizedResult ++ " " ++ lb ++ show expandedAst ++ rb
                            return (Just (currentEnv, maybeRedir))

                case result of
                    Left (e :: E.SomeException) -> do
                        liftIO $ putStrLn $ "error: " ++ show e
                        loop currentEnv maybeRedir
                    Right (Just (nextEnv, nextRedir)) -> loop nextEnv nextRedir
                    Right Nothing -> return ()

main :: IO ()
main = do
    args <- getArgs
    if null args
        then runREPL
        else runFile (head args)
