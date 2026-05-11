{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.List (nub, (\\), intercalate)
import qualified Data.Set as Set

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)

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
    = Var String
    | Lam String Expr
    | App Expr Expr

alphaEq :: Expr -> Expr -> Bool
alphaEq e1 e2 = go [] [] e1 e2
  where
    go env1 env2 (Var x) (Var y) =
        case (lookup x env1, lookup y env2) of
            (Just i, Just j) -> i == j
            (Nothing, Nothing) -> x == y
            _ -> False
    go env1 env2 (Lam x b1) (Lam y b2) =
        let n = length env1
        in go ((x, n) : env1) ((y, n) : env2) b1 b2
    go env1 env2 (App f1 a1) (App f2 a2) =
        go env1 env2 f1 f2 && go env1 env2 a1 a2
    go _ _ _ _ = False

instance Eq Expr where
    (==) = alphaEq

instance Show Expr where
    show (Var x) = x
    show (Lam x b) = "(\\" ++ x ++ " -> " ++ show b ++ ")"
    show (App f a) = "(" ++ show f ++ " " ++ show a ++ ")"

data Value
    = VVar String
    | VApp Value Value
    | VLam String Expr VEnv
    | VThunk Expr VEnv

type VEnv = [(String, Value)]

mkVEnv :: [(String, Expr)] -> VEnv
mkVEnv = foldl (\env (k, v) -> (k, VThunk v env) : env) []

eval :: VEnv -> Expr -> Value
eval env (Var x) = case lookup x env of
    Just v -> force v
    Nothing -> VVar x
  where
    force (VThunk e env') = eval env' e
    force v = v
eval env (Lam x b) = VLam x b env
eval env (App f a) = case eval env f of
    VLam x b env' -> eval ((x, VThunk a env) : env') b
    f' -> VApp f' (VThunk a env)

quote :: Int -> Value -> Expr
quote l (VVar x) = Var x
quote l (VApp f a) = App (quote l f) (quote l a)
quote l (VLam x b env) =
    let x' = x ++ show l
    in Lam x' (quote (l + 1) (eval ((x, VVar x') : env) b))
quote l (VThunk e env) = quote l (eval env e)

expand :: VEnv -> Expr -> Expr
expand env (Var x) = case lookup x env of
    Just (VVar y) -> Var y
    Just (VThunk e env') -> expand env' e
    _ -> Var x
expand env (Lam x b) =
    let x' = x ++ "'" -- simple freshening for expand
    in Lam x' (expand ((x, VVar x') : env) b)
expand env (App f a) = App (expand env f) (expand env a)

-- Pure reduction to WHNF
reduce :: VEnv -> Expr -> Value
reduce = eval

normalize :: VEnv -> Expr -> Expr
normalize env e = quote 0 (eval env e)

freeVars :: Expr -> Set.Set String
freeVars (Var x) = Set.singleton x
freeVars (Lam x b) = Set.delete x (freeVars b)
freeVars (App f a) = Set.union (freeVars f) (freeVars a)

fresh :: Set.Set String -> String -> String
fresh used x | x `Set.notMember` used = x
             | otherwise = fresh used (x ++ "'")

subst :: String -> Expr -> Expr -> Expr
subst x s (Var y) | x == y = s
                  | otherwise = Var y
subst x s (Lam y b)
    | x == y = Lam y b
    | y `Set.member` freeVars s =
        let y' = fresh (freeVars s `Set.union` freeVars b) y
        in Lam y' (subst x s (subst y (Var y') b))
    | otherwise = Lam y (subst x s b)
subst x s (App f a) = App (subst x s f) (subst x s a)

step :: [(String, Expr)] -> Expr -> Maybe Expr
step env (Var x) = lookup x env
step env (App f a) =
    case f of
        Lam x b -> Just (subst x a b)
        _ -> case step env f of
                Just f' -> Just (App f' a)
                Nothing -> App f <$> step env a
step env (Lam x b) = Lam x <$> step env b

lexer = Tok.makeTokenParser emptyDef
    { Tok.reservedOpNames = ["->", "\\", "="]
    , Tok.reservedNames   = ["->", "\\", "="]
    , Tok.commentLine     = "--"
    , Tok.identStart      = alphaNum <|> oneOf "!#$%&*+./<=>?@^|-~_"
    , Tok.identLetter     = alphaNum <|> oneOf "!#$%&*+./<=>?@^|-~_'"
    }
identifier = Tok.identifier lexer
parens     = Tok.parens lexer
reservedOp = Tok.reservedOp lexer
whiteSpace = Tok.whiteSpace lexer

atom :: Parser Expr
atom = parens expr
   <|> lam
   <|> (try (Var <$> identifier <* notFollowedBy (reservedOp "=")))

lam :: Parser Expr
lam = do
    reservedOp "\\"
    vars <- many1 identifier
    reservedOp "->"
    body <- expr
    return $ foldr Lam body vars

app :: Parser Expr
app = do
    es <- many1 atom
    return $ foldl1 App es

expr :: Parser Expr
expr = app

assignment :: Parser (String, Expr)
assignment = do
    name <- identifier
    reservedOp "="
    val <- expr
    return (name, val)

fileParser :: Parser [(String, Expr)]
fileParser = do
    whiteSpace
    defs <- many (try assignment)
    eof
    return defs

data ReplCmd = CmdAssign String Expr | CmdExpr Expr | CmdLoad String | CmdSave String | CmdLoadStd | CmdClear | CmdCD String | CmdShell String | CmdShowHelp | CmdShowDefs | CmdShowLicense | CmdDebug Expr | CmdChurch Expr | CmdRedirect String | CmdStopRedirect | CmdNOP

replParser :: Parser ReplCmd
replParser = do
    whiteSpace
    (try (do char ':'; char 'l'; whiteSpace; path <- many1 (satisfy (not . isSpace)); whiteSpace; eof; return $ CmdLoad path)
     <|> try (do char ':'; char 's'; whiteSpace; path <- many1 (satisfy (not . isSpace)); whiteSpace; eof; return $ CmdSave path)
     <|> try (do char ':'; char 'r'; whiteSpace; path <- many1 (satisfy (not . isSpace)); whiteSpace; eof; return $ CmdRedirect path)
     <|> try (do char ':'; char 'r'; whiteSpace; eof; return CmdStopRedirect)
     <|> try (do char ':'; char 'S'; whiteSpace; eof; return CmdLoadStd)
     <|> try (do char ':'; char '!'; whiteSpace; cmd <- many anyChar; eof; return $ CmdShell cmd)
     <|> try (do char ':'; char 'c'; char 'd'; whiteSpace; path <- many1 (satisfy (not . isSpace)); whiteSpace; eof; return $ CmdCD path)
     <|> try (do char ':'; char 'c'; whiteSpace; eof; return CmdClear)
     <|> try (do char ':'; char 'h'; whiteSpace; eof; return CmdShowHelp)
     <|> try (do char ':'; char 'd'; whiteSpace; eof; return CmdShowDefs)
     <|> try (do char ':'; char 'L'; whiteSpace; eof; return CmdShowLicense)
     <|> try (do char ':'; char 'D'; whiteSpace; e <- expr; whiteSpace; eof; return $ CmdDebug e)
     <|> try (do char ':'; char 'C'; whiteSpace; e <- expr; whiteSpace; eof; return $ CmdChurch e)
     <|> try (do (n, v) <- assignment; whiteSpace; eof; return $ CmdAssign n v)
     <|> try (do e <- expr; whiteSpace; eof; return $ CmdExpr e)
     <|> (eof >> return CmdNOP))

isChurchNumeral :: Expr -> Maybe Int
isChurchNumeral (Lam f (Lam x body)) | f /= x = go body
  where
    go (Var v) | v == x = Just 0
    go (App (Var v) arg) | v == f = (+1) <$> go arg
    go _ = Nothing
isChurchNumeral _ = Nothing

isChurchBool :: Expr -> Maybe Bool
isChurchBool (Lam t (Lam f' body)) | t /= f' = case body of
    Var v | v == t -> Just True
          | v == f' -> Just False
    _ -> Nothing
isChurchBool _ = Nothing

isChurchList :: Expr -> Maybe [Expr]
isChurchList (Lam c (Lam n body)) | c /= n = go body
  where
    go (Var v) | v == n = Just []
    go (App (App (Var v) x) rest) | v == c = (x:) <$> go rest
    go _ = Nothing
isChurchList _ = Nothing

decode :: Expr -> String
decode e
    | Just n <- isChurchNumeral e = show n
    | Just b <- isChurchBool e = show b
    | Just l <- isChurchList e = "[" ++ intercalate ", " (map decode l) ++ "]"
    | otherwise = case e of
        Var x -> x
        Lam x b -> "(\\" ++ x ++ " -> " ++ decode b ++ ")"
        App f a -> "(" ++ decode f ++ " " ++ decode a ++ ")"

runFile :: String -> IO ()
runFile filename = do
    bs <- BL.readFile filename
    let code = BL.unpack bs
    case parse fileParser filename code of
        Left err -> print err
        Right defs -> do
            case lookup "main" defs of
                Nothing -> putStrLn "execution error: no main definition found in the script\ntry using :l in interactive mode instead"
                Just mainExpr -> do
                    let vEnv = mkVEnv (filter ((/= "main") . fst) defs)
                    let res = normalize vEnv mainExpr
                    putStrLn $ show res

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
                        Left err -> print err >> return (Just (currentEnv, maybeRedir))
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
                                else mapM_ (\(name, expr) -> putStrLn' $ name ++ " = " ++ show expr) (reverse currentEnv)
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
                                Left err -> print err >> return (Just (currentEnv, maybeRedir))
                                Right defs -> do
                                    putStrLn' $ "loaded " ++ show (length defs) ++ " definitions from " ++ path
                                    return (Just (reverse defs ++ currentEnv, maybeRedir))
                        Right (CmdSave path) -> do
                            let content = concatMap (\(name, expr) -> name ++ " = " ++ show expr ++ "\n") (reverse currentEnv)
                            writeFile path content
                            putStrLn' $ "saved " ++ show (length currentEnv) ++ " definitions to " ++ path
                            return (Just (currentEnv, maybeRedir))
                        Right CmdLoadStd -> do
                            case parse fileParser "stdlib" stdlib of
                                Left err -> print err >> return (Just (currentEnv, maybeRedir))
                                Right defs -> do
                                    putStrLn' $ "loaded " ++ show (length defs) ++ " definitions from standard library"
                                    return (Just (reverse defs ++ currentEnv, maybeRedir))
                        Right (CmdAssign name val) -> do
                            putStrLn' $ "defined " ++ name
                            return (Just ((name, val) : currentEnv, maybeRedir))
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
                            let normalizedResult = normalize vEnv e
                            putStrLn' $ decode normalizedResult
                            return (Just (currentEnv, maybeRedir))
                        Right (CmdExpr e) -> do
                            let vEnv = mkVEnv (reverse currentEnv)
                            let normalizedResult = normalize vEnv e
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
