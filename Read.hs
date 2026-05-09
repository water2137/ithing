module Read where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

readCompileTimeFile :: FilePath -> Q Exp
readCompileTimeFile path = do
    contents <- runIO $ readFile path
    addDependentFile path
    return $ LitE (StringL contents)
