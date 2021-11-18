module WhileC (compile, main) where

import System.Environment
import System.Exit(exitSuccess, exitWith, ExitCode(ExitFailure))

import WhileAST
import BamAST
import qualified Parser(parse)

aritToInstrs :: AExpr -> Code -> Code
aritToInstrs (EVar x) = [IFetch x]
aritToInstrs (EInt n) = [IPush n]
aritToInstrs (AAdd op a b) = [aritToInstrs a, aritToInstrs b, IAdd]
aritToInstrs (ASub op a b) = [aritToInstrs a, aritToInstrs b, ISub]
aritToInstrs (AMul op a b) = [aritToInstrs a, aritToInstrs b, IMul]

boolToInstrs :: BExpr -> Code -> Code
boolToInstrs (BBool x) = [IFetch x]
boolToInstrs (BNot x) = [boolToInstrs x, INot]
boolToInstrs (BAnd a b) = [boolToInstrs a, boolToInstrs b, IAnd]
boolToInstrs (AEq a b) = [boolToInstrs a, boolToInstrs b, IEq]
boolToInstrs (ALe a b) = [boolToInstrs a, boolToInstrs b, ILe]

boolToCode :: BExpr -> Code
boolToCode b = boolToInstrs b []

stmtToInstrs :: Stmt -> Code -> Code
stmtToInstrs SSkip = []
stmtToInstrs (SAssign v e) = [aritToInstrs e, IStore v]
stmtToInstrs (SIte e a b) = [boolToInstrs e, IBranch a b]
stmtToInstrs (SWhile c s) = [ILoop c s]
stmtToInstrs (SSeq a b) = [a, b]

compile :: Stmt -> Code
compile s = stmtToInstrs s []

-- Reads the file named on the command line, then parse and compile it
-- Output is to stdout, redirect it to save
main :: IO ()
main = do
  a <- getArgs
  s <- parse a
  t <- case Parser.parse s of
         Right t  -> return t
         Left err -> putStrLn ("Parse error: " ++ err) >> die 2
  putStrLn (showCode True 0 (compile t))

parse :: [String] -> IO String
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse [f]    = readFile f
parse _      = usage   >> die 1

usage :: IO ()
usage   = putStrLn "Usage: whilec [-vh] [file]"

version :: IO ()
version = putStrLn "Haskell whilec 0.1"

exit :: IO a
exit    = exitSuccess

die :: Int -> IO a
die i   = exitWith (ExitFailure i)

