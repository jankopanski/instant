module Main where


import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath --(takeDirectory, takeBaseName)
import System.Process (callCommand)

import Data.List
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except

import ParInstant
import PrintInstant
import AbsInstant

import ErrM


type Verbosity = Int
type Instructions = [String]
type Variables = [String]

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> FilePath -> IO ()
runFile v f = putStrLn f >> readFile f >>= run v f

run :: Verbosity -> FilePath -> String -> IO ()
run v f s = let ts = myLexer s in case pProgram ts of
           Bad e    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn e
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree
                          -- code <- compile tree f
                          -- generateJFile f code
                          -- generateClassFile f
                          exitSuccess

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (files)         Parse content of files silently."
    , "  -v (files)      Parse content of files verbosely."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> putStrLn "No file specified!" >> usage
    "-v":fs -> mapM_ (runFile 2) fs
    fs -> mapM_ (runFile 0) fs

type GenM a = WriterT [String] (ExceptT String (State (Integer, Map.Map String Address))) a
data Address = Register Integer | Immediate Integer
type Instruction = String
type Operation = String

instance Show Address where
  show (Register i) = '%' : show i
  show (Immediate i) = show i

genCode :: [Instruction] -> [Instruction]
genCode ins = beginning ++ ins ++ ending where
  beginning = ["@dnl = internal constant [4 x i8] c\"%d\0A\00\"",
    "declare i32 @printf(i8*, ...)", "define void @printInt(i32 %x) {",
    "%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0",
    "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)", "ret void", "}", "",
    "define i32 @main() {"]
  ending = ["ret i32 0", "}"]

compile :: Program -> IO [Instruction]
compile program =
  case evalState (runExceptT (execWriterT (genProgram program))) (1, Map.empty) of
    Left err -> putStrLn err >> exitFailure
    Right ins -> do
      putStrLn "Compile Successful"
      return $ genCode ins

genProgram :: Program -> GenM ()
genProgram (Prog stmts) = mapM_ genStmt stmts

genStmt :: Stmt -> GenM ()
genStmt (SExp expr) = do
  addr <- genExp expr
  emit $ "call void @printInt(i32 " ++ show addr ++ ")"

genStmt (SAss (Ident name) expr) = do
  src <- genExp expr
  (n, vars) <- get
  case Map.lookup name vars of
    Just dest -> emit $ store src dest
    Nothing -> do
      dest <- freshTemp
      put (n, Map.insert name dest vars)
      emit $ show dest ++ " alloca i32"
      emit $ store src dest

genExp :: Exp -> GenM Address
genExp (ExpLit i) = return $ Immediate i
genExp (ExpAdd exp1 exp2) = genBinOp "add" exp1 exp2
genExp (ExpSub exp1 exp2) = genBinOp "sub" exp1 exp2
genExp (ExpMul exp1 exp2) = genBinOp "mul" exp1 exp2
genExp (ExpDiv exp1 exp2) = genBinOp "div" exp1 exp2
genExp (ExpVar (Ident name)) = do
  addr <- getAddr name
  temp <- freshTemp
  emit $ load temp addr
  return temp

freshTemp :: GenM Address
freshTemp = do
  (n, v) <- get
  put (n + 1, v)
  return $ Register n

getAddr :: String -> GenM Address
getAddr name = do
  (_, vars) <- get
  case Map.lookup name vars of
    Nothing -> throwError ("Reference to undefined variable: " ++ name)
    Just addr -> return addr

load :: Address -> Address -> Instruction
load temp addr = show temp ++ " = load i32, i32* " ++ show addr

store :: Address -> Address -> Instruction
store src dest = "store i32 " ++ show src ++ ", i32* " ++ show dest

emit :: Instruction -> GenM ()
emit ins = tell [ins]
-- %1 = sub i32 44, 2

iBinOp :: Address -> Operation -> Address -> Address -> Instruction
iBinOp temp op addr1 addr2 =
  show temp ++ " = " ++ op ++ " i32 " ++ show addr1 ++ ", " ++ show addr2

genBinOp :: Operation -> Exp -> Exp -> GenM Address
genBinOp op exp1 exp2 = do
  addr1 <- genExp exp1
  addr2 <- genExp exp2
  temp <- freshTemp
  emit $ iBinOp temp op addr1 addr2
  return temp
