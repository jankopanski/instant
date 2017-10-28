module Main where


import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )

import ParInstant
import PrintInstant
import AbsInstant

import Data.List
import Control.Monad
import Control.Monad.Except
import System.FilePath --(takeDirectory, takeBaseName)
import System.Process (callCommand)

import ErrM

type Verbosity = Int

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
                          code <- compile tree f
                          generateJFile f code
                          generateClassFile f
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

-- type State = Map.Map String Integer
-- data Instruction = Expr String | Ass String String deriving (Eq, Show)
type Instruction = String

generateCode :: [Instruction] -> FilePath -> Int -> Int -> [Instruction]
generateCode ins name stack locals = [".class public " ++ name] ++ beginning ++
  [".limit stack " ++ show stack, ".limit locals " ++ show locals] ++ ins ++ ending
  where
    beginning = [".super java/lang/Object", "", ".method public <init>()V",
      "aload_0", "invokespecial java/lang/Object/<init>()V", "return",
      ".end method", "", ".method public static main([Ljava/lang/String;)V"]
    ending = ["return", ".end method"]

generateJFile :: FilePath -> [Instruction] -> IO ()
generateJFile file code = writeFile (replaceExtension file "j") (unlines code)

generateClassFile :: FilePath -> IO ()
generateClassFile file = callCommand $ "java -jar lib/jasmin.jar -d " ++
  takeDirectory file ++ " " ++ replaceExtension file "j"

compile :: Program -> FilePath -> IO [Instruction]
-- compile program = mapM_ print (compileProgram program)
compile program filename =
  case runExcept $ compileProgram program of
    Left err -> putStrLn err >> exitFailure
    Right (ins, vars, stack) -> return $ generateCode ins (takeBaseName filename) stack (length vars + 1)
      -- do
      -- putStrLn ""
      -- mapM_ putStrLn ins
      -- print vars
      -- print stack
      -- exitFailure


compileProgram :: Program -> Except String ([Instruction], [String], Int) -- zmianić typ na listę
-- compileProgram (Prog stmts) = mapM compileStmt stmts
-- compileProgram (Prog stmts) = return ["ala"]
compileProgram (Prog stmts) = foldM compileStmtFold ([], [], 0) stmts
  where
    compileStmtFold :: ([Instruction], [String], Int) -> Stmt -> Except String ([Instruction], [String], Int)
    compileStmtFold (ins, vars, m) stmt = do
      (ins', vars', m') <- compileStmt stmt vars
      return (ins ++ ins', vars', max m m')

-- compileStmt :: Stmt -> Except String [Instruction]
compileStmt :: Stmt -> [String] -> Except String ([Instruction], [String], Int)
-- compileStmt (SExp expr) = [Exp $ show expr]
-- compileStmt (SExp expr) = [show expr]
-- compileStmt (SExp expr) = compileExpr expr [""] -- TODO
-- compileStmt (SExp expr) vars = compileExpr expr vars >>= \(ins, m) -> return (ins, vars, m)
compileStmt (SExp expr) vars = do
  (ins, m) <- compileExpr expr vars
  let ins' = ["getstatic java/lang/System/out Ljava/io/PrintStream;"] ++ ins
          ++ ["invokevirtual java/io/PrintStream/println(I)V"]
  return (ins', vars, m + 1)

-- compileStmt (SAss (Ident name) expr) = [Ass name (show expr)]
-- compileStmt (SAss (Ident name) expr) = [(show expr)]
-- compileStmt (SAss (Ident name) expr) = return ([name], 0)
compileStmt (SAss (Ident name) expr) vars = do
  (ins, m) <- compileExpr expr vars
  return $ case elemIndex name vars of
    Nothing -> (ins ++ [istore $ length vars], vars ++ [name], m)
    Just index -> (ins ++ [istore index], vars, m)
    where
      istore index = "istore" ++ if index <= 3 then "_" else " " ++ show index

-- compileExpr :: Exp -> Except String [Instruction]
compileExpr :: Exp -> [String] -> Except String ([Instruction], Int)
compileExpr (ExpAdd exp1 exp2) vars = do
  (ins1, m1) <- compileExpr exp1 vars
  (ins2, m2) <- compileExpr exp2 vars
  return $ if m1 > m2
    then (ins1 ++ ins2 ++ ["iadd"], m2 + 1)
    else (ins2 ++ ins1 ++ ["iadd"], m1 + 1)
  -- return ((if s1 + m1 > m2 then ins1 ++ ins2 else ins2 ++ ins1) ++ ["iadd"], 1, max3 m1 m2 (s1 + s2))
  -- return $ if max1 > max2 then (ins1 ++ ins2 ++ ["iadd"], 1, 1) else (ins2 ++ ins1 ++ ["iadd"], 1, 1)
  -- return (ins2 ++ ins1 ++ ["iadd"])
-- compileExpr exp2 >>= \ins2 -> compileExpr exp1 >>= \ins1 -> return (ins2 ++ ins1)

compileExpr (ExpSub exp1 exp2) vars = do
  (ins1, m1) <- compileExpr exp1 vars
  (ins2, m2) <- compileExpr exp2 vars
  return $ if m1 >= m2
    then (ins1 ++ ins2 ++ ["isub"], m2 + 1)
    else (ins2 ++ ins1 ++ ["swap", "isub"], m1 + 1)
  -- return (ins1 ++ ins2 ++ ["isub"])

compileExpr (ExpMul exp1 exp2) vars = do
  (ins1, m1) <- compileExpr exp1 vars
  (ins2, m2) <- compileExpr exp2 vars
  return $ if m1 >= m2
    then (ins1 ++ ins2 ++ ["imul"], m2 + 1)
    else (ins2 ++ ins1 ++ ["imul"], m1 + 1)

compileExpr (ExpDiv exp1 exp2) vars = do
  (ins1, m1) <- compileExpr exp1 vars
  (ins2, m2) <- compileExpr exp2 vars
  return $ if m1 >= m2
    then (ins1 ++ ins2 ++ ["idiv"], m2 + 1)
    else (ins2 ++ ins1 ++ ["swap", "idiv"], m1 + 1)

compileExpr (ExpLit n) _
  | n < 0 = throwError "Invalid number"
  | n <= 5 = return (["iconst_" ++ show n], 1)
  | n <= 127 = return (["bipush " ++ show n], 1)
  | n <= 32767 = return (["sipush " ++ show n], 1)
  | otherwise = return (["ldc " ++ show n], 1)

compileExpr (ExpVar (Ident name)) vars =
  case elemIndex name vars of
    Nothing -> throwError ("Reference to undefined variable: " ++ name)
    Just index -> return (["iload" ++ if index <= 3 then "_" else " " ++ show index], 1)
