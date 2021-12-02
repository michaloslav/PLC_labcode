module Goedel where

import WhileAST
import Parser (parse)
import Data.Either (fromRight)

encodeBool :: Bool -> Integer
encodeBool False = 0
encodeBool True = 1

decodeBool :: Integer -> Bool
decodeBool x = mod x 2 == 1

delta :: Integer -> Integer
delta x = 1/2 * x * (x + 1)

pairHU :: Integer -> Integer -> Integer
pairHU i j = delta(i+j-2)+i

unpairHU :: Integer -> (Integer, Integer)
unpairHU h = (i, j)
  where
    c	=	floor(sqrt 2h - 1/2)
    i = h-delta(c)
    j = c-i+2

pair :: Integer -> Integer -> Integer
pair i j = pairHU (i + 1, j + 1) - 1

unpair :: Integer -> (Integer, Integer)
unpair x = (i1 - 1, j1 - 1)
  where 
    k = x + 1
    (i1, j1) = unpairHU k


-- Encoding lists of numbers

encodeList :: [Integer] -> Integer
encodeList [] = 0
encodeList (n:ns) = pair n (encodeList ns) + 1

decodeList :: Integer -> [Integer]
decodeList 0 = []
decodeList n = let (x, m) = unpair (n - 1) in (x : decodeList m)


-- Encoding binary trees


data BTree = Empty | Fork Integer BTree BTree
  deriving (Eq, Show)

insert :: Integer -> BTree -> BTree
insert x Empty = Fork x Empty Empty
insert x (Fork n t1 t2) =
  if x <= n then
    Fork n (insert x t1) t2
  else
    Fork n t1 (insert x t2)

tExample :: BTree
tExample = foldr insert Empty [10, 8, 2, 5]

encodeBTree :: BTree -> Integer
encodeBTree Empty = 0
encodeBTree (Fork n t1 t2) = 1 + pair(n, pair(encodeBTree t1, encodeBTree t2))

decodeBTree :: Integer -> BTree
decodeBTree 0 = Empty
encodeBTree x = (n, t1, t2)
  where 
    (n, m) = unpair (x - 1)
    (t1, t2) = unpair m


-- Encoding variables

encodeVar :: String -> Integer
encodeVar ('x':s) = read s
encodeVar _       = error "not a valid variable"

decodeVar :: Integer -> String
decodeVar n = "x" ++ show n

-- Encoding WHILE programs

encodeAExpr :: AExpr -> Integer
encodeAExpr (EVar xs)           =     5 * encodeVar xs
encodeAExpr (EInt n)            = 1 + 5 * n
encodeAExpr (EBinOp AAdd a1 a2) = 2 + 5 * pair (encodeAExpr a1) (encodeAExpr a2)
encodeAExpr (EBinOp ASub a1 a2) = 3 + 5 * pair (encodeAExpr a1) (encodeAExpr a2)
encodeAExpr (EBinOp AMul a1 a2) = 4 + 5 * pair (encodeAExpr a1) (encodeAExpr a2)

decodeAExpr :: Integer -> AExpr
decodeAExpr n = case r of
  0 -> EVar (decodeVar (q / 5))
  1 -> EInt q / 5
  2 -> EBinOp AAdd i j
  3 -> EBinOp ASub i j
  4 -> EBinOp AMul i j
  where
    (q, r) = divMod n 5
    i = decodeAExpr i1
    j = decodeAExpr j1
    (i1, j1) = unpair (q / 5)

encodeBExpr :: BExpr -> Integer
encodeBExpr (BBool x) = 5 * encodeBool x
encodeBExpr (BNot x) = 1 + 5 * encodeBExpr x
encodeBExpr (BAnd a b) = 2 + 5 * pair (encodeBExpr a) (encodeBExpr b)
encodeBExpr (BComp AEq a b) = 3 + 5 * pair (encodeBExpr a) (encodeBExpr b)
encodeBExpr (BComp ALe a b) = 4 + 5 * pair (encodeBExpr a) (encodeBExpr b)

decodeBExpr :: Integer -> BExpr
decodeBExpr n = case r of
  0 -> BBool (encodeBool (q / 5))
  1 -> BNot q / 5
  2 -> BAnd i j
  3 -> BComp AEq i j
  4 -> BComp ALe i j
  where
    (q, r) = divMod n 5
    i = decodeBExpr i1
    j = decodeBExpr j1
    (i1, j1) = unpair (q / 5)

encodeStmt :: Stmt -> Integer
encodeStmt SSkip = 0
encodeStmt (SAssign x a) = 1 + 5 * pair (encodeVar x) (encodeAExpr a)
encodeStmt (SIte b s1 s2) = 2 + 5 * pair (encodeBExpr b) (pair (encodeStmt s1) (encodeStmt s2))
encodeStmt (SWhile b s) = 3 + 5 * pair (encodeBExpr b) (encodeStmt s)
encodeStmt (SSeq s1 s2) = 4 + 5 * pair (encodeStmt s1) (encodeStmt s2)

decodeStmt :: Integer -> Stmt
decodeStmt n = case r of
  0 -> SSkip
  1 -> SAssign (decodeVar i) (decodeAExpr j)
  2 -> SIte (decodeBExpr i) (decodeStmt j1) (decodeStmt j2)
  3 -> SWhile (decodeBExpr i) (decodeStmt j)
  4 -> SSeq (decodeStmt i) (decodeStmt j)
  where
    (q, r) = divMod n 5
    (i, j) = unpair (q / 5)
    (j1, j2) = unpair (j)

astExample1 :: Stmt
astExample1 =
  SSeq
    (SAssign "x1" (EInt 1))
    (SAssign "x2" (EBinOp AAdd (EInt 4) (EInt 2)))

astExample2 :: Stmt
astExample2 =
  fromRight SSkip $ parse $ unlines [
    "x1 := x1 + x2",
    "x1 := x2 - x1"
  ]

astExample3 :: Stmt
astExample3 =
  fromRight SSkip $ parse $ unlines [
    "x1 := x1 + x2",
    "x1 := x2 - x1",
    "x2 := x2 - x1"
  ]

