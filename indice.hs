import System.IO
import Data.List

type Word' = String
type Line = String
type Doc = String

data Tree = Node Word' [Int] Tree Tree | Leaf deriving Show

numLines :: [Line] -> [(Int,Line)]
numLines' :: [Line] -> Int -> [(Int,Line)]
-- ***********************************************************
allNumWords :: [(Int,Line)] -> [(Int,Word')]
allNumWords' :: (Int, [Word']) -> [(Int, Word')]
-- ***********************************************************
insOrd :: Int -> [Int] -> [Int]
-- ***********************************************************
ocorrenciaElem :: Int -> [Int] -> Bool
insArvore :: (Word', Int) -> Tree -> Tree
-- ***********************************************************
inverte :: [(Int,Word')] -> [(Word', Int)]
mIndexTree :: Int -> [(Word', Int)] -> Tree -> Tree
mIndexTree' :: Doc -> Tree
-- ***********************************************************
impArvore :: Tree -> IO ()
-- ***********************************************************
main :: IO ()



numLines (x:xs) = numLines' (x:xs) 1
numLines' [] _ = []
numLines' (x:xs) n = (n,x) : numLines' xs (n+1)

-- ***********************************************************

allNumWords [] = []
allNumWords ((l,y:ys):xs) = allNumWords' (l, words (y:ys)) ++ allNumWords (xs)

allNumWords' (_ , []) = [] 
allNumWords' (l , y:ys) = (l, y) : allNumWords' (l, ys)

-- ***********************************************************

insOrd e [] = [e]
insOrd e (x:xs) = if e < x then e:x:xs else x:insOrd e xs

-- ***********************************************************

ocorrenciaElem _ [] = False
ocorrenciaElem n (x:xs) | n == x = True
                        | otherwise = ocorrenciaElem n xs

insArvore (p, l) Leaf = Node p [l] Leaf Leaf
insArvore (p, l) (Node p' is esq dir) | p == p' && ocorrenciaElem l is == False = Node p' (insOrd l is) esq dir
                                      | p == p' && ocorrenciaElem l is == True = Node p' is esq dir
                                      | p < p' = Node p' is (insArvore (p, l) esq) dir
                                      | otherwise = Node p' is esq (insArvore (p, l) dir)

-- ***********************************************************

inverte [] = []
inverte ((l,p):cauda) = (p,l) : inverte cauda

mIndexTree _ [(p,l)] tree = insArvore (p, l) (tree)
mIndexTree n ((p,l):cauda) tree | n == 0 = mIndexTree (n+1) (cauda) (Node p [l] Leaf Leaf)
                                | otherwise = mIndexTree n (cauda) (insArvore (p, l) (tree))         

mIndexTree' txt = (mIndexTree 0 (inverte(allNumWords(numLines(lines txt)))) Leaf)

-- ***********************************************************

impArvore Leaf = putStr "\n"
impArvore (Node p is esq dir) = do impArvore esq
                                   putStr p
                                   putStr "-"
                                   print is
                                   putStr "\n"
                                   impArvore dir

-- ***********************************************************

main = do putStr "Arquivo:"
          hFlush stdout
          n <- getLine
          txt <- readFile n
          let x = mIndexTree' (txt)
          impArvore x
