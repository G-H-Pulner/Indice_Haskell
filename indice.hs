import System.IO

numLines :: [String] -> [(Int,String)]
numLines' :: [String] -> Int -> [(Int,String)]

menor :: [(Int, String)] -> (Int, String)

almalgamate' :: String -> [(Int,String)] -> [(Int,String)]
tuplaLista :: [(Int,String)] -> ([Int], String)
junta :: [(Int, String)] -> [([Int], String)]

xoxo' :: ([Int], String) -> ([Int], String)
juntaTudo :: [([Int], String)] -> [([Int], String)]

-- ***********************************************************

numLines (x:xs) = numLines' (x:xs) 1
numLines' [] _ = []
numLines' (x:xs) n = (n,x) : numLines' xs (n+1)

-- ***********************************************************

allNumWords [] = []
allNumWords ((l,y:ys):xs) = allNumWords' (l, words (y:ys)) ++ allNumWords (xs)

allNumWords' (_ , []) = [] 
allNumWords' (l , y:ys) = (l, y) : allNumWords' (l, ys)

segundo (y, z) = z
primeiro (y, z) = y

menor [(n, x)] = (n, x)
menor ((n, x):ys) = if x <= segundo (ns, xs) then (n, x) else menor ys
  where
    (ns, xs) = menor ys

removerElem _ [] = []
removerElem n ((x, y):xys) | n /= y = (x, y): removerElem n (xys)
                           | otherwise = xys

ordenar [] = []
ordenar ((x, y):xys) = o: ordenar (removerElem (segundo o) ((x, y):xys))
    where 
        o = menor ((x, y):xys)

sortLs ((x, y):xys) = ordenar ((x, y):xys)

-- ***********************************************************

proximo _ [(x, y)] = (0, y) 
proximo cont ((x, y):xys) = if cont == 0 then proximo (cont+1) xys else (x, y)

almalgamate' _ [] = []
almalgamate' p ((n, x):ys) = if (x == p) then (n, x) : almalgamate' p ys else almalgamate' p ys
    where
        ns = primeiro(proximo 0 ((n, x):ys)) 

tuplaLista [(x, y)] = ([x], y)
tuplaLista ((x,y):xys) = (x:xs, y)
  where
    (xs, y) = tuplaLista xys

elemOco _ [] = []
elemOco l ((x,y):xys) = if l == y then elemOco l xys else (x,y):elemOco l xys

junta [] = []
junta ((x,y):xys) = (tuplaLista(almalgamate' y ((x,y):xys))) : (junta(elemOco y ((x,y):xys)))

-- ***********************************************************

xixi _ _ [] = []
xixi cont n (x:xs) = if n == x then (if cont > 0 then x: xixi (cont-1) n xs else xixi cont n xs) else x: xixi cont n xs

xoxo y (x:xs) = (xs, y)

listaSR ([], y) = []
listaSR ((x:xs), y) = x:listaSR (xoxo y (xixi 1 x (x:xs)))

xoxo' ((x:xs),y) = ((listaSR((x:xs),y)),y) 

elemOco' _ [] = []
elemOco' l (((x:xs),y):xys) = if l == y then elemOco l xys else ((x:xs),y) : elemOco' l xys

juntaTudo [] = []
juntaTudo (((x:xs),y):xys) = (xoxo' ((x:xs),y)) : (juntaTudo(elemOco' y (((x:xs),y):xys)))

-- ***********************************************************

makeIndex txt = (juntaTudo(junta(sortLs(allNumWords (numLines (lines txt))))))

main = do putStr "Arquivo:"
          hFlush stdout
          n <- getLine
          txt <- readFile n
          print (makeIndex(txt))
