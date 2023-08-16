somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

multDois :: [Int] -> [Int]
multDois [] = [] 
multDois (x:xs) = 2*x : multDois xs 

-- Ex01
multLista :: Int -> [Int] -> [Int]
multLista x [] = []
multLista x (y:ys) = x*y : multLista x ys 

-- Ex02
elemento :: Int -> [Int] -> Bool
elemento x [] = False
elemento x (y:ys) 
    | (x == y) = True
    | otherwise = elemento x ys

-- Ex02.1
notElemento :: Int -> [Int] -> Bool
notElemento x [] = True
notElemento x (y:ys) 
    | (x == y) = False
    | otherwise = notElemento x ys

-- Ex03
conta :: Int -> [Int] -> Int
conta x [] = 0
conta x (y:ys) 
    | (x == y) = 1 + conta x ys
    | otherwise = conta x ys 
 
-- Ex04
contaMaiores :: Int -> [Int] -> Int
contaMaiores x [] = 0
contaMaiores x (y:ys) 
    | (x < y) = 1 + contaMaiores x ys
    | otherwise = contaMaiores x ys

-- Ex05 
maiores :: Int -> [Int] -> [Int]
maiores x [] = []
maiores x (y:ys)
    | (x < y) = y : maiores x ys
    | otherwise = maiores x ys

-- Ex06
geraLista :: Int -> Int -> [Int]
geraLista 0 n = []
geraLista m n = n : geraLista (m-1) n

-- Ex07
addFim :: Int -> [Int] -> [Int]
addFim x [] = [x]
addFim x (y:ys) = y : addFim x ys

-- Ex07.1
addFimDebois :: Int -> [Int] -> [Int]
addFimDebois x l = l ++ [x]

inverte :: [Int] -> [Int] 
inverte [] = []
inverte (y:ys) = inverte ys