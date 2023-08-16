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

-- Tipos Algébricos
data Temperatura = Frio | Calor
    deriving(Eq, Show)

data Estacao = Verao | Outono | Inverno | Primavera 
    deriving(Eq, Show)

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _ = Frio

data Forma = Circulo Float | Retangulo Float Float 
    deriving(Eq, Show)

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b * a

data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq, Show)

arv1 :: Arvore
arv1 = Nodo 13 (Folha 12) (Nodo 15 (Folha 14) (Folha 16))

arv2 :: Arvore
arv2 = Nodo 1 (Nodo 1 (Folha 1) (Folha 1)) (Nodo 1 (Folha 1) (Folha 1))

arv3 :: Arvore   
arv3 = Folha 1

arv4 :: Arvore
arv4 = Nodo 1 (Nodo 2 (Folha 3) (Folha 4)) (Nodo 5 (Folha 6) (Folha 7))


somaArvore :: Arvore -> Int
somaArvore (Folha n) = n
somaArvore (Nodo n a1 a2) = n + somaArvore a1 + somaArvore a2

multDoisArvore :: Arvore -> Arvore
multDoisArvore (Folha n) = Folha (2*n)
multDoisArvore (Nodo n a1 a2) = Nodo (2*n) (multDoisArvore a1) (multDoisArvore a2)

multArvore :: Int -> Arvore -> Arvore
multArvore x (Folha n) = Folha (x * n)
multArvore x (Nodo n a1 a2) = Nodo (x * n) (multArvore x a1) (multArvore x a2)

contaFolhas :: Arvore -> Int
contaFolhas (Folha n) = 1
contaFolhas (Nodo n a1 a2) = (contaFolhas a1) + (contaFolhas a2)

contaNodos :: Arvore -> Int
contaNodos (Folha n) = 0
contaNodos (Nodo n a1 a2) = 1 + (contaNodos a1) + (contaNodos a2)

quantasVezes :: Int -> Arvore -> Int
quantasVezes i (Folha n)
    | i == n = 1 
    | otherwise =  0
quantasVezes i (Nodo n a1 a2)
    | i == n = 1 + (quantasVezes i a1) + (quantasVezes i a2)
    | otherwise =  0 + (quantasVezes i a1) + (quantasVezes i a2)

maxArvore :: Arvore -> Int
maxArvore (Folha n) = n
maxArvore (Nodo n a1 a2) = max n (max (maxArvore a1) (maxArvore a2) )

refleteArvore :: Arvore -> Arvore
refleteArvore (Folha n) = (Folha n)
refleteArvore (Nodo n a1 a2) = (Nodo n (refleteArvore a2) (refleteArvore a1))

geraListaArv :: Arvore -> [Int]
geraListaArv (Folha n) = [n]
geraListaArv (Nodo n a1 a2) = n : (geraListaArv a1 ++ geraListaArv a2)