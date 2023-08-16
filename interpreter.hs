-- Ulian Gabriel Alff Ramires, Matrícula: 20200274
-- Definição da árvore sintática para representação dos programas

data E = Num Int
      | Var String
      | Soma E E
      | Sub E E
      | Mult E E
   deriving (Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E
   deriving (Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | DoWhile C B
    | Loop E C
    | DAtrrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
   deriving (Eq,Show)                

----- As próximas funções, servem para manipular a memória (sigma)

type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [("x", 10), ("temp",0), ("y",0)]

--- procuraVar exSigma "x"
--- 10

procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v

--- mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]

mudaVar  :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

ebigStep :: (E, Memoria) -> Int
ebigStep (Var x,s) = procuraVar s x
ebigStep (Num n,s) = n
ebigStep (Soma e1 e2,s)  = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s)  = ebigStep (e1,s) - ebigStep (e2,s)
ebigStep (Mult e1 e2,s)  = ebigStep (e1,s) * ebigStep (e2,s)

------------------------------------------------------------------

bbigStep :: (B, Memoria) -> Bool
bbigStep (TRUE,s)  = True
bbigStep (FALSE,s) = False
bbigStep (Not b,s) 
    | bbigStep (b,s) == True     = False
    | otherwise                  = True
bbigStep (And b1 b2,s ) -- Aqui eu poderia usar o And do Haskell, mas acabei complicando
    |   bbigStep (b1, s) == False = False
    |   otherwise = bbigStep (b2, s)
bbigStep (Or b1 b2,s ) -- Aqui eu poderia usar o Or do Haskell, mas acabei complicando
    |   bbigStep (b1, s) == True = True
    |   otherwise = bbigStep (b2, s)
bbigStep (Leq e1 e2,s) = ebigStep (e1, s) <= ebigStep(e2, s)
bbigStep (Igual e1 e2,s) = ebigStep (e1, s) == ebigStep(e2, s)

------------------------------------------------------------------

cbigStep :: (C, Memoria) -> (C,Memoria)

cbigStep (Skip, s) = (Skip, s)

cbigStep (If b c1 c2, s)
    | bbigStep (b, s) == True = cbigStep(c1, s)
    | otherwise = cbigStep(c2, s)

cbigStep (Seq c1 c2, s) =
    let (c1', s') = cbigStep (c1, s)
    in cbigStep (c2, s')

cbigStep (Atrib (Var x) e, s) = (Skip, mudaVar s x (ebigStep (e, s)))

cbigStep (While b c, s) 
    | bbigStep (b,s) == True = cbigStep (Seq c (While b c), s) 
    | otherwise = cbigStep (Skip, s)

--cbigStep (DoWhile c b,s) 
--    | bbigStep (b,s) == True = cbigStep (Seq c (DoWhile c b), s)
--    | otherwise = cbigStep (c,s) -- Executa 1x ao invés de Skip

cbigStep (DoWhile c b, s)
    | bbigStep (b, s) == True =
        let combinedCmd = Seq c (DoWhile c b)
        in cbigStep (combinedCmd, s)
    | otherwise = (Skip, s)

cbigStep (Loop e c, s)
   | ebigStep (e,s) <= 0 = (Skip, s)
   | otherwise = cbigStep(Seq c (Loop (Sub e (Num 1)) c), s)

cbigStep (DAtrrib (Var x) (Var y) e1 e2, s) =
    let n1 = ebigStep (e1, s)
        n2 = ebigStep (e2, s)
        s' = mudaVar (mudaVar s x n1) y n2
    in (Skip, s')

--------------------------------------
--- Memórias para Teste
--------------------------------------

sigma0 :: Memoria
sigma0 = [("x",0), ("y",0), ("z",0)]

sigma1 :: Memoria
sigma1 = [("x", 1), ("y",0), ("z",0)]

sigma2 :: Memoria
sigma2 = [("x",10), ("y",0), ("z",0)]

sigmaFib10 :: Memoria
sigmaFib10 = [("x", 10), ("a", 0), ("b", 1), ("c", 0)]

sigmaFib6 :: Memoria
sigmaFib6 = [("x", 6), ("a", 0), ("b", 1), ("c", 0)]

sigmaPotencia :: Memoria -- Potência >> z; Base >> x
sigmaPotencia = [("w", 0), ("x", 3), ("y",3), ("z",2)]

--------------------------------------
--- Programas Teste
---
--- * Loop 
--- * Dupla Atribuição
--- * Do While
-------------------------------------

--- Exemplos de expressões booleanas:

teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))

-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

progSeq1 :: C
progSeq1 = Seq
    (Atrib (Var "x") (Num 5)) -- X = 5
    (Atrib (Var "y") (Soma (Var "x") (Num 3))) -- Y = X + 3

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

--- Implementação com Loop:

progLoop1 :: C
progLoop1 = Loop (Num 3) (Atrib (Var "x") (Soma (Var "x") (Num 1)))

{--
    int x;
    x = sigma; 

    int n = 3; 

    for (int i = 0; i < n; i++) {
        x = x + 1;
--}

fibonacci :: C
fibonacci = Loop (Var "x")
    (Seq
        (Atrib (Var "c") (Soma (Var "a") (Var "b"))) -- c = a + b
        (Seq
            (Atrib (Var "a") (Var "b")) -- a = b
            (Atrib (Var "b") (Var "c")) -- b = c
        )
    )

{--
    int x = 10;
    int a = 0;
    int b = 1;

    for (int i = 0; i < x; i++) {
        int c = a + b;
        a = b;
        b = c;
    }
--}

--- Implementação com Dupla atribuição (potência contêm do while também):

progDuplaAtrib1 :: C
progDuplaAtrib1 = DAtrrib (Var "x") (Var "y") (Num 5) (Num 7)

potencia :: C -- Com dupla atribuição (x = base, z = potencia)
potencia = (Seq (Atrib (Var "w") (Var "x"))
           (DoWhile (DAtrrib (Var "w") (Var "y") (Mult (Var "w") (Var "x")) (Soma (Var "y") (Num 1)))
                    (Leq (Var "y") (Soma (Var "z") (Num 1)))))

{-- 
    int x, y, z, w;
    x = valorX (base)
    y = 0;
    z = valorZ (potencia)

    w = x;
    do {
        w = w * x;
        y = y + 1;
    } while (y <= z); 
--}

--- Implementação com Do While:

progDoWhile1 :: C
progDoWhile1 = DoWhile (Atrib (Var "x") (Soma (Var "x") (Num 1))) (Leq (Var "x") (Num 5))

whileLoop1 :: C
whileLoop1 = Seq
    (Atrib (Var "x") (Num 1)) -- Inicializa x com 1
    (DoWhile
        (Seq
            (Atrib (Var "x") (Mult (Var "x") (Num 2))) -- x = x * 2
            (Atrib (Var "x") (Soma (Var "x") (Num 1))) -- x = x + 1
        )
        (Leq (Var "x") (Num 100)) -- Executa o loop enquanto x for menor ou igual a 100
    )

{--
    int x = 1; // Inicializa x com 1 pra qualquer sigma
    do {
        x = x * 2;
        x = x + 1;
    } while (x <= 100);
--}
