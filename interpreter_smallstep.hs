-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | DoWhile C B  -- Do C while B
    | Loop E C  --- Recebe uma expressão "e" e um comando "c". Repete "e" vezes o comando "c"
    | DAtrrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
   deriving(Eq,Show)   

-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

smallStepE :: (E, Memoria) -> (E, Memoria)

smallStepE (Var x, s)                  = (Num (procuraVar s x), s)

smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Soma el e2,sl)

smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Mult el e2, sl)

smallStepE (Sub (Num n1) (Num n2), s)  = (Num (n1 - n2), s)
smallStepE (Sub (Num n) e, s)          = let (el, sl) = smallStepE (e, s)
                                         in (Sub (Num n) el, sl)
smallStepE (Sub e1 e2, s)              = let (el, sl) = smallStepE (e1, s)
                                         in (Sub el e2, sl)

-----------------------------------------------------------------------

smallStepB :: (B,Memoria) -> (B, Memoria)
smallStepB (Not TRUE,s)                = (FALSE, s)  --not3
smallStepB (Not FALSE, s)              = (TRUE, s)    --not2
smallStepB (Not b, s)                  = let (b', s') = smallStepB (b,s) --not1
                                          in (Not b', s')

smallStepB (And FALSE b, s )           = (FALSE, s)
smallStepB (And TRUE b, s )            = (b, s)
smallStepB (And b1 b2, s )             = let (b1' , s) = smallStepB (b1, s)
                                          in (And b1' b2 ,s)

smallStepB (Or FALSE b, s)             = (b, s)
smallStepB (Or TRUE b, s)              = (TRUE, s)
smallStepB (Or b1 b2, s)               = let (b1', s) = smallStepB (b1, s)
                                          in (Or b1' b2, s)

--smallStepB (Leq (Num n1) (Num n2), s)  = if n1 <= n2
--                                           then (TRUE, s)
--                                           else (FALSE, s)
--smallStepB (Leq (Num n1) e, s)         = let (e', s') = smallStepE (e, s)
--                                        in (Leq (Num n1) e', s')
--smallStepB (Leq e1 e2, s)              = let (e1', s') = smallStepE (e1, s)
--                                        in (Leq e1' e2, s')

smallStepB (Leq (Num n1) (Num n2), s)  = if n1 <= n2
                                           then (TRUE, s)
                                           else (FALSE, s)
smallStepB (Leq (Num n1) e, s)         = let (e', s') = smallStepE (e, s)
                                        in (Leq (Num n1) e', s')
smallStepB (Leq e1 e2, s)              = let (e1', s') = smallStepE (e1, s)
                                        in (Leq e1' e2, s')
                                 
--smallStepB (Igual (Num n1) (Num n2),s) =  if n1 == n2
--                                            then (TRUE, s)
--                                             else (FALSE, s)
--smallStepB (Igual (Num n1) e, s)         = let (e', s') = smallStepE (e, s)
--                                          in (Leq (Num n1) e', s')
--smallStepB (Igual e1 e2, s)              = let (e1', s) = smallStepE (e1, s)
--                                          in (Leq e1' e2, s)

smallStepB (Igual (Num n1) (Num n2),s) = if n1 == n2
                                           then (TRUE, s)
                                           else (FALSE, s)
smallStepB (Igual (Num n1) e, s)       = let (e', s') = smallStepE (e, s)
                                        in (Igual (Num n1) e', s')
smallStepB (Igual e1 e2, s)            = let (e1', s') = smallStepE (e1, s)
                                        in (Igual e1' e2, s')

--smallStepC :: (C,Memoria) -> (C,Memoria)
--smallStepC (If FALSE c1 c2,s) = (c2, s)
--smallStepC (If TRUE c1 c2,s) = (c1, s)
--smallStepC (If b c1 c2, s) = let (b', s) = smallStepB(b, s)
--                              in (If b' c1 c2, s)

smallStepC :: (C,Memoria) -> (C,Memoria)
smallStepC (If FALSE c1 c2,s) = (c2, s)
smallStepC (If TRUE c1 c2,s) = (c1, s)
smallStepC (If b c1 c2, s) = let (b', s') = smallStepB(b, s)
                              in (If b' c1 c2, s')

smallStepC (Seq Skip c2,s) = (c2, s)
smallStepC (Seq c1 c2, s) = let (c1', s') = smallStepC(c1, s)
                             in (Seq c1' c2, s')

smallStepC (Atrib (Var x) (Num n) ,s) = (Skip, mudaVar s x n)
smallStepC (Atrib (Var x) e, s) = let (e', s') = smallStepE(e, s)
                                   in (Atrib (Var x) e', s')

smallStepC (While b c, s) = (If b (Seq c (While b c)) (Skip), s)

smallStepC (DoWhile c b,s) = (Seq c (If b (DoWhile c b) (Skip)),s) 

smallStepC (Loop e c, s) = (If (Leq e (Num 0)) Skip (Seq c (Loop (Sub e (Num 1)) c)), s)

smallStepC (DAtrrib (Var x) (Var y) e1 e2, s) = 
    smallStepC (Seq (Atrib (Var x) e1) (Atrib (Var y) e2), s)


----------------------
--  INTERPRETADORES
----------------------


--- Interpretador para Expressões Aritméticas:

isFinalE :: E -> Bool
isFinalE (Num n) = True
isFinalE _       = False


interpretadorE :: (E,Memoria) -> (E, Memoria)
interpretadorE (e,s) = if (isFinalE e) then (e,s) else interpretadorE (smallStepE (e,s))

--- Interpretador para expressões booleanas


isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

-- Descomentar quanto a função smallStepB estiver implementada:

interpretadorB :: (B,Memoria) -> (B, Memoria)
interpretadorB (b,s) = if (isFinalB b) then (b,s) else interpretadorB (smallStepB (b,s))


-- Interpretador da Linguagem Imperativa

isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

-- Descomentar quando a função smallStepC estiver implementada:

interpretadorC :: (C,Memoria) -> (C, Memoria)
interpretadorC (c,s) = if (isFinalC c) then (c,s) else interpretadorC (smallStepC (c,s))


--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop 
--- * Dupla Atribuição
--- * Do While
-------------------------------------

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

sigmaFib6 :: Memoria -- 
sigmaFib6 = [("x", 9), ("a", 0), ("b", 1), ("c", 0)]

sigmaPotencia :: Memoria -- Potência >> z; Base >> x
sigmaPotencia = [("w", 0), ("x", 2), ("y", 2), ("z", 4)]

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

-- Expected: True, porque 6 <= 6

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))

-- Expected: False, 13 <= 6 com Sigma2

-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Seq (Atrib (Var "y") (Var "z")) (Atrib (Var "x") (Var "z"))))

-- Expected: Propagação do valor em X em todas variáveis

testec2 :: C
testec2 = (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 0)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

whileTest :: C
whileTest = 
    (Seq (Atrib (Var "y") (Num 10)) 
        (While
        (Igual (Num 6) (Num 5))
          (Atrib (Var "y") (Soma (Var "y") (Var "x")))
      ))

ifTest :: C
ifTest = (If TRUE (Seq (Atrib (Var "y") (Num 10)) (Skip)) Skip)
    

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
                    (Leq (Var "y") (Var "z"))))

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

doWhileLoop1 :: C
doWhileLoop1 = Seq
    (Atrib (Var "x") (Num 1))
    (DoWhile
        (Seq
            (Atrib (Var "x") (Mult (Var "x") (Num 2)))
            (Atrib (Var "x") (Soma (Var "x") (Num 1)))
        )
        (Leq (Var "x") (Num 100))
    )

{--
    int x = 1; // Inicializa x com 1 pra qualquer sigma
    do {
        x = x * 2;
        x = x + 1;
    } while (x <= 100);
--}
