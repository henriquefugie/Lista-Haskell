{-1) Faça uma função que calcule a distância entre dois pontos-}
--d=√((x_2-x_1)²+(y_2-y_1)²) 

dist_pontos :: (Float, Float) -> (Float, Float) -> Float
dist_pontos (x1, y1) (x2, y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

{-2) Faça uma função para verificar se um ano informado é bissexto ou não.-}

ano_bissexto :: Int -> Bool
ano_bissexto x = (x `mod` 4 == 0 && x `mod` 100 /= 0 || x `mod` 400 == 0)

ano_bissextog :: Int -> Bool
ano_bissextog x | (x `mod` 4 == 0 && x `mod` 100 /= 0) = True
                | (x `mod` 400 == 0) = True
                | otherwise = False

{-3) Defina uma função que recebe três números inteiros representando,
respectivamente, um dia, um mês e um ano e verifica se os números formam uma
data válida.-}

data_valida :: (Int, Int, Int) -> Bool
data_valida (x, y, z) | (y == 1 && x > 0 && x <= 31) = True
                      | (y == 2 && x > 0 && x <= 28) = True
                      | (y == 2 && ano_bissexto(z) && x > 0 && x <= 29) = True
                      | (y == 3 && x > 0 && x <= 31) = True
                      | (y == 4 && x > 0 && x <= 30) = True
                      | (y == 5 && x > 0 && x <= 31) = True
                      | (y == 6 && x > 0 && x <= 30) = True
                      | (y == 7 && x > 0 && x <= 31) = True
                      | (y == 8 && x > 0 && x <= 31) = True
                      | (y == 9 && x > 0 && x <= 30) = True
                      | (y == 10 && x > 0 && x <= 31) = True
                      | (y == 11 && x > 0 && x <= 30) = True
                      | (y == 12 && x > 0 && x <= 31) = True
                      | otherwise = False

{-4) Crie uma função par::Int->Bool para verificar se um numero é par ou impar.-}

par:: Int -> Bool
par x = x `mod` 2 == 0

{-5) Escreva a função conceito :: Float -> Char que recebe uma nota e retorne o
conceito correspondentes conforme as regras abaixo:
Nota abaixo de 4 – Conceito E, Nota entre 4 e 5.99 conceito D, Nota
entre 6 e 7.49 conceito C, Intervalo entre 7.5 e 8.99 conceito B e acima de 9
conceito A.-}

conceito :: Float -> Char
conceito x | (x < 4.0) = 'E'
           | (x >= 4.0 && x <= 5.99) = 'D'
           | (x >= 6.0 && x <= 7.49) = 'C'
           | (x >= 7.5 && x <= 8.99) = 'B'
           | (x >= 9) = 'A'
