conta:: [Int] -> [(Int, Int)]
processa:: Int -> [(Int, Int)] -> [(Int, Int)]
contaAux:: [Int] -> [(Int, Int)] -> [(Int, Int)]

conta [] = []
conta lista = contaAux lista []

contaAux [] resultado = resultado
contaAux lista resultado = let
    cabeca = head lista
    cauda = tail lista
    novoRes = processa cabeca resultado
    in contaAux cauda novoRes

processa x [] = [(x,1)]
processa x ((a,b):cauda) = if x == a
    then ((a,b+1):cauda)
    else((a,b):processa x cauda)