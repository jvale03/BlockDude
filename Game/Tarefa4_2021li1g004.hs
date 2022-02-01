{- |
Module      : Tarefa4_2021li1g004
Description : Movimentação do personagem
Copyright   : João Carlos Oliveira Vale <a100697@alunos.uminho.pt>; <carlosvalejcov@gmail.com>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}

module Tarefa4_2021li1g004 where

import LI12122

import Tarefa3_2021li1g004

import Mapas

-- | É a generalização da função correrMovimentos.

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo [] = jogo
correrMovimentos jogo (h:t) = correrMovimentos (moveJogador jogo h) t

{- | Esta função aplica o efeito de um comando (i.e. Movimento) sobre o jogador.

== Andar para a esquerda:

Caso o movimento introduzido seja "AndarEsquerda", a função vai avaliar se o jogador pode realizar esse movimento através de uma série de condições. Condições estas:
     
     1 - Existe um bloco à esquerda do jogador;
     
     2 - Se o jogador carrega uma caixa, existe um bloco diretamente por cima do bloco que fica à esquerda do jogador.

Desta forma, caso o jogador não carregue uma caixa, este só alterará a sua posição se a condição 1 for falsa.

Por outro lado, se carregar uma caixa, só poderá alterar a sua posição se a condição 1 e 2 forem falsas.

Caso contrário a função irá devolver o mesmo Jogo, com o jogador virado para o lado que tentou andar mas não consegiu.

== Andar para a direita:

Caso o movimento introduzido seja "AndarDireita", a função vai avaliar se o jogador pode realizar esse movimento através de uma série de condições. Condições estas:
     
     1 - Existe um bloco à direita do jogador;
     
     2 - Se o jogador carrega uma caixa, existe um bloco diretamente por cima do bloco que fica à direita do jogador.

 Desta forma, caso o jogador não carregue uma caixa, este só alterará a sua posição se a condição 1 for falsa.
 
 Por outro lado, se carregar uma caixa, só poderá alterar a sua posição se a condição 1 e 2 forem falsas.
 
 Caso contrário a função irá devolver o mesmo Jogo, com o jogador virado para o lado que tentou andar mas não consegiu.

 == Trepar:

 Caso o movimento introduzido seja "Trepar", a função vai avaliar se o jogador pode realizar esse movimento através de uma série de condições. Condições estas:
     
     1 - O bloco que quer trepar é vazio;

     2 - Existe um bloco/caixa por cima do bloco/caixa que o jogador quer trepar;
     
     3 - O jogador carrega uma caixa:
      
               * Exite um bloco duas unidades a cima do bloco/caixa que o jogador quer trepar;
      
               * O jogador está virado para oeste.
     
     4 - O jogador está virado para oeste.

Desta forma, caso o jogador não carregue uma caixa, este só poderá trepar se as condições 1 e 2 forem falsas para a direção em que este está virado.

Por outro lado, se carregar uma caixa, este só poderá trepar se as condições 1, 2 e 3 forem falsas para a direção em que o jogador está virado.

Caso contrário, a função irá devolver o mesmo Jogo.

== Interagir com caixas:

Caso o movimento introduzido seja "InterageCaixa", a função vai avaliar se o jogador pode realizar esse movimento através de uma série de condições. Condições estas:
     
     1 - O jogador carrega uma caixa:
          
          * O jogador está virado para oeste;
          
          * O bloco à frente do jogador é vazio e o bloco diretamente por cima desse também o é;
          
          * Existe um bloco à frente do jogador, mas o bloco diretamente por cima é vazio.
     
     2 - Existe um bloco por cima do jogador;
     
     3 - Não existe uma caixa no bloco diretamente à frente do jogador;
     
     4 - Existe um bloco/caixa por cima da caixa que o jogador quer pegar;
     
     5 - O jogador está virado para oeste.

Desta forma, caso o jogador não carregue uma caixa, este só poderá pegar numa caixa se as condições 2, 3 e 4 forem falsas para a direção em que este está virado.

Por outro lado, se carregar uma caixa, este só a poderá largar se a condição 1.2 for verdadeira para a direção em que este está virado ou se a condição 1.3 for verdadeira para a direção em que este está virado.

Caso contrário, a função irá devolver o mesmo Jogo.

-}

moveJogador :: Jogo -> Movimento -> Jogo

moveJogador (Jogo mapa (Jogador (x,y) direcao carrega)) AndarEsquerda
  | encontra mapa y (x - 1) == Bloco || encontra mapa y (x - 1) == Caixa = Jogo mapa (Jogador (x,y) Oeste carrega)
  | carrega == True && encontra mapa (y - 1) (x - 1) == Bloco = Jogo mapa (Jogador (x,y) Oeste carrega)
  | carrega == True = Jogo mapa (Jogador (x-1,(blocoCaixaEsquerdaSuperior mapa (x,y)) - 1) Oeste True)
  | otherwise = Jogo mapa (Jogador (x-1,(blocoCaixaEsquerdaSuperior mapa (x,y)) - 1) Oeste False)

moveJogador (Jogo mapa (Jogador (x,y) direcao carrega)) AndarDireita
  | encontra mapa y (x + 1) == Bloco || encontra mapa y (x + 1) == Caixa = Jogo mapa (Jogador (x,y) Este carrega)
  | carrega == True && encontra mapa (y - 1) (x + 1) == Bloco = Jogo mapa (Jogador (x,y) Este carrega)
  | carrega == True = Jogo mapa (Jogador (x+1,(blocoCaixaDireitaSuperior mapa (x,y)) - 1) Este True)
  | otherwise = Jogo mapa (Jogador (x+1,(blocoCaixaDireitaSuperior mapa (x,y)) - 1) Este False)

moveJogador (Jogo mapa (Jogador (x,y) direcao carrega)) Trepar
  | (direcao == Oeste && (encontra mapa y (x - 1) == Vazio || encontra mapa y (x - 1) == Porta)) || (direcao == Este && (encontra mapa y (x + 1) == Vazio || encontra mapa y (x + 1) == Porta)) = Jogo mapa (Jogador (x,y) direcao carrega)
  | (direcao == Oeste && (encontra mapa (y - 1) (x - 1) == Bloco || encontra mapa (y - 1) (x - 1) == Caixa)) || (direcao == Este && (encontra mapa (y - 1) (x + 1) == Bloco || encontra mapa (y - 1) (x + 1) == Caixa)) = Jogo mapa (Jogador (x,y) direcao carrega)
  | carrega == True = if (direcao == Oeste && encontra mapa (y - 2) (x - 1) == Bloco) || (direcao == Este && encontra mapa (y - 2) (x + 1) == Bloco)
                      then Jogo mapa (Jogador (x,y) direcao carrega)
                      else if direcao == Oeste
                           then Jogo mapa (Jogador (x-1,y-1) direcao carrega)
                           else Jogo mapa (Jogador (x+1,y-1) direcao carrega)
  | otherwise = if direcao == Oeste
                then Jogo mapa (Jogador (x-1,y-1) direcao carrega)
                else Jogo mapa (Jogador (x+1,y-1) direcao carrega)

moveJogador (Jogo mapa (Jogador (x,y) direcao carrega)) InterageCaixa
  | carrega == True = if direcao == Oeste
                      then if encontra mapa y (x - 1) == Vazio && encontra mapa (y - 1) (x - 1) == Vazio
                           then Jogo (adicionaCaixaOesteMapa1 mapa (x,y)) (Jogador (x,y) direcao False)
                           else if (encontra mapa y (x - 1) == Bloco || encontra mapa y (x - 1) == Caixa) && encontra mapa (y - 1) (x - 1) == Vazio
                                then Jogo (adicionaCaixaOesteMapa2 mapa (x,y)) (Jogador (x,y) direcao False)
                                else Jogo mapa (Jogador (x,y) direcao True)
                      else if encontra mapa y (x + 1) == Vazio && encontra mapa (y - 1) (x + 1) == Vazio
                           then Jogo (adicionaCaixaEsteMapa1 mapa (x,y)) (Jogador (x,y) direcao False)
                           else if (encontra mapa y (x + 1) == Bloco || encontra mapa y (x + 1) == Caixa) && encontra mapa (y - 1) (x + 1) == Vazio
                                then Jogo (adicionaCaixaEsteMapa2 mapa (x,y)) (Jogador (x,y) direcao False)
                                else Jogo mapa (Jogador (x,y) direcao True)
  | otherwise = if encontra mapa (y - 1) x == Bloco
                then Jogo mapa (Jogador (x,y) direcao carrega)
                else if (direcao == Oeste && encontra mapa y (x - 1) /= Caixa) || (direcao == Este && encontra mapa y (x + 1) /= Caixa)
                     then Jogo mapa (Jogador (x,y) direcao carrega)
                     else if (direcao == Oeste && (encontra mapa (y - 1) (x - 1) == Bloco || encontra mapa (y - 1) (x - 1) == Caixa)) || (direcao == Este && ((encontra mapa (y - 1) (x + 1) == Bloco) || encontra mapa (y - 1) (x + 1) == Caixa))
                          then Jogo mapa (Jogador (x,y) direcao carrega)
                          else if direcao == Oeste
                               then Jogo (removeCaixaOesteMapa mapa (x,y))  (Jogador (x,y) Oeste True)
                               else Jogo (removeCaixaEsteMapa mapa (x,y)) (Jogador (x,y) Este True)

{- | Devolve um elemento de uma lista de listas.

== Exemplos:
@
 Exemplo 1 : encontra [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Porta,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] 2 1 = Porta

 Exemplo 2 : encontra [[Bloco,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco]] 1 1 = Vazio
@
-}

encontra :: Mapa -> Int -> Int -> Peca
encontra [] _ _ = Bloco
encontra (h:t) e1 e2 = encontra2 (encontra1 (h:t) e1) e2

{- | Devolve uma lista de uma lista de listas.

== Exemplos:
@
 Exemplo 1 : encontra1 [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Porta,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] 2 = [[Bloco,Porta,Vazio,Caixa,Bloco]]

 Exemplo 2 : encontra1 [[Bloco,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco]] 1 = [Bloco,Vazio,Vazio,Bloco]
@
-}

encontra1 :: [[Peca]] -> Int -> [Peca]
encontra1 [] _ = [Bloco]
encontra1 (h:t) e = if e > 0
                   then encontra1 t (e - 1)
                   else h

{- | Devolve um elemento de uma lista.

== Exemplos:
@
 Exemplo 1 : encontra2 [Bloco,Porta,Vazio,Caixa,Bloco] 1 = Porta

 Exemplo 2 : encontra2 [Bloco,Vazio,Vazio,Bloco] 1 = Vazio
@
-}

encontra2 :: [Peca] -> Int -> Peca
encontra2 [] _ = Bloco
encontra2 (h:t) e = if e > 0
                   then encontra2 t (e - 1)
                   else h

{- | Calcula a ordenada do bloco ou caixa com menor ordenada à esquerda do jogador. Esta função só tem em conta as ordenadas dos blocos e caixas que têm ordenada inferior à do jogador.

== Exemplos:
@
 Exemplo 1 : blocoCaixaEsquerdaSuperior [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Porta,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (3,1) = 3

 Exemplo 2 : blocoCaixaEsquerdaSuperior [[Bloco,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco]] (2,2) = 3
@
-}

blocoCaixaEsquerdaSuperior :: Mapa -> Coordenadas -> Int
blocoCaixaEsquerdaSuperior mapa (x,y) = minimum (blocosCaixasEsquerda mapa (x,y) (y+1))

{- | Calcula as ordenadas de todas caixas e blocos com abcissa igual à abcissa do jogador menos uma unidade e com ordenada superior à do jogador.

== Exemplos:
@
 Exemplo 1 : blocosCaixasEsquerda [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Porta,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (3,1) = [3]

 Exemplo 2 : blocosCaixasEsquerda [[Bloco,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco]] (2,2) = [3]
@
-}

blocosCaixasEsquerda :: Mapa -> Coordenadas -> Int -> [Int]
blocosCaixasEsquerda mapa (x,y) ordenada = if ordenada <= length mapa 
                                             then if encontra mapa ordenada (x-1) == Bloco || encontra mapa ordenada (x-1) == Caixa
                                                  then ordenada : blocosCaixasEsquerda mapa (x,y) (ordenada + 1)
                                                  else blocosCaixasEsquerda mapa (x,y) (ordenada + 1)
                                             else []

{- | Calcula a ordenada do bloco ou caixa com menor ordenada à esquerda do jogador. Esta função só tem em conta as ordenadas dos blocos e caixas que têm ordenada inferior à do jogador.

== Exemplos:
@
 Exemplo 1 : blocoCaixaDireitaSuperior [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Porta,Caixa,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (2,2) = 2

 Exemplo 2 : blocoCaixaDireitaSuperior [[Bloco,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco]] (1,1) = 3
@
-}

blocoCaixaDireitaSuperior :: Mapa -> Coordenadas -> Int
blocoCaixaDireitaSuperior mapa (x,y) = minimum (blocosCaixasDireita mapa (x,y) (y+1))

{- | Calcula as ordenadas de todas caixas e blocos com abcissa igual à abcissa do jogador mais uma unidade e com ordenada superior à do jogador.

== Exemplos:
@
 Exemplo 1 : blocosCaixasDireita [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Porta,Caixa,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (2,2) = [2,3]

 Exemplo 2 : blocosCaixasDireita [[Bloco,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco]] (1,1) = [3]
@
-}

blocosCaixasDireita :: Mapa -> Coordenadas -> Int -> [Int]
blocosCaixasDireita mapa (x,y) ordenada = if ordenada <= length mapa 
                                             then if encontra mapa ordenada (x+1) == Bloco || encontra mapa ordenada (x+1) == Caixa
                                                  then ordenada : blocosCaixasDireita mapa (x,y) (ordenada + 1)
                                                  else blocosCaixasDireita mapa (x,y) (ordenada + 1)
                                             else []

{- | Substitui um espaço vazio por uma caixa quando o jogador larga uma caixa. Usa as funções auxiliares blocoCaixaEsquerdaSuperior e adicionaCaixaLinha. Esta função só se aplica se o bloco à frente do jogador for vazio.

== Exemplos:
@
 Exemplo 1 : adicionaCaixaOesteMapa1 [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (2,1) = [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]

 Exemplo 2 : adicionaCaixaOesteMapa1 [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (3,2) = [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Caixa,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]
@
-}

adicionaCaixaOesteMapa1 :: Mapa -> Coordenadas -> Mapa
adicionaCaixaOesteMapa1 mapa (x,y) = take ((blocoCaixaEsquerdaSuperior mapa (x,y)) - 1) mapa ++ [adicionaCaixaLinha (encontra1 mapa ((blocoCaixaEsquerdaSuperior mapa (x,y)) - 1)) (x-1,y)] ++ drop (blocoCaixaEsquerdaSuperior mapa (x,y)) mapa

{- | Substitui um espaço vazio por uma caixa quando o jogador larga uma caixa. Usa a função auxiliar adicionaCaixaLinha. Esta função só se aplica se no bloco à frente do jogador existir um bloco ou uma caixa e o bloco por cima desse for vazio.

== Exemplos:
@
 Exemplo 1 : adicionaCaixaOesteMapa2 [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Bloco,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (2,1) = [[Bloco,Caixa,Vazio,Vazio,Bloco],[Bloco,Bloco,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]

 Exemplo 2 : adicionaCaixaOesteMapa2 [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (2,2) = [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]
@
-}

adicionaCaixaOesteMapa2 :: Mapa -> Coordenadas -> Mapa
adicionaCaixaOesteMapa2 mapa (x,y) = take (y-1) mapa ++ [adicionaCaixaLinha (encontra1 mapa (y-1)) (x-1,y)] ++ drop y mapa

{- | Substitui um espaço vazio por uma caixa quando o jogador larga uma caixa. Usa as funções auxiliares blocoCaixaEsquerdaSuperior e adicionaCaixaLinha. Esta função só se aplica se o bloco à frente do jogador for vazio.

== Exemplos:
@
 Exemplo 1 : adicionaCaixaEsteMapa1 [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (2,1) = [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]

 Exemplo 2 : adicionaCaixaEsteMapa1 [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (2,2) = [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]
@
-}

adicionaCaixaEsteMapa1 :: Mapa -> Coordenadas -> Mapa
adicionaCaixaEsteMapa1 mapa (x,y) = take ((blocoCaixaDireitaSuperior mapa (x,y)) - 1) mapa ++ [adicionaCaixaLinha (encontra1 mapa ((blocoCaixaDireitaSuperior mapa (x,y)) - 1)) (x+1,y)] ++ drop (blocoCaixaDireitaSuperior mapa (x,y)) mapa

{- | Substitui um espaço vazio por uma caixa quando o jogador larga uma caixa. Usa a função auxiliar adicionaCaixaLinha. Esta função só se aplica se no bloco à frente do jogador existir um bloco ou uma caixa e o bloco por cima desse for vazio.

== Exemplos:
@
 Exemplo 1 : adicionaCaixaEsteMapa2 [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,vazio,Vazio,Bloco,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (2,1) = [[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Vazio,Vazio,Bloco,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]

 Exemplo 2 : adicionaCaixaEsteMapa2 [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (2,2) = [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]
@
-}

adicionaCaixaEsteMapa2 :: Mapa -> Coordenadas -> Mapa
adicionaCaixaEsteMapa2 mapa (x,y) = take (y-1) mapa ++ [adicionaCaixaLinha (encontra1 mapa (y-1)) (x+1,y)] ++ drop y mapa

{- | Substitui um espaço vazio (" ") por uma caixa numa linha do mapa.

== Exemplos:
@
 Exemplo 1 : adicionaCaixaLinha [Bloco,Vazio,Vazio,Vazio,Bloco] (2,1) = [Bloco,Vazio,Vazio,Caixa,Bloco]

 Exemplo 2 : adicionaCaixaLinha [Bloco,Caixa,Vazio,Vazio,Bloco] (2,2) = [Bloco,Caixa,Vazio,Caixa,Bloco]
@
-}

adicionaCaixaLinha :: [Peca] -> Coordenadas -> [Peca]
adicionaCaixaLinha linha (x,y) = take x linha ++ [Caixa] ++ drop (x+1) linha

{- | Substitui um bloco com uma caixa por um bloco vazio. Esta função usa a função auxiliar removeCaixaLinha.

== Exemplos:
@
 Exemplo 1 : removeCaixaOesteMapa [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (2,1) = [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]

 Exemplo 2 : removeCaixaOesteMapa [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Caixa,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (2,2) = [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]
@
-}

removeCaixaOesteMapa :: Mapa -> Coordenadas -> Mapa
removeCaixaOesteMapa mapa (x,y) = take y mapa ++ [removeCaixaLinha (encontra1 mapa y) (x-1,y)] ++ drop (y+1) mapa

{- | Substitui um bloco com uma caixa por um bloco vazio. Esta função usa a função auxiliar removeCaixaLinha.

== Exemplos:
@
 Exemplo 1 : removeCaixaEsteMapa [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (2,1) = [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]

 Exemplo 2 : removeCaixaEsteMapa [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (2,2) = [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Vazio,Bloco],[Bloco,Caixa,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]
@
-}

removeCaixaEsteMapa :: Mapa -> Coordenadas -> Mapa
removeCaixaEsteMapa mapa (x,y) = take y mapa ++ [removeCaixaLinha (encontra1 mapa y) (x+1,y)] ++ drop (y+1) mapa

{- | Substitui uma caixa por um espaço vazio (" ") numa linha do mapa.

== Exemplos:
@
 Exemplo 1 : removeCaixaLinha [Bloco,Vazio,Vazio,Caixa,Bloco] (2,1) = [Bloco,Vazio,Vazio,Vazio,Bloco]

 Exemplo 2 : removeCaixaLinha [Bloco,Caixa,Vazio,Caixa,Bloco] (2,2) = [Bloco,Caixa,Vazio,Vazio,Bloco]
@
-}

removeCaixaLinha :: [Peca] -> Coordenadas -> [Peca]
removeCaixaLinha linha (x,y) = take x linha ++ [Vazio] ++ drop (x+1) linha

