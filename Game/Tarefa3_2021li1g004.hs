{- |
Module      : Tarefa3_2021li1g004
Description : Representação textual do jogo
Copyright   : João Carlos Oliveira Vale <a100697@alunos.uminho.pt>; <carlosvalejcov@gmail.com>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g004 where

import LI12122

instance Show Jogo where
        show jogo = show1 jogo

{- | A partir de um jogador e de um mapa, com as pecas já organizadas por posições e linhas, retribuí uma string que é a representação textual do mapa.

== Exemplos:
@
 Exemplo 1 : show1 (Jogo ([[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]) (Jogador (1,2) Este False)) = "X   X\nX   X\nX> CX\nXXXXX"

 Exemplo 2 : show1 (Jogo ([[Bloco,Vazio,Vazio,Bloco],[Bloco,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco]]) (Jogador (1,1) Oeste True)) = "XC X\nX<CX\nXXXX"
@
-}

show1 :: Jogo -> String
show1 (Jogo mapa (Jogador coordenadas direcao carrega)) = separarLinhas (Jogo mapa (Jogador coordenadas direcao carrega)) 0

{- | Divide o mapa, que se encontra em string e já tem o jogador, em várias linhas, em que o número de linhas é igual à altura do mapa e o número de caractéres por linha é igual ao comprimento do mapa. Nesta função o acumulaador é responável por eliminar os caractéres que já estão nas linhas anteriores.

== Exemplos:
@
 Exemplo 1 : separarLinhas (Jogo ([[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]) (Jogador (1,2) Este False)) 0 = "X   X\nX   X\nX> CX\nXXXXX"

 Exemplo 2 : separarLinhas (Jogo ([[Bloco,Vazio,Vazio,Bloco],[Bloco,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco]]) (Jogador (1,1) Oeste True)) 0 = "XC X\nX<CX\nXXXX"
@
-}

separarLinhas :: Jogo -> Int -> String
separarLinhas (Jogo mapa (Jogador (x,y) direcao carrega)) tiraLinha = if tiraLinha < areaMapa mapa - comprimentoMapa mapa
                                                                      then take (comprimentoMapa mapa) (drop tiraLinha (placeCaixa (Jogo mapa (Jogador (x,y) direcao carrega)))) ++ "\n" ++ separarLinhas (Jogo mapa (Jogador (x,y) direcao carrega)) (tiraLinha + comprimentoMapa mapa)
                                                                      else take (comprimentoMapa mapa) (drop tiraLinha (placeCaixa (Jogo mapa (Jogador (x,y) direcao carrega))))

{- | A partir de um mapa com o jogador já em string, que resultou da função auxiliar placeJogador, retira um espaço vazio (" ") no bloco por cima do jogador e coloca um C na mesma posição do espaço vazio caso o jogador carregue uma caixa. Caso o jogador não carregue uma caixa, a função devolve apenas a string dada pela função auxiliar.

== Exemplos:
@
 Exemplo 1 : placeCaixa (Jogo ([[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]) (Jogador (1,2) Este False)) = "X   XX   XX> CXXXXXX"

 Exemplo 2 : placeCaixa (Jogo ([[Bloco,Vazio,Vazio,Bloco],[Bloco,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco]]) (Jogador (1,1) Oeste True)) = "XC XX<CXXXXX"
@
-}

placeCaixa :: Jogo -> String
placeCaixa (Jogo mapa (Jogador (x,y) direcao carrega)) = if carrega == False
                                                         then placeJogador (Jogo mapa (Jogador (x,y) direcao carrega))
                                                         else take (((y-1) * comprimentoMapa  mapa) + x) (placeJogador (Jogo mapa (Jogador (x,y) direcao carrega))) ++ "C" ++ drop (((y-1) * comprimentoMapa mapa) + x + 1) (placeJogador (Jogo mapa (Jogador (x,y) direcao carrega)))

{- | A partir de um mapa já em string, que resultou da função auxiliar placePecas, retira um espaço (" "), que representa um vazio, e coloca um < ou >, que representa o jogador, na mesma posição, conforme o jogador estiver virado para Oeste ou Este.

== Exemplos:
@
 Exemplo 1 : placeJogador (Jogo ([[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]) (Jogador (1,2) Este False)) = "X   XX   XX> CXXXXXX"

 Exemplo 2 : placeJogador (Jogo ([[Bloco,Vazio,Vazio,Bloco],[Bloco,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco]]) (Jogador (1,1) Oeste True)) = "X  XX<CXXXXX"
@
-}

placeJogador :: Jogo -> String
placeJogador (Jogo mapa (Jogador (x,y) direcao carrega)) = if direcao == Oeste
                                                           then take ((y * comprimentoMapa  mapa) + x) (placePecas (Jogo mapa (Jogador (x,y) direcao carrega))) ++ "<" ++ drop ((y * comprimentoMapa mapa) + x + 1) (placePecas (Jogo mapa (Jogador (x,y) direcao carrega)))
                                                           else take ((y * comprimentoMapa mapa) + x) (placePecas (Jogo mapa (Jogador (x,y) direcao carrega))) ++ ">" ++ drop ((y * comprimentoMapa mapa) + x + 1) (placePecas (Jogo mapa (Jogador (x,y) direcao carrega)))

{- | Transforma a lista de peças (Mapa) em uma string em que cada peça é representada por um caracter específico, sendo o Bloco representado por um X, a Caixa por um C, a porta por um P e o espaço vazio por um espaço (" ").

== Exemplos:
@
 Exemplo 1 : placePecas (Jogo ([[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]) (Jogador (1,2) Este False)) = "X   XX   XX  CXXXXXX"

 Exemplo 2 : placePecas (Jogo ([[Bloco,Vazio,Vazio,Bloco],[Bloco,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco]]) (Jogador (1,1) Oeste True)) = "X  XX CXXXXX"
@
-}

placePecas :: Jogo -> String
placePecas (Jogo [] (Jogador coordenadas direcao carrega)) = []
placePecas (Jogo ([]:t) (Jogador coordenadas direcao carrega)) = placePecas (Jogo t (Jogador coordenadas direcao carrega))
placePecas (Jogo (h:t) (Jogador coordenadas direcao carrega)) | head h == Bloco = "X" ++ placePecas (Jogo (tail h:t) (Jogador coordenadas direcao carrega))
                                                              | head h == Caixa = "C" ++ placePecas (Jogo (tail h:t) (Jogador coordenadas direcao carrega))
                                                              | head h == Porta = "P" ++ placePecas (Jogo (tail h:t) (Jogador coordenadas direcao carrega))
                                                              | otherwise = " " ++ placePecas (Jogo (tail h:t) (Jogador coordenadas direcao carrega))

{- | Calcula a área do mapa a partir da altura do mapa e da função auxiliar comprimentoMapa.

== Exemplos:
@
 Exemplo 1 : areaMapa [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] = 20

 Exemplo 2 : areaMapa [[Bloco,Vazio,Vazio,Bloco],[Bloco,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco]] = 12
@
-}

areaMapa :: Mapa -> Int
areaMapa mapa = length mapa * comprimentoMapa mapa

{- | Calcula o comprimento do mapa.

== Exemplos:
@
 Exemplo 1 : comprimentoMapa [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] = 5

 Exemplo 2 : comprimentoMapa [[Bloco,Vazio,Vazio,Bloco],[Bloco,Vazio,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco]] = 4
@
-}

comprimentoMapa :: Mapa -> Int
comprimentoMapa [] = 0
comprimentoMapa (h:t) = length h


