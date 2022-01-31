module Fixtures where

import LI12122

m1,m2, m3, m4, m5, m6, m7, m8:: [(Peca,Coordenadas)]
m1 = [(Porta,(0,3)),(Bloco,(0,4)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(3,4)),(Caixa,(4,3)),(Bloco,(4,4)),(Bloco,(5,4)),(Bloco,(6,1)),(Bloco,(6,2)),(Bloco,(6,3)),(Bloco,(6,4))] --True
m2 = [(Bloco,(1,0)),(Bloco,(5,0)),(Bloco,(6,0)),(Bloco,(0,1)),(Bloco,(7,1)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(3,3)),(Bloco,(0,2)),(Caixa,(1,2)),(Bloco,(2,0)),(Bloco,(3,0)),(Bloco,(4,0)),(Bloco,(2,2)),(Bloco,(7,2)),(Bloco,(7,3)),(Caixa,(6,3)),(Bloco,(4,4)),(Bloco,(7,4)),(Caixa,(6,4)),(Bloco,(4,5)),(Porta,(3,2)),(Bloco,(4,6)),(Bloco,(6,5)),(Bloco,(5,6))] --True
m3 = [(Porta,(0,4)),(Bloco,(0,5)),(Bloco,(1,5)),(Bloco,(2,5)),(Bloco,(3,2)),(Bloco,(3,3)),(Bloco,(3,4)),(Bloco,(4,1)),(Bloco,(5,1)),(Bloco,(6,2)),(Bloco,(7,3)),(Bloco,(8,4)),(Bloco,(9,5))] -- False
m4 = [(Bloco,(0,3)),(Bloco,(1,3)),(Porta,(2,1)),(Bloco,(2,2)),(Bloco,(3,1)),(Bloco,(4,1)),(Bloco,(5,1)),(Bloco,(5,2)),(Bloco,(5,3)),(Bloco,(6,3)),(Bloco,(7,3)),(Caixa,(8,3)),(Bloco,(8,4)),(Bloco,(8,5)),(Caixa,(9,2)),(Caixa,(9,3)),(Caixa,(9,4)),(Bloco,(9,5)),(Bloco,(10,2)),(Bloco,(10,3)),(Bloco,(10,4)),(Bloco,(10,5)),(Caixa,(11,1)),(Bloco,(11,2)),(Bloco,(12,2)),(Bloco,(12,3)),(Bloco,(13,4))] --True
m5 = [(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(1,4)),(Bloco,(1,5)),(Bloco,(1,6)),(Bloco,(2,6)),(Bloco,(3,3)),(Bloco,(3,4)),(Bloco,(3,5)),(Bloco,(3,6)),(Bloco,(4,3)),(Porta,(5,3)),(Bloco,(5,4))] -- True
m6 = [(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(1,4)),(Bloco,(1,5)),(Bloco,(1,6)),(Bloco,(3,3)),(Bloco,(3,4)),(Bloco,(3,5)),(Bloco,(3,6)),(Bloco,(4,3)),(Porta,(5,3)),(Bloco,(5,4))] --False
m7 = [(Bloco,(0,4)),(Porta,(1,3)),(Bloco,(1,4)),(Bloco,(2,1)),(Caixa,(2,3)),(Bloco,(2,4)),(Bloco,(3,5)),(Bloco,(4,4))] --True
m8 = [(Bloco,(0,0)),(Bloco,(0,1)),(Bloco,(0,2)),(Bloco,(0,3)),(Bloco,(0,4)),(Bloco,(0,5)),(Bloco,(0,6)),(Bloco,(1,0)),(Bloco,(1,1)),(Porta,(1,4)),(Bloco,(1,5)),(Bloco,(2,0)),(Bloco,(2,1)),(Bloco,(2,6)),(Bloco,(3,0)),(Bloco,(3,1)),(Bloco,(3,2)),(Caixa,(3,5)),(Bloco,(3,6)),(Bloco,(4,0)),(Bloco,(4,1)),(Bloco,(4,2)),(Bloco,(4,3)),(Bloco,(4,4)),(Bloco,(4,5)),(Bloco,(4,6))] --true

m1r,m2r,m3r, m4r, m5r, m6r, m7r, m8r, m9r, m10r, m11r  :: Mapa
m1r = [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Porta,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]
m2r = [[Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],
       [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
       [Bloco,Caixa,Bloco,Porta,Vazio,Vazio,Vazio,Bloco],
       [Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Caixa,Bloco],
       [Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Bloco],
       [Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio],
       [Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio]]
m3r = [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],[Porta,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio],[Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]]
m4r = [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Porta,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio],[Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Caixa,Bloco,Bloco,Bloco,Vazio],[Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Caixa,Caixa,Bloco,Vazio,Bloco,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Bloco,Vazio,Vazio,Bloco],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio]]
m5r = [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
       [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
       [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
       [Bloco,Bloco,Vazio,Bloco,Bloco,Porta],
       [Vazio,Bloco,Vazio,Bloco,Vazio,Bloco],
       [Vazio,Bloco,Vazio,Bloco,Vazio,Vazio],
       [Vazio,Bloco,Bloco,Bloco,Vazio,Vazio]]
m6r = [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
       [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
       [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
       [Bloco,Bloco,Vazio,Bloco,Bloco,Porta],
       [Vazio,Bloco,Vazio,Bloco,Vazio,Bloco],
       [Vazio,Bloco,Vazio,Bloco,Vazio,Vazio],
       [Vazio,Bloco,Vazio,Bloco,Vazio,Vazio]] 
m7r = [[Vazio,Vazio,Vazio,Vazio,Vazio],
       [Vazio,Vazio,Bloco,Vazio,Vazio],
       [Vazio,Vazio,Vazio,Vazio,Vazio],
       [Vazio,Porta,Caixa,Vazio,Vazio],
       [Bloco,Bloco,Bloco,Vazio,Bloco],
       [Vazio,Vazio,Vazio,Bloco,Vazio]]
m8r = [[Bloco,Bloco,Bloco,Bloco,Bloco], 
       [Bloco,Bloco,Bloco,Bloco,Bloco],
       [Bloco,Vazio,Vazio,Bloco,Bloco],
       [Bloco,Vazio,Vazio,Vazio,Bloco],
       [Bloco,Porta,Vazio,Vazio,Bloco],
       [Bloco,Bloco,Vazio,Caixa,Bloco],
       [Bloco,Vazio,Bloco,Bloco,Bloco]]
m9r = [[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
      ,[Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio]
      ,[Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]
      ,[Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]
      ,[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco]
      ,[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco]
      ,[Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Bloco,Bloco,Vazio]
      ,[Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio]
      ,[Bloco,Vazio,Bloco,Vazio,Bloco,Caixa,Caixa,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
      ,[Bloco,Porta,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
      ,[Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]
m10r = [[Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio]
       ,[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio]
       ,[Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio]
       ,[Bloco,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio]
       ,[Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]
       ,[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Bloco]
       ,[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Caixa,Caixa,Vazio,Vazio,Vazio,Bloco]
       ,[Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]
       ,[Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Caixa,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
       ,[Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]
m11r = [[Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],
        [Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Porta,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
        [Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
        [Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Caixa,Caixa,Caixa,Bloco],
        [Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
        [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)
m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

jogo1 :: Jogo
jogo1 = (Jogo [[Vazio,Vazio,Vazio,Vazio], [Vazio,Vazio,Vazio,Bloco,Bloco], [Vazio,Vazio,Vazio,Bloco,Bloco],[Bloco,Porta,Vazio,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (4,1) Oeste False))

resolveJogo1, resolveJogo2, resolveJogo3:: Jogo
resolveJogo1 = (Jogo m4r (Jogador (10,1) Oeste False)) 
resolveJogo2 = (Jogo m3r (Jogador (9,3) Oeste False))
resolveJogo3 = (Jogo m1r (Jogador (5,3) Oeste False))


