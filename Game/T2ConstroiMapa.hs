
{- |
Module      : T2ConstroiMapa
Description : Construção/Desconstrução do mapa
Copyright   : João Carlos Oliveira Vale <a100697@alunos.uminho.pt>; <carlosvalejcov@gmail.com>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module T2ConstroiMapa where

import T0Dados
import T1ValidaMapa 

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa pecas = updateMatrix pecas (buildMatrix pecas)


{- |
= __ConstroiMapa__
-}
{- |
== Auxiliares
-}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa [] = []
desconstroiMapa x = ordena (cortavazio (function x (concat x) (0,0)))

{- |
Função que __cria__ uma /matriz/ com as dimensões do mapa em que @todos@ os elementos 
da mesma são 'Vazio'. Ficando ja assim declarados também os elementos por 
omissão.
-}
buildMatrix :: [(Peca,Coordenadas)] -> [[Peca]]
buildMatrix [] = []
buildMatrix [(peca,(x,y))] = replicate (y+1) (replicate (x+1) Vazio)
buildMatrix f@((peca,(x,y)):t) = replicate   
    ((maximum (listy f))+1) (replicate ((maximum (listx f))+1) Vazio)
        where 
            listx [] = []
            listx ((peca,(x,y)):t) = x:listx t
            listy [] = []
            listy ((peca,(x,y)):t) = y:listy t

{- |
Esta função __substitui__ na /matriz/ construida na auxiliar anterior, os 
espaços onde as peças são @diferentes@ de 'Vazio'. Ficando assim o 'Mapa'
do jogo concluido.

>>> updateMatrix [(Bloco,(1,1))] [[Vazio,Vazio],[Vazio,Vazio]]
[[Vazio,Vazio],[Vazio,Bloco]]
-}
{- |
= __DesconstroiMapa__
-}
{- |
== Auxiliares
-}
updateMatrix :: [(Peca,Coordenadas)] -> [[Peca]] -> [[Peca]] 
updateMatrix [(peca,(x,y))] m = 
    take y m ++ 
    [take x (m !! y) ++ [peca] ++ drop (x + 1) (m !! y)] ++ 
    drop (y + 1) m
updateMatrix ((peca,(x,y)):t) m = updateMatrix t (updateMatrix [(peca,(x,y))] m)


{- |
Esta função auxilia na contagem de coordenadas, pois a cada vez
que acaba a linha de __x__, adiciona +1 ao __y__.
-}
tamanho :: [[Peca]] -> Int 
tamanho [] = 0
tamanho [a] = length a
tamanho a = div (length (concat a)) (length a) 


{- |
__Elimina__ os blocos 'Vazio' existentes na lista final de modo
a ficarem apenas os importantes.
-}
cortavazio :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
cortavazio [] = []
cortavazio ((peca,(x,y)):t) = case peca of Vazio -> cortavazio t
                                           _     -> (peca,(x,y)):cortavazio t

{- |
Tal como o nome indica, esta funçao __ordena__ as /peças/ para a desconstrução
do mapa ser mais perceptivel.
-}
ordena :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
ordena [] = []
ordena [(x)] = [(x)]
ordena ((peca,coords):t) = insert (peca,coords) (ordena t)
    where 
        insert x [] = [x]
        insert (peace,x) ((peca,coords):t) 
            | x > coords = (peca,coords) : insert (peace,x) t
            | x < coords = (peace,x) : (peca,coords) : t 

{- |
Esta é a função que trata de obter as /coordenadas/ para cada 'Peca'.
Isto é, calcula-se quantas listas tem na /matriz/, e sabe-se de quantos
em quantos __x__ se tem de __aumentar__ o __y__. Assim, é possível calcular
as /coordenadas/ de cada bloco __não__ 'Vazio'.
-}
function :: [[Peca]] -> [Peca] -> (Coordenadas) -> [(Peca,Coordenadas)]
function _ [] _ = [] 
function l (peca:b) (x,y) 
    | tamanho l > x+1 = (peca,(x,y)):function l b (x+1,y)
    | tamanho l == x+1 = (peca,(x,y)):function l b (x-tamanho l +1,y+1)
