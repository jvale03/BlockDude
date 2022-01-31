{- |
Module      : Tarefa1_2021li1g004
Description : Validação de um potencial mapa
Copyright   : João Carlos Oliveira Vale <a100697@alunos.uminho.pt>; <carlosvalejcov@gmail.com>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g004 where

import LI12122
import Data.List (sortBy,nub,groupBy,group,sort)
import Data.Function (on)
import Data.Ord (comparing)

{- |
= __Validar Blocos__
-}

validaPotencialMapa mapa = validablocos mapa && validaporta mapa && validaCaixaFlutua mapa
                           && validavazio mapa && validabase mapa

{- |
@
validablocos blocos = testbool (mergelists (validablocosaux blocos)) (validablocosaux blocos)
@
-}
{- |
A função 'validablocos' retorna um @Bool@ se o mapa não tiver /pecas/ nas mesmas
coordenadas. Sendo composta por 5 auxiliares da seguinte forma:

>>> validablocos [(Bloco,(5,5)),(Caixa,(5,5))]
False
>>> validablocos [(Bloco,(0,1)),(Bloco,(1,1))]
True
-}
{- |
== Auxiliares
-}
validablocos :: [(Peca,Coordenadas)] -> Bool
validablocos blocos = testbool (mergelists (validablocosaux blocos)) (validablocosaux blocos)

{- |
@
validablocosaux [] = []
validablocosaux ((peca,coord):rl) = coord:validablocosaux rl
@
-}
{- |
Função usada para simplificar a lista
devolvendo apenas uma lista com as 'Coordenadas'

>>> validablocosaux [(Bloco,(5,2)),(Caixa,(3,3))]
[(5,2),(3,3)]
-}
validablocosaux :: [(Peca,Coordenadas)] -> [Coordenadas] 
validablocosaux [] = []
validablocosaux ((peca,coord):rl) = coord:validablocosaux rl

{- |
@
mergelists [x] = [[x]]
mergelists x = group (sort x)
@
-}
{- |
Faz um 'group' na lista composta apenas por
coordenadas de modo a juntar numa só lista.

>>> mergelists [(2,1),(5,1),(2,1)]
[[(2,1),(2,1)],[5,1]]
-}
mergelists :: [Coordenadas] -> [[Coordenadas]]
mergelists [x] = [[x]]
mergelists x = group (sort x) 

{- | 
@
testbool xx x 
    | length xx == length x = True
    | otherwise = False
@
-}
{- |
Ligada à função anterior 'testbool', mede o /tamanho/ da
lista obtida por 'mergelist' e o da obtida por 'validablocosaux'.
Em caso de resultado diferente, devolve um /@False@/

>>> testbool [[(2,1)],[(3,1),(3,1)]] [(3,1),(2,1),(3,1)]
False
-}
{- |
= __Validar Porta__
-}
testbool :: [[Coordenadas]] -> [Coordenadas] -> Bool
testbool xx x 
    | length xx == length x = True
    | otherwise = False


{- |
@
validaporta [] = False
validaporta porta 
    | length (portaaux porta) == 1 = True
    | otherwise = False
@
-}
{- |
Composta por uma auxiliar, 'validaporta' permite concluir se 
existe __uma e uma só__ 'Porta' no mapa.

>>> [(Bloco,(3,2)),(Porta,(2,2))]
True
-}
{- |
== Auxiliar
-}
validaporta :: [(Peca,Coordenadas)] -> Bool
validaporta [] = False
validaporta porta 
    | length (portaaux porta) == 1 = True
    | otherwise = False


{- | 
@
portaaux [] = []
portaaux ((peca,(x,y)):t) 
    | peca == Porta = peca:portaaux t
    | otherwise = portaaux t
@
-}
{- |
'portaaux' devolve uma lista em que as peças são iguais 
a 'Porta'.

>>> portaaux [(Bloco,(2,1))]
[ ]

>>> portaaux [(Bloco,(2,1)),(Porta,(2,2))]
[Porta] 
-}
{- |
= __Validar Caixas__
-}
portaaux :: [(Peca,Coordenadas)] -> [Peca]
portaaux [] = []
portaaux ((peca,(x,y)):t) 
    | peca == Porta = peca:portaaux t
    | otherwise = portaaux t


 
{- | 
Função que avalia se nenhuma 'Caixa' flutua, ou seja, se todas as peças 'Caixa' 
têm por baixo outra 'Caixa' __ou__ um 'Bloco' e se a __ultima__ 'Caixa' está em cima de um 'Bloco'.

>>> validaCaixaFlutua [(Bloco,(1,2)),(Caixa,(1,1)),(Bloco,(2,3)),(Vazio,(4,5)),(Caixa,(2,2)),(Caixa,(2,1)),(Vazio,(0,0)),(Bloco,(0,2))]
True

>>> validaCaixaFlutua [(Vazio,(0,0)),(Vazio,(0,1)),(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1)),(Vazio,(3,2)),(Caixa,(3,1))]
False
-}
{- |
== Auxiliares
-}
validaCaixaFlutua :: [(Peca,Coordenadas)] -> Bool
validaCaixaFlutua a = validaCaixa (armazenaCaixaCoord a) (armazenaCaixaCoord1 a) (armazenaBlocoCoord a)

{- |
@
validaCaixa [] _ _ = True
validaCaixa (h1:t) caixas blocos  | (elem h1 caixas || elem h1 blocos) && validaCaixa t caixas blocos = True
                               | otherwise = False
@
-}
{- |
Função que recebe a lista de coordendas das peças 'Caixa', 
a lista de /coordenadas/ de 'Caixa', __subtraindo__ uma unidade a __y__,
e a lista de /coordenadas/ das peças 'Bloco' __subtraindo__ uma
unidade a __y__. A partir destas listas, testa se __nenhuma__ 
'Caixa' flutua.

>>> validaCaixa [(1,0),(2,1),(2,0)] [(1,0),(2,1),(2,0)] [(1,1),(2,2),(0,1)]
True

>>> validaCaixa [(2,1),(3,1)] [(2,0),(3,0)] [(0,1),(1,1),(2,1)]
False
-}
validaCaixa :: [Coordenadas] -> [Coordenadas] -> [Coordenadas] -> Bool
validaCaixa [] _ _ = True
validaCaixa (h1:t) caixas blocos  | (elem h1 caixas || elem h1 blocos) && validaCaixa t caixas blocos = True
                               | otherwise = False

{- |
@
armazenaCaixaCoord [] = []
armazenaCaixaCoord ((peca,coord):cl) | peca == Caixa = coord : armazenaCaixaCoord cl
                                     | otherwise = armazenaCaixaCoord cl
@
-}
{- |
Função que __armazena__ as /coordenadas/ de todas as peças 'Caixa' do mapa.

>>> armazenaCaixaCoord [(Bloco,(1,2)),(Caixa,(1,1)),(Bloco,(2,3)),(Vazio,(4,5)),(Caixa,(2,2)),(Caixa,(2,1)),(Vazio,(0,0)),(Bloco,(0,2))]
[(1,1),(2,2),(2,1)]

>>> armazenaCaixaCoord [(Vazio,(0,0)),(Vazio,(0,1)),(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1)),(Vazio,(3,2)),(Caixa,(3,1))]
[(2,1),(3,1)]
-}
armazenaCaixaCoord :: [(Peca,Coordenadas)] -> [Coordenadas]
armazenaCaixaCoord [] = []
armazenaCaixaCoord ((peca,coord):cl) | peca == Caixa = coord : armazenaCaixaCoord cl
                                     | otherwise = armazenaCaixaCoord cl

{- |
@
armazenaCaixaCoord1 [] = []
armazenaCaixaCoord1 ((peca,(x,y)):cl) | peca == Caixa = (x,y-1) : armazenaCaixaCoord1 cl
                                      | otherwise = armazenaCaixaCoord1 cl
@
-}
{- |
Função que __armazena__ as /coordenadas/ de toda a 'Caixa' do mapa,
__subtraindo-lhes__ 1 unidade a __y__.

>>> armazenaCaixaCoord1 [(Bloco,(1,2)),(Caixa,(1,1)),(Bloco,(2,3)),(Vazio,(4,5)),(Caixa,(2,2)),(Caixa,(2,1)),(Vazio,(0,0)),(Bloco,(0,2))]
[(1,0),(2,1),(2,0)]

>>> armazenaCaixaCoord1 [(Vazio,(0,0)),(Vazio,(0,1)),(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1)),(Vazio,(3,2)),(Caixa,(3,1))]
[(2,0),(3,0)]
-}
armazenaCaixaCoord1 :: [(Peca,Coordenadas)] -> [Coordenadas]
armazenaCaixaCoord1 [] = []
armazenaCaixaCoord1 ((peca,(x,y)):cl) | peca == Caixa = (x,y-1) : armazenaCaixaCoord1 cl
                                      | otherwise = armazenaCaixaCoord1 cl

{- |
@
armazenaBlocoCoord [] = []
armazenaBlocoCoord ((peca,(x,y)):cl) | peca == Bloco = (x,y-1) :armazenaBlocoCoord cl
                                     | otherwise = armazenaBlocoCoord cl
@
-}
{- |
Função que armazena as /coordenadas/ de todo o 'Bloco' do mapa,
__subtraindo-lhes__ 1 unidade a __y__.

>>> armazenaBlocoCoord [(Bloco,(1,2)),(Caixa,(1,1)),(Bloco,(2,3)),(Vazio,(4,5)),(Caixa,(2,2)),(Caixa,(2,1)),(Vazio,(0,0)),(Bloco,(0,2))]
[(1,1),(2,2),(0,1)]

>>> armazenaBlocoCoord [(Vazio,(0,0)),(Vazio,(0,1)),(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1)),(Vazio,(3,2)),(Caixa,(3,1))]
[(0,1),(1,1),(2,1)]
-}
{- |
= __Validar Vazio__
-}
armazenaBlocoCoord :: [(Peca,Coordenadas)] -> [Coordenadas]
armazenaBlocoCoord [] = []
armazenaBlocoCoord ((peca,(x,y)):cl) | peca == Bloco = (x,y-1) :armazenaBlocoCoord cl
                                     | otherwise = armazenaBlocoCoord cl


{- |
@
validavazio x 
    | validavazioaux x 0 > 0 = True 
    | validavazioaux x 0 == 0 && omitidos (medidas x) x == False = False
    | validavazioaux x 0 == 0 && omitidos (medidas x) x == True = True
@
-}
{- |
Esta função, composta por 3 auxiliares, testa se existe __pelo menos__
um 'Vazio' no mapa.
Caso exista um 'Vazio' definido no mapa, a função devolve 
logo um @True@. Caso contrário, a função irá testar se 
existe 'Vazio' por omissão.

>>> validavazio [(Bloco,(0,0)),(Bloco,(1,0)),(Bloco,(2,1))]
True
-}
{- |
== Auxiliares
-}
validavazio :: [(Peca,Coordenadas)] -> Bool
validavazio x 
    | validavazioaux x 0 > 0 = True 
    | validavazioaux x 0 == 0 && omitidos (medidas x) x == False = False
    | validavazioaux x 0 == 0 && omitidos (medidas x) x == True = True


{- |
@
validavazioaux [] _ = 0
validavazioaux ((vazio,coords):xl) i 
    | vazio == Vazio = 1 + validavazioaux xl i
    | otherwise = validavazioaux xl i
@
-}
{- | 
Esta função irá contar o número de 'Vazio' fornecidos 
ao mapa.

>>> validavazioaux [(Bloco,(5,9)),(Caixa,(10,5)),(Vazio,(4,2)),(Vazio,(3,3)),(Bloco,(2,9))] 0
2
-}

validavazioaux :: [(Peca,Coordenadas)] -> Int -> Int 
validavazioaux [] _ = 0
validavazioaux ((vazio,coords):xl) i 
    | vazio == Vazio = 1 + validavazioaux xl i
    | otherwise = validavazioaux xl i

{- |
@
medidas [] = (0,0)
medidas [(peca,(x,y))] = (x+1,y+1)
medidas f@((peca,(x,y)):t) = ((maximum (listax f)+1),(maximum (listay f)+1)) 
    where
        listax [] = []
        listax ((peca,(x,y)):t) = x:listax t    
        listay [] = []
        listay ((peca,(x,y)):t) = y:listay t
@
-}
{- | 
A função 'medidas' mede o tamanho do mapa em __x__ e em __y__
para auxiliar a funçao seguinte e permitir descobrir se existe
'Vazio' __declarados por omissão__. 

>>> medidas [(Bloco,(5,9)),(Caixa,(10,5)),(Vazio,(4,2)),(Vazio,(3,3)),(Bloco,(2,9))]
(11,10)
-}
medidas :: [(Peca, Coordenadas)] -> (Int,Int) --mede tamanho do mapa para x e y
medidas [] = (0,0)
medidas [(peca,(x,y))] = (x+1,y+1)
medidas f@((peca,(x,y)):t) = ((maximum (listax f)+1),(maximum (listay f)+1)) 
    where
        listax [] = []
        listax ((peca,(x,y)):t) = x:listax t    
        listay [] = []
        listay ((peca,(x,y)):t) = y:listay t

{- |
@
omitidos _ [] = False  
omitidos (xf,yf) xw 
    | length xw < xf * yf = True
    | otherwise = False
@
-}
{- |
'omitidos' calcula a área do mapa com a função 'medidas' anterior
e compara ao número de peças __declaradas__. Caso a diferença
seja diferente de 0, 'omitidos' devolve um @True@ .
-}
{- |
= __validabase__
-}
omitidos :: (Int,Int) -> [(Peca,Coordenadas)]-> Bool 
omitidos _ [] = False  
omitidos (xf,yf) xw 
    | length xw < xf * yf = True
    | otherwise = False



{- |
@
validabase list 
    | blocksbycolumn (justBlocks list) == True = mainfunction (superGroup (nub (order (superGroupSort (superGroup (justBlocks list))))))
    | otherwise = False
@
-}
{- |
Esta função mais complexa, constituida por __7__ funções auxiliares,
testa a __base do mapa__, __não__ podendo haver espaços vazios, impedindo quedas.

>>> validabase [(Bloco,(0,0)),(Bloco,(1,0)),(Bloco,(2,1)),(Bloco(3,0))]
True

>>> validabase [(Bloco,(0,0)),(Bloco,(2,0)),(Bloco,(2,1)),(Bloco(3,0))]
False
-}
{- | 
== Auxiliares
-}
validabase :: [(Peca, Coordenadas)] -> Bool
validabase list 
    | blocksbycolumn (justBlocks list) == True = mainfunction (superGroup (nub (order (superGroupSort (superGroup (justBlocks list))))))
    | otherwise = False

{- |
@
justBlocks [] = []
justBlocks [(peca,(x,y))] 
    | peca == Bloco = [(x,y)]
    | otherwise = []
justBlocks ((peca,(x,y)):t) 
    | peca == Bloco = (x,y):justBlocks t
    | otherwise = justBlocks t
@
-}
{- |
'justBlocks', tal como o nome indica, simplifica a lista, /devolvendo apenas/
uma lista com as __coordenadas__ das peças 'Bloco'. 
-}
justBlocks :: [(Peca, Coordenadas)] -> [Coordenadas]   
justBlocks [] = []
justBlocks [(peca,(x,y))] 
    | peca == Bloco = [(x,y)]
    | otherwise = []
justBlocks ((peca,(x,y)):t) 
    | peca == Bloco = (x,y):justBlocks t
    | otherwise = justBlocks t

{- |
@
justxBlocks [] = []
justxBlocks [(x,y)] = [x]
justxBlocks ((x,y):t) = x:justxBlocks t
@
-}
{- | 
'justxBlocks' simplifica a função simplificada, de modo a devolver 
uma lista /apenas com os __x__ das peças 'Bloco'/.
-} 
justxBlocks :: [Coordenadas] -> [Int]
justxBlocks [] = []
justxBlocks [(x,y)] = [x]
justxBlocks ((x,y):t) = x:justxBlocks t
 
{- |
@
blocksbycolumn [] = False
blocksbycolumn [(x,y)] = False
blocksbycolumn f@((x,y):t)
    | length (nub (justxBlocks f)) -1 == lengths f = True
    | otherwise = False
        where 
            lengths [] = 0
            lengths [(x,y)] = x
            lengths h@((x,y):t) = maximum (list h)
                where 
                    list [] = [0]
                    list ((x,y):t) = x:list t
@
-}
{- | 
A função 'blocksbycolumn' permite testar se em /cada coluna/,
existe __pelo menos__ um 'Bloco' devolvendo um @Bool@.
-}
blocksbycolumn :: [Coordenadas] -> Bool 
blocksbycolumn [] = False
blocksbycolumn [(x,y)] = False
blocksbycolumn f@((x,y):t)
    | length (nub (justxBlocks f)) -1 == lengths f = True
    | otherwise = False
        where 
            lengths [] = 0
            lengths [(x,y)] = x
            lengths h@((x,y):t) = maximum (list h)
                where 
                    list [] = [0]
                    list ((x,y):t) = x:list t


{- |
@
superGroup = map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst) . sortBy (comparing fst)
@
-}
{- |
'superGroup' é funçao muito especifica que converte os pares com __x__ igual, num unico 
par com __x__, e com os __y__ concatenados numa só lista.

>>> superGroup [(0,1),(1,3),(0,2)]
[(0,[1,2]),(1,[3])]
-}
superGroup :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
superGroup = map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst) . sortBy (comparing fst)
    
{- |
@
superGroupSort [] = []
superGroupSort ((x,y):t) = (x,reverse(sort y)):superGroupSort t
@
-}
{- |
'superGroupSort' é uma funçao que __ordena__ a lista dentro do par, ou seja, 
ordena os __y__ por ordem /decrescente/.

>>> superGroupSort [(1,[6,2,4,8,5]),(3,[51,2,4,5,4])]
[(1,[8,6,5,4,2]),(3,[51,5,4,4,2])]
-}
superGroupSort :: [(Int,[Int])] -> [(Int,[Int])]
superGroupSort [] = []
superGroupSort ((x,y):t) = (x,reverse(sort y)):superGroupSort t

{- |
@
order [] = []
order ((x,[y]):t) = (x,y):order t
order ((x,(y:yy:t1)):t)
    | x == 0 = (x,maximum (y:yy:t1)):order t
    | otherwise = 
        if y-1 == yy then (x,y):(x,yy):order ((x,yy:t1):t) 
        else (x,y):order t
@
-}
{- | 
Esta funçao devolve as coordenadas com maior __y__ de /cada coluna/, ou seja,
de cada lista dada no /segundo/ elemento de cada par.
No entanto, caso haja um __muro__ ou queda, esta função irá
devolver mais que um __y__ pois se trata de uma grande parede
que __não__ pode ter buraco.

>>> order [(0,[9,7,1]),(1,[7,6,5,4])]
[(0,9),(1,7),(1,6),(1,5)]
-}
order :: [(Int,[Int])] -> [Coordenadas]
order [] = []
order ((x,[y]):t) = (x,y):order t
order ((x,(y:yy:t1)):t)
    | x == 0 = (x,maximum (y:yy:t1)):order t
    | otherwise = 
        if y-1 == yy then (x,y):(x,yy):order ((x,yy:t1):t) 
        else (x,y):order t


{- |
@
mainfunction :: [(Int,[Int])] -> Bool
mainfunction [] = False
mainfunction [x] = True
mainfunction [(x,y),(xx,yy)]
    | x+1==xx && ((elem ((head y)+1) yy)
                ||(elem ((last y)-1) yy)
                ||(elem (head y) yy)
                ||(elem (last y) yy)
                ||(testeauxiliar y yy)) = True
    | otherwise = False
mainfunction ((x,y):(xx,yy):t)
    | x+1==xx && ((elem ((head y)+1) yy)
                ||(elem ((last y)-1) yy)
                ||(elem (head y) yy)
                ||(elem (last y) yy)
                ||(testeauxiliar y yy)) = mainfunction ((xx,yy):t)
    | otherwise = False

testeauxiliar :: [Int] -> [Int] -> Bool
testeauxiliar [] _ = False
testeauxiliar (h:t) yy 
    | elem h yy = True
    | otherwise = testeauxiliar t yy 
@
-}
{- |
Esta é a função principal, pois irá testar se existe uma /ligação/ 
em toda a base, ou seja, __não há espaços vazios__ lá. Para isso, irá ser sempre testado, para todos
os casos possíveis, se o __y__ de __x__ está na lista dos __y__ para __x+1__, ou seja, se no __x__ a seguir há
alguma continuidade de chão. Caso haja, devolve um 'True', caso contrário, devolve um 'False'!

>>> mainfunction [(0,[4])(1,[4,5,6]),(2,[7])]
True

>>> mainfunction [(0,[4])(1,[4,5,6]),(2,[8])]
False
-}

mainfunction :: [(Int,[Int])] -> Bool
mainfunction [] = False
mainfunction [x] = True
mainfunction [(x,y),(xx,yy)]
    | x+1==xx && ((elem ((head y)+1) yy)
                ||(elem ((last y)-1) yy)
                ||(elem (head y) yy)
                ||(elem (last y) yy)
                ||(testeauxiliar y yy)) = True
    | otherwise = False
mainfunction ((x,y):(xx,yy):t)
    | x+1==xx && ((elem ((head y)+1) yy)
                ||(elem ((last y)-1) yy)
                ||(elem (head y) yy)
                ||(elem (last y) yy)
                ||(testeauxiliar y yy)) = mainfunction ((xx,yy):t)
    | otherwise = False


{- |
@
testeauxiliar :: [Int] -> [Int] -> Bool
testeauxiliar [] _ = False
testeauxiliar (h:t) yy 
    | elem h yy = True
    | otherwise = testeauxiliar t yy
@
Esta função testa se um certo __y__ em __x__ se encontra na lista de __y__ quando __x+1__ para saber então se existe
continuação de mapa! 
-}
testeauxiliar :: [Int] -> [Int] -> Bool
testeauxiliar [] _ = False
testeauxiliar (h:t) yy 
    | elem h yy = True
    | otherwise = testeauxiliar t yy

