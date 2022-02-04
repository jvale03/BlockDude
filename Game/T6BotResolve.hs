{- |
Module      : T6BotResolve
Description : Resolução de um puzzle
Copyright   : João Carlos Oliveira Vale <a100697@alunos.uminho.pt>; <carlosvalejcov@gmail.com>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module T6BotResolve where

import T0Dados
import T4MovimentarJogo

{- |
A função 'resolveJogo' é a função que determina se o /Bot/ consegue ou não
devolver um resultado com o número de jogadas fornecido. Caso consiga, irá devolver apenas
uma solução e esta será a primeira, daí o 'head', calculada por ele pois é essa a que terá menor número 
de jogadas! 'reverse' irá colocar as jogadas pela ordem correta. 
-}
{- |
== Auxiliar
-}
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo i jogo 
    | resolveAuxiliar i [jogo] [] == [] = Nothing
    | otherwise = Just $ reverse $ head $ resolveAuxiliar i [jogo] []

{- |
Esta função tem o nome de auxiliar, no entanto, é a função principal de todo o /Bot/.

Com o __acumulador__ 'jogadas', é possível armazenar a lista de jogadas que o /Bot/ vai fazendo até
chegar à 'Porta' ¹, excluindo depois aqueles que ele fez mas que __não__ chegaram à 'Porta' através do tamanho
da lista com as jogadas, ou seja, se o tamanho da lista for igual ao número de jogadas, então já nao
há mais jogadas possiveis e por isso, se não chegou à 'Porta', então essa não é uma solução! Nesse caso, irá devolver uma lista vazia!

¹ /(sabe-se que chegou à 'Porta', pois através das coordenadas do personagem, sabe-se a posição que ele ocupa
nas listas do mapa. Se após descobrir a 'Peca' nas respetivas coordenadas a mesma for igual à 'Porta', 
então a função irá devolver a lista de movimentos correspondente!)/

'num' significa o número de 'jogadas' dados pelo utilizador. A sua utilidade na função foi explicada no paragrafo
anterior. 

__ @x < 0 = []@ __: este caso de paragem em específico serve para cortar os casos em que o 'Jogador' sai do mapa às vezes se não tiver
muros a volta, o personagem pode sair do mapa e dar casos negativos, por exemplo.

Por fim, após o 'otherwise', a função testa sempre se a posição em que se encontra o boneco (para as quatro jogadas
possíveis), assim como o estado do mapa, ja foram repetidos alguma vez. Isto é, se as 'Pecas' e o estado do personagem estiverem iguais ao que já estiveram alguma vez, 
então a função não faz nada. No caso de ser diferente, a função irá juntar esse movimento, 
que proporcionou um Mapa diferente, à lista de listas de 'jogadas'! 

 -- 'pos' representa um 'Jogo'

 -- 'list' representa a lista onde se vão armazenas as jogadas diferentes!

>>> moveJogador (Jogo [[Bloco,Porta,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (3,0) Oeste False)) AndarEsquerda)
[(Jogo [[Bloco,Porta,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (3,0) Oeste False)),(Jogo [[Bloco,Porta,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (2,0) Oeste False))]

>>> moveJogador (Jogo [[Bloco,Porta,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (3,0) Este False)) AndarDireita)
[(Jogo [[Bloco,Porta,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (3,0) Este False))]

Estes exemplos são apenas para o que a função faz no 'otherwise'!
 
O aspeto mais importante nesta função é o teste de igualdade, pois é isso que vai fazer devolver sempre a jogada
com o menor número de movimentos possíveis, pois rejeita aqueles que não afetam o mapa, ou seja, desnecessários!
-}

resolveAuxiliar :: Int -> [Jogo] -> [Movimento] -> [[Movimento]]
resolveAuxiliar num list @ (pos @ (Jogo mapa (Jogador (x,y) _ _)) : _) jogadas 
    | x < 0 = []
    | Porta == last (take (x+1) (last (take (y+1) mapa)))  = [jogadas]
    | length jogadas == num = []
    | otherwise = (if elem (moveJogador pos AndarEsquerda) list then [] 
                   else resolveAuxiliar num ((moveJogador pos AndarEsquerda) : list) (AndarEsquerda : jogadas)) ++ 
                  (if elem (moveJogador pos AndarDireita) list then []
                   else resolveAuxiliar num ((moveJogador pos AndarDireita) : list) (AndarDireita : jogadas)) ++
                  (if elem (moveJogador pos Trepar) list then [] 
                   else resolveAuxiliar num ((moveJogador pos Trepar) : list) (Trepar : jogadas)) ++
                  (if elem (moveJogador pos InterageCaixa) list then [] 
                   else resolveAuxiliar num ((moveJogador pos InterageCaixa) : list) (InterageCaixa : jogadas))


