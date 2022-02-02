{- |
Module      : T0Dados
Description : Módulo auxiliar para LI1 21/22

Tipos de dados e funções auxiliares para a realização do projeto de LI1 em 2021/22.
 -}
module T0Dados (
    -- * Tipos de dados
    -- ** Básicos
  Coordenadas , Direcao(..),
    -- ** Mapas
  Mapa , Peca(..),
    -- ** Jogo
  Jogo(..) , Jogador(..) , Movimento(..) , Tipos(..) , Jogos(..) , OpcoesPrincipal(..) , OpcoesNiveis (..) , Menu(..)
  ) where

-- | Par de coordenadas de uma posição no 'Mapa'.
type Coordenadas = (Int, Int)

-- | Uma peça no 'Mapa'.
data Peca
  = Bloco -- ^ um bloco que é indestrutível e não movivel
  | Caixa -- ^ a caixa é como um bloco mas pode ser movida pelo 'Jogador'
  | Porta -- ^ a porta é a posição final do jogo
  | Vazio
  | PersonagemD
  | PersonagemE
  | BackgroundGame
  deriving (Show, Read, Eq, Ord)

type Mapa = [[Peca]]

-- | Direção de um 'Jogador' no 'Mapa'.
data Direcao
  = Este
  | Oeste
  deriving (Show, Read, Eq, Ord)

-- | O personagem que é controlado pelo 'Jogador'.
data Jogador = 
  Jogador
    Coordenadas -- ^ a posição atual no 'Mapa'
    Direcao -- ^ a direção atual
    Bool -- ^ um booleano que indica se o 'Jogador' está a carregar uma 'Caixa' ou não
  deriving (Show, Read, Eq, Ord)

-- | O nível de um jogo, que inclui o puzzle (mapa) e o personagem (jogador).
data Jogo =
  Jogo
    Mapa -- ^ o puzzle em si
    Jogador -- ^ o personagem do jogo
  deriving (Read, Eq)

-- | Os movimentos que podem ser tomados pelo jogador em cada estado do 'Jogo'.
data Movimento
  = AndarEsquerda -- ^ a acção de andar para a esquerda
  | AndarDireita -- ^ a ação de andar para a direita
  | Trepar -- ^ a ação de trepar uma caixa ou bloco
  | InterageCaixa -- ^ a ação de pegar ou largar uma caixa
  deriving (Show, Read, Eq, Ord)

{- | 
 - 'Tipos' representa um vasto tipo de informações, já que o mesmo podia ser utilizado em várias funções, decici utilizar
uma só 'data'; 
-}
data Tipos = Nada
           | Select
           | Play
           | Level
           | Tips
           | Exit
           | Back
           | Lvl1
           | Lvl2
           | Lvl3
           | Lvl4
           | Lvl5
           | Lvl6
           | Lvl7
           | Lvl8
           | Lvl9
           | Lvl10
           | Texto
           | WinCima
           | WinBaixo
           | Continue
           | NiveisGeral
           | OpcoesGeral
           | GameGeral 
           | BlockDude 
           | DragonBall
           | AmongUs
           | Theme
           | Show1
           | Show2
           | Show3
           | Show4
           | Show5
           | Show6
           | Show7
           | Show8
           | Show9
           | Show10
           | Theme1
           | Theme2
           | Theme3
           | ATheme
           | DBTheme
           | NTheme

{- | 
 - 'Jogos' é utilizado como __acumulador__ ou método de __"mémoria"__ pois é essa 'data' que organiza o nível que se está;
-}
data Jogos = Jogo1
           | Jogo2
           | Jogo3
           | Jogo4
           | Jogo5
           | Jogo6
           | Jogo7
           | Jogo8
           | Jogo9
           | Jogo10

{- | 
 - 'OpcoesPrincipal' representa as opções que irão aparecer no menu inicial;
-}
data OpcoesPrincipal = Jogar 
                     | Niveis
                     | Continuar
                     | Instrucoes
                     | Tema
                     | Sair

{- | 
 - 'OpcoesNiveis' semelhante ao de cima mas para os niveis;
-}
data OpcoesNiveis = Nivel1
                  | Nivel2
                  | Nivel3
                  | Nivel4
                  | Nivel5
                  | Nivel6
                  | Nivel7
                  | Nivel8
                  | Nivel9
                  | Nivel10
                  | Voltar

{- | 
 - 'Menu' é o que vai definir todas as anteriores de modo a poder dar /base/ ao jogo.
-}
data Menu = Controlador OpcoesPrincipal Tipos
          | Selecionador OpcoesNiveis Tipos
          | ModoJogo Jogo Jogo Jogos Tipos
          | Pergaminho Tipos
          | ImagemVitoria OpcoesPrincipal Jogo Jogos Tipos
          | Temas Tipos Tipos
