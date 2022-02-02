{- |
Module      : T5Grafica
Description : Aplicação Gráfica
Copyright   : João Carlos Oliveira Vale <a100697@alunos.uminho.pt>; <carlosvalejcov@gmail.com>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}

module T5Grafica where

import T0Dados
import T0Mapas
import T4MovimentarJogo
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit

------------
--- data ---
------------

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
           | NiveisNormal
           | OpcoesNormal
           | GameNormal  
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

data OpcoesPrincipal = Jogar 
                     | Niveis
                     | Continuar
                     | Instrucoes
                     | Tema
                     | Sair

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

data Menu = Controlador OpcoesPrincipal Tipos
          | Selecionador OpcoesNiveis Tipos
          | ModoJogo Jogo Jogo Jogos Tipos
          | Pergaminho Tipos
          | ImagemVitoria OpcoesPrincipal Jogo Jogos Tipos
          | Temas Tipos Tipos

------------
--- type ---
------------

{- |
== Types
-}
{- |
Nesta 'type':
 
 -- 'Menu' vai representar em que parte o 'Jogo' tem de estar.
 -- '(Float,Float)' representa as __coordenadas__ em que as imagens começarão por ser apresentadas!
 -- '[Picture]''s representam todas as listas de 'loads' para poder apresentar as imagens quando necessário.
 -- 'Float' tem como efeito representar o número em que o __tempo__ irá começar a ser contado. 
-}

type EstadoGloss = (Menu, (Float,Float), [Picture], [Picture], [Picture], [Picture], [Picture], [Picture],Float)

---------------
--- loadBmp ---
---------------

{- |
== Load Imagens 
-}
{- |
Dividido em categorias, cada __load__ está atribuido a uma parte do jogo, isto é, um load para
o __mapa__, __menu__, __etc__.
-}

loadMenus :: IO [Picture]
loadMenus = do jogar <- loadBMP "Jogar.bmp"
               nivel <- loadBMP "Niveis.bmp"
               instrucoes <- loadBMP "Instrucoes.bmp"
               sair <- loadBMP "Sair.bmp"
               seta <- loadBMP "Select.bmp"
               blockdude <- loadBMP "BlockDude.bmp"
               db <- loadBMP "DragonBall.bmp"
               among <- loadBMP "Among.bmp"
               voltar <- loadBMP "Voltar.bmp"
               temas <- loadBMP "Temas.bmp"
               atheme <- loadBMP "AmongTheme.bmp"
               dbtheme <- loadBMP "DBTheme.bmp"
               ntheme <- loadBMP "NormalTheme.bmp" 
               return [(Scale 5 5 jogar),(Scale 5 5 nivel),(Scale 5 5 instrucoes),(Scale 5 5 sair),(Translate (-38) (92) (Scale 3 3 seta)),
                       (Scale 5 5 blockdude),(Scale 5 5 db),(Scale 5 5 among),(Scale 5 5 voltar),(Scale 5 5 temas),
                       (Translate 250 (0) (Scale 0.45 0.45 ntheme)),(Translate 250 (0) (Scale 0.45 0.45 dbtheme)),(Translate 250 (0) (Scale 0.45 0.45 atheme))]

loadJogar :: IO [Picture]
loadJogar = do portaD <- loadBMP "PrincesaRight.bmp"
               portaE <- loadBMP "PrincesaLeft.bmp"
               personD <- loadBMP "DudeRight.bmp"
               personE <- loadBMP "DudeLeft.bmp"
               bloco <- loadBMP "NormalBlock.bmp"
               caixa <- loadBMP "NormalBox.bmp"
               --
               buttom <- loadBMP "Buttom.bmp"
               amongR <- loadBMP "AmongR.bmp"
               amongL <- loadBMP "AmongL.bmp"
               amongBox <- loadBMP "AmongBox.bmp"
               amongBlock <- loadBMP "AmongBlock.bmp"
               --
               shenlong <- loadBMP "Shenlong.bmp"
               gokuR <- loadBMP "GokuR.bmp"
               gokuL <- loadBMP "GokuL.bmp"
               dbBox <- loadBMP "DBBox.bmp"
               dbBlock <- loadBMP "DBBlock.bmp"            
               return [(Scale 0.24 0.24 portaE),(Scale 0.24 0.24 portaD),(Translate 0 (0) (Scale 0.3 0.29 personD)),(Translate 0 (0) (Scale 0.3 0.29 personE)),(Translate (0) (0) (Scale 0.3 0.3 bloco)),(Translate (0) (0) (Scale 0.3 0.3 caixa)),
                       (Translate (30) (-31) (Scale 1.05 1.05 buttom)),(Translate (78) (-76) (Scale 2 2 amongR)),(Translate (64) (-76) (Scale 2 2 amongL)),(Translate (126) (-126) (Scale 3 3 amongBlock)),(Translate (126) (-126) (Scale 3 3 amongBox)),
                       (Translate (0) (0) (Scale 0.11 0.14 shenlong)),(Translate (-3) (1) (Scale 0.19 0.170 gokuR)),(Translate (-3) (1) (Scale 0.19 0.170 gokuL)),(Translate (126) (-126) (Scale 3 3 dbBlock)),(Translate (120) (-120) (Scale 2.9 2.9 dbBox))]

loadNiveis :: IO [Picture]
loadNiveis = do nivel1 <- loadBMP "1.bmp"
                nivel2 <- loadBMP "2.bmp"
                nivel3 <- loadBMP "3.bmp"
                nivel4 <- loadBMP "4.bmp"
                nivel5 <- loadBMP "5.bmp"
                nivel6 <- loadBMP "6.bmp"
                nivel7 <- loadBMP "7.bmp"
                nivel8 <- loadBMP "8.bmp"
                nivel9 <- loadBMP "9.bmp"
                nivel10 <- loadBMP "10.bmp"
                voltar <- loadBMP "Voltar.bmp"
                seta <- loadBMP "Select.bmp"
                level1 <- loadBMP "Level1.bmp"
                level2 <- loadBMP "Level2.bmp"
                level3 <- loadBMP "Level3.bmp"
                level4 <- loadBMP "Level4.bmp"
                level5 <- loadBMP "Level5.bmp"
                level6 <- loadBMP "Level6.bmp"
                level7 <- loadBMP "Level7.bmp"
                level8 <- loadBMP "Level8.bmp"
                level9 <- loadBMP "Level9.bmp"
                level10 <- loadBMP "Level10.bmp"
                return [(Scale 5 5 nivel1),(Scale 5 5 nivel2),(Scale 5 5 nivel3),(Scale 5 5 nivel4),(Scale 5 5 nivel5),(Scale 5 5 nivel6),(Scale 5 5 nivel7),
                        (Scale 5 5 nivel8),(Scale 5 5 nivel9),(Scale 5 5 nivel10),(Translate (0) 0 (Scale 5 5 voltar)),(Translate (-50) 95 (Scale 3 3 seta)),
                        (Translate 390 85 (Scale 0.3 0.3 level1)),(Translate 390 85 (Scale 0.3 0.3 level2)),(Translate 390 85 (Scale 0.3 0.3 level3)),
                        (Translate 390 85 (Scale 0.3 0.3 level4)),(Translate 390 85 (Scale 0.3 0.3 level5)),(Translate 390 85 (Scale 0.3 0.3 level6)),
                        (Translate 390 85 (Scale 0.3 0.3 level7)),(Translate 390 85 (Scale 0.3 0.3 level8)),(Translate 390 85 (Scale 0.3 0.3 level9)),(Translate 390 85 (Scale 0.3 0.3 level10))]

loadInstrucoes :: IO [Picture]
loadInstrucoes = do pergaminho <- loadBMP "Pergaminho.bmp"
                    texto <- loadBMP "InstrucoesTexto.bmp"
                    voltar <- loadBMP "Voltar.bmp"
                    seta <- loadBMP "Select.bmp"
                    return [(Translate 40 0 (Scale 9.5 9.5 pergaminho)),(Translate (-20) 100 (Scale 0.85 0.85 texto)),(Translate 80 (-100) (Scale 4 4 voltar)),
                            (Translate (-140) 105 (Rotate 90 (Scale 2.5 2.5 seta)))]

loadVitoria :: IO [Picture]
loadVitoria = do baixo <- loadBMP "ConcluidoBaixo.bmp"
                 cima <- loadBMP "ConcluidoCima.bmp"    
                 sair <- loadBMP "Sair.bmp"
                 seta <- loadBMP "Select.bmp"
                 continuar <- loadBMP "Continuar.bmp"
                 niveis <- loadBMP "Niveis.bmp"
                 return [(Translate 70 (180) (Scale 6 6 baixo)),(Translate 70 (180) (Scale 6 6 cima)),(Translate 210 (-150) (Scale 4.5 4.5 sair)),
                         (Translate (98) (-77) (Scale 3 3 seta)),(Translate 125 (-150) (Scale 4.5 4.5 continuar)),(Translate 175 (-150) (Scale 4.5 4.5 niveis))]

loadBackgrounds :: IO [Picture]
loadBackgrounds = do opcoesDB <- loadBMP "OpcoesDB.bmp"
                     niveisDB <- loadBMP "NiveisDB.bmp"
                     niveisNormal <- loadBMP "NiveisNormal.bmp"
                     opcoesNormal <- loadBMP "OpcoesNormal.bmp"
                     gameNormal <- loadBMP "GameNormal.bmp"
                     gameAmong <- loadBMP "GameAmong.bmp"
                     niveisAmong <- loadBMP "NiveisAmong.bmp"
                     opcoesAmong <- loadBMP "OpcoesAmong.bmp"
                     return [(Translate 0 (-23) (Scale 4.6 4.2  opcoesDB)),(Translate 0 (-23) (Scale 4.6 4.2  niveisDB)),(Translate 0 (-23) (Scale 4.6 4.2  niveisNormal)),
                             (Translate 0 (-23) (Scale 4.6 4.2  opcoesNormal)),(Translate 0 (-23) (Scale 4.6 4.2  gameNormal)),
                             (Translate 0 (-23) (Scale 4.6 4.2  gameAmong)),(Translate 0 (-23) (Scale 4.6 4.2  niveisAmong)),(Translate 0 (-23) (Scale 4.6 4.2  opcoesAmong))]



--------------
--- Janela ---
--------------
{- | 
== Janela
-}
{- |
Neste caso, atribuimos um 'FullScreen', pois desejamos que o jogo seja aberto em tela cheia, seja qual for o monitor!
-}

window :: Display 
window = FullScreen

------------------
--- Frame Rate ---
------------------
{- | 
== FrameRate
-}
{- | 
Atribuido para reagir ao __tempo__!
-}

fr :: Int
fr = 200

----------------------
--- Estado Inicial ---
----------------------
{- | 
== Estado Inicial
-}
estadoInicial :: [Picture] -> [Picture] -> [Picture] -> [Picture] -> [Picture] -> [Picture] -> EstadoGloss
estadoInicial menu jogo niveis instrucoes vitoria background = (Controlador Jogar Theme1, (0,0), menu, jogo, niveis, instrucoes, vitoria, background, 0)
----------------------
--- Desenha Estado ---
----------------------
{- | 
== Desenha Estado
-}
draw :: EstadoGloss -> IO Picture

------------------------------
--- Desenhar Parte do Menu ---
------------------------------

draw (Controlador Jogar Theme1, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ (Translate 340 (-140) (Scale 2.5 2.5 (pecasN (PersonagemD, jogo) 0))) : backgroundsN (OpcoesNormal, bg) : drawMenu ([[Select, Nada, Nada, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Niveis Theme1, (x,y), menu, jogo, _, _, _,bg, _) = 
    return $ Pictures $ (Translate 340 (-140) (Scale 2.5 2.5 (pecasN (PersonagemD, jogo) 0))) : backgroundsN (OpcoesNormal, bg) : drawMenu ([[Nada, Select, Nada, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Instrucoes Theme1, (x,y), menu, jogo, _, _, _,bg, _) = 
    return $ Pictures $ (Translate 340 (-140) (Scale 2.5 2.5 (pecasN (PersonagemD, jogo) 0))) : backgroundsN (OpcoesNormal, bg) : drawMenu ([[Nada, Nada, Select, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Tema Theme1, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ (Translate 340 (-140) (Scale 2.5 2.5 (pecasN (PersonagemD, jogo) 0))) : backgroundsN (OpcoesNormal, bg) : drawMenu ([[Nada, Nada, Nada, Select, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Sair Theme1, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ (Translate 340 (-140) (Scale 2.5 2.5 (pecasN (PersonagemD, jogo) 0))) : backgroundsN (OpcoesNormal, bg) : drawMenu ([[Nada, Nada, Nada, Nada, Select],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 

draw (Controlador Jogar Theme2, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ (Translate 400 (-160) (Scale 3.5 3.5 (pecasDB (PersonagemD, jogo) 0))) : backgroundsDB (OpcoesNormal, bg) : drawMenu ([[Select, Nada, Nada, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Niveis Theme2, (x,y), menu, jogo, _, _, _,bg, _) = 
    return $ Pictures $ (Translate 400 (-160) (Scale 3.5 3.5 (pecasDB (PersonagemD, jogo) 0))) : backgroundsDB (OpcoesNormal, bg) : drawMenu ([[Nada, Select, Nada, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Instrucoes Theme2, (x,y), menu, jogo, _, _, _,bg, _) = 
    return $ Pictures $ (Translate 400 (-160) (Scale 3.5 3.5 (pecasDB (PersonagemD, jogo) 0))) : backgroundsDB (OpcoesNormal, bg) : drawMenu ([[Nada, Nada, Select, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Tema Theme2, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ (Translate 400 (-160) (Scale 3.5 3.5 (pecasDB (PersonagemD, jogo) 0))) : backgroundsDB (OpcoesNormal, bg) : drawMenu ([[Nada, Nada, Nada, Select, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Sair Theme2, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ (Translate 400 (-160) (Scale 3.5 3.5 (pecasDB (PersonagemD, jogo) 0))) : backgroundsDB (OpcoesNormal, bg) : drawMenu ([[Nada, Nada, Nada, Nada, Select],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 

draw (Controlador Jogar Theme3, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ backgroundsA (OpcoesNormal, bg) : (Translate 390 (-160) (Scale 3.5 3.5 (pecasA (PersonagemD, jogo) 0))) : drawMenu ([[Select, Nada, Nada, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Niveis Theme3, (x,y), menu, jogo, _, _, _,bg, _) = 
    return $ Pictures $ backgroundsA (OpcoesNormal, bg) : (Translate 390 (-160) (Scale 3.5 3.5 (pecasA (PersonagemD, jogo) 0))) : drawMenu ([[Nada, Select, Nada, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Instrucoes Theme3, (x,y), menu, jogo, _, _, _,bg, _) = 
    return $ Pictures $ backgroundsA (OpcoesNormal, bg) : (Translate 390 (-160) (Scale 3.5 3.5 (pecasA (PersonagemD, jogo) 0))) : drawMenu ([[Nada, Nada, Select, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Tema Theme3, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ backgroundsA (OpcoesNormal, bg) : (Translate 390 (-160) (Scale 3.5 3.5 (pecasA (PersonagemD, jogo) 0))) : drawMenu ([[Nada, Nada, Nada, Select, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Sair Theme3, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ backgroundsA (OpcoesNormal, bg) : (Translate 390 (-160) (Scale 3.5 3.5 (pecasA (PersonagemD, jogo) 0))) : drawMenu ([[Nada, Nada, Nada, Nada, Select],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
------------------------------
--- Desenhar Parte do Jogo ---
------------------------------

draw (ModoJogo game reload lvl Theme1, (x,y), _, jogo, _, _, _, bg,b) = 
    return $ Pictures $ backgroundsN (GameNormal, bg) : (drawMapaN (getMap game, jogo) (0,0) b) ++ (getPlayer game)
    where getMap (Jogo mapa (Jogador (x,y) w bool)) = mapa
          getPlayer (Jogo mapa (Jogador (x,y) Oeste False)) = drawPlayerN ([PersonagemE], (x,y), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Este False)) = drawPlayerN ([PersonagemD], (x,y), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Oeste True)) = drawPlayerN ([Caixa,PersonagemE], (x,y-1), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Este True)) = drawPlayerN ([Caixa,PersonagemD], (x,y-1), jogo) b

draw (ModoJogo game reload lvl Theme2, (x,y), _, jogo, _, _, _, bg,b) = 
    return $ Pictures $ backgroundsDB (GameNormal, bg) : (drawMapaDB (getMap game, jogo) (0,0) b) ++ (getPlayer game)
    where getMap (Jogo mapa (Jogador (x,y) w bool)) = mapa
          getPlayer (Jogo mapa (Jogador (x,y) Oeste False)) = drawPlayerDB ([PersonagemE], (x,y), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Este False)) = drawPlayerDB ([PersonagemD], (x,y), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Oeste True)) = drawPlayerDB ([Caixa,PersonagemE], (x,y-1), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Este True)) = drawPlayerDB ([Caixa,PersonagemD], (x,y-1), jogo) b

draw (ModoJogo game reload lvl Theme3, (x,y), _, jogo, _, _, _, bg,b) = 
    return $ Pictures $ backgroundsA (GameNormal, bg) : (drawMapaA (getMap game, jogo) (0,0) b) ++ (getPlayer game)
    where getMap (Jogo mapa (Jogador (x,y) w bool)) = mapa
          getPlayer (Jogo mapa (Jogador (x,y) Oeste False)) = drawPlayerA ([PersonagemE], (x,y), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Este False)) = drawPlayerA ([PersonagemD], (x,y), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Oeste True)) = drawPlayerA ([Caixa,PersonagemE], (x,y-1), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Este True)) = drawPlayerA ([Caixa,PersonagemD], (x,y-1), jogo) b

-----------------------
--- Desenhar Niveis ---
----------------------- 

draw (Selecionador Nivel1 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ levels (Show1 , niveis) : backgroundsN (NiveisNormal, bg) : drawNiveis ([[Select, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel2 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ levels (Show2 , niveis) : backgroundsN (NiveisNormal, bg) : drawNiveis ([[Nada, Select, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel3 Theme1, (x,y), _, _, niveis, _, _,bg, _) =
    return $ Pictures $ levels (Show3 , niveis) : backgroundsN (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Select, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel4 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ levels (Show4 , niveis) : backgroundsN (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Select, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel5 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ levels (Show5 , niveis) : backgroundsN (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Select], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel6 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ levels (Show6 , niveis) : backgroundsN (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Select, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel7 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ levels (Show7 , niveis) : backgroundsN (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Select, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel8 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ levels (Show8 , niveis) : backgroundsN (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Select, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel9 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ levels (Show9 , niveis) : backgroundsN (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Select, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel10 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ levels (Show10 , niveis) : backgroundsN (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Select], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Voltar Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsN (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                      [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Select],[Nada,Nada,Back]], niveis) (0,0) 

draw (Selecionador Nivel1 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ (Translate (20) (-10) (levels (Show1 , niveis))) : backgroundsDB (NiveisNormal, bg) : drawNiveis ([[Select, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel2 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ (Translate (20) (-10) (levels (Show2 , niveis))) : backgroundsDB (NiveisNormal, bg) : drawNiveis ([[Nada, Select, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel3 Theme2, (x,y), _, _, niveis, _, _,bg, _) =
    return $ Pictures $ (Translate (20) (-10) (levels (Show3 , niveis))) : backgroundsDB (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Select, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel4 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ (Translate (20) (-10) (levels (Show4 , niveis))) : backgroundsDB (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Select, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel5 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ (Translate (20) (-10) (levels (Show5 , niveis))) : backgroundsDB (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Select], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel6 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ (Translate (20) (-10) (levels (Show6 , niveis))) : backgroundsDB (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Select, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel7 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ (Translate (20) (-10) (levels (Show7 , niveis))) : backgroundsDB (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Select, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel8 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ (Translate (20) (-10) (levels (Show8 , niveis))) : backgroundsDB (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Select, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel9 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ (Translate (20) (-10) (levels (Show9 , niveis))) : backgroundsDB (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Select, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel10 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ (Translate (20) (-10) (levels (Show10 , niveis))) : backgroundsDB (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Select], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Voltar Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsDB (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                      [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Select],[Nada,Nada,Back]], niveis) (0,0) 

draw (Selecionador Nivel1 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisNormal, bg) : (Translate 15 (-30) (levels (Show1 , niveis))) : drawNiveis ([[Select, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel2 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisNormal, bg) : (Translate 15 (-30) (levels (Show2 , niveis))) : drawNiveis ([[Nada, Select, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel3 Theme3, (x,y), _, _, niveis, _, _,bg, _) =
    return $ Pictures $ backgroundsA (NiveisNormal, bg) : (Translate 15 (-30) (levels (Show3 , niveis))) : drawNiveis ([[Nada, Nada, Select, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel4 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisNormal, bg) : (Translate 15 (-30) (levels (Show4 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Select, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel5 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisNormal, bg) : (Translate 15 (-30) (levels (Show5 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Select], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel6 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisNormal, bg) : (Translate 15 (-30) (levels (Show6 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Select, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel7 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisNormal, bg) : (Translate 15 (-30) (levels (Show7 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Select, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel8 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisNormal, bg) : (Translate 15 (-30) (levels (Show8 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Select, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel9 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisNormal, bg) : (Translate 15 (-30) (levels (Show9 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Select, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel10 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisNormal, bg) : (Translate 15 (-30) (levels (Show10 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Select], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Voltar Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisNormal, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                      [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Select],[Nada,Nada,Back]], niveis) (0,0) 

--------------------------
--- Desenhar Intrucoes ---
--------------------------

draw (Pergaminho Theme1, (x,y), _, _, _, instrucoes, _, bg,_) = 
    return $ Pictures $ backgroundsN (GameNormal, bg) : drawInstrucoes ([Tips, Texto, Back, Select], instrucoes) 0

draw (Pergaminho Theme2, (x,y), _, _, _, instrucoes, _, bg,_) = 
    return $ Pictures $ backgroundsDB (GameNormal, bg) : drawInstrucoes ([Tips, Texto, Back, Select], instrucoes) 0

draw (Pergaminho Theme3, (x,y), _, _, _, instrucoes, _, bg,_) = 
    return $ Pictures $ backgroundsA (GameNormal, bg) : drawInstrucoes ([Tips, Texto, Back, Select], instrucoes) 0

------------------------
--- Desenhar Vitoria ---
------------------------

draw (ImagemVitoria Continuar _ _ Theme1, (x,y), _, _, _, _, vitoria, bg,b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsN (GameNormal, bg) : drawVitoria ([[Nada, Select, Nada, Nada],[WinCima, Continue, Level, Exit]], vitoria) (0,0) 
    | otherwise = 
        return $ Pictures $ backgroundsN (GameNormal, bg) : drawVitoria ([[Nada, Select, Nada, Nada],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0) 
draw (ImagemVitoria Niveis _ _ Theme1, (x,y), _, _, _, _, vitoria,bg, b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsN (GameNormal, bg) : drawVitoria ([[Nada, Nada, Select, Nada],[WinCima, Continue, Level, Exit]], vitoria) (0,0)
    | otherwise = 
        return $ Pictures $ backgroundsN (GameNormal, bg) : drawVitoria ([[Nada, Nada, Select, Nada],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0)
draw (ImagemVitoria Sair _ _ Theme1, (x,y), _, _, _, _, vitoria, bg,b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsN (GameNormal, bg) : drawVitoria ([[Nada, Nada, Nada, Select],[WinCima, Continue, Level, Exit]], vitoria) (0,0) 
    | otherwise = 
        return $ Pictures $ backgroundsN (GameNormal, bg) : drawVitoria ([[Nada, Nada, Nada, Select],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0) 

draw (ImagemVitoria Continuar _ _ Theme2, (x,y), _, _, _, _, vitoria, bg,b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsDB (GameNormal, bg) : drawVitoria ([[Nada, Select, Nada, Nada],[WinCima, Continue, Level, Exit]], vitoria) (0,0) 
    | otherwise = 
        return $ Pictures $ backgroundsDB (GameNormal, bg) : drawVitoria ([[Nada, Select, Nada, Nada],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0) 
draw (ImagemVitoria Niveis _ _ Theme2, (x,y), _, _, _, _, vitoria,bg, b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsDB (GameNormal, bg) : drawVitoria ([[Nada, Nada, Select, Nada],[WinCima, Continue, Level, Exit]], vitoria) (0,0)
    | otherwise = 
        return $ Pictures $ backgroundsDB (GameNormal, bg) : drawVitoria ([[Nada, Nada, Select, Nada],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0)
draw (ImagemVitoria Sair _ _ Theme2, (x,y), _, _, _, _, vitoria, bg,b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsDB (GameNormal, bg) : drawVitoria ([[Nada, Nada, Nada, Select],[WinCima, Continue, Level, Exit]], vitoria) (0,0) 
    | otherwise = 
        return $ Pictures $ backgroundsDB (GameNormal, bg) : drawVitoria ([[Nada, Nada, Nada, Select],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0) 

draw (ImagemVitoria Continuar _ _ Theme3, (x,y), _, _, _, _, vitoria, bg,b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsA (GameNormal, bg) : drawVitoria ([[Nada, Select, Nada, Nada],[WinCima, Continue, Level, Exit]], vitoria) (0,0) 
    | otherwise = 
        return $ Pictures $ backgroundsA (GameNormal, bg) : drawVitoria ([[Nada, Select, Nada, Nada],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0) 
draw (ImagemVitoria Niveis _ _ Theme3, (x,y), _, _, _, _, vitoria,bg, b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsA (GameNormal, bg) : drawVitoria ([[Nada, Nada, Select, Nada],[WinCima, Continue, Level, Exit]], vitoria) (0,0)
    | otherwise = 
        return $ Pictures $ backgroundsA (GameNormal, bg) : drawVitoria ([[Nada, Nada, Select, Nada],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0)
draw (ImagemVitoria Sair _ _ Theme3, (x,y), _, _, _, _, vitoria, bg,b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsA (GameNormal, bg) : drawVitoria ([[Nada, Nada, Nada, Select],[WinCima, Continue, Level, Exit]], vitoria) (0,0) 
    | otherwise = 
        return $ Pictures $ backgroundsA (GameNormal, bg) : drawVitoria ([[Nada, Nada, Nada, Select],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0) 

----------------------
--- Desenhar Temas ---
----------------------

draw (Temas BlockDude Theme1, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsN (GameNormal, bg) : opcoes (NTheme, menu) : drawMenu ([[Select, Nada, Nada, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas DragonBall Theme1, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsN (GameNormal, bg) : opcoes (DBTheme, menu) : drawMenu ([[Nada, Select, Nada, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas AmongUs Theme1, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsN (GameNormal, bg) : opcoes (ATheme, menu) : drawMenu ([[Nada, Nada, Select, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas Back Theme1, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsN (GameNormal, bg) : opcoes (NTheme, menu) : drawMenu ([[Nada, Nada, Nada, Select],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)

draw (Temas BlockDude Theme2, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsDB (GameNormal, bg) : opcoes (NTheme, menu) : drawMenu ([[Select, Nada, Nada, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas DragonBall Theme2, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsDB (GameNormal, bg) : opcoes (DBTheme, menu) : drawMenu ([[Nada, Select, Nada, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas AmongUs Theme2, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsDB (GameNormal, bg) : opcoes (ATheme, menu) : drawMenu ([[Nada, Nada, Select, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas Back Theme2, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsDB (GameNormal, bg) : opcoes (DBTheme, menu) : drawMenu ([[Nada, Nada, Nada, Select],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)

draw (Temas BlockDude Theme3, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsA (GameNormal, bg) : opcoes (NTheme, menu) : drawMenu ([[Select, Nada, Nada, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas DragonBall Theme3, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsA (GameNormal, bg) : opcoes (DBTheme, menu) : drawMenu ([[Nada, Select, Nada, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas AmongUs Theme3, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsA (GameNormal, bg) : opcoes (ATheme, menu) : drawMenu ([[Nada, Nada, Select, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas Back Theme3, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsA (GameNormal, bg) : opcoes (ATheme, menu) : drawMenu ([[Nada, Nada, Nada, Select],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)

------------------------------
--- Desenhar Parte do Menu ---
------------------------------

drawMenu :: ([[Tipos]], [Picture]) -> (Int,Int) -> [Picture]
drawMenu ([], _) _ = []
drawMenu ((h:t), menu) (x,y) = (Translate (fromIntegral (x*(100))) (-55) (Pictures (drawMenu2 (h, menu) y))) : (drawMenu (t, menu) (x+1,y))

drawMenu2 :: ([Tipos], [Picture]) -> Int -> [Picture]
drawMenu2 ([], _) _ = []
drawMenu2 ((h:t), menu) y = (Translate (-400) (fromIntegral (y*65)) (opcoes (h, menu))) : (drawMenu2 (t, menu) (y-1))

opcoes :: (Tipos, [Picture]) -> Picture
opcoes (Nada, menu) = Blank
opcoes (Play, menu) = menu !! 0
opcoes (Level, menu) = menu !! 1
opcoes (Tips, menu) = menu !! 2
opcoes (Exit, menu) = menu !! 3
opcoes (Select, menu) = menu !! 4
opcoes (BlockDude, menu) = menu !! 5
opcoes (DragonBall, menu) = menu !! 6
opcoes (AmongUs, menu) = menu !! 7
opcoes (Back, menu) = menu !! 8
opcoes (Theme, menu) = menu !! 9
opcoes (NTheme, menu) = menu !! 10
opcoes (DBTheme, menu) = menu !! 11
opcoes (ATheme, menu) = menu !! 12




------------------------------
--- Desenhar Parte do Mapa ---
------------------------------

drawMapaN :: (Mapa, [Picture]) -> (Int,Int) -> Float -> [Picture]
drawMapaN ([], _) _ _ = []
drawMapaN ((h:t), jogo) (x,y) b = (Translate (-600) (fromIntegral (y*(-48))) (Pictures (drawMapa2N (h, jogo) x b))) : (drawMapaN (t, jogo) (x,y+1) b)

drawMapa2N :: ([Peca],[Picture]) -> Int -> Float -> [Picture]
drawMapa2N ([], _) _ _ = []
drawMapa2N ((h:t), jogo) x b = (Translate (fromIntegral (x*48)) 308 (pecasN (h, jogo) b)) : (drawMapa2N (t, jogo) (x+1) b)

drawPlayerN :: ([Peca], (Int,Int), [Picture]) -> Float -> [Picture]
drawPlayerN ([],(_,_), _) _ = []
drawPlayerN ((h:t),(x,y), jogo) b = (Translate (fromIntegral ((x*(48))-600)) (fromIntegral (y*(-48))+308) (pecasN (h, jogo) b)) : (drawPlayerN (t,(x,y+1), jogo) b)

drawMapaDB :: (Mapa, [Picture]) -> (Int,Int) -> Float -> [Picture]
drawMapaDB ([], _) _ _ = []
drawMapaDB ((h:t), jogo) (x,y) b = (Translate (-600) (fromIntegral (y*(-48))) (Pictures (drawMapa2DB (h, jogo) x b))) : (drawMapaDB (t, jogo) (x,y+1) b)

drawMapa2DB :: ([Peca],[Picture]) -> Int -> Float -> [Picture]
drawMapa2DB ([], _) _ _ = []
drawMapa2DB ((h:t), jogo) x b = (Translate (fromIntegral (x*48)) 308 (pecasDB (h, jogo) b)) : (drawMapa2DB (t, jogo) (x+1) b)

drawPlayerDB :: ([Peca], (Int,Int), [Picture]) -> Float -> [Picture]
drawPlayerDB ([],(_,_), _) _ = []
drawPlayerDB ((h:t),(x,y), jogo) b = (Translate (fromIntegral ((x*(48))-600)) (fromIntegral (y*(-48))+308) (pecasDB (h, jogo) b)) : (drawPlayerDB (t,(x,y+1), jogo) b)

drawMapaA :: (Mapa, [Picture]) -> (Int,Int) -> Float -> [Picture]
drawMapaA ([], _) _ _ = []
drawMapaA ((h:t), jogo) (x,y) b = (Translate (-600) (fromIntegral (y*(-48))) (Pictures (drawMapa2A (h, jogo) x b))) : (drawMapaA (t, jogo) (x,y+1) b)

drawMapa2A :: ([Peca],[Picture]) -> Int -> Float -> [Picture]
drawMapa2A ([], _) _ _ = []
drawMapa2A ((h:t), jogo) x b = (Translate (fromIntegral (x*48)) 308 (pecasA (h, jogo) b)) : (drawMapa2A (t, jogo) (x+1) b)

drawPlayerA :: ([Peca], (Int,Int), [Picture]) -> Float -> [Picture]
drawPlayerA ([],(_,_), _) _ = []
drawPlayerA ((h:t),(x,y), jogo) b = (Translate (fromIntegral ((x*(48))-600)) (fromIntegral (y*(-48))+308) (pecasA (h, jogo) b)) : (drawPlayerA (t,(x,y+1), jogo) b)

pecasN :: (Peca, [Picture]) -> Float -> Picture
pecasN (Porta, jogo) b
    | mod (round (b*50)) 200 < 100 = jogo !! 0
    | otherwise = jogo !! 1
pecasN (PersonagemD, jogo) _ = jogo !! 2
pecasN (PersonagemE, jogo) _ = jogo !! 3
pecasN (Bloco, jogo) _ = jogo !! 4
pecasN (Caixa, jogo) _ = jogo !! 5
pecasN (Vazio, jogo) _ = Blank

pecasA :: (Peca, [Picture]) -> Float -> Picture
pecasA (Porta, jogo) _ = jogo !! 6
pecasA (PersonagemD, jogo) _ = jogo !! 7
pecasA (PersonagemE, jogo) _ = jogo !! 8
pecasA (Bloco, jogo) _ = jogo !! 9
pecasA (Caixa, jogo) _ = jogo !! 10
pecasA (Vazio, jogo) _ = Blank


pecasDB :: (Peca, [Picture]) -> Float -> Picture
pecasDB (Porta, jogo) _ = jogo !! 11
pecasDB (PersonagemD, jogo) _ = jogo !! 12
pecasDB (PersonagemE, jogo) _ = jogo !! 13
pecasDB (Bloco, jogo) _ = jogo !! 14
pecasDB (Caixa, jogo) _ = jogo !! 15
pecasDB (Vazio, jogo) _ = Blank

-----------------------
--- Desenhar Niveis ---
----------------------- 

drawNiveis :: ([[Tipos]], [Picture]) -> (Int,Int) -> [Picture]
drawNiveis ([], _) _ = []
drawNiveis ((h:t), niveis) (x,y) = (Translate (fromIntegral (x*(85))) (-70) (Pictures (drawNiveis2 (h, niveis) y))) : (drawNiveis (t, niveis) (x+1,y))

drawNiveis2 :: ([Tipos], [Picture]) -> Int -> [Picture]
drawNiveis2 ([], _) _ = []
drawNiveis2 ((h:t), niveis) y = (Translate (-380) (fromIntegral (y*55)) (levels (h, niveis))) : (drawNiveis2 (t, niveis) (y-1))

levels :: (Tipos, [Picture]) -> Picture
levels (Nada, niveis) = Blank
levels (Lvl1, niveis) = niveis !! 0
levels (Lvl2, niveis) = niveis !! 1
levels (Lvl3, niveis) = niveis !! 2
levels (Lvl4, niveis) = niveis !! 3
levels (Lvl5, niveis) = niveis !! 4
levels (Lvl6, niveis) = niveis !! 5
levels (Lvl7, niveis) = niveis !! 6
levels (Lvl8, niveis) = niveis !! 7
levels (Lvl9, niveis) = niveis !! 8
levels (Lvl10, niveis) = niveis !! 9
levels (Back, niveis) = niveis !! 10
levels (Select, niveis) = niveis !! 11
levels (Show1, niveis) = niveis !! 12
levels (Show2, niveis) = niveis !! 13
levels (Show3, niveis) = niveis !! 14
levels (Show4, niveis) = niveis !! 15
levels (Show5, niveis) = niveis !! 16
levels (Show6, niveis) = niveis !! 17
levels (Show7, niveis) = niveis !! 18
levels (Show8, niveis) = niveis !! 19
levels (Show9, niveis) = niveis !! 20
levels (Show10, niveis) = niveis !! 21


--------------------------
--- Desenhar Intrucoes ---
--------------------------

drawInstrucoes :: ([Tipos], [Picture]) -> Int -> [Picture]
drawInstrucoes ([], _) _ = []
drawInstrucoes ((h:t), instrucoes) y = (Translate 0 (fromIntegral (y*100)) (instruct (h,instrucoes))) : (drawInstrucoes (t,instrucoes) (y-1))

instruct :: (Tipos, [Picture]) -> Picture
instruct (Tips, instrucoes) = instrucoes !! 0
instruct (Texto, instrucoes) = instrucoes !! 1
instruct (Back, instrucoes) = instrucoes !! 2
instruct (Select, instrucoes) = instrucoes !! 3

------------------------
--- Desenhar Vitoria ---
------------------------

drawVitoria :: ([[Tipos]], [Picture]) -> (Int,Int) -> [Picture]
drawVitoria ([], _) _ = []
drawVitoria ((h:t), vitoria) (x,y) = (Translate (fromIntegral (x*(90))) (-50) (Pictures (drawVitoria2 (h, vitoria) y))) : (drawVitoria (t, vitoria) (x+1,y))

drawVitoria2 :: ([Tipos], [Picture]) -> Int -> [Picture]
drawVitoria2 ([], _) _ = []
drawVitoria2 ((h:t), vitoria) y = (Translate (-150) (fromIntegral (y*65)) (victory (h, vitoria))) : (drawVitoria2 (t, vitoria) (y-1))

victory :: (Tipos, [Picture]) -> Picture
victory (Nada, vitoria) = Blank
victory (WinBaixo, vitoria) = vitoria !! 0
victory (WinCima, vitoria) = vitoria !! 1
victory (Exit, vitoria) = vitoria !! 2
victory (Select, vitoria) = vitoria !! 3
victory (Continue, vitoria) = vitoria !! 4
victory (Level, vitoria) = vitoria !! 5

--- Função para backgrounds ---

backgroundsDB :: (Tipos, [Picture]) -> Picture
backgroundsDB (NiveisNormal, x) = x !! 1
backgroundsDB (OpcoesNormal, x) = x !! 0
backgroundsDB (GameNormal, x) = x !! 4

backgroundsN :: (Tipos,[Picture]) -> Picture
backgroundsN (NiveisNormal, x) = x !! 2
backgroundsN (OpcoesNormal, x) = x !! 3
backgroundsN (GameNormal, x) = x !! 4

backgroundsA :: (Tipos,[Picture]) -> Picture
backgroundsA (GameNormal, x) = x !! 5
backgroundsA (NiveisNormal, x) = x !! 6
backgroundsA (OpcoesNormal, x) = x !! 7



--- Função para encontrar a porta! ---

porta :: Jogo -> (Int,Int)
porta (Jogo mapa1 (Jogador (x1,y1) w bool)) = findPorta mapa1 0 0

findPorta :: Mapa -> Int -> Int -> (Int,Int)
findPorta [] x y = (0,0)
findPorta ([]:t2) x y = findPorta t2 0 (y+1)
findPorta ((h:t1):t2) x y = if h == Porta
                        then (x,y)
                        else findPorta (t1:t2) (x+1) y

--------------------
--- Reage Evento ---
--------------------

{- | 
== Reage Evento
-}
reageEvento :: Event -> EstadoGloss -> IO EstadoGloss

--- Menu ---
reageEvento (EventKey (SpecialKey KeyEsc) Down _ _) (Controlador _ _, _, _, _, _, _, _, _, _) =
    exitSuccess

reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Controlador Niveis g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b) =
    return (Controlador Sair g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b) = 
    return (ModoJogo jogo1 jogo1 Jogo1 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) 

reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Niveis g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Controlador Instrucoes g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Niveis g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b) =
    return (Controlador Jogar g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Niveis g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel1 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)

reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Instrucoes g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Controlador Tema g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Instrucoes g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Controlador Niveis g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Instrucoes g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Pergaminho g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)

reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Tema g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Controlador Sair g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Tema g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Controlador Instrucoes g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Tema g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Temas BlockDude  g , pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)

reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Controlador Jogar g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Controlador Tema g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    exitSuccess 

--- Menu dos Niveis ---

reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Selecionador Nivel1 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel2 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Selecionador Nivel1 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel6 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Selecionador Nivel1 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo1 jogo1 Jogo1 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Selecionador Nivel2 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel1 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Selecionador Nivel2 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel3 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Selecionador Nivel2 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel7 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Selecionador Nivel2 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo2 jogo2 Jogo2 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Selecionador Nivel3 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel4 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Selecionador Nivel3 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel2 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Selecionador Nivel3 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel8 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Selecionador Nivel3 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo3 jogo3 Jogo3 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Selecionador Nivel4 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel5 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Selecionador Nivel4 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel3 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Selecionador Nivel4 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b) =
    return (Selecionador Nivel9 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Selecionador Nivel4 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg,b) =
    return (ModoJogo jogo4 jogo4 Jogo4 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Selecionador Nivel5 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel4 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Selecionador Nivel5 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel10 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Selecionador Nivel5 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b) =
    return (ModoJogo jogo5 jogo5 Jogo5 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)  
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Selecionador Nivel6 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel7 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Selecionador Nivel6 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b) =
    return (Selecionador Nivel1 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Selecionador Nivel6 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Voltar g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Selecionador Nivel6 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo6 jogo6 Jogo6 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Selecionador Nivel7 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel8 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Selecionador Nivel7 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel6 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Selecionador Nivel7 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel2 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Selecionador Nivel7 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Voltar g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Selecionador Nivel7 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo7 jogo7 Jogo7 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Selecionador Nivel8 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel9 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Selecionador Nivel8 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel7 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Selecionador Nivel8 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Voltar g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Selecionador Nivel8 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel3 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Selecionador Nivel8 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo8 jogo8 Jogo8 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Selecionador Nivel9 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel10 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Selecionador Nivel9 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel8 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Selecionador Nivel9 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel4 g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Selecionador Nivel9 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Voltar g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Selecionador Nivel9 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo9 jogo9 Jogo9 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Selecionador Nivel10 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel9 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Selecionador Nivel10 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel5 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (Selecionador Nivel10 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Voltar g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Selecionador Nivel10 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo10 jogo10 Jogo10 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (Selecionador Voltar g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel8 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Selecionador Voltar g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Controlador Niveis g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)

--- ModoJogo ---

reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo (Jogo mapa (Jogador (x,y) w bool)) reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) 
    | moveJogador (Jogo mapa (Jogador (x,y) w bool)) AndarEsquerda == Jogo mapa (Jogador (porta (Jogo mapa (Jogador (x,y) w bool))) w bool) = 
        return (ImagemVitoria Continuar reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
    | otherwise = 
        return (ModoJogo (moveJogador (Jogo mapa (Jogador (x,y) w bool)) AndarEsquerda) reload lvl g,pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo (Jogo mapa (Jogador (x,y) w bool)) reload lvl g,pos, menu, jogo, niveis, instrucoes, vitoria, bg,b) 
    | moveJogador (Jogo mapa (Jogador (x,y) w bool)) AndarDireita == Jogo mapa (Jogador (porta (Jogo mapa (Jogador (x,y) w bool))) w bool) = 
        return (ImagemVitoria Continuar reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
    | otherwise = 
        return (ModoJogo (moveJogador (Jogo mapa (Jogador (x,y) w bool)) AndarDireita) reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo (Jogo mapa (Jogador (x,y) w bool)) reload lvl g,pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) 
    | moveJogador (Jogo mapa (Jogador (x,y) w bool)) Trepar == Jogo mapa (Jogador (porta (Jogo mapa (Jogador (x,y) w bool))) w bool) = 
        return (ImagemVitoria Continuar reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
    | otherwise = 
        return (ModoJogo (moveJogador (Jogo mapa (Jogador (x,y) w bool)) Trepar) reload lvl g,pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo (Jogo mapa (Jogador (x,y) w bool)) reload lvl g,pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) 
    | moveJogador (Jogo mapa (Jogador (x,y) w bool)) InterageCaixa == Jogo mapa (Jogador (porta (Jogo mapa (Jogador (x,y) w bool))) w bool) = 
        return (ImagemVitoria Continuar reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
    | otherwise = 
        return (ModoJogo (moveJogador (Jogo mapa (Jogador (x,y) w bool)) InterageCaixa) reload lvl g,pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (Char 'a') Down _ _) (ModoJogo (Jogo mapa (Jogador (x,y) w bool)) reload lvl g,pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) 
    | moveJogador (Jogo mapa (Jogador (x,y) w bool)) AndarEsquerda == Jogo mapa (Jogador (porta (Jogo mapa (Jogador (x,y) w bool))) w bool) = 
        return (ImagemVitoria Continuar reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
    | otherwise = 
        return (ModoJogo (moveJogador (Jogo mapa (Jogador (x,y) w bool)) AndarEsquerda) reload lvl g,pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (Char 'd') Down _ _) (ModoJogo (Jogo mapa (Jogador (x,y) w bool)) reload lvl g,pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) 
    | moveJogador (Jogo mapa (Jogador (x,y) w bool)) AndarDireita == Jogo mapa (Jogador (porta (Jogo mapa (Jogador (x,y) w bool))) w bool) = 
        return (ImagemVitoria Continuar reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
    | otherwise = 
        return (ModoJogo (moveJogador (Jogo mapa (Jogador (x,y) w bool)) AndarDireita) reload lvl g,pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (Char 'w') Down _ _) (ModoJogo (Jogo mapa (Jogador (x,y) w bool)) reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) 
    | moveJogador (Jogo mapa (Jogador (x,y) w bool)) Trepar == Jogo mapa (Jogador (porta (Jogo mapa (Jogador (x,y) w bool))) w bool) = 
        return (ImagemVitoria Continuar reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
    | otherwise = 
        return (ModoJogo (moveJogador (Jogo mapa (Jogador (x,y) w bool)) Trepar) reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (Char 's') Down _ _) (ModoJogo (Jogo mapa (Jogador (x,y) w bool)) reload lvl g,pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) 
    | moveJogador (Jogo mapa (Jogador (x,y) w bool)) InterageCaixa == Jogo mapa (Jogador (porta (Jogo mapa (Jogador (x,y) w bool))) w bool) = 
        return (ImagemVitoria Continuar reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
    | otherwise = 
        return (ModoJogo (moveJogador (Jogo mapa (Jogador (x,y) w bool)) InterageCaixa) reload lvl g,pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (Char 'r') Down _ _) (ModoJogo game reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo reload reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEsc) Down _ _) (ModoJogo game reload lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Controlador Jogar g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)

--- Instrucoes --- 

reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Pergaminho g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Controlador Instrucoes g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)

--- Vitoria ---

reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (ImagemVitoria Continuar game lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ImagemVitoria Niveis game lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (ImagemVitoria Niveis game lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ImagemVitoria Sair game lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (ImagemVitoria Niveis game lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ImagemVitoria Continuar game lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ImagemVitoria Niveis game lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Nivel1 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) 
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ImagemVitoria Sair game lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Controlador Niveis g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (ImagemVitoria Sair game lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ImagemVitoria Niveis game lvl g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)

reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ImagemVitoria Continuar jogo1 Jogo1 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo2 jogo2 Jogo2 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ImagemVitoria Continuar jogo2 Jogo2 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo3 jogo3 Jogo3 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ImagemVitoria Continuar jogo3 Jogo3 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo4 jogo4 Jogo4 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ImagemVitoria Continuar jogo4 Jogo4 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo5 jogo5 Jogo5 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ImagemVitoria Continuar jogo5 Jogo5 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo6 jogo6 Jogo6 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ImagemVitoria Continuar jogo6 Jogo6 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo7 jogo7 Jogo7 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ImagemVitoria Continuar jogo7 Jogo7 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo8 jogo8 Jogo8 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ImagemVitoria Continuar jogo8 Jogo8 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo9 jogo9 Jogo9 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ImagemVitoria Continuar jogo9 Jogo9 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (ModoJogo jogo10 jogo10 Jogo10 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (ImagemVitoria Continuar jogo10 Jogo10 g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Selecionador Voltar g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b)

--- Menu dos Temas ---
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Temas BlockDude g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Temas Back g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Temas BlockDude g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Temas DragonBall g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Temas BlockDude g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Temas BlockDude Theme1, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b) ------------------ 

reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Temas DragonBall g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Temas BlockDude g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Temas DragonBall g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Temas AmongUs g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Temas DragonBall g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Temas DragonBall Theme2 , pos, menu, jogo, niveis, instrucoes, vitoria, bg,b) ---------------

reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Temas AmongUs g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Temas DragonBall g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Temas AmongUs g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Temas Back g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Temas AmongUs g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Temas AmongUs Theme3, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b) ------------

reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Temas Back g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Temas AmongUs g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Temas Back g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Temas BlockDude g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Temas Back g, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) =
    return (Controlador Tema g, pos, menu, jogo, niveis, instrucoes, vitoria, bg,b)


reageEvento _ n = return n 

-------------------
--- Reage Tempo ---
-------------------
{- | 
== Reage Tempo
-}
reageTempo :: Float -> EstadoGloss -> IO EstadoGloss   
reageTempo n (modo, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) = return (modo, pos, menu, jogo, niveis, instrucoes, vitoria,bg, (b+n))

------------
--- Main ---
------------
{- | 
== Main
-}
main :: IO ()
main = do
    loadedMenus <- loadMenus
    loadedJogar <- loadJogar
    loadedNiveis <- loadNiveis
    loadedInstrucoes <- loadInstrucoes
    loadedVitoria <- loadVitoria
    loadedBackgrounds <- loadBackgrounds
    playIO window 
           blue 
           fr
           (estadoInicial loadedMenus loadedJogar loadedNiveis loadedInstrucoes loadedVitoria loadedBackgrounds)
           draw
           reageEvento
           reageTempo
