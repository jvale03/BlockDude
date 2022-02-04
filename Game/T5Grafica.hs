{- |
Module      : T5Grafica
Description : Aplicação Gráfica
Copyright   : João Carlos Oliveira Vale <a100697@alunos.uminho.pt>; <carlosvalejcov@gmail.com>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}


module Main (
      -- * Type
    EstadoGloss(..),
      -- * Load de Imagens
    loadMenus, loadJogar, loadNiveis, loadInstrucoes, loadVitoria, loadBackgrounds,   
      -- * Janela
    window,
      -- * Frame Rate
    fr,
      -- * Estado Inicial
    estadoInicial,  
      -- * Desenhar Estado
    draw,
      -- * Reage Evento
    reageEvento,
      -- * Reage Tempo
    reageTempo,
      -- * MainFunction
    main
      ) where


import T0Dados
import T0Mapas
import T0Auxiliares
import T4MovimentarJogo
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit

------------
--- type ---
------------

{- |
Nesta 'type':
 
 - 'Menu' vai representar em que parte o 'Jogo' tem de estar.
 - '(Float,Float)' representa as __coordenadas__ em que as imagens começarão por ser apresentadas!
 - '[Picture]''s representam todas as listas de 'loads' para poder apresentar as imagens quando necessário.
 - 'Float' tem como efeito representar o número em que o __tempo__ irá começar a ser contado. 
-}

type EstadoGloss = (Menu, (Float,Float), [Picture], [Picture], [Picture], [Picture], [Picture], [Picture],Float)

---------------
--- loadBmp ---
---------------

{- |
Dividido em categorias, cada __load__ está atribuido a uma parte do jogo, isto é, um load para
o __mapa__, __menu__, __backgrounds__, __etc__.
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

{- |
Dividido em categorias, cada __load__ está atribuido a uma parte do jogo, isto é, um load para
o __mapa__, __menu__, __backgrounds__, __etc__.
-}

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
                       (Translate (0) (0) (Scale 0.11 0.14 shenlong)),(Translate (-1) (1) (Scale 0.19 0.170 gokuR)),(Translate (-1) (1) (Scale 0.19 0.170 gokuL)),(Translate (126) (-126) (Scale 3 3 dbBlock)),(Translate (123) (-123) (Scale 2.9 2.9 dbBox))]

{- |
Dividido em categorias, cada __load__ está atribuido a uma parte do jogo, isto é, um load para
o __mapa__, __menu__, __backgrounds__, __etc__.
-}

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
                        (Translate 390 85 (Scale 1 1 level1)),(Translate 390 85 (Scale 0.3 0.3 level2)),(Translate 390 85 (Scale 0.3 0.3 level3)),
                        (Translate 390 85 (Scale 0.3 0.3 level4)),(Translate 390 85 (Scale 0.3 0.3 level5)),(Translate 390 85 (Scale 0.3 0.3 level6)),
                        (Translate 390 85 (Scale 0.3 0.3 level7)),(Translate 390 85 (Scale 0.3 0.3 level8)),(Translate 390 85 (Scale 0.3 0.3 level9)),(Translate 390 85 (Scale 0.3 0.3 level10))]

{- |
Dividido em categorias, cada __load__ está atribuido a uma parte do jogo, isto é, um load para
o __mapa__, __menu__, __backgrounds__, __etc__.
-}

loadInstrucoes :: IO [Picture]
loadInstrucoes = do pergaminho <- loadBMP "Pergaminho.bmp"
                    texto <- loadBMP "InstrucoesTexto.bmp"
                    voltar <- loadBMP "Voltar.bmp"
                    seta <- loadBMP "Select.bmp"
                    return [(Translate 40 0 (Scale 9.5 9.5 pergaminho)),(Translate (-20) 100 (Scale 0.85 0.85 texto)),(Translate 80 (-100) (Scale 4 4 voltar)),
                            (Translate (-140) 105 (Rotate 90 (Scale 2.5 2.5 seta)))]

{- |
Dividido em categorias, cada __load__ está atribuido a uma parte do jogo, isto é, um load para
o __mapa__, __menu__, __backgrounds__, __etc__.
-}

loadVitoria :: IO [Picture]
loadVitoria = do baixo <- loadBMP "ConcluidoBaixo.bmp"
                 cima <- loadBMP "ConcluidoCima.bmp"    
                 sair <- loadBMP "Sair.bmp"
                 seta <- loadBMP "Select.bmp"
                 continuar <- loadBMP "Continuar.bmp"
                 niveis <- loadBMP "Niveis.bmp"
                 return [(Translate 70 (180) (Scale 6 6 baixo)),(Translate 70 (180) (Scale 6 6 cima)),(Translate 210 (-150) (Scale 4.5 4.5 sair)),
                         (Translate (98) (-77) (Scale 3 3 seta)),(Translate 125 (-150) (Scale 4.5 4.5 continuar)),(Translate 175 (-150) (Scale 4.5 4.5 niveis))]

{- |
Dividido em categorias, cada __load__ está atribuido a uma parte do jogo, isto é, um load para
o __mapa__, __menu__, __backgrounds__, __etc__.
-}

loadBackgrounds :: IO [Picture]
loadBackgrounds = do opcoesDB <- loadBMP "OpcoesDB.bmp"
                     niveisDB <- loadBMP "NiveisDB.bmp"
                     gameDB <- loadBMP "GameDB.bmp"
                     opcoesNormal <- loadBMP "OpcoesNormal.bmp"
                     niveisNormal <- loadBMP "NiveisNormal.bmp"
                     gameNormal <- loadBMP "GameNormal.bmp"
                     opcoesAmong <- loadBMP "OpcoesAmong.bmp"
                     niveisAmong <- loadBMP "NiveisAmong.bmp"
                     gameAmong <- loadBMP "GameAmong.bmp"
                     return [(Translate 0 (-23) (Scale 4.6 4.2  opcoesDB)),(Translate 0 (-23) (Scale 4.6 4.2  niveisDB)),(Translate 0 (-23) (Scale 4.6 4.2  gameDB)),
                             (Translate 0 (-23) (Scale 4.6 4.2  niveisNormal)),(Translate 0 (-23) (Scale 4.6 4.2  opcoesNormal)),(Translate 0 (-23) (Scale 4.6 4.2  gameNormal)),
                             (Translate 0 (-23) (Scale 4.6 4.2  gameAmong)),(Translate 0 (-23) (Scale 4.6 4.2  niveisAmong)),(Translate 0 (-23) (Scale 4.6 4.2  opcoesAmong))]

--------------
--- Janela ---
--------------

{- |
Neste caso, atribuimos um 'FullScreen', pois desejamos que o jogo seja aberto em tela cheia, seja qual for o monitor!
-}

window :: Display 
window = FullScreen
------------------
--- Frame Rate ---
------------------

{- | 
Atribuido para reagir ao __tempo__!
-}

fr :: Int
fr = 200

----------------------
--- Estado Inicial ---
----------------------

{- | 
Representa como o jogo irá ser __apresentado inicialmente__, ou seja, ao ser aberto.
-}

estadoInicial :: [Picture] -> [Picture] -> [Picture] -> [Picture] -> [Picture] -> [Picture] -> EstadoGloss
estadoInicial menu jogo niveis instrucoes vitoria background = (Controlador Jogar Theme1, (0,0), menu, jogo, niveis, instrucoes, vitoria, background, 0)

----------------------
--- Desenha Estado ---
----------------------

{- | 
'draw' é a função que irá desenhar o 'jogo' cada vez que muda alguma coisa, ou seja, reproduz algo para
todos os 'estados' possíveis.

O mesmo está organizado de acordo com cada "grupo" de 'estados' possíveis, isto é, __Tema__, __Menu__, __ModoJogo__, Instruções__, __etc__.

__Todas__ as funções aqui envolvidas estão num __ficheiro__ onde se encontram todas as __Auxiliares__ desta tarefa de modo a tornar o
código mais fluído e legível. 
-}

draw :: EstadoGloss -> IO Picture

------------------------------
--- Desenhar Parte do Menu ---
------------------------------

draw (Controlador Jogar Theme1, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ (Translate 340 (-140) (Scale 2.5 2.5 (pecasN (PersonagemD, jogo) 0))) : backgroundsN (OpcoesGeral, bg) : drawMenu ([[Select, Nada, Nada, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Niveis Theme1, (x,y), menu, jogo, _, _, _,bg, _) = 
    return $ Pictures $ (Translate 340 (-140) (Scale 2.5 2.5 (pecasN (PersonagemD, jogo) 0))) : backgroundsN (OpcoesGeral, bg) : drawMenu ([[Nada, Select, Nada, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Instrucoes Theme1, (x,y), menu, jogo, _, _, _,bg, _) = 
    return $ Pictures $ (Translate 340 (-140) (Scale 2.5 2.5 (pecasN (PersonagemD, jogo) 0))) : backgroundsN (OpcoesGeral, bg) : drawMenu ([[Nada, Nada, Select, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Tema Theme1, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ (Translate 340 (-140) (Scale 2.5 2.5 (pecasN (PersonagemD, jogo) 0))) : backgroundsN (OpcoesGeral, bg) : drawMenu ([[Nada, Nada, Nada, Select, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Sair Theme1, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ (Translate 340 (-140) (Scale 2.5 2.5 (pecasN (PersonagemD, jogo) 0))) : backgroundsN (OpcoesGeral, bg) : drawMenu ([[Nada, Nada, Nada, Nada, Select],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 

draw (Controlador Jogar Theme2, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ backgroundsDB (OpcoesGeral, bg) : (Translate 400 (-160) (Scale 3.5 3.5 (pecasDB (PersonagemD, jogo) 0))) : drawMenu ([[Select, Nada, Nada, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Niveis Theme2, (x,y), menu, jogo, _, _, _,bg, _) = 
    return $ Pictures $ backgroundsDB (OpcoesGeral, bg) : (Translate 400 (-160) (Scale 3.5 3.5 (pecasDB (PersonagemD, jogo) 0))) : drawMenu ([[Nada, Select, Nada, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Instrucoes Theme2, (x,y), menu, jogo, _, _, _,bg, _) = 
    return $ Pictures $ backgroundsDB (OpcoesGeral, bg) : (Translate 400 (-160) (Scale 3.5 3.5 (pecasDB (PersonagemD, jogo) 0))) : drawMenu ([[Nada, Nada, Select, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Tema Theme2, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ backgroundsDB (OpcoesGeral, bg) : (Translate 400 (-160) (Scale 3.5 3.5 (pecasDB (PersonagemD, jogo) 0))) : drawMenu ([[Nada, Nada, Nada, Select, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Sair Theme2, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ backgroundsDB (OpcoesGeral, bg) : (Translate 400 (-160) (Scale 3.5 3.5 (pecasDB (PersonagemD, jogo) 0))) : drawMenu ([[Nada, Nada, Nada, Nada, Select],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 

draw (Controlador Jogar Theme3, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ backgroundsA (OpcoesGeral, bg) : (Translate 390 (-160) (Scale 3.5 3.5 (pecasA (PersonagemD, jogo) 0))) : drawMenu ([[Select, Nada, Nada, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Niveis Theme3, (x,y), menu, jogo, _, _, _,bg, _) = 
    return $ Pictures $ backgroundsA (OpcoesGeral, bg) : (Translate 390 (-160) (Scale 3.5 3.5 (pecasA (PersonagemD, jogo) 0))) : drawMenu ([[Nada, Select, Nada, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Instrucoes Theme3, (x,y), menu, jogo, _, _, _,bg, _) = 
    return $ Pictures $ backgroundsA (OpcoesGeral, bg) : (Translate 390 (-160) (Scale 3.5 3.5 (pecasA (PersonagemD, jogo) 0))) : drawMenu ([[Nada, Nada, Select, Nada, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Tema Theme3, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ backgroundsA (OpcoesGeral, bg) : (Translate 390 (-160) (Scale 3.5 3.5 (pecasA (PersonagemD, jogo) 0))) : drawMenu ([[Nada, Nada, Nada, Select, Nada],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 
draw (Controlador Sair Theme3, (x,y), menu, jogo, _, _, _, bg,_) = 
    return $ Pictures $ backgroundsA (OpcoesGeral, bg) : (Translate 390 (-160) (Scale 3.5 3.5 (pecasA (PersonagemD, jogo) 0))) : drawMenu ([[Nada, Nada, Nada, Nada, Select],[Play, Level, Tips, Theme, Exit]], menu) (0,0) 

------------------------------
--- Desenhar Parte do Jogo ---
------------------------------

draw (ModoJogo game reload lvl Theme1, (x,y), _, jogo, _, _, _, bg,b) = 
    return $ Pictures $ backgroundsN (GameGeral, bg) : (drawMapaN (getMap game, jogo) (0,0) b) ++ (getPlayer game)
    where getMap (Jogo mapa (Jogador (x,y) w bool)) = mapa
          getPlayer (Jogo mapa (Jogador (x,y) Oeste False)) = drawPlayerN ([PersonagemE], (x,y), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Este False)) = drawPlayerN ([PersonagemD], (x,y), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Oeste True)) = drawPlayerN ([Caixa,PersonagemE], (x,y-1), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Este True)) = drawPlayerN ([Caixa,PersonagemD], (x,y-1), jogo) b

draw (ModoJogo game reload lvl Theme2, (x,y), _, jogo, _, _, _, bg,b) = 
    return $ Pictures $ backgroundsDB (GameGeral, bg) : (drawMapaDB (getMap game, jogo) (0,0) b) ++ (getPlayer game)
    where getMap (Jogo mapa (Jogador (x,y) w bool)) = mapa
          getPlayer (Jogo mapa (Jogador (x,y) Oeste False)) = drawPlayerDB ([PersonagemE], (x,y), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Este False)) = drawPlayerDB ([PersonagemD], (x,y), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Oeste True)) = drawPlayerDB ([Caixa,PersonagemE], (x,y-1), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Este True)) = drawPlayerDB ([Caixa,PersonagemD], (x,y-1), jogo) b

draw (ModoJogo game reload lvl Theme3, (x,y), _, jogo, _, _, _, bg,b) = 
    return $ Pictures $ backgroundsA (GameGeral, bg) : (drawMapaA (getMap game, jogo) (0,0) b) ++ (getPlayer game)
    where getMap (Jogo mapa (Jogador (x,y) w bool)) = mapa
          getPlayer (Jogo mapa (Jogador (x,y) Oeste False)) = drawPlayerA ([PersonagemE], (x,y), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Este False)) = drawPlayerA ([PersonagemD], (x,y), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Oeste True)) = drawPlayerA ([Caixa,PersonagemE], (x,y-1), jogo) b
          getPlayer (Jogo mapa (Jogador (x,y) Este True)) = drawPlayerA ([Caixa,PersonagemD], (x,y-1), jogo) b

-----------------------
--- Desenhar Niveis ---
----------------------- 

draw (Selecionador Nivel1 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsN (NiveisGeral, bg) : levels (Show1 , niveis) : drawNiveis ([[Select, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel2 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsN (NiveisGeral, bg) : levels (Show2 , niveis) : drawNiveis ([[Nada, Select, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel3 Theme1, (x,y), _, _, niveis, _, _,bg, _) =
    return $ Pictures $ backgroundsN (NiveisGeral, bg) : levels (Show3 , niveis) : drawNiveis ([[Nada, Nada, Select, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel4 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsN (NiveisGeral, bg) : levels (Show4 , niveis) : drawNiveis ([[Nada, Nada, Nada, Select, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel5 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsN (NiveisGeral, bg) : levels (Show5 , niveis) : drawNiveis ([[Nada, Nada, Nada, Nada, Select], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel6 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsN (NiveisGeral, bg) : levels (Show6 , niveis) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Select, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel7 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsN (NiveisGeral, bg) : levels (Show7 , niveis) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Select, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel8 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsN (NiveisGeral, bg) : levels (Show8 , niveis) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Select, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel9 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsN (NiveisGeral, bg) : levels (Show9 , niveis) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Select, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel10 Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsN (NiveisGeral, bg) : levels (Show10 , niveis) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Select], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Voltar Theme1, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsN (NiveisGeral, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                      [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Select],[Nada,Nada,Back]], niveis) (0,0) 

draw (Selecionador Nivel1 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsDB (NiveisGeral, bg) : (Translate (20) (-10) (levels (Show1 , niveis))) : drawNiveis ([[Select, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel2 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsDB (NiveisGeral, bg) : (Translate (20) (-10) (levels (Show2 , niveis))) : drawNiveis ([[Nada, Select, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel3 Theme2, (x,y), _, _, niveis, _, _,bg, _) =
    return $ Pictures $ backgroundsDB (NiveisGeral, bg) : (Translate (20) (-10) (levels (Show3 , niveis))) : drawNiveis ([[Nada, Nada, Select, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel4 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsDB (NiveisGeral, bg) : (Translate (20) (-10) (levels (Show4 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Select, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel5 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsDB (NiveisGeral, bg) : (Translate (20) (-10) (levels (Show5 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Select], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel6 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsDB (NiveisGeral, bg) : (Translate (20) (-10) (levels (Show6 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Select, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel7 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsDB (NiveisGeral, bg) : (Translate (20) (-10) (levels (Show7 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Select, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel8 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsDB (NiveisGeral, bg) : (Translate (20) (-10) (levels (Show8 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Select, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel9 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsDB (NiveisGeral, bg) : (Translate (20) (-10) (levels (Show9 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Select, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel10 Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsDB (NiveisGeral, bg) : (Translate (20) (-10) (levels (Show10 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Select], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Voltar Theme2, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsDB (NiveisGeral, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                      [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Select],[Nada,Nada,Back]], niveis) (0,0) 

draw (Selecionador Nivel1 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisGeral, bg) : (Translate 15 (-30) (levels (Show1 , niveis))) : drawNiveis ([[Select, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel2 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisGeral, bg) : (Translate 15 (-30) (levels (Show2 , niveis))) : drawNiveis ([[Nada, Select, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel3 Theme3, (x,y), _, _, niveis, _, _,bg, _) =
    return $ Pictures $ backgroundsA (NiveisGeral, bg) : (Translate 15 (-30) (levels (Show3 , niveis))) : drawNiveis ([[Nada, Nada, Select, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel4 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisGeral, bg) : (Translate 15 (-30) (levels (Show4 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Select, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel5 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisGeral, bg) : (Translate 15 (-30) (levels (Show5 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Select], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel6 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisGeral, bg) : (Translate 15 (-30) (levels (Show6 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Select, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel7 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisGeral, bg) : (Translate 15 (-30) (levels (Show7 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Select, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel8 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisGeral, bg) : (Translate 15 (-30) (levels (Show8 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Select, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel9 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisGeral, bg) : (Translate 15 (-30) (levels (Show9 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Select, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Nivel10 Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisGeral, bg) : (Translate 15 (-30) (levels (Show10 , niveis))) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                                                [Nada, Nada, Nada, Nada, Select], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Nada],[Nada,Nada,Back]], niveis) (0,0) 
draw (Selecionador Voltar Theme3, (x,y), _, _, niveis, _, _, bg,_) =
    return $ Pictures $ backgroundsA (NiveisGeral, bg) : drawNiveis ([[Nada, Nada, Nada, Nada, Nada], [Lvl1, Lvl2, Lvl3, Lvl4, Lvl5],
                                                                      [Nada, Nada, Nada, Nada, Nada], [Lvl6, Lvl7, Lvl8, Lvl9, Lvl10],[Nada,Nada,Select],[Nada,Nada,Back]], niveis) (0,0) 

--------------------------
--- Desenhar Intrucoes ---
--------------------------

draw (Pergaminho Theme1, (x,y), _, _, _, instrucoes, _, bg,_) = 
    return $ Pictures $ backgroundsN (GameGeral, bg) : drawInstrucoes ([Tips, Texto, Back, Select], instrucoes) 0

draw (Pergaminho Theme2, (x,y), _, _, _, instrucoes, _, bg,_) = 
    return $ Pictures $ backgroundsDB (GameGeral, bg) : drawInstrucoes ([Tips, Texto, Back, Select], instrucoes) 0

draw (Pergaminho Theme3, (x,y), _, _, _, instrucoes, _, bg,_) = 
    return $ Pictures $ backgroundsA (GameGeral, bg) : drawInstrucoes ([Tips, Texto, Back, Select], instrucoes) 0

------------------------
--- Desenhar Vitoria ---
------------------------

draw (ImagemVitoria Continuar _ _ Theme1, (x,y), _, _, _, _, vitoria, bg,b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsN (GameGeral, bg) : drawVitoria ([[Nada, Select, Nada, Nada],[WinCima, Continue, Level, Exit]], vitoria) (0,0) 
    | otherwise = 
        return $ Pictures $ backgroundsN (GameGeral, bg) : drawVitoria ([[Nada, Select, Nada, Nada],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0) 
draw (ImagemVitoria Niveis _ _ Theme1, (x,y), _, _, _, _, vitoria,bg, b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsN (GameGeral, bg) : drawVitoria ([[Nada, Nada, Select, Nada],[WinCima, Continue, Level, Exit]], vitoria) (0,0)
    | otherwise = 
        return $ Pictures $ backgroundsN (GameGeral, bg) : drawVitoria ([[Nada, Nada, Select, Nada],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0)
draw (ImagemVitoria Sair _ _ Theme1, (x,y), _, _, _, _, vitoria, bg,b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsN (GameGeral, bg) : drawVitoria ([[Nada, Nada, Nada, Select],[WinCima, Continue, Level, Exit]], vitoria) (0,0) 
    | otherwise = 
        return $ Pictures $ backgroundsN (GameGeral, bg) : drawVitoria ([[Nada, Nada, Nada, Select],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0) 

draw (ImagemVitoria Continuar _ _ Theme2, (x,y), _, _, _, _, vitoria, bg,b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsDB (GameGeral, bg) : drawVitoria ([[Nada, Select, Nada, Nada],[WinCima, Continue, Level, Exit]], vitoria) (0,0) 
    | otherwise = 
        return $ Pictures $ backgroundsDB (GameGeral, bg) : drawVitoria ([[Nada, Select, Nada, Nada],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0) 
draw (ImagemVitoria Niveis _ _ Theme2, (x,y), _, _, _, _, vitoria,bg, b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsDB (GameGeral, bg) : drawVitoria ([[Nada, Nada, Select, Nada],[WinCima, Continue, Level, Exit]], vitoria) (0,0)
    | otherwise = 
        return $ Pictures $ backgroundsDB (GameGeral, bg) : drawVitoria ([[Nada, Nada, Select, Nada],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0)
draw (ImagemVitoria Sair _ _ Theme2, (x,y), _, _, _, _, vitoria, bg,b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsDB (GameGeral, bg) : drawVitoria ([[Nada, Nada, Nada, Select],[WinCima, Continue, Level, Exit]], vitoria) (0,0) 
    | otherwise = 
        return $ Pictures $ backgroundsDB (GameGeral, bg) : drawVitoria ([[Nada, Nada, Nada, Select],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0) 

draw (ImagemVitoria Continuar _ _ Theme3, (x,y), _, _, _, _, vitoria, bg,b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsA (GameGeral, bg) : drawVitoria ([[Nada, Select, Nada, Nada],[WinCima, Continue, Level, Exit]], vitoria) (0,0) 
    | otherwise = 
        return $ Pictures $ backgroundsA (GameGeral, bg) : drawVitoria ([[Nada, Select, Nada, Nada],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0) 
draw (ImagemVitoria Niveis _ _ Theme3, (x,y), _, _, _, _, vitoria,bg, b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsA (GameGeral, bg) : drawVitoria ([[Nada, Nada, Select, Nada],[WinCima, Continue, Level, Exit]], vitoria) (0,0)
    | otherwise = 
        return $ Pictures $ backgroundsA (GameGeral, bg) : drawVitoria ([[Nada, Nada, Select, Nada],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0)
draw (ImagemVitoria Sair _ _ Theme3, (x,y), _, _, _, _, vitoria, bg,b) 
    | mod (round (b*90)) 200 < 100 = 
        return $ Pictures $ backgroundsA (GameGeral, bg) : drawVitoria ([[Nada, Nada, Nada, Select],[WinCima, Continue, Level, Exit]], vitoria) (0,0) 
    | otherwise = 
        return $ Pictures $ backgroundsA (GameGeral, bg) : drawVitoria ([[Nada, Nada, Nada, Select],[WinBaixo, Continue, Level, Exit]], vitoria) (0,0) 

----------------------
--- Desenhar Temas ---
----------------------

draw (Temas BlockDude Theme1, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsN (GameGeral, bg) : opcoes (NTheme, menu) : drawMenu ([[Select, Nada, Nada, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas DragonBall Theme1, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsN (GameGeral, bg) : opcoes (DBTheme, menu) : drawMenu ([[Nada, Select, Nada, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas AmongUs Theme1, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsN (GameGeral, bg) : opcoes (ATheme, menu) : drawMenu ([[Nada, Nada, Select, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas Back Theme1, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsN (GameGeral, bg) : opcoes (NTheme, menu) : drawMenu ([[Nada, Nada, Nada, Select],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)

draw (Temas BlockDude Theme2, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsDB (GameGeral, bg) : opcoes (NTheme, menu) : drawMenu ([[Select, Nada, Nada, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas DragonBall Theme2, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsDB (GameGeral, bg) : opcoes (DBTheme, menu) : drawMenu ([[Nada, Select, Nada, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas AmongUs Theme2, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsDB (GameGeral, bg) : opcoes (ATheme, menu) : drawMenu ([[Nada, Nada, Select, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas Back Theme2, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsDB (GameGeral, bg) : opcoes (DBTheme, menu) : drawMenu ([[Nada, Nada, Nada, Select],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)

draw (Temas BlockDude Theme3, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsA (GameGeral, bg) : opcoes (NTheme, menu) : drawMenu ([[Select, Nada, Nada, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas DragonBall Theme3, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsA (GameGeral, bg) : opcoes (DBTheme, menu) : drawMenu ([[Nada, Select, Nada, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)
draw (Temas AmongUs Theme3, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsA (GameGeral, bg) : opcoes (ATheme, menu) : drawMenu ([[Nada, Nada, Select, Nada],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)

draw (Temas Back Theme3, _, menu, _, _, _, _, bg, _) = 
    return $ Pictures $ backgroundsA (GameGeral, bg) : opcoes (ATheme, menu) : drawMenu ([[Nada, Nada, Nada, Select],[BlockDude, DragonBall, AmongUs, Back]], menu) (0,0)

--------------------
--- Reage Evento ---
--------------------

{- | 
Esta função trata de atribuir funções às /teclas/ e trazer-lhes consequências de acordo com o 'estado' atual 
do 'jogo'. 

As mesmas estão organizadas de acordo com cada parte do 'jogo', __menu__, __niveis__, __ModoJogo__, __etc__.
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
Desta forma, todos os 'estados' estão submetidos à __mesma contagem__ de tempo. Isto é,
quando o 'jogo' é aberto, começa a ser contado o tempo e então, mesmo que o 'estado' mude. 
-}

reageTempo :: Float -> EstadoGloss -> IO EstadoGloss   
reageTempo n (modo, pos, menu, jogo, niveis, instrucoes, vitoria,bg, b) = return (modo, pos, menu, jogo, niveis, instrucoes, vitoria,bg, (b+n))

------------
--- Main ---
------------

{- | 
As funções 'loaded' auxiliam o 'estadoInicial'. Não há muito a dizer acerca desta função.
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
