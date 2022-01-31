module Tarefa2_2021li1g004_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g004
import Fixtures

testsT2 =
  test
    [ "Tarefa 2 - Teste Construir Mapa m1" ~: m1r ~=? constroiMapa m1
    , "Tarefa 2 - Teste Construir Mapa vazio" ~: [] ~=? constroiMapa []
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m1 ~=?  sort (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Desconstruir Mapa vazio" ~: [] ~=? desconstroiMapa []
    , "Tarefa 2 - Teste Identidade m1" ~: sort m1 ~=?  sort (desconstroiMapa (constroiMapa m1))
    , "Tarefa 2 - Teste Identidade m1r" ~: m1r ~=?  constroiMapa (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Construir Sobrepor Peças" ~: constroiMapa [(Porta, (7, 4))] ~=?  constroiMapa [(Porta, (7, 4)), (Porta, (7, 4))]
    , "Tarefa 2 - Teste Construir Mapa m2" ~: m2r ~=? constroiMapa m2
    , "Tarefa 2 - Teste Construir Mapa m3" ~: m3r ~=? constroiMapa m3
    , "Tarefa 2 - Teste Construir Mapa m4" ~: m4r ~=? constroiMapa m4
    , "Tarefa 2 - Teste Construir Mapa m5" ~: m5r ~=? constroiMapa m5
    , "Tarefa 2 - Teste Construir Mapa m6" ~: m6r ~=? constroiMapa m6
    , "Tarefa 2 - Teste Construir Mapa m7" ~: m7r ~=? constroiMapa m7
    , "Tarefa 2 - Teste Construir Mapa m8" ~: m8r ~=? constroiMapa m8
    , "Tarefa 2 - Teste Desconstruir Mapa m2r" ~: sort m2 ~=? sort (desconstroiMapa m2r)
    , "Tarefa 2 - Teste Desconstruir Mapa m3r" ~: sort m3 ~=? sort (desconstroiMapa m3r)
    , "Tarefa 2 - Teste Desconstruir Mapa m4r" ~: sort m4 ~=? sort (desconstroiMapa m4r)
    , "Tarefa 2 - Teste Desconstruir Mapa m5r" ~: sort m5 ~=? sort (desconstroiMapa m5r)
    , "Tarefa 2 - Teste Desconstruir Mapa m6r" ~: sort m6 ~=? sort (desconstroiMapa m6r)
    , "Tarefa 2 - Teste Desconstruir Mapa m7r" ~: sort m7 ~=? sort (desconstroiMapa m7r)
    , "Tarefa 2 - Teste Desconstruir Mapa m8r" ~: sort m8 ~=? sort (desconstroiMapa m8r)
    , "Tarefa 2 - Teste Identidade m2" ~: sort m2 ~=? sort (desconstroiMapa (constroiMapa m2))  
    , "Tarefa 2 - Teste Identidade m4r" ~: m4r ~=? constroiMapa (desconstroiMapa m4r)
    , "Tarefa 2 - Teste Identidade m8" ~: m8 ~=? desconstroiMapa (constroiMapa m8)
    ]