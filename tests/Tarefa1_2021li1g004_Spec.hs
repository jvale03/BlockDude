module Tarefa1_2021li1g004_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g004
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: True ~=? validaPotencialMapa m1
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: False ~=? validaPotencialMapa [] 
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: False ~=? validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))]
    , "Tarefa 1 - Testa Mapa válido m2" ~: True ~=? validaPotencialMapa m2
    , "Tarefa 1 - Teste Mapa válido m3" ~: True ~=? validaPotencialMapa m3
    , "Tarefa 1 - Teste Mapa válido m4" ~: True ~=? validaPotencialMapa m4
    , "Tarefa 1 - Teste Mapa válido m5" ~: True ~=? validaPotencialMapa m5
    , "Tarefa 1 - Teste Mapa válido m6" ~: False ~=? validaPotencialMapa m6
    , "Tarefa 1 - Teste Mapa válido m7" ~: True ~=? validaPotencialMapa m7
    , "Tarefa 1 - Teste Mapa válido m8" ~: True ~=? validaPotencialMapa m8
    ]
