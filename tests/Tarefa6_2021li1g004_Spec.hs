module Tarefa6_2021li1g004_Spec where


import Test.HUnit
import LI12122
import Tarefa6_2021li1g004
import Fixtures


testsT6 =
  test
    [ "Tarefa 6 - Teste bot sem  jogadas suficientes" ~: Nothing ~=? resolveJogo 10 resolveJogo1
    , "Tarefa 6 - Teste bot sem  jogadas suficientes" ~: Nothing ~=? resolveJogo 8 resolveJogo2
    , "Tarefa 6 - Teste bot com jogadas suficientes" ~: Just [AndarEsquerda,Trepar,Trepar,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda] ~=?  resolveJogo 13 resolveJogo2
    , "Tarefa 6 - Teste bot com imensas jogadas" ~: Just [Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda] ~=? resolveJogo 30 resolveJogo3
    , "Tarefa 6 - Teste bot com poucas jogadas" ~: Just [Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda] ~=? resolveJogo 12 resolveJogo3
    ]




    ghcjs