-- Eliabe Vinicius Costa e Silva - 11721BCC032
--1)

--   1 + 2 * 3
-- => 7
-- liguraguem respeita as regras matematicas de prioridade

--    5 ^ 3
-- => 125
-- ^ é o operador de potenciação com retorno int

--    5 ** 3
-- => 125.0
-- ** é o operador de potenciação com retorno doube

--    5 / 3
-- => 1.6666666666666667
-- / é o operador de divisão copm retorno double

--    div 5 3
-- => 1
-- div é a divisão com retorno int

--    mod 5 3
-- => 2
-- mod retorna o resto da divisão de 2 numeros

--    5 < 3
-- => False
-- operações de comparação são aceitas

--    mod 5 3 < 2
-- => False
--    mod 5 3 == 2
-- => True
-- operações finalizadas retornam se a resposta é True ou false


--    sqrt 81
-- => 9.0
-- sqrt é a operação de raiz quadrada

--    logBase 2 1024
-- => 10.0
-- operação de log

--    floor 5.7
-- => 5
-- arredontar para baixo

--    ceiling 5.7
-- => 6
-- arredontar para cima

--    abs (-5)
-- => 5
-- modlo do numero


--    min 6 7
-- => 6
-- menor numero entra os apresentados

--    max 6 7
-- => 7
-- maior numero entre os apresentados

--    sin (pi/2)
-- => 1.0
-- Seno

--    sum [1..5]
-- => 15
-- soma de varios numeros

--    not True
-- => False
-- negação

--    True && False
-- => False
-- operações logicas


--2)
dobro n = n * 2
--3)
quadri x = dobro(dobro x)
--4)
hip x y = sqrt((x^2)+(y^2))
--5)
dist x1 y1 x2 y2 = sqrt(((x2-x1)^2)+((y2-y1)^2))