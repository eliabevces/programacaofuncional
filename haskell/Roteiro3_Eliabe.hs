--1)a)
ou ::Bool->Bool->Bool
ou  False False = False
ou _ _ = True

ou1 ::Bool->Bool->Bool
ou1 _ True = True
ou1 True _ = True
ou1 _ _ = False

ou2 ::Bool->Bool->Bool
ou2 True False = True
ou2 True True = True
ou2 False True = True
ou2 False False = False

--b)
ou3 ::Bool->Bool->Bool
ou3 x y 
    | x == True || y == True = True
    |otherwise = False


ou4 ::Bool->Bool->Bool
ou4 x y 
    | x == False && y == False = False
    |otherwise = True

--2)
distancia::Float->Float->Float->Float->Float
distancia x1 y1 x2 y2 = sqrt(((x2-x1)^2)+((y2-y1)^2))

--3)
-- Main> 1:[2,3,4]
-- [1,2,3,4]
-- Main> 'a':['b','c','d']
-- "abcd"
-- insere no começo da lista


-- Main> head [1,2,3]
-- 1
-- retorna cabeça da lista

-- Main> tail [1,2,3]
-- [2,3]
-- retorna cauda da lista

-- Main> [1,5,2,3]!!1
-- 5 
-- Main> [1,5,2,3]!!3
-- 3
-- retorna a posição definida depois de !!

-- Main> elem 2 [1,5,2,3]
-- True
-- Retorna True se o elemnto estiver na lista

-- Main> take 2 [1,5,2,3,7]
-- [1,5]
-- retorna uma lista com a quantidade de elementos escolhidos (retira a partir da cabeça da lista)

-- Main> drop 2 [1,5,2,3,7]
-- [2,3,7]
-- retira da lista a quantidade de elementos escolhidos (a partir da cabeça)

-- Main> [1,2] ++ [3,4]
-- [1,2,3,4]
-- concatena listas

-- Main> [1..10]
-- [1,2,3,4,5,6,7,8,9,10]
-- Main> [7,6..3]
-- [7,6,5,4,3]
-- cria uma lista com todos os numeros no intervalo apresentado

-- Main> ['b'..'g']
-- "bcdefg"
-- cria uma lista com letras do intervalo apresentado de acordo com a ordem alfabetica

-- Main> take 5 [1,3..]
-- [1,3,5,7,9]
-- pega 5 primeiros numeros da lista "infinita" de numeros impares

-- Main> sum [1..10]
-- 55
-- soma todos numeros no intervalo apresentado

-- Main> maximum [1,5,2,3,7]
-- 7
-- retorna maior valor da lista

-- Main> minimum [1,5,2,3,7]
-- 1
-- retorna menor valor da lista

--4)
fatorial::Int->Int
fatorial x 
    |x==0 = 1
    |otherwise = fatorial (x-1)*x 

fat::Int->Int
fat 0 = 1
fat n = fat(n-1)*n

--5)
fibo::Int->Int
fibo x
    |x==0 = 0
    |x==1 = 1
    |otherwise = fibo(x-2)+fibo(x-1)

--6)
triangular::Int->Int
triangular x
    |x==1 = 1
    |otherwise = x+triangular(x-1)
    
--7)
passo::(Int,Int)->(Int,Int)
passo (x,y) = (y,x+y)

fibo2::Int->(Int,Int)
fibo2 x = (fibo(x),fibo(x+1))


--8)
potencia2::Int->Int
potencia2 x
    |x==0 = 1
    |otherwise = potencia2(x-1) * 2

--9)a)
prodIntervalo::(Int,Int)->Int
prodIntervalo (m,n)
    |m==(n-1) = (m*n)
    |otherwise = m*prodIntervalo(m+1,n)

--b)
fatorialprod::Int->Int
fatorialprod x = prodIntervalo(1,x)

--11)
resto_div::(Int,Int)->Int
resto_div (m,n)
    |n>m = m
    |otherwise = resto_div((m-n),n)

div_inteira::(Int,Int)->Int
div_inteira (m,n)
    |n>m = 0
    |otherwise = div_inteira((m-n),n)+1

--12)
mdc::(Int,Int)->Int
mdc (m,n)
    |n==0 = m
    |otherwise = mdc(n,mod m n) 

mdc_casa::(Int,Int)->Int
mdc_casa(m,0) = m
mdc_casa(m,n) = mdc_casa(n, mod m n)

--13)
binomial::(Int,Int)->Int
binomial(x,y)
    |y==0 = 1
    |y==x = 1
    |otherwise = binomial(x-1,y)+binomial(x-1,y-1)

binomial_casa::(Int,Int)->Int
binomial_casa (x,0) = 1
binomial_casa (x,y) = if x==y then 1
    else binomial_casa(x-1,y)+binomial_casa(x-1,y-1)

--14)
--a)[5,4..1]
--b)['a','c'..'e']
--c)[1,4..16]
--d)zip [1,-2..(-11)] [1,5..17]

--15)a)
listar (x,y)
    |x==y = [x]
    |otherwise = [x..y]

--b)
listar_pares(x,y)
    |x==y = []
    |mod x 2 == 0 = [x,x+2..y]
    |otherwise = [x+1,x+3..y]