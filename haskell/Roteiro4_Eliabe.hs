--1)

lst1 = [x*2 | x <- [1..10], x*2 >= 12]
-- Main> lst1
-- [12,14,16,18,20]
-- Lista de numeros de 1 a 10 ao quadrado q sejam maior que 12

lst2 = [ x | x <- [50..100], mod x 7 == 3]
-- Main> lst2
-- [52,59,66,73,80,87,94]
-- multiplos de 7 + 3 cno intervalo de 50 a 100

lst3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
-- Main> lst3
-- [10,11,12,14,16,17,18,20]
-- lista de 10 a 20 excluindo o 13,15 e 19

lst4=[(x,y)| x <- [1..4], y <- [x..5]]
-- Main> lst4
-- [(1,1),(1,2),(1,3),(1,4),(1,5),(2,2),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5),(4,4),(4,5)]
-- todas duplas com x variando de 1 a 4 e y do "x atual" ate 5

--2)
quadrados::Int->Int->[Int]
quadrados x y = [ z*z | z<-[x..y]]

--3)
seleciona_impares::[Int]->[Int]
seleciona_impares x = [ y| y<-x, odd y]

--4)
tabuada::Int->[Int]
tabuada x = [ y*x | y<-[1..10]]

--5)
--bissexto do roteiro 2
bissexto::Int->Bool
bissexto a = if mod a 4 == 0 && mod a 100 /= 0 || mod a 400 == 0 then True
else False

bissextos::[Int]->[Int]
bissextos x = [ y | y <- x, bissexto y]

--6)
sublistas::[[Int]]->[Int]
sublistas x  = [ y | t<-x, y<-t]

--7)
type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

getData::Emprestimo->Data
getData (_,_,_,x,_) = x

atrasado::(Data,Data)->Bool
atrasado ((da,ma,aa),(de,me,ae))
    |(aa==ae && ma==me && da>de) || (aa==ae && ma>me) || (aa>ae) = True
    |otherwise = False

atrasados::Emprestimos->Data->Emprestimos
atrasados x (da,ma,aa) = [y | y<-x, atrasado((da,ma,aa),(getData y ))]

--8)
npares::[Int]->Int
npares [] = 0
npares (x:xs)
    |even x = (npares xs) + 1
    |otherwise = npares xs

--9)
produtorio::[Int]->Int
produtorio [] = 1
produtorio (x:xs) = x*produtorio(xs)

--10)
comprime::[[Int]]->[Int]
comprime [] = []
comprime (x:xs) = x++comprime(xs)

--11)
tamanho::[a]->Int
tamanho [] = 0
tamanho (x:xs) = 1+tamanho(xs)

--12)
uniaoNRec::Eq a=>[a]->[a]->[a]
uniaoNRec x y = x++[ z | z<-y, (elem z x)==False]

--13)
uniaoNRec2::Eq a=>[a]->[a]->[a]
uniaoNRec2 x [] = x
uniaoNRec2 x (y:ys)
    |elem y x = uniaoNRec2 x ys
    |otherwise = uniaoNRec2 (x++[y]) ys
