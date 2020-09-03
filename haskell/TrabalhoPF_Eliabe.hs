--1)
triangulo::(Int,Int,Int)->String
triangulo(x,y,z)
    |x==60 && y==60 && z==60 = "equilatero"
    |(x==90 || z==90 || y==90) && (x+y+z)==180 && x>0 && y>0 && z>0 = "retangulo"
    |(x>90 || z>90 || y>90) && (x+y+z)==180 && x>0 && y>0 && z>0 = "obtuso"
    |(x+y+z)==180 && x>0 && y>0 && z>0 = "simples"
    |otherwise = "nao_triangulo"


--3)
type Data = (Int,Int,Int)

idade::(Data,Data)->Int
idade ((d1,m1,a1),(d2,m2,a2))
    |m1<m2 || (m1==m2 && d1<d2) = (a1-a2)-1
    |otherwise = (a1-a2)

passagem::Float->(Data,Data)->Float
passagem preco datas
    |(idade(datas)) < 2 = (preco*0.15)
    |(idade(datas)) <= 10 = (preco*0.40)
    |(idade(datas)) >= 70 = (preco*0.50)
    |otherwise = preco
    
--5)a)
contaNegM2::[Int]->Int
contaNegM2 x = length([ y|y<-x,even y, y<0 ])

--b)
listaNegM2::[Int]->[Int]
listaNegM2 x = [ y|y<-x,even y, y<0 ]

--7)
fatores::Int->[Int]
fatores x = [ y| y <- [1..x] , (mod x y) == 0 ]

primos::Int->Int->[Int]
primos x y = [ z| z <- [x..y], length(fatores z) == 2 ]

--9)
listimpares::Int->[Float]
listimpares n = [ fromIntegral y | y<-[1, 3..n]]

listpares::Int->[Float]
listpares n = [ fromIntegral y | y<-[2,4..n]]

divimpares::Float->Int->[Float]
divimpares x n = [ y/x | y<-(listimpares n) ]

divpares::Float->Int->[Float]
divpares x n = [ y/x | y<-(listpares n) ]

calculaserie::Float->Int->Float
calculaserie x n = sum(divimpares x n)+sum(divpares x n)