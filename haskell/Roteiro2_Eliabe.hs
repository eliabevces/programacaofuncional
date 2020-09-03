--1)
dobro::Int->Int
dobro n = n * 2
quadri::Int->Int
quadri x = dobro(dobro x)
hip::Double->Double->Double
hip x y = sqrt((x^2)+(y^2))
dist::Double->Double->Double->Double->Double
dist x1 y1 x2 y2 = sqrt(((x2-x1)^2)+((y2-y1)^2))


-- 2)
-- Hugs> fst (2,5)
-- 2
-- retorna o primeiro elemento da tupla

-- Hugs> snd(5,"Bom dia")
-- "Bom dia"
-- retorna o segundo elemento da tupla

-- Hugs> (1,1) == (1,1)
-- True
-- Hugs> (1,1) /= (1,1)
-- False
-- Hugs> (1,1) < (1,2)
-- True
-- Hugs> (2,1) < (1,2)
-- False
-- Hugs> (1,2,3) < (1,2)
-- ERROR - Type error in application
-- *** Expression     : (1,2,3) < (1,2)
-- *** Term           : (1,2,3)
-- *** Type           : (c,d,e)
-- *** Does not match : (a,b)
-- é possivel comparar tuplas diretamente sem ajuda de funçoes 

-- Hugs> "azul" < "verde"
-- True
-- Hugs> "azul" < "amarelo"
-- False
-- strings são comparadas de acordo com a ordem alfabetica

-- Hugs> (1,2,3) == (,,) 1 2 3
-- True
-- a linguagem aceita esse tipo de notação prefixa


--3)
conversao::Double->(Double,Double,Double)
conversao x = (x,x/3.98,x/4.45)

--4)
bissexto::Int->Bool
bissexto a = if mod a 4 == 0 && mod a 100 /= 0 || mod a 400 == 0 then True
else False

--5)
type Data = (Int,Int,Int)
bissexto2::Data->Bool
bissexto2 (d,m,a) = if mod a 4 == 0 && mod a 100 /= 0 || mod a 400 == 0 then True
else False

--6)
valida::Data->Bool
valida (d,m,a) 
        |d>=1 && d<=31 && (m==1 || m== 3 || m==5 || m== 7 || m==8 || m==10 || m==12) = True
        |d>=1 && d<=30 && (m==4 || m==6 || m==9 || m==11) = True
        |d>=1 && d<=30 && m==2 && (bissexto a == True) = True
        |d>=1 && d<=29 && m==2 = True
        |otherwise = False

--7)
precede::Data->Data->Bool
precede (d1,m1,a1) (d2,m2,a2) 
        |(valida (d1,m1,a1) == True) && (valida(d2,m2,a2)==True) && a1<a2= True
        |a1>a2 = False
        |(valida (d1,m1,a1) == True) && (valida(d2,m2,a2)==True) && m1<m2 = True
        |m1>m2 = False
        |(valida (d1,m1,a1) == True) && (valida(d2,m2,a2)==True) && d1<d2 = True
        |otherwise = False


--8)
type Livro = (String,Int,String,String,String,Data)
type Aluno = (String,String,String,Int)
type Emprestimo = (String,String,Data,Data,String)

--9)
verifica::Emprestimo->Data->String
verifica (cod_livro,cod_aluno,(d1,m1,a1),(d2,m2,a2),sit) (da,ma,aa)
        |sit == "devolvido" = "Em dia"
        |(precede(da,ma,aa) (d2,m2,a2) == True) = "Emprestado Regular"
        |(da,ma,aa) == (d2,m2,a2) = "Ultimo dia de emprestimo"
        |otherwise = "Livro emprestado atrasado"
