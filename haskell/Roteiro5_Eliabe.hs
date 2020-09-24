--1)
    --a)
    type Data = (Int,Int,Int)
    valida::Data->Bool
    valida (d,m,a) = t1 || t2 || t3 || t4
            where
                t1 = d>=1 && d<=31 && (m==1 || m== 3 || m==5 || m== 7 || m==8 || m==10 || m==12)
                t2 = d>=1 && d<=30 && (m==4 || m==6 || m==9 || m==11)
                t3 = d>=1 && d<=30 && m==2 && (bissexto a == True)
                t4 = d>=1 && d<=29 && m==2
    --b)
    bissexto::Int->Bool
    bissexto a = t1 && t2 || t3
        where
            t1 = mod a 4 == 0
            t2 = mod a 100 /= 0
            t3 = mod a 400 == 0

    bissextos::[Int]->[Int]
    bissextos x = list
        where list = [ y | y <- x, bissexto y]


    --c)
    type Emprestimo = (String, String, Data, Data, String)
    type Emprestimos = [Emprestimo]
    bdEmprestimo::Emprestimos
    bdEmprestimo = [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

    getData::Emprestimo->Data
    getData (_,_,_,x,_) =  y
        where y = x

    atrasado::(Data,Data)->Bool
    atrasado ((da,ma,aa),(de,me,ae)) = t1 || t2 || t3
        where
            t1 = (aa==ae && ma==me && da>de)
            t2 = (aa==ae && ma>me)
            t3 = (aa>ae)

    atrasados::Emprestimos->Data->Emprestimos
    atrasados x (da,ma,aa) = list
        where
            list = [y | y<-x, atrasado((da,ma,aa),(getData y ))]


    --d)
    passo::(Int,Int) -> (Int,Int)
    passo (x,y) = resp
        where
            resp = (y,x+y)

    fibo2::Int ->(Int,Int)
    fibo2 0 = (0,1)
    fibo2 x = resp
        where
            resp = passo(fibo2(x-1))


    --e)
    prodIntervalo::(Int,Int)->Int
    prodIntervalo (m,n) =  if t1 then resp1 else resp2
        where
            t1 = (m==(n-1))
            resp1 = (m*n)
            resp2  = m*prodIntervalo(m+1,n)

    fatorialprod::Int->Int
    fatorialprod x = resp
        where
            resp = prodIntervalo(1,x)

--2)
    --a)
    validalet::Data->Bool
    validalet (d,m,a) =
            let t1 = d>=1 && d<=31 && (m==1 || m== 3 || m==5 || m== 7 || m==8 || m==10 || m==12)
                t2 = d>=1 && d<=30 && (m==4 || m==6 || m==9 || m==11)
                t3 = d>=1 && d<=30 && m==2 && (bissextolet a == True)
                t4 = d>=1 && d<=29 && m==2
            in t1 || t2 || t3 || t4
    --b)
    bissextolet::Int->Bool
    bissextolet a =
        let t1 = mod a 4 == 0
            t2 = mod a 100 /= 0
            t3 = mod a 400 == 0
        in t1 && t2 || t3

    bissextoslet::[Int]->[Int]
    bissextoslet x =
        let list = [ y | y <- x, bissextolet y]
        in list


    --c)

    getDatalet::Emprestimo->Data
    getDatalet (_,_,_,x,_) =
        let y = x
        in y

    atrasadolet::(Data,Data)->Bool
    atrasadolet ((da,ma,aa),(de,me,ae)) =
        let t1 = (aa==ae && ma==me && da>de)
            t2 = (aa==ae && ma>me)
            t3 = (aa>ae)
        in t1 || t2 || t3

    atrasadoslet::Emprestimos->Data->Emprestimos
    atrasadoslet x (da,ma,aa) =
        let list = [y | y<-x, atrasadolet((da,ma,aa),(getData y ))]
        in list

    --d)
    passolet::(Int,Int) -> (Int,Int)
    passolet (x,y) =
        let resp = (y,x+y)
        in resp

    fibo2let::Int ->(Int,Int)
    fibo2let 0 = (0,1)
    fibo2let x =
        let resp = passolet(fibo2(x-1))
        in resp

    --e)
    prodIntervalolet::(Int,Int)->Int
    prodIntervalolet (m,n) =
        let resp = if(m==(n-1)) then (m*n) else (m*prodIntervalolet(m+1,n))
        in resp

    fatorialprodlet::Int->Int
    fatorialprodlet x =
        let resp = prodIntervalolet(1,x)
        in resp

--3)
--3.1)
-- (\x. 2 * x + 1) 3
-- 2*3 + 1
-- 6+1
-- 7

--3.2)
-- (\ xy .x-y) 5 7
-- 5-7
-- -2

--3.3)
-- (\ yx .x-y) 5 7
-- 7-5
-- 2

--3.4)
-- (\xy. x-y) (\z. z/2)
-- (\y. (\z. z/2)-y)

--3.5)
-- (\xy. x-y) ((\z.z/2) 6) 1
-- (\xy. x-y) (6/2) 1
-- (\xy. x-y) 3 1
-- 3- 1
-- 2

--3.6)
-- (\x. \y. - x y) 9 4
-- (- 9 4)
--  9 - 4
-- 5

--3.7)
-- (\x. xx ) ( \y. y)
-- (\y. yy)

--4)
-- Main> (\x -> x + 3) 5
-- 8
-- equivalente a 5+3

-- Main> (\x -> \y -> x * y + 5) 3 4
-- 17
-- equivalente a 3*4+5

-- Main> (\(x,y) -> x * y^2) (3,4)
-- 48
-- equivalente a 3 * 4^2

-- Main> (\(x,y,_) -> x * y^2) (3,4,2)
-- 48
-- equivalente a 3*4^2, o terceito numero (2) é ignorado

-- Main>  (\xs -> zip xs [1,2,3]) [4,5,6]
-- [(4,1),(5,2),(6,3)]
-- equivalente a zip [4,5,6] [1,2,3]

--5)
--a)
    a = (\x -> \y-> y) ((\z-> z)(\z->z))(\w->w) 5
    -- Main> a
    -- 5

--b)
    b = ((\f-> (\x-> f(f x))) (\y-> (y * y))) 3
    -- Main> b
    -- 81  

--c)
    c = ((\f-> (\x-> f(f x)))(\y->(y + y))) 5
    -- Main> c
    -- 20

--d)
    d = ((\x-> (\y->  x + y) 5) ((\y-> y - 3) 7))
    -- Main> d
    -- 9

--e)
    e = (((\f-> (\x-> f(f(f x)))) (\y-> (y * y))) 2)
    -- Main> e
    -- 256

--f)
    f = (\x-> \y-> x + ((\x-> x - 3) y)) 5 6
    -- Main> f
    -- 8