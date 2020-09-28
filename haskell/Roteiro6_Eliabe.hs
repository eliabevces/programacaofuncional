--1}
paridade::[Int]->[Bool]
paridade list = map even list

-- --2)
prefixos::[[Char]]->[[Char]] 
prefixos list = map (take 3) list

--3)
saudacao::[[Char]]->[[Char]]
saudacao list = map ("Oi " ++) list

--4)
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar cond list = [y | y<-list, cond y == True]

--5)
pares::[Int]->[Int]
pares list = filter even list

--6)
solucoes::[Int]->[Int]
solucoes list = filter (\x->5*x+6 < x*x) list

--7)
maior::[Int]->Int
maior list = foldr1 max list

--8)
menor_min10::[Int]->Int
menor_min10 list = foldr min 10 list

--9)
junta_silabas_plural::[[Char]]->[Char]
junta_silabas_plural list = foldr (++) "s" list

--10)
lst1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
lst2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
lst3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
lst4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
lst5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
lst6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
lst7 = [1..1000]
lst8 = [1000,999..1]
lst9 = lst1++[0]
lst10 = [0]++lst3
lst11 = lst1++[0]++lst3
lst12 = lst3++[0]++lst1

--bubble sort
bubble_sort [] = []
bubble_sort lista = bubbleOrd lista (length lista)

bubbleOrd lista 0 = lista
bubbleOrd lista n = bubbleOrd (troca lista) (n-1)

troca [x] = [x]
troca (x:y:zs)
    |x>y = y:troca(x:zs) 
    |otherwise = x:troca(y:zs)

--selection sort
selection_sort::(Ord a) => [a]->[a]
selection_sort [] = []
selection_sort xs = [x] ++ selection_sort (remove x xs) where x = minimo xs

remove::(Ord a) => a->[a]->[a]
remove a [] = []
remove a (x:xs)
    |a==x = xs
    |otherwise = x:(remove a xs)

minimo::(Ord a)=>[a]->a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
    |x<=(minimo xs) = x
    |otherwise = minimo xs

--insertion sort
insertion_sort::Ord a=>[a]->[a]
insertion_sort = foldr insereOrd []

insereOrd::(Ord a)=>a->[a]->[a]
insereOrd x [] = [x]
insereOrd x (y:ys)
    |x<=y = (x:y:ys)
    |otherwise = y:(insereOrd x ys)

--quick sort
quick_sort::(Ord a)=>[a]->[a]
quick_sort [] = []
quick_sort (s:xs) = quick_sort [x | x<-xs, x<s ] ++ [s] ++ quick_sort [ x|x<-xs,x>=s ]






--11)
--bubble sort
bubble_sortcont [] = ([],0)
bubble_sortcont lista = bubbleOrdcont (lista,0) (length lista)

bubbleOrdcont (lista,cont) 0 = (lista,cont) 
bubbleOrdcont (lista,cont) n = bubbleOrdcont (trocacont (lista,cont)) (n-1)

trocacont ([x],cont) = ([x],cont)
trocacont ((x:y:zs),cont) =
    if x > y
        then aux (trocacont ((x : zs), cont + 1)) y
        else aux (trocacont ((y : zs), cont + 1)) x
      where
        aux (list, count) a = (a : list, count)
   
-- Main> bubble_sortcont lst1
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],380)
-- Main> bubble_sortcont lst2
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],380)
-- Main> bubble_sortcont lst3
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],380)
-- Main> bubble_sortcont lst4
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],380)
-- Main> bubble_sortcont lst5
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],380)
-- Main> bubble_sortcont lst6
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],380)
-- Main> bubble_sortcont lst7
-- ERROR - C stack overflow
-- Main> bubble_sortcont lst8
-- ERROR - C stack overflow
-- Main> bubble_sortcont lst9
-- ERROR - C stack overflow
-- Main> bubble_sortcont lst10
-- ERROR - C stack overflow
-- Main> bubble_sortcont lst11
-- ERROR - C stack overflow
-- Main> bubble_sortcont lst12
-- ERROR - C stack overflow





-- --selection sort
selection_sortcont::(Ord a) => [a]->([a],Int)
selection_sortcont list = selection_sortcont2 list 0


selection_sortcont2::(Ord a) => [a]->Int->([a],Int)
selection_sortcont2 [] x = ([],x)
selection_sortcont2 (x:xs) cont = 
    let (least, n_num) = minimocont (x : xs) cont

        removecont _ [] = []
        removecont cont (h : t) = if (cont == h) then t else h : (removecont cont t)

        add (lst, cont) y = (y : lst, cont)
    in add (selection_sortcont2 (removecont least (x : xs)) n_num) least


minimocont::(Ord a)=>[a]->Int->(a,Int)
minimocont [] _ = undefined
minimocont [x] cont = (x,cont)
minimocont (x:y:xs) cont
    | x > y = minimocont (y : xs) (cont + 1)
    | otherwise = minimocont (x : xs) (cont + 1)

    -- Main> selection_sortcont lst1
    -- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],190)
    -- Main> selection_sortcont lst2
    -- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],190)
    -- Main> selection_sortcont lst3
    -- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],190)
    -- Main> selection_sortcont lst4
    -- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],190)
    -- Main> selection_sortcont lst5
    -- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],190)
    -- Main> selection_sortcont lst6
    -- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],190)
    -- Main> selection_sortcont lst7
    -- ERROR - C stack overflow
    -- Main> selection_sortcont lst8
    -- ERROR - C stack overflow
    -- Main> selection_sortcont lst9
    -- ERROR - C stack overflow
    -- Main> selection_sortcont lst10
    -- ERROR - C stack overflow
    -- Main> selection_sortcont lst11
    -- ERROR - C stack overflow
    -- Main> selection_sortcont lst12
    -- ERROR - C stack overflow





--insertion sort
insertion_sortcont::Ord a=>[a]->([a],Int)
insertion_sortcont [] = ([],0)
insertion_sortcont [x] = ([x],0)
insertion_sortcont (x : xs) =
  let (calda_ord, n) = insertion_sortcont xs

      (lst, n1) = insereOrdcont x calda_ord n
   in (lst, n1)


insereOrdcont::(Ord a)=>a->[a]->Int->([a],Int)
insereOrdcont x [] cont = ([x],cont)
insereOrdcont x (y:ys) cont = if (x<=y) then ((x:y:ys),cont+1) else aux(insereOrdcont x ys (cont+1)) y
    where
        aux (list,n) x = (x:list,n)

-- Main> insertion_sortcont lst1
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],19)
-- Main> insertion_sortcont lst2
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],190)
-- Main> insertion_sortcont lst3
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],118)
-- Main> insertion_sortcont lst4
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],100)
-- Main> insertion_sortcont lst5
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],109)
-- Main> insertion_sortcont lst6
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],83)
-- Main> insertion_sortcont lst7
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432,433,434,435,436,437,438,439,440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,474,475,476,477,478,479,480,481,482,483,484,485,486,487,488,489,490,491,492,493,494,495,496,497,498,499,500,501,502,503,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,526,527,528,529,530,531,532,533,534,535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,553,554,555,556,557,558,559,560,561,562,563,564,565,566,567,568,569,570,571,572,573,574,575,576,577,578,579,580,581,582,583,584,585,586,587,588,589,590,591,592,593,594,595,596,597,598,599,600,601,602,603,604,605,606,607,608,609,610,611,612,613,614,615,616,617,618,619,620,621,622,623,624,625,626,627,628,629,630,631,632,633,634,635,636,637,638,639,640,641,642,643,644,645,646,647,648,649,650,651,652,653,654,655,656,657,658,659,660,661,662,663,664,665,666,667,668,669,670,671,672,673,674,675,676,677,678,679,680,681,682,683,684,685,686,687,688,689,690,691,692,693,694,695,696,697,698,699,700,701,702,703,704,705,706,707,708,709,710,711,712,713,714,715,716,717,718,719,720,721,722,723,724,725,726,727,728,729,730,731,732,733,734,735,736,737,738,739,740,741,742,743,744,745,746,747,748,749,750,751,752,753,754,755,756,757,758,759,760,761,762,763,764,765,766,767,768,769,770,771,772,773,774,775,776,777,778,779,780,781,782,783,784,785,786,787,788,789,790,791,792,793,794,795,796,797,798,799,800,801,802,803,804,805,806,807,808,809,810,811,812,813,814,815,816,817,818,819,820,821,822,823,824,825,826,827,828,829,830,831,832,833,834,835,836,837,838,839,840,841,842,843,844,845,846,847,848,849,850,851,852,853,854,855,856,857,858,859,860,861,862,863,864,865,866,867,868,869,870,871,872,873,874,875,876,877,878,879,880,881,882,883,884,885,886,887,888,889,890,891,892,893,894,895,896,897,898,899,900,901,902,903,904,905,906,907,908,909,910,911,912,913,914,915,916,917,918,919,920,921,922,923,924,925,926,927,928,929,930,931,932,933,934,935,936,937,938,939,940,941,942,943,944,945,946,947,948,949,950,951,952,953,954,955,956,957,958,959,960,961,962,963,964,965,966,967,968,969,970,971,972,973,974,975,976,977,978,979,980,981,982,983,984,985,986,987,988,989,990,991,992,993,994,995,996,997,998,999,1000],999)
-- Main> insertion_sortcont lst8
-- (ERROR - C stack overflow
-- Main> insertion_sortcont lst9
-- ERROR - C stack overflow
-- Main> insertion_sortcont lst10
-- ERROR - C stack overflow
-- Main> insertion_sortcont lst11
-- ERROR - C stack overflow
-- Main> insertion_sortcont lst12
-- ERROR - C stack overflow



--quick sort

quick_sortcont :: (Ord a) => [a] -> ([a], Int)
quick_sortcont [] = ([], 0)
quick_sortcont (piv : xs) =
  let (left, n_L) = quick_sortcont2 xs 0 (<= piv)
      (right, n_R) = quick_sortcont2 xs 0 (> piv)
      (sorted_L, n1_L) = quick_sortcont left
      (sorted_R, n1_R) = quick_sortcont right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)

quick_sortcont2:: [a] -> Int -> (a -> Bool) -> ([a], Int)
quick_sortcont2 [] n _ = ([], n)
quick_sortcont2 (x : xs) n cond =
    if (cond x)
    then add (quick_sortcont2 xs (n + 1) cond) x
    else quick_sortcont2 xs (n + 1) cond
    where
    add (list, n) y = (y : list, n)

-- Main> quick_sortcont lst1
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],380)
-- Main> quick_sortcont lst2
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],380)
-- Main> quick_sortcont lst3
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],200)
-- Main> quick_sortcont lst4
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],200)
-- Main> quick_sortcont lst5
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],160)
-- Main> quick_sortcont lst6
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],168)
-- Main> quick_sortcont lst7
-- ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85
-- ERROR - Garbage collection fails to reclaim sufficient space
-- Main> quick_sortcont lst8
-- (
-- ERROR - Garbage collection fails to reclaim sufficient space
-- Main> quick_sortcont lst9
-- ([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],382)
-- Main> quick_sortcont lst10
-- ([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],240)
-- Main> quick_sortcont lst11
-- ([0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20],804)
-- Main> quick_sortcont lst12
-- ([0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20],466)


--12)
bubble_sortcontdec [] = ([],0)
bubble_sortcontdec lista = bubbleOrdcontdec (lista,0) (length lista)

bubbleOrdcontdec (lista,cont) 0 = (lista,cont) 
bubbleOrdcontdec (lista,cont) n = bubbleOrdcontdec (trocacontdec (lista,cont)) (n-1)

trocacontdec ([x],cont) = ([x],cont)
trocacontdec ((x:y:zs),cont) =
    if x < y
        then aux (trocacontdec ((x : zs), cont + 1)) y
        else aux (trocacontdec ((y : zs), cont + 1)) x
      where
        aux (list, count) a = (a : list, count)

-- Main> bubble_sortcontdec lst1
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],380)
-- Main> bubble_sortcontdec lst2
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],380)
-- Main> bubble_sortcontdec lst3
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],380)
-- Main> bubble_sortcontdec lst4
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],380)
-- Main> bubble_sortcontdec lst5
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],380)
-- Main> bubble_sortcontdec lst6
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],380)
-- Main> bubble_sortcontdec lst7
-- ERROR - C stack overflow
-- Main> bubble_sortcontdec lst8
-- ERROR - C stack overflow
-- Main> bubble_sortcontdec lst9
-- ERROR - C stack overflow
-- Main> bubble_sortcontdec lst10
-- ERROR - C stack overflow
-- Main> bubble_sortcontdec lst11
-- ERROR - C stack overflow
-- Main> bubble_sortcontdec lst12
-- ERROR - C stack overflow


--selection sort
selection_sortcontdec::(Ord a) => [a]->([a],Int)
selection_sortcontdec list = selection_sortcont2dec list 0


selection_sortcont2dec::(Ord a) => [a]->Int->([a],Int)
selection_sortcont2dec [] x = ([],x)
selection_sortcont2dec (x:xs) cont = 
    let (least, n_num) = maximocontdec (x : xs) cont

        removecontdec _ [] = []
        removecontdec cont (h : t) = if (cont == h) then t else h : (removecontdec cont t)

        add (lst, cont) y = (y : lst, cont)
    in add (selection_sortcont2dec (removecontdec least (x : xs)) n_num) least


maximocontdec::(Ord a)=>[a]->Int->(a,Int)
maximocontdec [] _ = undefined
maximocontdec [x] cont = (x,cont)
maximocontdec (x:y:xs) cont
    | x < y = maximocontdec (y : xs) (cont + 1)
    | otherwise = maximocontdec (x : xs) (cont + 1)

-- Main> selection_sortcontdec lst1
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],190)
-- Main> selection_sortcontdec lst2
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],190)
-- Main> selection_sortcontdec lst3
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],190)
-- Main> selection_sortcontdec lst4
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],190)
-- Main> selection_sortcontdec lst5
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],190)
-- Main> selection_sortcontdec lst6
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],190)
-- Main> selection_sortcontdec lst7
-- ERROR - C stack overflow
-- Main> selection_sortcontdec lst8
-- ERROR - C stack overflow
-- Main> selection_sortcontdec lst9
-- ERROR - C stack overflow
-- Main> selection_sortcontdec lst10
-- ERROR - C stack overflow
-- Main> selection_sortcontdec lst11
-- ERROR - C stack overflow
-- Main> selection_sortcontdec lst12
-- ERROR - C stack overflow

--insertion sort
insertion_sortcontdec::Ord a=>[a]->([a],Int)
insertion_sortcontdec [] = ([],0)
insertion_sortcontdec [x] = ([x],0)
insertion_sortcontdec (x : xs) =
  let (calda_ord, n) = insertion_sortcontdec xs

      (lst, n1) = insereOrdcontdec x calda_ord n
   in (lst, n1)


insereOrdcontdec::(Ord a)=>a->[a]->Int->([a],Int)
insereOrdcontdec x [] cont = ([x],cont)
insereOrdcontdec x (y:ys) cont = if (x>=y) then ((x:y:ys),cont+1) else aux(insereOrdcontdec x ys (cont+1)) y
    where
        aux (list,n) x = (x:list,n)

-- Main> insertion_sortcontdec lst1
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],190)
-- Main> insertion_sortcontdec lst2
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],19)
-- Main> insertion_sortcontdec lst3
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],100)
-- Main> insertion_sortcontdec lst4
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],118)
-- Main> insertion_sortcontdec lst5
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],113)
-- Main> insertion_sortcontdec lst6
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],139)
-- Main> insertion_sortcontdec lst7
-- (ERROR - C stack overflow
-- Main> insertion_sortcontdec lst8
-- ERROR - C stack overflow
-- Main> insertion_sortcontdec lst9
-- ERROR - C stack overflow
-- Main> insertion_sortcontdec lst10
-- ERROR - C stack overflow
-- Main> insertion_sortcontdec lst11
-- ERROR - C stack overflow
-- Main> insertion_sortcontdec lst12
-- ERROR - C stack overflow


--quick sort

quick_sortcontdec :: (Ord a) => [a] -> ([a], Int)
quick_sortcontdec [] = ([], 0)
quick_sortcontdec (piv : xs) =
  let (left, n_L) = quick_sortcont2dec xs 0 (>= piv)
      (right, n_R) = quick_sortcont2dec xs 0 (< piv)
      (sorted_L, n1_L) = quick_sortcontdec left
      (sorted_R, n1_R) = quick_sortcontdec right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)

quick_sortcont2dec:: [a] -> Int -> (a -> Bool) -> ([a], Int)
quick_sortcont2dec [] n _ = ([], n)
quick_sortcont2dec (x : xs) n cond =
    if (cond x)
    then add (quick_sortcont2dec xs (n + 1) cond) x
    else quick_sortcont2dec xs (n + 1) cond
    where
    add (list, n) y = (y : list, n)

-- Main> quick_sortcontdec lst1
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],380)
-- Main> quick_sortcontdec lst2
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],380)
-- Main> quick_sortcontdec lst3
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],200)
-- Main> quick_sortcontdec lst4
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],200)
-- Main> quick_sortcontdec lst5
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],160)
-- Main> quick_sortcontdec lst6
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1],168)
-- Main> quick_sortcontdec lst7
-- (
-- ERROR - Garbage collection fails to reclaim sufficient space
-- Main> quick_sortcontdec lst8
-- ([1000,999,998,997,996,995,994,993,992,991,990,989,988,987,986,985,984,983,982,981,980,979,978,977,976,975,974,973,972,971,970,969,968,967,966,965,964,963,962,961,960,959,958,957,956,955,954,953,952,951,950,949,948,947,946,945,944,943,942,941,940,939,938,937,936,935,934,933,932,931,930,929,928,927,926,925,924,923,922,921,920,919,918,917,916
-- ERROR - Garbage collection fails to reclaim sufficient space
-- Main> quick_sortcontdec lst9
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0],382)
-- Main> quick_sortcontdec lst10
-- ([20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0],240)
-- Main> quick_sortcontdec lst11
-- ([20,20,19,19,18,18,17,17,16,16,15,15,14,14,13,13,12,12,11,11,10,10,9,9,8,8,7,7,6,6,5,5,4,4,3,3,2,2,1,1,0],840)
-- Main> quick_sortcontdec lst12
-- ([20,20,19,19,18,18,17,17,16,16,15,15,14,14,13,13,12,12,11,11,10,10,9,9,8,8,7,7,6,6,5,5,4,4,3,3,2,2,1,1,0],480)