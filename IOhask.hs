-- main = putStrLn "hello, world"   show (fmr1 5 8 9)
main = do
    putStrLn "Selecta choise:\n1.-Ejercicio 1**\n2.-Ejercicio 2\n3.-Ejercicio 3\n4.-Extra"
    option <- getLine
    if option == "1"
        then do 
            form1
            main
        else if option == "2"
            then do 
                form2
                main
            else if option == "3"
                then do 
                    form3
                    main
                else if option == "4"
                    then do 
                        form4
                        main
                    else  putStrLn ("Error: opcion no valida")
    putStrLn ("Finish ")

form1 = do
    a <- getLine
    b <- getLine
    c <- getLine
    let a1 = read a :: Int
    let b1 = read b :: Int
    let c1 = read c :: Int
    print [a,b,c]
    putStrLn ("resultado " ++ (operacionuno a1 b1 c1))
form2 = do
    x <- getLine
    v <- getLine
    l <- getLine
    t <- getLine
    let x1 = read x :: Int
    let v1 = read v :: Int
    let l1 = read l :: Int
    let t1 = read t :: Int
    putStrLn ("Result " ++ (fmr2 x1 v1 l1 t1))
form3 = do
    b <- getLine
    c <- getLine
    d <- getLine
    let b1 = read b :: Int
    let c1 = read c :: Int
    let d1 = read d :: Int
    putStrLn ("Result " ++ (fmr3 b1 c1 d1))
form4 = do
    k <- getLine
    g <- getLine
    s <- getLine
    m <- getLine
    let k1 = read k :: Int
    let g1 = read g :: Int
    let s1 = read s :: Int
    let m1 = read m :: Int
    putStrLn ("Result " ++ (frm5 k1 g1 s1 m1))
-- primero
operacionuno a b c = show (r)
    where
        p1 = b * b
        p2 = 4 * (a * a)
        p3 = div c a
        p4 = div p1 p2
        p5 = p3 + p4
        r = p5
-- segundo
fmr2 x v l t = show (x + ((div (v+l) 2)) * t)
-- tercero
fmr3 b c d = show (res)
    where
        a = div (b + c + d) 2
        res = (div a b) + (div c d)
frm5 k g s m= show (div (k * g) ((s * s) + (m * m)))