import Data.Complex
import Data.List
import Control.Monad.State
fact n = if n == 1 then 1 else n * (fact (n-1))
phi = length . relativos 
relativos n = aux 1 [] 
  where aux i rel = if i == n+1 then rel
                    else aux (i+1) (case algEEN i n of
                                      (_,_,1) -> i:rel
                                      _ -> rel)
                                         
prod x = foldr (*) 1 x 

algEEN a b = evalState comp (a,b,1,0,0,1)
  where
    comp = do (old_r , r , old_s , s , old_t , t) <- get
              if r /= 0
                then do let quot = div old_r r
                        put(r,old_r - (quot * r ),
                               s,old_s - (quot * s ),
                               t,old_t - (quot * t))
                        comp
                else
                return (old_s , old_t , old_r)


---el teorema chino del residuo
teoremaChinoRes ps = let m = prod (map snd ps)
                     in sum (map (\(bi,mi) ->
                                     let ni = div m mi
                                         (_,si,_) = algEEN mi ni
                                     in bi * si * ni)
                              ps)

unidades n  = filter (\x -> gcd n x == 1) [1..n]
--dada una lista de primos relativos dar el mapeo
mapeo ps n = map (\p -> rem n p) ps 

--la funcion de euler
divisores n = [ i | i <- [1..n] , rem n i == 0]

--probar que en realidad la suma de los numeros primos relativos 
--entre cada uno de los divisores es igual al numero original 
prueba1 n = (do putStrLn "los divisores de n son"
                let ds = divisores n
                putStrLn $ show ds
                mapM_ (\n -> do putStrLn $ "los numeros relativos a " ++ (show n) ++ " son"
                                putStrLn $ show $ relativos n) ds) 
  --               mapM_ impNR ds)
  -- where impNR n = do putStrLn "los numeros relativos a " ++ n ++ "son"
  --                    putStrLn $ show $ relativos n

--funcion que dado un grupo de unidades donde el grupo de unidades
--esta dado con el numero n ,da el grado de cada elemento del grupo
dame_grados n = let unids = relativos n
                in do putStrLn $ "las unidades son " ++ (show unids)
                      putStrLn $ "el numero de unidades es " ++ (show $ length unids)
                      mapM_
                        (\x -> putStrLn $ "el grado del elemento " ++ (show x) ++ " es "  ++ (show $ grado x n))
                        unids
  
--funcion para decirme dado un grupo de unidades que me diga cuales son los elementos que tienen un grado en especifico
grado n p = aux 1
  where 
    aux i = if rem (n ^ i) p  == 1 then i
            else aux (i+1)

--calculos para el grupo de las unidades con respecto a una potencia de primos
prueba2 = do putStrLn "escoge un numero primo distinto de 2"
             p <- readLn :: IO Integer
             putStrLn "escoge un numero a talque el primo no divida a a"
             a <- readLn :: IO Integer
             putStrLn "escoge una potencia para el modulo"
             n <- readLn :: IO Integer
             putStrLn "el orden de 1 + ap mod p^n es p^(n-1)"
             putStrLn $ "el resultado es" ++ ( show $ rem ((1 + a*p)^(p ^(n-1))) (p ^ n))

--probar la congruencia con el 5
prueba3 = do putStrLn "escoge una l mayor o igual que 3 "
             l <- readLn :: IO Integer
             let orden = 2 ^ (l - 2)
             putStrLn $ "el orden de 5 modulo " ++ (show (2 ^ l)) ++ " deberia ser " ++ (show orden)
             putStrLn $ "las sucesivas potencias del numero son"
             mapM_
               (\x -> do putStrLn $ "con la potencia " ++ (show x) ++ " es " ++ (show $ rem (5 ^ x) (2 ^ l)))
               [1..orden]
  
---resolver ecuaciones de la forma x ^ n = a (m)
--- solamente supongamos que m tiene raices primitivas y que (a,m) = 1 entonces
---la formula tiene solucion si a ^ (phi(m)/d) = 1 (m) donde d = (phi(m),n)
--hagamos una prueba supongamos que queremos un x talqeu x ^4 = 10 (17) , entonces tendria que suceder que
--10 ^ phi(17) / 4 = 10 ^ 4 = 1 (17) pero esto no pasa , ahora me voy a asegurar que no hay un elemento que en efecto cumpla con ello
prueba4 = mapM_
          (\x -> do let act = x ^ 4
                    putStrLn $ (show x) ++ "^4=" ++  (show act) ++ "=" ++ (show $ rem act 17) ++"(17)")
          [1..17]

--ecuaciones de la forma x^n = a (m)
prueba6 = do putStrLn "dame el numero al que quieres hacer igual la potencia"
             a <- readLn :: IO Integer
             putStrLn "dame el modulo que por el momento sea primo"
             m <- readLn :: IO Integer
             putStrLn "dame la raiz a la que lo quieres elevar"
             n <- readLn :: IO Integer
             let d = gcd n (m-1)
             putStrLn $ "d es "++ (show d)
             let r =  a ^ (div (m-1) d)
             if potM a (div (m-1) d) m == 1 then
               putStrLn "es soluble"
               else putStrLn "no es soluble"
--el siguiente programa te da una solucion si es que esta existe de la ecuacion
--x * a = b (m)
inverso a b m = let (a',m',d) = (algEEN a m)
                    r = div b d 
                in if rem b d /= 0 then error "no tiene solucion"
                   else a' * r 
  --cual es la solucion
modl n m = let r = rem n m
           in if r < 0 then r + m
              else r
raizPrim n = find (\x -> grado x n == n-1) [2..(n-1)]
--y en efecto no hay
potM :: Integral a => a -> a -> a -> a
potM a n m = evalState comp (1,0)
  where
    comp = do (c,p) <- get
              if p == n  then return c
                else do put (rem (c * a) m,p+1)
                        comp
          
--sistema residual para el problema con 2
prueba5 = do putStrLn "escoje una l mayor o igual a 3"
             l <- readLn :: (IO Int)
             let res = [((-1)^a*(5^b),(a,b)) | a <- [0,1],b <- [0..((2^(l-2)) -1)]]
             --hacer los a todos modulo 2 l
             let res' = map (\(x,m) -> let pot = 2 ^ l 
                                           y = rem x pot
                                       in if y < 0 then (y + pot,m) else (y,m)) res 
             putStrLn $ "el sistema es modulo 2^l " ++ show (res')
             putStrLn $ "la longitud es " ++  (show $ length res')
  
--programa para sacar los cuadrados
imprimeCuadrados x = mapM_ (\y -> do putStrLn $ "el cuadrado de " ++ (show y) ++ "es " ++ (show $ modl (y ^ 2) x)) [2..(x-1)]

cuadrados x = nub [modl (y ^ 2 ) x | y <- [1..(x-1)]]

mu p a = let lim = - (div (p - 1) 2)
             res_neg = map (\x -> modl x p) [lim .. -1]
             mult =  map (\x -> let r' = modl x p
                                in if r' <= div (p - 1) 2 then r' else r' - p ) [ a*i | i <- [1..(-lim)]] 
         in length $ filter (\x -> x < 0) mult
isPrime n = length [i | i <- [2..(ceiling $ sqrt (fromIntegral n))] , rem n i == 0 ] == 0 

primosHasta n = [i | i <- [2..n], isPrime i]

--por alguna razon cuando multiplicas todos los eleentos menores que tienen maximo comun divisor como 1 obtienes o 1 o
--obtienes el numero original menos 1
prueba7 n = do putStrLn "imprimir la multiplicacion de la multiplicacion de los elementos menores y que tienen mcd 1"
               mapM_ (\x -> do putStrLn $ "el numero es " ++ (show x)
                               let mul = rem (prod $ unidades x) x 
                               putStrLn $ "la multiplicacion de todos sus elementos es " ++ (show mul)) [2..n]  
--ahora que pasa si dado un numero a todas sus unidades multiplicas phi n veces un numero que tiene
--maximo comun divisor igual a 1
prueba8 = do putStrLn "dame un numero"
             n <- readLn :: IO Int
             let un = unidades n
             putStrLn $ "las unidades son " ++ (show $ un)
             putStrLn "dame un numero que sea primo relativo con el numero escogido "
             rel <- readLn :: IO Int
             putStrLn $  "el numero escogido es congruente con " ++ (show (rem rel n))
             putStrLn $ "el nuevo sistema es " ++ (show (map (*rel) un))
             putStrLn "que pasa si de cada numero tomas el resto"
             putStrLn $ show (map (\x -> rem (rel*x) n) un)
---la funcion con la que se demuestra el ...cuadrado.
--la funcion es
fun z = let a1 = exp (0 :+ 2 * pi* z)
            a2 = exp (-(0 :+ 2*pi *z))
        in a1 - a2 

---dado un conjunto de numeros imprimir el ciclico que generan
--prueba9 n = mapM_ (do putStrLn "el grupo que genera "

conj_generado y n = aux 1 []
  where aux x l = let new = rem (y*x) n
                  in
                    if new  == 1 then (l ++ [1])
                    else aux new (l ++ [new])
conj_generado_sum y n = aux 0 []
  where aux x l = let new = rem (y+x) n
                  in
                    if new  == 0 then (l ++ [0])
                    else aux new (l ++ [new])
--dado un numero imprime todos los conjuntos generados de los numeros hasta ese numero
prueba9 n = mapM_ (\x -> do putStrLn $ "el conjunto generado del numero " ++ (show x) ++ " es " ++ (show $ conj_generado x n )) [1..(n-1)]
--dar una lista con pares para ver lo de la funcion de euler
prueba10 n = map (\(i,n) -> let g = (gcd i n) in (div i g , div n g)) $ [(i,n) | i <- [1..n]]


--la igualdad del coeficiente binomial modulo un primo
cambio_base 0 p = [] 
cambio_base n p =  (cambio_base (div n p) p ) ++ [(rem n p)]

coef_bin n k = div (fact n) ((fact k) * (fact (n - k)))

valuacion_p p n = aux n 0
  where aux n i = if rem n p == 0 then aux (div n p) (i+1)
                  else i

norm gi = (imagPart gi) ^ 2 +  (realPart gi) ^ 2 

--pequeno programa para ver como se hace una division en los
--complejos gaussianos

prueba11 a b = do putStrLn "escoje dos numeros gaussianos que puedan cumplir"
                  q <- readLn :: IO (Complex Double)
                  putStrLn $ "a es igual a " ++ show a
                  putStrLn $ "b * q es" ++ show (b * q)
                  let res = a - (b*q)
                  putStrLn $ "para a te falta" ++ show res
                  putStrLn $ "la norma de q es " ++ show (norm q)
                  putStrLn $ "la norma del resto es" ++ show (norm res)
                  if a == (b * q) + res && (norm res) < (norm q) then
                    do putStrLn "funcionan"
                    else do putStrLn "no funcionan"
                            prueba11 a b
prueba12 a b = take 1 [(r,i) | r <- [-100..100],i <- [-100..100],norm (a - (b* (r :+ i))) <  norm b ]
--acercar un vector a otro hasta que su norma sea menor que 1
acercar c1 c2 = aux c2 1 
  where
    ini = c1
    aux y t = if magnitude (c1 - y) < 1
              then y
              else aux (c1 + ((t / 2) * c2)) (t/2)
