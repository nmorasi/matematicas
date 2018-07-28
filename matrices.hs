import Data.Array
import Control.Monad.Trans.State
data Matriz a = Mtz (Int -> Int -> a) (Int,Int)

listMat rws = Mtz (\x y -> (rws !! x) !! y) (length rws , length $ head rws)

nrow (Mtz _ (i,_)) = i 
ncol (Mtz _ (_,j)) = j
getM (Mtz f (i,j)) n m = f n m
row (Mtz f (_, j)) n = [f n m | m <- [0..(j-1)]]
col (Mtz f (i,j)) n = [f m n | m <- [0 .. (i-1)]]

printMat m = mapM_ (\i -> putStrLn $ show $ row m i) [0..((nrow m) - 1)]

productoPunto v1 v2 = sum (zipWith (*) v1 v2)
multMat m1 m2 = listMat $ map (\i -> map (\m -> productoPunto (row m1 i ) (col m2 m)) [0..((ncol m2) - 1)]) [0 .. ( (nrow m1) -1 )] 
--hacer la multiplicacion de matrices 

m1 = listMat [[1,2],[3,4]]
m2 = listMat [[2,0],[1,2]]
prueba1 = let p1 = listMat [[1,0,0],[0,0,1],[0,1,0]]
              p2 = listMat [[0,1,0],[0,0,1],[1,0,0]] 
              m = listMat [[1,2,3],[4,5,6],[7,8,9]]
          in do putStrLn "la primera matriz es"
                printMat m
                putStrLn "la matriz permutada es"
                printMat $ multMat p1 m
                putStrLn "la matriz permutada es"
                printMat $ multMat p2 m
                
prueba2 = let p = listMat [[0,0,1],[1,0,0],[0,1,0]]
              l = listMat [[1,0,0],[0.2,1,0],[0.6,0.5,1]]
              u = listMat [[5,6,3],[0,0.8,-0.6],[0,0,2.5]]
              a = listMat [[1,2,0],[3,4,4],[5,6,3]]
          in do printMat $ multMat p a
                printMat $ multMat l u 
--primero , sacar a los bi's
--dado un vector b y una matriz de permutacion y la matriz triangular
--la matriz triangular l , el vector de permutacion y
--el vector v
-- forwardSubs :: Array Int Double -> Matriz Double -> Array Int Int -> Double -> Array Int Double
-- forwardSubs b l p zero = evalStateT comp (arrIn,0)
--   where
--     n = nrow l
--     arrIn = (listArray (0,n - 1) (repeat zero))
--     comp :: StateT (Array Int Double,Int) IO (Array Int Double)
--     comp =
--       do (y,i) <- get
--          if i == n
--            then return y
--            else do let ny = y // [(i,(b ! (p ! i)) - sum[ (getM l i j) * (y ! j) | j <- [0 .. i-1 ]])]
--                    put (ny , i+1)
--                    comp 
          
                        
-- --                                (sum [*]))
-- --   [0 ... ((nrows l) - 1)] 
-- --prueba de que funciona forwardSubs
-- prueba3 = let p = listArray (0,2) [2,0,1]
--               b = listArray (0,2) [7,3,8]
--               l = listMat [[1,0,0],[0.2,1,0],[0.6,0.5,1]]
--           in forwardSubs b l p 0
              
sumaNums 0 = [1]
sumaNums n = let ls = sumaNums (n-1)
             in ls ++ [(sum ls)]
--definicion del outer product
outerProd v1 v2 = listMat $ map (\y -> map (\x -> y*x) v2) v1 
--se supone que una matriz se puede factorizar en dos matrices y
--le quta el renglon y la columna a la matriz m
submatriz (Mtz f (r,c)) = Mtz (\ r' c' -> f (r'+1) (c'+1 )) (r-1, c-1)
sumaMat (Mtz f (c,r)) (Mtz g (c',r')) = Mtz (\x y -> (f x y) + (g x y)) (c,r)
restaMat (Mtz f (c,r)) (Mtz g (c',r')) = Mtz (\x y -> (f x y) - (g x y)) (c,r)
multEsc a (Mtz f (c,r)) = Mtz (\x y -> a * (f x y)) (c,r)
componer a v w (Mtz f (c,r)) =
  Mtz (\x y -> if x >= 1 && y >= 1
               then f (x-1)  (y-1)
               else if x == 0 && y == 0
                    then a
                    else if y == 0 && x > 0
                         then v !! (x -1)
                         else  w !! (y-1))
  (c+1,r+1)
--descomposicion en 2
desc m = let a00 = getM m 0 0
             r0 = tail $ row m 0 
             c0 = tail $ col m 0
             m1 = componer a00 (map (\x -> x * (1/a00)) c0) [0|_<-[1..((ncol m) - 1)]] (ident ((ncol m) - 1))
             m2 =  componer a00 [0| _<- [1..((nrow m) - 1)]] r0 (restaMat (submatriz m) (multEsc (1/a00) $ outerProd c0 r0))
         in (m1,m2)
ident n = Mtz (\x y -> if x == y then 1 else 0) (n,n)
--voy a hacer la prueba si lo que dice en el libro es cierto 
prueba3  = let m1 = listMat [[1,0,0],[4,1,0],[7,0,1]]
               m2 = listMat [[1,2,3],[0,-3,-6],[0,-6,-12]]
           in do printMat m1
                 printMat m2 
                 printMat $ multMat m1 m2

min_l (x:xs) = aux x xs
  where aux x [] = x 
        aux x (y:ys) = if y < x then aux y ys
                       else aux x ys 
--si el minimo es igual al primero entonces aumenta uno de otra manera no aumentes
inter n m l = evalState comp ([],l)
  where comp = do (izq,d:dr) <- get
                  if d == n then return $ izq ++ (m:dr)
                    else if dr == [] then return $ izq++[d]
                         else do put (izq ++ [d],dr)
                                 comp
--aqui el indice comienza en 1
num_inter l = evalState (comp l) (0,0)
  where comp [] = do (ind,ni) <- get
                     return ni 
        comp (x:xs) = do (ind,ni) <- get
                         if ind == x then do put (ind+1,ni)
                                             comp xs
                           else do put (ind+1,ni + 1)
                                   comp (inter ind x xs)
---funcion signo
signo l = if even (num_inter l) then 1 else -1 

determ m = let n = (nrow m) - 1
               prod l = foldr (*) 1 l 
           in sum [ (signo p) * prod [(getM m (p !! i) i ) | i <- [0 .. n]] | p <-  permutaciones [0 .. n] ]
              
  
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys):[y:i | i <- intercala x ys]                            
permutaciones [] = [[]]
permutaciones (l:ls) = concat $ map (intercala l) (permutaciones ls)


--quitar el renglon i y la columna j a la matriz m
quitar_r_c r c (Mtz f (rm,cm)) = Mtz g (rm - 1, cm - 1)
  where g = \ x y -> if x < r then
                       if y < c then
                         f x y
                       else
                         f x (y + 1)
                     else
                       if y < c then
                         f (x + 1) y 
                       else
                         f (x+1) (y+1)
                         
adjoin m = Mtz f ((nrow m), (ncol m))
  where f = \x y -> (-1) ^ (x + y)  * (determ (quitar_r_c y x m))
