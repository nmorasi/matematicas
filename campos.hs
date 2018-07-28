import Data.List
--hacer una funcion que tome un polinomio y lo evalue
--los polinomios como son de 3 variables van a ser una lista de listas de parejas con el
--coeficiente y con el y con la cadena que representa a la variable 
evalua p vs md = foldr (\ (c,n) s  -> rem ((c * (eva_sum n)) + s) md) 0 p 
  where
    eva_sum s = rem (foldr (\(n,e) m -> case (find (\(x,_) -> x == n) vs ) of
                                          Just (x,v) -> (v ^ e)*m
                                          _ -> error "no se puede encontrar" ) 1 s) md
buscar_zeros p =  length  $ filter (\x -> evalua p x 5 == 0 ) [[("X",v1),("Y",v2),("Z",v3)] | v1 <- [0..4],v2  <- [0..4] , v3 <- [0..4]]
