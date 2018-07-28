--algunas pruebas con sucesiones
prueba eps = let nMay = 1/(sqrt eps)
             in do putStrLn $ "dame un numero mayor que " ++ (show nMay)
                   n <- readLn :: IO Int
                   let res =  1/((fromIntegral n)^2)
                   putStrLn $ "el resultado de la funcion es" ++ 
                      (show res)
                   putStrLn $ show $ res < eps 
prueba2 f feps lim = do putStrLn "menor que que numero quieres hacer la funcion"
                        eps <- (readLn :: IO Double)
                        putStrLn $ "la N adecuada es" ++ (show (feps eps))
                        putStrLn $ "escoge un numero mayor que " ++ (show $ (feps eps))
                        n <- readLn :: IO Int 
                        putStrLn $ "la funcion y el limite estan a " ++ (show $ abs $ (f n) - lim)
                    
prueba3 = prueba2 (\n -> (1 + (-1)^n * (1/fromIntegral n))) (\e -> 1/e) 1
--tiende  a e 
prueba4 x = mapM_ (\k -> putStrLn $ show ((1 + (1 / ((fromIntegral k) + 2)))^(k+2))) [1..x]
--tiende a la raiz cuadrada de e 
prueba5 x = mapM_ (\k -> do let a1 = 2*k
                            putStrLn $ "2k es:" ++ show a1
                            let a2 = 1/(fromIntegral a1)
                            putStrLn $ "1/2k es:" ++ show a2
                            let a3 = 1 + a2
                            putStrLn $ "1 + 1/2k es:" ++ show a3
                            putStrLn $ show $ a3 ^ k
                            putStrLn "---------") [1..x]
--el limite es 1
prueba6 x = mapM_(\k -> do putStrLn $ "raiz n sima de 2 es " ++ (show $ 2 ** (1/k))) [1..x]
--tiende a 1             
prueba7 x = mapM_(\k -> do putStrLn $ "raiz n sima de 2 es " ++ (show $ 5 ** (1/k))) [1..x]

prueba8 x = mapM_ (\k -> do putStrLn $ "el resultado es " ++ (show $ (sqrt (x + 1)) - sqrt x)) [1..x]
n `nthRoot` x = fst $ until (uncurry(==)) (\(_,x0) -> (x0,((n-1)*x0+x/x0**(n-1))/n)) (x,x/n)

--algoritmo para encontrar la raiz n-sima
--la raiz n-sima del numero m con el guess nicial par y precision menor que prec
raiz_n_int :: Int  -> Double ->  Double -> Double -> IO()
raiz_n_int n m par prec = do putStrLn $ "la posible raiz es " ++ (show par)
                             let nue = (1/(fromIntegral n))*((fromIntegral (n-1))*par + (m / (par ^ (n-1))))
                             if abs ((par ^ n) - m) <= prec then
                               putStrLn $ "la raiz es " ++ (show par)
                               else do putStrLn "no se alcanzo la precision calculando de nuevo"
                                       raiz_n_int n m nue prec
