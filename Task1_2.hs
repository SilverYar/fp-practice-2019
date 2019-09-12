module Task1_2 where

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
--gcd :: Integer -> Integer -> Integer
--gcd x y = todo

-- наибольший общий делитель двух чисел

gcd' :: Integer -> Integer -> Integer

gcd' x y | x < 0            = error "x must be x > 0 !!!"
         | y < 0            = error "y must be y > 0 !!!"
         | x == 0 && y == 0 = error "Invalid input x != 0, y != 0!!!"
         | y == 0           = x
         | otherwise = gcd' y (rem x y)
		 
-- наибольший общий делитель двух чисел
--gcd :: Integer -> Integer -> Integer
--gcd x y = todo

--gcd :: Integer -> Integer -> Integer
--gcd 0 0 = Error "Unsupported: both numbers are equal to 0."
--gcd x y = case x y  of
--		x < 0 -> error
--		y < 0  -> error
--   then error "Unsupported: negative arguments."
--    else res_ x y
--    where res_ a 0 = a
--res_ a b = res_ b (a `rem` b)


-- существует ли полный целочисленный квадрат в диапазоне [from, to)?

doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = if from == to then error "from must be unequal to (from != to) " 
                                                else (ceiling $ sqrt $ fromIntegral from) <= (floor $ sqrt $ fromIntegral to-1)


--simple from to = elem (ceiling $ sqrt $ fromIntegral $ takeWhile (<= to) . iterate succ $ from)


-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?

isRealDate :: Int -> Int -> Int -> Bool
isRealDate day month year = if year  < 1 || 
                                 month < 1|| 
								   month > 12 || 
								      day  < 1 then False 
									             else if month == 2 
												    then if year `mod` 4 == 0 
												           then day <= 29 else day <= 28
                                                         else day <= md!!(month - 1)
															where md = [31, 28, 31, 30, 31, 30,
             															31, 31, 30, 31, 30, 31]

 -- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя

-- Первый вариант

-- mod - остаток от деления
		
power :: Integer -> Integer -> Integer
power _ 0 = 1
power 0 _ = 0
power x n  = let qvdr m = m * m
                 ndiv2 = n `div` 2  
                 qvpow = qvdr ( power x ndiv2 )
             in if even n then qvpow else x * qvpow 


			 
-- Второй вариант		   
pow:: Integer->Integer->Integer
pow x n  | n < 0                       = error "!!!Pow must be positive"
		 | x == 1 || n == 0 || x == 0  = 1
         | n == 1                      = x
         | even n                      = (pow x ( div n 2))*(pow x ( div n 2)) 
         | odd n                       = x * (pow x (n-1))

--Первое решение
-- является ли данное число простым?
--isPrime :: Integer -> Bool
--isPrime x = todo

is_prime :: Integer -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False 
           | otherwise = True


-- Второе решение
isPrime' k = if k > 1 then null [ x | x <- [2..k - 1], k `mod` x == 0] else False

--type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
--shapeArea :: [Point2D] -> Double
--shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
--triangleKind :: Point2D -> Point2D -> Point2D -> Integer
--triangleKind a b c = todo
