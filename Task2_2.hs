module Task2_2 where

import Todo(todo)
    
import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f i [] = i
foldl f i (x:xs) = foldl f (f i x) xs 

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f i [] = i
foldr f i (x:xs) = x `f` foldr f i xs


unfoldr foo ini = helper (foo ini) where
  helper (Just (x, i')) = x : unfoldr foo i'
  helper Nothing = []
 
-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f  =  foldr ((:) . f) []

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldr (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes lst = foldl f [] lst where
                             f lst' (Just x) = x:lst'
                             f lst' Nothing = lst'
-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal x = zipWith (!!) x [0..]
 
-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = foldr (\x xs -> if f x then xs else x:xs) []

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem x  =  foldl (\ s x' -> if x' == x then True else s) False

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step   | to < from = error "Range error!!"
                       | otherwise = unfoldr (\ x -> if x >= to then Nothing else Just(x, x + step)) from
 
-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr (\ x -> if null x
                               then Nothing 
                               else Just(take (fromIntegral n) x , drop (fromIntegral n) x)) lst
