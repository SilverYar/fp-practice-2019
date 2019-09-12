module Task2_1 where

import Prelude hiding (lookup)
import Todo(todo)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа

data TreeMap v = EmpTreeMap 
                | NodeTreeMap Integer v (TreeMap v) (TreeMap v) 
                deriving(Show) 

-- Пустое дерево

emptyTree :: TreeMap v
emptyTree = EmpTreeMap
    
-- Содержится ли заданный ключ в дереве?

contains :: TreeMap v -> Integer -> Bool
contains EmpTreeMap x = False
contains (NodeTreeMap  key z lt rt ) k  | k == key = True
                                        | k < key = contains lt k
                                        | k > key = contains rt k 

-- Значение для заданного ключа

lookup :: Integer -> TreeMap v -> v
lookup k (NodeTreeMap key z lt rt )  | k == key = z
                                     | k < key= lookup k lt
                                     | k > key = lookup k rt 
lookup k  EmpTreeMap = error "!! The key is not found in the tree"

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmpTreeMap = NodeTreeMap  k v EmpTreeMap EmpTreeMap
insert (k, v) (NodeTreeMap  key z lt rt) | k < key   = NodeTreeMap key z (insert (k, v) lt) rt
                                         | k > key   = NodeTreeMap key z lt (insert (k, v) rt)
                                         | otherwise = (NodeTreeMap key z lt rt)	

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove n (NodeTreeMap key z lt rt) | key  == n = select (NodeTreeMap key z lt rt) 
                                   | key  > n = NodeTreeMap key z ( remove n lt ) rt 
                                   | key  < n = NodeTreeMap key z lt ( remove n rt )

select :: TreeMap v -> TreeMap v
select (NodeTreeMap k z lt rt) =  case (k, lt, rt) of
                                         (_, EmpTreeMap, EmpTreeMap) -> EmpTreeMap
                                         (_, EmpTreeMap, _) -> rt 
                                         (_, _, EmpTreeMap) -> lt
                                         (k, lt, rt) -> rmv k lt rt
									
rmv :: Integer -> TreeMap v -> TreeMap v -> TreeMap v
rmv k (NodeTreeMap kx zx ltx rtx) (NodeTreeMap ky zy lty rty) | k > ky = NodeTreeMap ky zy (insert (kx,zx) lty) rty
                                                              | otherwise = NodeTreeMap kx zx ltx (insert (ky,zy) rtx)	
								 
							   
										  
-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE _ EmpTreeMap = error "No element"
nearestLE k (NodeTreeMap key z lt rt)
                         | key > k = nearestLE k lt
                         | key < k  = case rt of
                          (NodeTreeMap key z _ _) | (k == key) -> (key, z)
                          (NodeTreeMap key z _ _) | (k /= key) -> nearestLE k rt
                          otherwise -> (key, z)
                         | otherwise = (key, z)

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert EmpTreeMap lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmpTreeMap =  []
listFromTree (NodeTreeMap  key z lt rt) = listFromTree(lt) ++ [(key, z)] ++ listFromTree(rt)

size :: TreeMap v -> Integer
size EmpTreeMap  = 0
size (NodeTreeMap _ _ lt rt) = 1 + (size lt) + (size rt)

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ EmpTreeMap  = error "Tree is empty"
kMean k (NodeTreeMap key z lt rt)
                          | (size lt) == k = (key, z)
                          | (size lt) > k  = kMean k lt
                          | otherwise = kMean (k - (size lt) - 1) rt
