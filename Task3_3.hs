module Task3_3 where

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

--			РЕШЕНИЕ

-- Моноид – это тип, отвечающий двум требованиям: 1) есть нейтральное значение, называемоенулём моноида; 
--                                                2) есть операция добавления моноида к моноиду

-- Функтором является любой тип данных, для которого определено, как к нему применяется fmap.Можно сказать, что map отображает функцию над 
-- обычными значениями в функцию над списками. Тип данных (в данном случае, список), поддерживающийтакое отображение, называется функтором.

-- Виды функторов: 
-- 1) Ковариантный функтор - это тот, в котором стрелки "внутри" и "снаружи" указывают в одном направлении.
-- 	Пример: fmap :: (a -> b) -> f a -> f b => функтор является ковариантным
-- 2) Контравариантный функтор - это тот, в котором стрелки "внутри" и "снаружи" указывают в противоположных направлениях.
--  Пример: PSet{ contains :: (a -> Bool) => PSet - контрвариативный
-- => нужно изменить либо PSet либо fmap так, чтобы оба были либо ковариантными либо контрвариативными

class Contravariant f 
  where
    conmap :: (a -> b) -> f b -> f a -- контравариантый функтор

instance Contravariant PSet 
  where
    conmap f (PSet a) = PSet (a . f)
 

-- Основные операции: пересечение, объединение и симметрическая разность множеств

--1) Пересечние // это множество, которому принадлежат те и только те элементы, которые одновременно принадлежат всем данным множествам//

newtype InterSet a = InterSet {cont_interset :: (a -> Bool) }

instance Monoid (InterSet  a) 
   where
    mempty = InterSet (\m-> True)
    mappend (InterSet m) (InterSet n)  = InterSet (\p -> m p && n p) 
    
instance Semigroup (InterSet a) 
   where
    (<>) m n = mappend m n

--2) Объединение //множество, содержащее в себе все элементы исходных множеств//

newtype UnionSet a = UnionSet{ cont_unionset :: (a -> Bool) }

instance Monoid (UnionSet a)
  where
    mempty = UnionSet (\m-> False)
    mappend (UnionSet m) (UnionSet n)  = UnionSet (\p -> m p || n p) 
    
instance Semigroup (UnionSet a) 
   where
    (<>) m n = mappend m n
    
--3) Симметрическая разность // теоретико-множественная операция, результатом которой является новое множество, 
-- включающее все элементы исходных множеств, не принадлежащие одновременно обоим исходным множествам //

newtype DifSet a = DifSet{ cont_diffset :: (a -> Bool) }

instance Monoid (DifSet a) 
   where
    mempty = DifSet (\m-> False)
    mappend (DifSet m) (DifSet n)  = DifSet (\p -> ((m p) && (not $ n p)) || ((n p) && (not $ m p))) 
    
instance Semigroup (DifSet a) 
  where
   (<>) m n = mappend m n