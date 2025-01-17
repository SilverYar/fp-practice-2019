{-  Задание 7
Описанная в лекции структура данных «очередь» представляет собой одностороннюю очередь. 
В то же самое время достаточно часто необходимо использовать двустороннюю очередь 
(или «деку»). Покажите, как с помощью модификаций односторонней очереди получить 
аналогичную структуру данных, но поддерживающую операции двусторонней очереди. 
Операции должны иметь наилучшие показатели сложности, которых вы (как вы считаете) 
можете достичь.
Реализуйте предложенную структуру.
Оцените амортизированную сложность операций над предложенной структурой данных 
методами банкира и физика.
-}

module Main where

{-     В лекции односторонняя очередь имела стек входящих и стек исходящих элементов,
    для двусторонней очереди эти стеки превращаются просто в левый и правый.
    С ними обоими можно проводить одни и те же операции - заталкивать (push) и 
    выталкивать (pop) элементы.
       Также при опустении исходящего стека мы перебрасывали в него весь входящий
    стек, если он был не пуст. Для двусторонней очереди логичным будет перебрасывать
    половину другого стека в пустой: в обоих стеках будет одинаковое кол-во элементов
    (ну, за исключением тех случаев, когда общее кол-во элементов будет нечетным).
-}

--         левый стек v
data Deque a = Deque [a] [a]
--            правый стек ^

-- Для наглядного отображения инвертируем стек исходящих, чтоб как на картинке было
instance (Show a) => Show (Deque a) where
    show (Deque [] []) = "[]><[]"
    show (Deque in' out) = show in' ++ "><" ++ show (reverse out)
--

-- Создание пустой очереди
empty :: Deque a
empty = Deque [] []
--

-- Делим список на два и возвращаем их в паре
pairList :: [a] -> ([a], [a])
pairList list  = splitAt (length list `div` 2) list
--

-- Добавление в начало очереди 
pushFront :: Deque a -> a -> Deque a
pushFront (Deque in' out) x = Deque (x:in') out
--

-- Удаление из начала очереди
popFront :: Deque a -> (Deque a, a)
popFront (Deque [] []) = error "Deque is empty"
popFront (Deque (h:t) out) = (Deque t out, h)
popFront (Deque [] out) = popFront$ Deque (reverse $ snd half) (fst half)
    where half = pairList out

-- Добавление в конец очереди 
pushBack :: Deque a -> a -> Deque a
pushBack (Deque in' out) x = Deque in' (x:out)
--


-- Удаление из конца очереди 
popBack :: Deque a -> (Deque a, a)
popBack (Deque [] []) = error "Deque is empty"
popBack (Deque in' (h:t)) = (Deque in' t, h)
popBack (Deque in' []) = popBack $ Deque (fst half) (reverse $ snd half)
    where half = pairList in'
--

--
my02 = pushBack (pushBack (pushFront (pushFront empty 1) 3) 2) 6
(my03, a) = popBack my02
(my04, b) = popBack my03
(my05, c) = popBack my04
(my06, d) = popBack my02

main :: IO ()
main = do
        print . show $ my02
        putStrLn " "
        print . show $ (my03, a)
        print . show $ (my04, b)
        print . show $ (my05, c)
        print . show $ (d, my06)

{-
Метод банкира:
    • За каждое добавление и обычное удаление элемента получаем $1.
    • Если при удалении возникает потребность перебросить из одного стека в другой N/2 элементов,
      это будет стоить $N/2, при этом $N мы уже точно заработали.
    Получается, что данный набор операций имеет амортизированную сложность O(1).
Метод физика:
    • Потенциал - это кол-во элементов в очереди (сумма размеров двух стеков). 
    • Так как минимальный размер стека - 0, потенциал никогда не опускается ниже нуля.
    • Добавление в любой из двух стеков увеличивает потенциал на 1.
    • Обычное удаление не изменяет потенциал.
    • Удаление с переброской стека уменьшает потенциал на N и занимает O(N).
    • Перебрасываемое кол-во элементов не может быть больше размера очереди и между
      перебрасываниями не может быть менее N/2 операций, значит потенциал не опускается ниже нуля.
    Получается, что данный набор операций имеет амортизированную сложность O(1).
-}