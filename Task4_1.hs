module Task4_1 where

-- Интересным (но почти бесполезным) примером монады является обычная функция, 
--если типовой аргумент присутствует в её возвращаемом значении. 
--В файле Task4_1.hs приведён тип данных, построенный на основе функции из String в a. 
--Реализуйте для него классы Functor, Applicative и Monad.

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`.

data FunMonad a = FunMonad { fun :: String -> a }

-- Реализация класса `Functor` для типа `FunMonad`.
instance Functor FunMonad where
    fmap f (FunMonad f') = FunMonad (f . f')
-- Реализация класса `Applicative` для типа `FunMonad`.	
instance Applicative FunMonad where
    pure a = FunMonad (\x -> a)
    (<*>) (FunMonad fa) (FunMonad fb) = FunMonad (\x -> fa x $ fb x)
	
-- Реализация класса `Monad` для типа `FunMonad`.		
instance Monad FunMonad where
    return = pure
    (>>=) fa fb = FunMonad (\x -> fun (fb $ (fun fa) x) x)	