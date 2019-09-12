module Task4_2 where
import Control.Monad (ap, liftM)

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where fmap = liftM

instance Applicative FourOf where 
  pure = return 
  (<*>) = ap
	
instance Monad FourOf where
  return x = FourOf x x x x
  (FourOf x1 x2 x3 x4) >>= f = FourOf y1 y2 y3 y4 where
    FourOf y1 _ _ _ = f x1
    FourOf _ y2 _ _ = f x2
    FourOf _ _ y3 _ = f x3
    FourOf _ _ _ y4 = f x4