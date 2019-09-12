module Task3_1 where


data WeirdPeanoNumber = Zero | Succ (WeirdPeanoNumber) | Pred (WeirdPeanoNumber)

toInt :: WeirdPeanoNumber -> Integer
toInt Zero = 0
toInt (Succ a) = 1 + (toInt a)
toInt (Pred a) = (toInt a) - 1

toNum :: Integer -> WeirdPeanoNumber
toNum x
  | x > 0 = Succ $ toNum $ x - 1
  | x < 0 = Pred $ toNum $ x + 1
  | otherwise = Zero

todec Zero = Zero
todec (Succ (Pred a)) = todec a
todec (Pred (Succ a)) = todec a
todec (Succ a) = let dec = todec a in 
                 case dec of (Pred b) -> b 
                             _        -> Succ $ dec
todec (Pred a) = let dec = todec a in
                 case dec of (Succ b) -> b
                             _        -> Pred $ dec

instance Show WeirdPeanoNumber where
    show a = show $ toInt a
	
instance Enum WeirdPeanoNumber where
    toEnum = fromIntegral
    fromEnum = fromInteger.toInt

instance Real WeirdPeanoNumber where
    toRational x = toRational $ toInt x							 
							 
instance Num WeirdPeanoNumber where
    (+) Zero a = a
    (+) a Zero = a
    (+) (Pred a) b = (+) a (Pred b)
    (+) (Succ a) b = (+) a (Succ b)

    signum a = case todec a of 
                      Succ _ -> Succ Zero
                      Pred _ -> Pred Zero
                      _      -> Zero

    negate Zero = Zero
    negate (Pred a) = Succ $ negate a
    negate (Succ a) = Pred $ negate a
	
    abs a = if signum a < Zero then negate a else a

    (*) a b  = dcMlt (todec a) (todec b)
		where
			dcMlt Zero _ = Zero
                        dcMlt _ Zero = Zero
			dcMlt (Succ a) (Succ b) = todec $ Succ $ a * b + a + b
                        dcMlt (Pred a) (Pred b) = todec $ Succ $ a * b - a - b
                        dcMlt (Succ a) (Pred b) = todec $ Pred $ a * b - a + b
                        dcMlt (Pred a) (Succ b) = todec $ Pred $ a * b + a - b

    fromInteger = toNum

instance Eq WeirdPeanoNumber where
    (==) a b = decEq (todec a) (todec b)
	where
    		decEq Zero Zero = True
    		decEq Zero _    = False
    		decEq _ Zero    = False
   		decEq (Pred a) (Pred b) = decEq a b
    		decEq (Succ a) (Succ b) = decEq a b
    		decEq _ _               = False
	
instance Ord WeirdPeanoNumber where
    (<=) a b = dcOrd (todec a) (todec b)
	where
		dcOrd (Pred a) (Pred b) = dcOrd a b
		dcOrd (Succ a) (Succ b) = dcOrd a b
		dcOrd Zero a  = case a of
                                    Pred _ -> False
                                    _      -> True
		dcOrd a Zero  = case a of
                                    Succ _ -> False
                                    _      -> True

instance Integral WeirdPeanoNumber where
    toInteger = toInt
    quotRem a b 
	      | signum a == signum b = divCnt (abs b) (Zero, (abs a)) 
          | otherwise  = (\(a,b) -> (todec $ negate a, todec b)) (divCnt (abs b) (Zero, abs a))
                   where divCnt b  res@(quot, rem) | rem >= b = divCnt b (quot + 1, rem - b)
                                                   | otherwise = res
-- Test:												   
-- show $ fst ( quotRem (toNum (-7)) (toNum 8) ) + (toNum 2) + fst ( quotRem (toNum (-7)) (toNum 8) )												   
												   