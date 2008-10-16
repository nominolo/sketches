{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies  #-}

class R'HasX r a | r -> a where
  r'x :: r -> a; r'x'set :: r -> a -> r
class R'HasY r a | r -> a where
  r'y :: r -> a; r'y'set :: r -> a -> r

-- { x,y :: Int }

newtype R'P = R'P (Int,Int)

instance R'HasX R'P Int where r'x (R'P (x,_)) = x;  r'x'set (R'P (_,y)) x = R'P (x,y)
instance R'HasY R'P Int where r'y (R'P (_,y)) = y;  r'y'set (R'P (x,_)) y = R'P (x,y)
{-
 :t f2
 f2 :: (R'HasY r a, R'HasX r a) => r -> r
===
 f2 :: (r <: { x, y :: a }) => r -> r 
 f2 pt@{x, y, ..} = pt { x = y, y = x }
 f2 pt = pt { x = 'y pt, y = 'x pt }
-}
f2 pt = let x = r'x pt; y = r'y pt in
        r'y'set (r'x'set pt y) x