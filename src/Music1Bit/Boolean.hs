module Music1Bit.Boolean
  (
    Boolean,
    (&), (\&), (#), (!),
  )
  where

class Boolean a where
  (&) :: a -> a -> a
  (\&):: a -> a -> a
  (#) :: a -> a -> a
  (!) :: a -> a


instance Boolean Char where
  (&) 'i' 'i' = 'i' 
  (&)  _   _  = 'o'

  (\&) 'o' 'o' = 'o'
  (\&)  _   _  = 'i'

  (#) x y | x == y  = 'o'
          | otherwise = 'i'

  (!) 'i' = 'o'
  (!)  _  = 'i'

instance  Boolean a => Boolean [a] where
  (&)  xs ys = zipWith (&)  xs ys
  (\&) xs ys = zipWith (\&) xs ys
  (#)  xs ys = zipWith (#)  xs ys
  (!) (x:xs) = (!)x : map (!) xs

