module Data.List.Origami where

cata :: b -> (a -> b -> b) -> [a] -> b
cata b _ []     = b
cata b f (x:xs) = f x (cata b f xs)

ana :: (b -> Maybe (a,b)) -> b -> [a]
ana g b = case g b of
  Nothing     -> []
  Just (a,b') -> a : ana g b'

hylo :: c -> (b -> c -> c) -> (a -> Maybe (b,a)) -> a -> c
hylo c f g a = case g a of
  Nothing     -> c
  Just (b,a') -> f b (hylo c f g a')

-- hylo c f g p a = cat c f . ana g p a

para :: b -> (a -> ([a],b) -> b) -> [a] -> b
para b _ []     = b
para b f (a:as) = f a (as, para b f as)
