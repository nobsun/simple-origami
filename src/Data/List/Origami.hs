module Data.List.Origami where

cata :: b -> (a -> b -> b) -> [a] -> b
cata b _ []     = b
cata b f (x:xs) = f x (cata b f xs)

ana :: (b -> (a,b)) -> (b -> Bool) -> b -> [a]
ana g p b | p b       = []
          | otherwise = a : ana g p b'
                        where
                          (a,b') = g b

hylo :: c -> (b -> c -> c) -> (a -> (b,a)) -> (a -> Bool) -> a -> c
hylo c f g p a | p a       = c
               | otherwise = f b (hylo c f g p a')
                             where
                               (b,a') = g a

-- hylo c f g p a = cat c f . ana g p a

para :: b -> (a -> ([a],b) -> b) -> [a] -> b
para b f []     = b
para b f (a:as) = f a (as, para b f as)
