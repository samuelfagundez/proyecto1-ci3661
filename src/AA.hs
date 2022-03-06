module AA where

aa :: (a,b) -> a
aa (x,a) = x

-- k es el tipo de claves de busqueda en el diccionario string, number, whatever
-- a es el tipo de los valores almacenados en el diccionario

data AA k a = Empty
            | Node {
                lvl :: Int ,
                key :: k ,
                val :: a ,
                lAA :: AA k a ,
                rAA :: AA k a
            } deriving Show

arbol = Node 0 1 "Uno" (Node 1 2 "Dos" Empty Empty) (Node 1 3 "Tres" Empty Empty)

foldAA :: (a -> b -> b) -> b -> AA k a -> b
foldAA f b Empty = b
foldAA f b (Node lv k v l r) = foldAA f (f v (foldAA f b r)) l

instance Foldable (AA a) where 
    foldr = foldAA