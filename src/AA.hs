module AA where

aa :: (a,b) -> a
aa (x,a) = x

-- k es el tipo de claves de busqueda en el diccionario string, number, whatever
-- a es el tipo de los valores almacenados en el diccionario

-- video 3 2:01 functor

data AA k a = Empty
            -- | Invalido porque no respeta la secuencia de niveles
            | Node {
                lvl :: Int ,
                key :: k ,
                val :: a ,
                lAA :: AA k a ,
                rAA :: AA k a
            } deriving (Show, Read, Eq)

arbol = Node 0 1 "Uno" (Node 1 2 "Dos" Empty Empty) (Node 1 3 "Tres" Empty Empty)
arbolVacio = Empty

foldAAByValue :: (a -> b -> b) -> b -> AA k a -> b
foldAAByValue f b Empty = b
foldAAByValue f b (Node lv k v l r) = foldAAByValue f (f v (foldAAByValue f b r)) l

foldAAById :: (k -> b -> b) -> b -> AA k a -> b
foldAAById f b Empty = b
foldAAById f b (Node lv k v l r) = foldAAById f (f k (foldAAById f b r)) l

instance Foldable (AA a) where 
    foldr = foldAAByValue

empty :: AA k a
empty = Empty

isEmpty :: AA k a -> Bool
isEmpty Empty = True
isEmpty _ = False

insert :: k -> v -> AA k v -> AA k v
-- Reemplazar Empty del final por una recursion que ubica la posicion del 
-- nodo, creo que hace falta otra funcion para eso
insert k v a = if isEmpty a then Node 0 k v Empty Empty else Empty


-- TODO: Falta poner un maybe para devolver SiEsta | NoEsta en vez de True | False
lookup' :: Eq k => k -> AA k a -> Bool
lookup' k Empty = False
lookup' k tree = foldAAById (\key acc -> acc || key == k) False tree

-- Esta es la estructura para buscar y devolver el maybe
-- foldAAById :: (k -> b -> b) -> b -> AA k a -> b
-- foldAAById f b Empty = b
-- foldAAById f b (Node lv k v l r) = foldAAById f (f k (foldAAById f b r)) l

checkInvariants :: AA k a -> AA k a
checkInvariants a = Empty