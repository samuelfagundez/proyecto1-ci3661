module AA where

aa :: (a,b) -> a
aa (x,a) = x

-- k es el tipo de claves de busqueda en el diccionario string, number, whatever
-- a es el tipo de los valores almacenados en el diccionario

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

foldAA :: (a -> b -> b) -> b -> AA k a -> b
foldAA f b Empty = b
foldAA f b (Node lv k v l r) = foldAA f (f v (foldAA f b r)) l

instance Foldable (AA a) where 
    foldr = foldAA

empty :: AA k a
empty = Empty

isEmpty :: AA k a -> Bool
isEmpty Empty = True
isEmpty _ = False

insert :: k -> v -> AA k v -> AA k v
-- Reemplazar Empty del final por una recursion que ubica la posicion del 
-- nodo, creo que hace falta otra funcion para eso
insert k v a = if isEmpty a then Node 0 k v Empty Empty else Empty

lookup' :: Eq k => k -> AA k v -> Bool
lookup' k Empty = False
lookup' k (Node lvl key val Empty Empty) = key == k
lookup' k (Node lvl key val lAA Empty) = if key == k then key == k else lookup' k lAA
lookup' k (Node lvl key val Empty rAA) = if key == k then key == k else lookup' k rAA
-- Esto esta malo porque solo va a buscar en el arbol izquierdo
lookup' k (Node lvl key val lAA rAA) = if key == k then key == k else lookup' k lAA

checkInvariants :: AA k a -> AA k a
checkInvariants a = Empty