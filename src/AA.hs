module AA (
    AA(..),
    empty,
    isEmpty,
    lookup,
    insert,
    -- incluyendo por desarrollo, borrar luego
    arbol
)
where
import Prelude hiding (lookup)

-- cositas de pruebas
aa :: (a,b) -> a
arbol = Node 0 1 "Uno" (Node 1 2 "Dos" Empty Empty) (Node 1 3 "Tres" Empty Empty)
arbolVacio = Empty
aa (x,a) = x

-- k es el tipo de claves de busqueda en el diccionario string, number, whatever
-- a es el tipo de los valores almacenados en el diccionario
-- lvl nivel del nodo

data AA k a = Empty
            | Node {
                lvl :: Int ,
                key :: k ,
                val :: a ,
                lAA :: AA k a ,
                rAA :: AA k a
            } deriving (Show, Read, Eq, Ord)

data Invariantes = Valido
                | ClaveRepetida
                | NivelesInvalidos
                -- maybe meter tipos???
                deriving (Show)

--- Permite usar foldr en el arbol y triggerea esta funcion
foldAAByValue :: (a -> b -> b) -> b -> AA k a -> b
foldAAByValue f b Empty = b
foldAAByValue f b (Node lv k v l r) = foldAAByValue f (f v (foldAAByValue f b r)) l

foldAAById :: (k -> b -> b) -> b -> AA k a -> b
foldAAById f b Empty = b
foldAAById f b (Node lv k v l r) = foldAAById f (f k (foldAAById f b r)) l

-- Permite usar fmap en el arbol
mapAA :: (a -> b) -> AA k a -> AA k b
mapAA f Empty = Empty
mapAA f (Node lvl key val lAA rAA) = Node lvl key (f val) (mapAA f lAA) (mapAA f rAA)

instance Foldable (AA a) where
    foldr = foldAAByValue

instance Functor (AA a) where
    fmap = mapAA

empty :: AA k a
empty = Empty

isEmpty :: AA k a -> Bool
isEmpty Empty = True
isEmpty _ = False

insert :: (Ord k) => k -> v -> AA k v -> AA k v
insert k v Empty = Node 0 k v Empty Empty
insert k v (Node lvl key val Empty rAA)
  | key == k = Node lvl key val Empty rAA
  | k < key = Node lvl key val (Node (lvl+1) k v Empty Empty) rAA
  | otherwise = Node lvl key val Empty (insert k v rAA)
insert k v (Node lvl key val lAA Empty)
  | key == k = Node lvl key val lAA Empty
  | k > key = Node lvl key val Empty (Node (lvl+1) k v Empty Empty)
  | otherwise = Node lvl key val (insert k v lAA) Empty
insert k v (Node lvl key val lAA rAA)
  | key == k = Node lvl key val lAA rAA
  | k < key = Node lvl key val (insert k v lAA) rAA
  | otherwise = Node lvl key val lAA (insert k v rAA)

-- reduce arbol a bool
encontrarKey :: Eq k => k -> AA k a -> Bool
encontrarKey k Empty = False
encontrarKey k tree = foldAAById (\key acc -> acc || key == k) False tree

lookup :: Eq k => k -> AA k a -> Maybe (AA k a)
lookup k tree = if encontrarKey k tree then Just tree else Nothing

-- Aca podemos llamar a las funciones que hemos hecho para verificar los invariantes que hayamos definido en Invariantes
checkInvariants :: AA k a -> Invariantes
checkInvariants a = Valido