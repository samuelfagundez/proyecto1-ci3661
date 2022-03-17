module AA (
    AA(..),
    empty,
    isEmpty,
    lookup,
    insert,
    insert',
    encontrarKey,
    mapAA,
    foldAAById,
    -- incluyendo por desarrollo, borrar luego
    arbol,
    checkInvariants,
    areLevelsValid
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
                | NivelDeHojaInvalido
                | NivelDeHijoIzquierdoRespectoASuPadreInvalido
                | NivelDeHijoDerechoRespectoASuPadreInvalido
                | NivelDeNIetoDerechoInvalido
                | NumeroDeHijosInvalido
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

skew :: AA k v -> AA k v
skew Empty = Empty
skew (Node lvl key val Empty rAA) = Node lvl key val Empty rAA
skew (Node lvl key val (Node lvlL keyL valL lAAL rAAL) rAA)
  | lvl == lvlL = Node lvlL keyL valL lAAL (Node lvl key val rAAL rAA)
  | otherwise = Node lvl key val (Node lvlL keyL valL lAAL rAAL) rAA

split :: AA k v -> AA k v
split Empty = Empty
split (Node lvl key val lAA Empty) = Node lvl key val lAA Empty
split (Node lvl key val lAA (Node lvlR keyR valR lAAR Empty)) = Node lvl key val lAA (Node lvlR keyR valR lAAR Empty)
split (Node lvl key val lAA (Node lvlR keyR valR lAAR (Node lvlR2 keyR2 valR2 lAAR2 rAAR2)))
  | lvl == lvlR2 = Node (lvlR+1) keyR valR (Node lvl key val lAA lAAR) (Node lvlR2 keyR2 valR2 lAAR2 rAAR2)
  | otherwise = Node lvl key val lAA (Node lvlR keyR valR lAAR (Node lvlR2 keyR2 valR2 lAAR2 rAAR2))

insert :: (Ord k) => k -> v -> AA k v -> AA k v
insert k v Empty = Node 1 k v Empty Empty
insert k v (Node lvl key val lAA rAA)
  | k < key = split (skew (Node lvl key val (insert k v lAA) rAA))
  | k > key = split(skew (Node lvl key val lAA (insert k v rAA)))
  | otherwise = Node lvl key val lAA rAA

insert' :: (Ord k) => k -> v -> AA k v -> AA k v
insert' k v Empty = Node 0 k v Empty Empty
insert' k v (Node lvl key val Empty rAA)
  | key == k = Node lvl key val Empty rAA
  | k < key = Node lvl key val (Node (lvl+1) k v Empty Empty) rAA
  | otherwise = Node lvl key val Empty (insert' k v rAA)
insert' k v (Node lvl key val lAA Empty)
  | key == k = Node lvl key val lAA Empty
  | k > key = Node lvl key val Empty (Node (lvl+1) k v Empty Empty)
  | otherwise = Node lvl key val (insert' k v lAA) Empty
insert' k v (Node lvl key val lAA rAA)
  | key == k = Node lvl key val lAA rAA
  | k < key = Node lvl key val (insert' k v lAA) rAA
  | otherwise = Node lvl key val lAA (insert' k v rAA)

-- reduce arbol a bool
encontrarKey :: Eq k => k -> AA k a -> Bool
encontrarKey k Empty = False
encontrarKey k tree = foldAAById (\key acc -> acc || key == k) False tree

lookup :: Eq k => k -> AA k a -> Maybe (AA k a)
lookup k tree = if encontrarKey k tree then Just tree else Nothing

areLevelsValid :: AA k a -> Bool
areLevelsValid Empty = True
areLevelsValid (Node lv k v Empty Empty) = True
areLevelsValid (Node lv k v Empty (Node lvR kR vR lR rR)) = lv == lvR - 1 && areLevelsValid (Node lvR kR vR lR rR)
areLevelsValid (Node lv k v (Node lvL kL vL lL rL) Empty) =  lv == lvL - 1 && areLevelsValid (Node lvL kL vL lL rL)
areLevelsValid (Node lv k v (Node lvL kL vL lL rL) (Node lvR kR vR lR rR)) = lv == lvL-1 && lv == lvR-1 && areLevelsValid (Node lvL kL vL lL rL) && areLevelsValid (Node lvR kR vR lR rR)

-- Invariantes
    -- 1. The level of every leaf node is one.
    -- 2. The level of every left child is exactly one less than that of its parent.
    -- 3. The level of every right child is equal to or one less than that of its parent.
    -- 4. The level of every right grandchild is strictly less than that of its grandparent.
    -- 5. Every node of level greater than one has two children.

esInvariante1Valido :: AA k a -> Bool
esInvariante1Valido (Node lvl key value Empty Empty) = lvl == 1
esInvariante1Valido _ = True

esInvariante2Valido :: AA k a -> Bool
esInvariante2Valido Empty = True
esInvariante2Valido (Node lvl key value Empty _) = True
esInvariante2Valido (Node lvl key value (Node lvlL keyL valueL lAAL rAAL) rAA) = (lvl-1) == lvlL && (esInvariante2Valido (Node lvlL keyL valueL lAAL rAAL) && esInvariante2Valido rAA)

esInvariante3Valido :: AA k a -> Bool
esInvariante3Valido Empty = True
esInvariante3Valido (Node lvl key value _ Empty) = True
esInvariante3Valido (Node lvl key value lAA (Node lvlR keyR valueR lAAR rAAR)) = ((lvl-1) == lvlR || lvl == lvlR) && (esInvariante3Valido lAA && esInvariante3Valido (Node lvlR keyR valueR lAAR rAAR))

esInvariante4Valido :: AA k a -> Bool
esInvariante4Valido _ = True

esInvariante5Valido :: AA k a -> Bool
esInvariante5Valido Empty = True
esInvariante5Valido (Node lvl key value Empty Empty) = True
esInvariante5Valido (Node lvl key value Empty rAA) = lvl <= 1 && esInvariante5Valido rAA
esInvariante5Valido (Node lvl key value lAA Empty) =  lvl <= 1 && esInvariante5Valido lAA
esInvariante5Valido (Node lvl key value lAA rAA) = lvl >= 2 && (esInvariante5Valido lAA && esInvariante5Valido rAA)


-- Aca podemos llamar a las funciones que hemos hecho para verificar los invariantes que hayamos definido en Invariantes
checkInvariants :: (Eq k) => AA k a -> Invariantes
-- Primero revisa las llaves
checkInvariants tree
  | not (esInvariante1Valido tree) = NivelDeHojaInvalido
  | not (esInvariante2Valido tree) = NivelDeHijoIzquierdoRespectoASuPadreInvalido
  | not (esInvariante3Valido tree) = NivelDeHijoDerechoRespectoASuPadreInvalido
  | not (esInvariante4Valido tree) = NivelDeNIetoDerechoInvalido
  | not (esInvariante5Valido tree) = NumeroDeHijosInvalido
  | otherwise = Valido