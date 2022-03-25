module AA (
    AA(..),
    empty,
    isEmpty,
    lookup,
    insert,
    mapAA,
    foldAAById,
)
where
import Prelude hiding (lookup)

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
                | NivelDeNietoDerechoInvalido
                | NumeroDeHijosInvalido
                deriving (Show)

foldAAByValue :: (a -> b -> b) -> b -> AA k a -> b
foldAAByValue f b Empty = b
foldAAByValue f b (Node lv k v l r) = foldAAByValue f (f v (foldAAByValue f b r)) l

foldAAById :: (k -> b -> b) -> b -> AA k a -> b
foldAAById f b Empty = b
foldAAById f b (Node lv k v l r) = foldAAById f (f k (foldAAById f b r)) l

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

-- reduce arbol a bool
findKey :: Eq k => k -> AA k a -> Bool
findKey k Empty = False
findKey k tree = foldAAById (\key acc -> acc || key == k) False tree

lookup :: Eq k => k -> AA k a -> Maybe (AA k a)
lookup k tree = if findKey k tree then Just tree else Nothing

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

isValidInvariant1 :: AA k a -> Bool
isValidInvariant1 (Node lvl key value Empty Empty) = lvl == 1
isValidInvariant1 _ = True

isValidInvariant2 :: AA k a -> Bool
isValidInvariant2 Empty = True
isValidInvariant2 (Node _ _ _ Empty _) = True
isValidInvariant2 (Node lvl _ _ (Node lvlL keyL valueL lAAL rAAL) rAA) = (lvl-1) == lvlL && (isValidInvariant2 (Node lvlL keyL valueL lAAL rAAL) && isValidInvariant2 rAA)

isValidInvariant3 :: AA k a -> Bool
isValidInvariant3 Empty = True
isValidInvariant3 (Node _ _ _ _ Empty) = True
isValidInvariant3 (Node lvl _ _ lAA (Node lvlR keyR valueR lAAR rAAR)) = ((lvl-1) == lvlR || lvl == lvlR) && (isValidInvariant3 lAA && isValidInvariant3 (Node lvlR keyR valueR lAAR rAAR))

isValidInvariant4 :: AA k a -> Bool
isValidInvariant4 Empty = True
isValidInvariant4 (Node _ _ _ _ Empty) = True
isValidInvariant4 (Node _ _ _ _ (Node _ _ _ _ Empty)) = True
isValidInvariant4 (Node lvl _ _ lAA ((Node _ _ _ _ ((Node lvlR2 keyR2 valueR2 lAAR2 rAAR2))))) = lvl > lvlR2 && (isValidInvariant4 lAA && isValidInvariant4 (Node lvlR2 keyR2 valueR2 lAAR2 rAAR2))

isValidInvariant5 :: AA k a -> Bool
isValidInvariant5 Empty = True
isValidInvariant5 (Node _ _ _ Empty Empty) = True
isValidInvariant5 (Node lvl _ _ Empty rAA) = lvl <= 1 && isValidInvariant5 rAA
isValidInvariant5 (Node lvl _ _ lAA Empty) =  lvl <= 1 && isValidInvariant5 lAA
isValidInvariant5 (Node lvl _ value lAA rAA) = lvl >= 2 && (isValidInvariant5 lAA && isValidInvariant5 rAA)


checkInvariants :: (Eq k) => AA k a -> Invariantes
checkInvariants tree
  | not (isValidInvariant1 tree) = NivelDeHojaInvalido
  | not (isValidInvariant2 tree) = NivelDeHijoIzquierdoRespectoASuPadreInvalido
  | not (isValidInvariant3 tree) = NivelDeHijoDerechoRespectoASuPadreInvalido
  | not (isValidInvariant4 tree) = NivelDeNietoDerechoInvalido
  | not (isValidInvariant5 tree) = NumeroDeHijosInvalido
  | otherwise = Valido