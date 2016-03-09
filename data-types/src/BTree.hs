{-# LANGUAGE GADTs #-}
module BTree where

import Data.List (foldl')
import Text.PrettyPrint.HughesPJClass

treesize = 4

data PtrNode a = PtrNode a (BTree a) deriving Show
data EmptyNode a = EmptyNode (BTree a) deriving Show

instance Pretty a => Pretty (PtrNode a) where
  pPrint (PtrNode el tree) = vcat [ nest 3 (pPrint tree)
                                  , text "(" <> pPrint el <> text ")"
                                  , text ""]

instance Pretty a => Pretty (EmptyNode a) where
  pPrint (EmptyNode tree) = nest 3 (pPrint tree)

-- data LeafNode a = LeafNode a
data BTree a where
  --Empty :: BTree a
  Leaf :: Ord a => [a] -> BTree a
  Node :: Ord a => [PtrNode a] -> EmptyNode a -> BTree a

instance Show a => Show (BTree a) where
  show (Node ptrs empt) = unwords ["Node", show ptrs, show empt]
  show (Leaf elms) = unwords ["Leaf", show elms]

instance Pretty a => Pretty (BTree a) where
  pPrint (Leaf elms) = fsep . punctuate comma $ map pPrint elms
  pPrint (Node ptrs empt) = vcat [ vcat $ map pPrint ptrs
                                     , pPrint empt]

instance Foldable PtrNode where
  foldMap f (PtrNode x btree) = foldMap f btree `mappend` f x

instance Foldable EmptyNode where
  foldMap f (EmptyNode btree) = foldMap f btree

instance Foldable BTree where
  foldMap f (Leaf x) = foldMap f x
  foldMap f (Node x1 x2) = foldMap (foldMap f) x1 `mappend` foldMap f x2
  --foldMap f (Node x1 x2) = mconcat (map (foldMap f) x1) `mappend` foldMap f x2

nodes :: BTree a -> [PtrNode a]
nodes (Node ptrs _) = ptrs

endTree :: BTree a -> BTree a
endTree (Node _ (EmptyNode lst)) = lst

find :: a -> BTree a -> Bool
find v (Leaf tr) = v `elem` tr
find v tr@(Node tr1 (EmptyNode tr2))
  | cPos == size  = find v tr2
  | otherwise     = (curValues !! cPos) == v || find v pTree 
  where
    curValues = nodeValues tr
    size = length curValues 
    cPos = posIndex v tr
    (PtrNode _ pTree) = tr1 !! cPos

insert :: a -> BTree a -> BTree a
insert v = snd . insert' v

insert' :: a -> BTree a -> (Bool, BTree a)
insert' v tr@(Node pNodes enode) = newSub
  where
    position = posIndex v tr
    (newNode, newSubTree) = insert' v $ subtrees tr !! position 
    (Node ptNs ed@(EmptyNode lf)) = newSubTree -- trust laziness as newSubTree may be Leaf
    [PtrNode nv ntree] = ptNs -- it should always be of size 1
    newSub
      | not newNode = (False, updateSubtree position newSubTree tr)
      | length pNodes < treesize = -- then new node to be added
          (False, addSubTree position newSubTree tr)
      | otherwise =
          let temp = addSubTree position newSubTree tr
              (xs, y:ys) = splitAt (treesize `div` 2) . nodes $ temp
              (PtrNode yv ytree) = y
              left  = Node xs (EmptyNode ytree)
              right = Node ys (EmptyNode $ endTree temp)
          in (True, Node [PtrNode yv left] (EmptyNode right))

insert' v tr@(Leaf elems) = newSub
  where
    position = posIndex v tr
    newList  = insertAt position [v] elems
    newSub
      | length newList <= treesize =
          (False, Leaf newList)
      | otherwise =
         let (xs, y:ys) = splitAt (treesize `div` 2) newList
         in (True, Node [PtrNode y (Leaf xs)] (EmptyNode (Leaf ys)))

addSubTree :: Int -> BTree a -> BTree a -> BTree a
addSubTree indx st tr
  | indx < length (nodes tr) = insertSubTree indx st tr
  | otherwise                = appendSubTree st tr

appendSubTree :: BTree a -> BTree a -> BTree a
appendSubTree (Node sp sl) (Node ptrs _) = Node (ptrs ++ sp) sl
    
insertSubTree :: Int -> BTree a -> BTree a -> BTree a
insertSubTree indx (Node sp (EmptyNode sl)) (Node ptrs lst) = Node newPtrs lst
  where
    ptrs'   = updateAt indx (\(PtrNode a _) -> PtrNode a sl) ptrs 
    newPtrs = insertAt indx sp ptrs'


remove :: BTree a -> a -> BTree a
remove = undefined

updateSubtree :: Int -> BTree a -> BTree a -> BTree a
updateSubtree _ _     (Leaf _) = undefined
updateSubtree indx newST tr@(Node ptrs lst)
  | indx < size  = Node (updateAt indx (\(PtrNode a _) -> PtrNode a newST) ptrs) lst
  | indx == size = Node ptrs (EmptyNode newST)
  | otherwise = tr
  where
    size = length ptrs

posIndex :: Ord a => a -> BTree a -> Int
posIndex v = length . takeWhile (<v) . nodeValues

insertAt :: Int -> [a] -> [a] -> [a]
insertAt indx values xs = let (ys, zs) = splitAt indx xs
                          in ys ++ values ++ zs

replaceAt :: Int -> a -> [a] -> [a]
replaceAt indx value xs
  | indx >= length xs = undefined --xs ++ [value]
  | otherwise = let (ys, z:zs) = splitAt indx xs
                in ys ++ [value] ++ zs

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt indx f xs 
  | indx >= length xs = undefined
  | otherwise         = let (ys, z:zs) = splitAt indx xs
                        in ys ++ [f z] ++ zs

nodeValues :: BTree a -> [a]
nodeValues (Leaf x) = x
nodeValues (Node x1 _) = map (\(PtrNode a _) -> a) x1

subtrees :: BTree a -> [BTree a]
subtrees (Leaf tr) = []
subtrees (Node trs (EmptyNode tr)) = map (\(PtrNode _ ts) -> ts) trs  ++ [tr]

emptyTree :: BTree Int
emptyTree = Leaf []

fromList :: Ord a => [a] -> BTree a
fromList = foldl' (\tree v -> insert v tree) (Leaf [])

depth :: BTree a -> Int
depth = go 0
  where
    go acc (Leaf xs) = acc + 1
    go acc (Node ptrs _) = go (acc+1) tree
      where
        (PtrNode _ tree) = head ptrs

sampleTree :: BTree Integer
sampleTree =
  Node [ PtrNode 15 (Node [ PtrNode 5 (Leaf [1,2,3,4])
                          , PtrNode 10 (Leaf [6,7,8,9])
                          ]
                          (EmptyNode (Leaf [11,12,13,14]))
                    )
       ]
       (EmptyNode (Node [ PtrNode 20 (Leaf [16,17,18,19])
                        ]
                        (EmptyNode (Leaf [21,22,23,24]))
                  )
       )

{- initial b-tree implementation -}
data BTreeOld a where
  NodeOld :: Ord a => [a] -> [BTreeOld a] -> BTreeOld a

instance Show a => Show (BTreeOld a) where
  show (NodeOld el trees) = unwords ["NodeOld", show el, show trees]

-- instance Functor BTreeOld where
--  fmap f (Node xs children) = Node (map f xs) (map (fmap f) children)

findOld :: a -> BTreeOld a -> Bool
findOld x (NodeOld [] _) = False
findOld x (NodeOld xs trees)= case position' x xs of
  Nothing  -> True -- found in non-empty list
  Just pnt -> (pnt < length trees) && findOld x (trees !! pnt)


position' :: Ord a => a -> [a] -> Maybe Int
position' _ [] = Nothing
position' x ys = go 0 ys
  where
    go indx [] = Just indx
    go indx (z:zs)
      | x < z     = Just indx
      | x == z    = Nothing
      | otherwise = go (indx+1) zs

sampleTreeOld :: BTreeOld Int
sampleTreeOld =
  NodeOld [15] [NodeOld [5,10] [NodeOld [1,2,3,4] []
                         ,NodeOld [6,7,8,9] []
                         ,NodeOld [11,12,13,14] []
                         ]
            ,NodeOld [20]   [NodeOld [16,17,18,19] []
                         ,NodeOld [21,22,23,24] []
                         ]
            ]

