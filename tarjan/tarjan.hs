module Tarjan where

import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.List
import Data.Tuple
-- import Data.Graph hiding (Table, Graph, vertices, Edge, Bounds, graph, buildG, Vertex, edges)

type Vertex = Char
type Table a = Array Vertex a
type Graph = Table [Vertex]
type Edge = (Vertex,Vertex)
type Bounds = (Vertex,Vertex)

vertices :: Graph -> [Vertex]
vertices = indices

edges :: Graph -> [Edge]
edges g = [ (v,w) | v <- vertices g, w <- g!v ]

mapT :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [(v, f v (t!v)) | v <- indices t]

outdegree :: Graph -> Table Int
outdegree = mapT (const length)

indegree :: Graph -> Table Int
indegree = outdegree . transposeG 

buildG :: Bounds -> [Edge] -> Graph
buildG bnds = accumArray (flip (:)) [] bnds 

transposeG :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE :: Graph -> [Edge]
reverseE = (map swap) . edges

graph :: Graph
graph = buildG ('a', 'j')
          [('a', 'j'), ('a', 'g'), ('b', 'i'),
           ('b', 'a'), ('c', 'h'), ('c', 'e'),
           ('e', 'j'), ('e', 'h'), ('e', 'd'),
           ('f', 'i'), ('g', 'f'), ('g', 'b')]   

--------------------------
--  Depth-first search  --
--------------------------

-- specifications
data Tree a   = Node a (Forest a)
type Forest a = [Tree a]

dfs :: Graph -> [Vertex] -> Forest Vertex
dfs g vs = prune (bounds g) (map (generate g) vs)

dff :: Graph -> Forest Vertex
dff g = dfs g (vertices g)

preorder :: Tree a -> [a]
preorder (Node a ts) = a:preorderF ts

preorderF :: Forest a -> [a]
preorderF = concatMap preorder

preOrd :: Graph -> [Vertex]
preOrd = preorderF . dff

tabulate :: Bounds -> [Vertex] -> Table Int
tabulate bnds vs = array bnds (zip vs [1..])

preArr :: Bounds -> Forest Vertex -> Table Int
preArr bnds = tabulate bnds . preorderF

--------------------------
--  Topological sorting --
--------------------------

postorder :: Tree a -> [a]
postorder (Node a ts) = postorderF ts ++ [a]

postorderF :: Forest a -> [a]
postorderF ts = concat (map postorder ts)

postOrd :: Graph -> [Vertex]
postOrd g = postorderF (dff g)

topSort :: Graph -> [Vertex]
topSort g = reverse (postOrd g)

components :: Graph -> Forest Vertex
components g = dff (undirected g)

undirected :: Graph -> Graph
undirected g = buildG (bounds g) (edges g ++ reverseE g)

scc :: Graph -> Forest Vertex
scc g = dfs (transposeG g) (reverse (postOrd g))

scc' :: Graph -> Forest Vertex
scc' g = dfs g (reverse (postOrd (transposeG g)))

--------------------------------
-- Depth-first implementation --
--------------------------------

-- Genarating
generate :: Graph -> Vertex -> Tree Vertex
generate g v = Node v (map (generate g) (g!v))

-- Pruning

-- Vertex sets
type Set s = STArray s Vertex Bool

mkEmpty :: Bounds -> ST s (Set s)
mkEmpty bnds = newArray bnds False

contains :: Set s -> Vertex -> ST s Bool
contains m v = readArray m v

include :: Set s -> Vertex -> ST s ()
include m v = writeArray m v True

prune :: Bounds -> Forest Vertex -> Forest Vertex
prune bnds ts = runST (mkEmpty bnds >>= \m -> chop m ts)

chop :: Set s -> Forest Vertex -> ST s (Forest Vertex)
chop m [] = return []
chop m (Node v ts : us) = do
    visited <- contains m v
    if visited then
      chop m us
    else do
      include m v
      as <- chop m ts
      bs <- chop m us
      return ((Node v as) : bs)

---------------------
-- More Algorithms --
---------------------

-- Classifying edges

reachable :: Graph -> Vertex -> [Vertex]
reachable g v = preorderF (dfs g [v])

path :: Graph -> Vertex -> Vertex -> Bool
path g v w = w `elem` (reachable g v)

-- Biconnected components
tree :: Bounds -> Forest Vertex -> Graph
tree bnds ts = buildG bnds (concat (map flat ts))
 where
  flat (Node v ts) = [(v,w) | Node w us <- ts] ++ concat (map flat ts)

back :: Graph -> Table Int -> Graph
back g post = mapT select g
 where
  select v ws = [ w | w <- ws, post!v < post!w ]

cross :: Graph -> Table Int -> Table Int -> Graph
cross g pre post = mapT select g
 where
  select v ws = [ w | w <- ws, post!v > post!w, pre!v > pre!w ]

forward :: Graph -> Graph -> Table Int -> Graph
forward g tree pre = mapT select g
 where
  select v ws = [ w | w <-ws, pre!v < pre!w ] \\ tree!v

bcc :: Graph -> Forest [Vertex]
bcc g = (concat . map bicomps . map (label g dnum)) forest
 where
  forest = dff g
  dnum = preArr (bounds g) forest

label :: Graph -> Table Int -> Tree Vertex -> Tree (Vertex, Int, Int)
label g dnum (Node v ts) = Node (v, dnum!v, lv) us
 where
  us = map (label g dnum) ts
  lv = minimum ([dnum!v] ++ [dnum!w | w <- g!v] 
               ++ [lu | Node (u, dw, lu) xs <- us])

bicomps :: Tree (Vertex, Int, Int) -> Forest [Vertex]
bicomps (Node (v, dv, lv) ts)
  = [ Node (v:vs) us | (l, Node vs us) <- map collect ts ]

collect :: Tree (Vertex, Int, Int) -> (Int, Tree [Vertex])
collect (Node (v,dv,lv) ts) = (lv, Node (v:vs) cs)
 where
  collected = map collect ts
  vs = concat [ ws | (lw, Node ws us) <- collected, lw < dv]
  cs = concat [ if lw < dv then us else [Node (v:ws) us]
                   | (lw, Node ws us) <- collected]
