{--------------------------------------------------------------------------------

LAB 2: Trees

--------------------------------------------------------------------------------}

-- We're going to use trees to represent maps from keys to values.  We will
-- assume (and assure) that these trees maintain the binary search tree
-- invariant; that is, for a node with key `k`, all nodes in the left subtree
-- have keys less than `k`, and all nodes in the right subtree have keys greater
-- than `k`.
data Tree k v = Leaf | Node k v (Tree k v) (Tree k v)

-- Write a function that returns the height (i.e., length of the longest path
-- from the root to a leaf) of a given tree.
height :: Tree k v -> Int
height = error "unimplemented"

-- Write a function that returns the size (i.e., the total number of nodes) of a
-- given tree.
size :: Tree k v -> Int
size = error "unimplemented"

-- Write a function that finds the value associated with a given key.  Return
-- `Nothing` if the key is not in the tree.
find :: Ord k => k -> Tree k v -> Maybe v
find = error "unimplemented"

-- Write a function that inserts a key-value pair into a given tree.  Make sure
-- you maintain the BST variant.  If the key is already in the given tree,
-- update the stored value.
insert :: Ord k => k -> v -> Tree k v -> Tree k v
insert = error "unimplemented"

-- Write a function that, given some function `f`, applies `f` to each value
-- stored in the tree.
treeMap :: Ord k => (v -> u) -> Tree k v -> Tree k u
treeMap = error "unimplemented"

-- Write a function that, given two tree t1 and t2, returns a new tree based on
-- t2, but with t1 grafted into the tree.  Maintain the BST invairant.
graft :: Ord k => Tree k v -> Tree k v -> Tree k v
graft = error "unimplemented"

-- Write a function that deletes the entry with a given key from the tree.  If
-- the key is not present in the tree, simply return the input tree.  Make sure
-- to maintain the BST invariant.
delete :: Ord k => k -> Tree k v -> Tree k v
delete = error "unimplemented"

-- Write a function that returns the keys and values appearing in the tree, in
-- sorted order.
pairs :: Tree k v -> [(k, v)]
pairs = error "unimplemented"

-- Write a function that, given some predicate function `p`, returns a tree in
-- which all values satisfy `p`.
filterTree :: Ord k => (k -> Bool) -> Tree k v -> Tree k v
filterTree = error "unimplemented"
