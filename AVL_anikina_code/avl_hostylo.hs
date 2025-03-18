data AVLTree a = Empty
               | Node a (AVLTree a) (AVLTree a) Int
               deriving (Show, Eq)

-- Utility function to get the height of a node
height :: AVLTree a -> Int
height Empty = 0
height (Node _ _ _ h) = h

-- Utility function to calculate the balance factor of a node
balanceFactor :: AVLTree a -> Int
balanceFactor Empty = 0
balanceFactor (Node _ left right _) = height left - height right

-- Utility function to update the height of a node
updateHeight :: AVLTree a -> AVLTree a
updateHeight Empty = Empty
updateHeight (Node x left right _) = Node x left right newHeight
  where newHeight = 1 + max (height left) (height right)

-- Right rotation
rotateRight :: AVLTree a -> AVLTree a
rotateRight Empty = Empty
rotateRight (Node x (Node y ly ry _) r _) =
    Node y ly (Node x ry r updatedHeight) updatedHeight
  where updatedHeight = 1 + max (height ry) (height r)

-- Left rotation
rotateLeft :: AVLTree a -> AVLTree a
rotateLeft Empty = Empty
rotateLeft (Node x l (Node y ly ry _) _) =
    Node y (Node x l ly updatedHeight) ry updatedHeight
  where updatedHeight = 1 + max (height l) (height ly)

-- Balancing a node
balance :: AVLTree a -> AVLTree a
balance node@(Node x left right _)
  | bFactor > 1 && balanceFactor left >= 0 = rotateRight node
  | bFactor > 1 = rotateRight (Node x (rotateLeft left) right updatedHeight)
  | bFactor < -1 && balanceFactor right <= 0 = rotateLeft node
  | bFactor < -1 = rotateLeft (Node x left (rotateRight right) updatedHeight)
  | otherwise = updateHeight node
  where bFactor = balanceFactor node
        updatedHeight = 1 + max (height left) (height right)
balance Empty = Empty

-- Insertion
insert :: (Ord a) => a -> AVLTree a -> AVLTree a
insert x Empty = Node x Empty Empty 1
insert x (Node y left right h)
  | x < y     = balance $ Node y (insert x left) right h
  | x > y     = balance $ Node y left (insert x right) h
  | otherwise = Node x left right h  -- No duplicates

-- Deletion (simplified for brevity)
delete :: (Ord a) => a -> AVLTree a -> AVLTree a
delete _ Empty = Empty
delete x (Node y left right _)
  | x < y     = balance $ Node y (delete x left) right updatedHeight
  | x > y     = balance $ Node y left (delete x right) updatedHeight
  | otherwise = deleteNode (Node y left right updatedHeight)
  where updatedHeight = 1 + max (height left) (height right)

deleteNode :: AVLTree a -> AVLTree a
deleteNode (Node _ Empty right _) = right
deleteNode (Node _ left Empty _) = left
deleteNode (Node _ left right _) =
  let (minRight, newRight) = deleteMin right
  in balance $ Node minRight left newRight (1 + max (height left) (height newRight))

deleteMin :: AVLTree a -> (a, AVLTree a)
deleteMin (Node x Empty right _) = (x, right)
deleteMin (Node x left right _) =
  let (minLeft, newLeft) = deleteMin left
  in (minLeft, balance $ Node x newLeft right (1 + max (height newLeft) (height right)))

-- Convert an AVL tree to a sorted list
treeToList :: AVLTree a -> [a]
treeToList Empty = []
treeToList (Node x left right _) = treeToList left ++ [x] ++ treeToList right

-- Build an AVL tree from a sorted list
listToTree :: (Ord a) => [a] -> AVLTree a
listToTree = foldr insert Empty

-- Union of two AVL trees
treeUnion :: (Ord a) => AVLTree a -> AVLTree a -> AVLTree a
treeUnion t1 t2 = listToTree $ ordUnion (treeToList t1) (treeToList t2)

ordUnion :: (Ord a) => [a] -> [a] -> [a]
ordUnion [] ys = ys
ordUnion xs [] = xs
ordUnion (x:xs) (y:ys)
  | x < y     = x : ordUnion xs (y:ys)
  | x == y    = x : ordUnion xs ys
  | otherwise = y : ordUnion (x:xs) ys

-- Intersection of two AVL trees
treeIntersection :: (Ord a) => AVLTree a -> AVLTree a -> AVLTree a
treeIntersection t1 t2 = listToTree $ ordIntersection (treeToList t1) (treeToList t2)

ordIntersection :: (Ord a) => [a] -> [a] -> [a]
ordIntersection [] _ = []
ordIntersection _ [] = []
ordIntersection (x:xs) (y:ys)
  | x < y     = ordIntersection xs (y:ys)
  | x == y    = x : ordIntersection xs ys
  | otherwise = ordIntersection (x:xs) ys

-- Difference of two AVL trees
treeDifference :: (Ord a) => AVLTree a -> AVLTree a -> AVLTree a
treeDifference t1 t2 = listToTree $ ordDifference (treeToList t1) (treeToList t2)

ordDifference :: (Ord a) => [a] -> [a] -> [a]
ordDifference [] _ = []
ordDifference xs [] = xs
ordDifference (x:xs) (y:ys)
  | x < y     = x : ordDifference xs (y:ys)
  | x == y    = ordDifference xs ys
  | otherwise = ordDifference (x:xs) ys

-- Initialize AVL tree from list
initTree :: (Ord a) => [a] -> AVLTree a
initTree = foldr insert Empty

-- Updated function signature with Integral constraint
treeStats :: (Integral a, Fractional b) => AVLTree a -> (Int, a, b)
treeStats tree = let (count, sum) = treeStatsHelper tree
                 in (count, sum, fromIntegral sum / fromIntegral count)

-- Helper function to traverse the tree and calculate count and sum
treeStatsHelper :: Num a => AVLTree a -> (Int, a)
treeStatsHelper Empty = (0, 0)
treeStatsHelper (Node value left right _) =
    let (leftCount, leftSum) = treeStatsHelper left
        (rightCount, rightSum) = treeStatsHelper right
    in (1 + leftCount + rightCount, value + leftSum + rightSum)

-- Function to print an AVL tree
printTree :: Show a => AVLTree a -> IO ()
printTree tree = mapM_ putStrLn (reverse (treeLevels tree 0))

-- Helper function to generate the tree levels as strings
treeLevels :: Show a => AVLTree a -> Int -> [String]
treeLevels Empty _ = []
treeLevels (Node v l r _) depth =
    let indent = replicate (depth * 4) ' '
        current = indent ++ show v
        leftSubtree = treeLevels l (depth + 1)
        rightSubtree = treeLevels r (depth + 1)
    in rightSubtree ++ [current] ++ leftSubtree

testInsertion :: IO ()
testInsertion = do
    let elements = [3, 2, 1, 4, 5]
    let tree = initTree elements
    putStrLn "Test Insertion - Tree Structure:"
    printTree tree

testDeletion :: IO ()
testDeletion = do
    let elements = [3, 2, 1, 4, 5]
    let tree = foldr delete (initTree elements) [2, 3]
    putStrLn "\nTest Deletion - Tree Structure after deleting 2 and 3:"
    printTree tree
    -- Expected output: a balanced AVL tree with elements 1, 4, 5

testStatistics :: IO ()
testStatistics = do
    let elements = [1, 2, 3, 4, 5]
    let tree = initTree elements
    let (count, sum, average) = treeStats tree
    putStrLn "\nTest Statistics - Node Count, Sum, Average:"
    putStrLn $ "Node Count: " ++ show count  -- Expected: 5
    putStrLn $ "Sum of Values: " ++ show sum  -- Expected: 15
    putStrLn $ "Average Value: " ++ show average  -- Expected: 3.0

testSetOperations :: IO ()
testSetOperations = do
    let tree1 = initTree [1, 2, 3, 4, 5]
    let tree2 = initTree [4, 5, 6, 7, 8]
    putStrLn "\nTest Union - Tree Structure:"
    printTree $ treeUnion tree1 tree2  -- Expected: 1, 2, 3, 4, 5, 6, 7, 8

    putStrLn "\nTest Intersection - Tree Structure:"
    printTree $ treeIntersection tree1 tree2  -- Expected: 4, 5

    putStrLn "\nTest Difference - Tree Structure (tree1 - tree2):"
    printTree $ treeDifference tree1 tree2  -- Expected: 1, 2, 3

main :: IO ()
main = do
    testInsertion
    testDeletion
    testStatistics
    testSetOperations
