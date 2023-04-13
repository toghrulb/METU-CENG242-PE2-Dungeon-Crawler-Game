module PE2 where

-- PE2: Dungeon Crawler
-- Dungeon map is :: Tree Chamber [Encounter]
-- Each encounter is either a fight or a treasure
-- Fights deal you damage (reduce HP) but enemies drop some gold (add
-- gold)
-- Tresures just give gold, or potions (which give hp)
-- Nodes hold encounters, when you visit a node you go through all of them in order
-- You start with a certain amount of HP and 0 gold.
-- You lose HP and accumulate gold as you descend the tree and go through encounters

-- Polymorphic tree structure
data Tree a b = EmptyTree | Leaf a b | Node a b [Tree a b] deriving (Show, Eq)

-- Every location in the tree is of some Chamber type.
data Chamber = Cavern |
               NarrowPassage |
               UndergroundRiver |
               SlipperyRocks deriving (Show, Eq)

-- An enemy has a name, an amount of damage that it deals
-- and an amount of gold that it drops (in that order).
data Enemy = Enemy String Integer Integer deriving (Show, Eq)

-- Gold n gives n amount of gold
-- Potion n heals n hp
data Loot = Gold Integer | Potion Integer deriving (Show, Eq)

-- An encounter is either a Fight with an Enemy, or a treasure where
-- you find Loot
data Encounter = Fight Enemy | Treasure Loot deriving (Show, Eq)

-- This is a type synonym for how we will represents our dungeons
type Dungeon = Tree Chamber [Encounter]

-- First argument is starting HP
-- Second argument is the dungeon map
-- Third argument is the path (each integer in the list shows what child
-- you descend into)
-- Calculate how much HP you have left and how much gold you've
-- accumulated after traversing the given path
traversePath :: Integer -> Dungeon -> [Int] -> (Integer,Integer)
traversePath hp (Leaf chamber encounters) path = passEncounterList encounters hp 0
traversePath hp (Node chamber encounters children) (x:xs) = (hpTotal, goldTotal)
    where hpTotal = hpChild
          goldTotal = goldCurr + goldChild
          (hpCurr, goldCurr) = passEncounterList encounters hp 0
          (hpChild, goldChild) = traversePath hpCurr (children !! x) xs

passEncounter :: Encounter  -> Integer -> Integer -> (Integer, Integer)
passEncounter (Treasure (Gold a)) hp gold = (hp, gold + a)
passEncounter (Treasure (Potion a)) hp gold = (hp+a,gold)
passEncounter (Fight (Enemy a b c)) hp gold = (hp-b,gold+c)
passEncounterList :: [Encounter]  -> Integer -> Integer -> (Integer, Integer)
passEncounterList (x:xs) hp gold = (hpNew, goldOld + goldNew)
        where (hpOld, goldOld) = passEncounter x hp gold
              (hpNew, goldNew) = passEncounterList xs hpOld gold
passEncounterList _ hp gold = (hp, gold)

-- First argument is starting HP
-- Second argument is dungeon map
-- Find which path down the tree yields the most gold for you
-- You cannot turn back, i.e. you'll find a non-branching path
-- You do not need to reach the bottom of the tree
-- Return how much gold you've accumulated
findMaximumGain :: Integer -> Dungeon -> Integer
findMaximumGain hp dungeon = maximum [gainFromEachPath hp path | path <- allPaths dungeon]

gainFromEachPath :: Integer -> [Encounter] -> Integer
gainFromEachPath hp [] = 0
gainFromEachPath hp (Treasure (Gold gold) : rest) = gold + gainFromEachPath hp rest
gainFromEachPath hp (Treasure (Potion health) : rest) = gainFromEachPath (hp + health) rest
gainFromEachPath hp (Fight (Enemy name damage gold) : rest) = if hp <= damage then 0 else gold + gainFromEachPath (hp - damage) rest


allPaths :: Dungeon -> [[Encounter]]
allPaths EmptyTree = []
allPaths (Leaf name encounters) = [encounters]
allPaths (Node name encounters children) = [encounters ++ path | child <- children, path <- allPaths child]

-- First argument is starting HP
-- Second argument is the dungeon map
-- Remove paths that you cannot go thorugh with your starting HP. (By
-- removing nodes from tree).
-- Some internal nodes may become leafs during this process, make the
-- necessary changes in such a case.
findViablePaths :: Integer -> Dungeon -> Dungeon
findViablePaths hp EmptyTree = EmptyTree
findViablePaths hp (Leaf chamber encounters) =
    if hpResult < 0 then EmptyTree
    else Leaf chamber encounters
        where (hpResult, result) = passEncounterList2 encounters hp 0
findViablePaths hp (Node chamber encounters children) = 
    if hpResult <= 0  then EmptyTree
    else if countOfLeftChildren > 0 then Node chamber encounters leftChildren
    else Leaf chamber encounters
        where (hpResult, result) = passEncounterList2 encounters hp 0
              leftChildren = viableChildren hpResult children
              countOfLeftChildren = length leftChildren
passEncounterList2 :: [Encounter]  -> Integer -> Integer -> (Integer, Integer)
passEncounterList2 [] hp gold = (hp, gold)
passEncounterList2 (x:xs) hp gold =
    if (hpAtEnc <= 0) || (hpAfterEncs <= 0) then (-1, 0)
    else if (xs == []) then (hpAtEnc, goldAtEnc)
    else (hpAfterEncs, goldAfterEncs)
    where (hpAtEnc, goldAtEnc) = passEncounter x hp gold
          (hpAfterEncs, goldAfterEncs) = passEncounterList2 xs hpAtEnc goldAtEnc
viableChildren :: Integer -> [Dungeon] -> [Dungeon]
viableChildren hp [] = []
viableChildren hp (x:xs) = if r == EmptyTree then viableChildren hp xs else [r] ++ viableChildren hp xs
        where r = findViablePaths hp x

-- First argument is starting HP
-- Second Argument is dungeon map
-- Find, among the viable paths in the tree (so the nodes you cannot
-- visit is already removed) the two most distant nodes, i.e. the two
-- nodes that are furthest away from each other.
-- Returns a tuple with the two maximum depth values found in the dungeon.
depthList :: Dungeon -> [Integer]
depthList EmptyTree = []
depthList (Leaf _ _) = [1]
depthList (Node _ _ children) =
  let depths = concatMap depthList children
  in 1 : map (\d -> d+1) depths

findMax1 :: [Integer] -> (Integer, Integer) -> Integer -> (Integer, Integer)
findMax1 [] (maxx, index) indexcounter = (maxx, index)
findMax1 (x:xs) (maxx, index) indexcounter = if x > maxx then findMax1 xs (x,indexcounter) (indexcounter+1)
                                                         else findMax1 xs (maxx,index) (indexcounter+1)
                                                         
findMax2 :: [Integer] -> Integer -> Integer -> Integer -> Integer
findMax2 [] previndex maxx index = maxx
findMax2 (x:xs) previndex maxx index 
    | x > maxx = if index /= previndex then findMax2 xs previndex x (index+1) else findMax2 xs previndex maxx (index+1)  
    | otherwise = findMax2 xs previndex maxx (index+1)
    
mostDistantPair :: Integer -> Dungeon -> (Integer, Dungeon)
mostDistantPair hp EmptyTree = (0, EmptyTree)
mostDistantPair hp (Leaf chamber encounters) = (0, Leaf chamber encounters)
mostDistantPair hp dungeon = 
    (dist, findViablePaths hp dungeon)
    where
        max1 = fst (findMax1 (depthList (findViablePaths hp dungeon)) (0,0) 0) - 1
        max2 = (findMax2 (depthList (findViablePaths hp dungeon)) (snd (findMax1 (depthList (findViablePaths hp dungeon)) (0,0) 0)) 0 0) - 1
        dist = max1 + max2

-- Find the subtree that has the highest total gold/damage ratio
-- Simply divide the total gold in the subtree by the total damage
-- in the subtree. You only take whole subtrees (i.e you can take a new
-- node as the root of your subtree, but you cannot remove nodes
-- below it). Note that the answer may be the whole tree.
mostEfficientSubtree :: Dungeon -> Dungeon
mostEfficientSubtree EmptyTree = EmptyTree
mostEfficientSubtree dungeon = findMaxEfc (makeDungEfficPair (subTrees dungeon)) (EmptyTree,0)

countGoldHp :: Float -> Dungeon -> (Float,Float)
countGoldHp hp (Leaf chamber encounters) = passEncounterListEfc encounters hp 0
countGoldHp hp (Node chamber encounters children) = (hpTotal, goldTotal)
    where hpTotal = hpChild
          goldTotal = goldCurr + goldChild
          (hpCurr, goldCurr) = passEncounterListEfc encounters hp 0
          hpChild = sum [fst (countGoldHp hpCurr x) | x<-children]   
          goldChild = sum [snd (countGoldHp hpCurr x) | x<-children]
          
findMaxEfc :: [(Dungeon,Float)] -> (Dungeon,Float) -> Dungeon
findMaxEfc [] (dung,maxx) = dung
findMaxEfc (x:xs) (dung,maxx) = if snd x > maxx then
                                                  findMaxEfc xs (fst x, snd x)
                                                else 
                                                  findMaxEfc xs (dung,maxx)
makeDungEfficPair :: [Dungeon] -> [(Dungeon,Float)]
makeDungEfficPair [] = []
makeDungEfficPair (x:xs) = if fst (countGoldHp 0 x) <= 0 then [(x, 999999)] ++ makeDungEfficPair xs else [(x, snd (countGoldHp 0 x) / fst (countGoldHp 0 x))] ++ makeDungEfficPair xs    

passEncounterEfc :: Encounter  -> Float -> Float -> (Float, Float)
passEncounterEfc (Treasure (Gold a)) hp gold = (hp, gold + fromIntegral a)
passEncounterEfc (Treasure (Potion a)) hp gold = (hp- fromIntegral a,gold)
passEncounterEfc (Fight (Enemy a b c)) hp gold = (hp+ fromIntegral b,gold+ fromIntegral c)
passEncounterListEfc :: [Encounter]  -> Float -> Float -> (Float, Float)
passEncounterListEfc (x:xs) hp gold = (hpNew, goldOld + goldNew)
        where (hpOld, goldOld) = passEncounterEfc x hp gold
              (hpNew, goldNew) = passEncounterListEfc xs hpOld gold
passEncounterListEfc _ hp gold = (hp, gold)

subTrees :: Dungeon -> [Dungeon]
subTrees EmptyTree = []
subTrees (Leaf a b) = [Leaf a b]
subTrees (Node a b children) = [Node a b children] ++ concat [subTrees x | x<-children]

