
type Graph = [(Int,[Int])]
vertices :: Graph -> [Int]
vertices = map fst

children :: Int -> Graph -> [Int]
children  node gr = snd $ head  $ filter (\ x -> fst x == node) gr 

none :: (a -> Bool) -> [a] -> Bool
none p  = all (\x -> not $ p x) 

parents :: Int -> Graph -> [Int]
parents u gr = [p | p <- vertices gr,  u `elem` children p gr]

data Player = Monster String Int Int
            | Wizard Int Float
            | Princess Int [String]

data GNode a = GNode
  { value :: a,         -- име на поле - тип 
    adjacents :: [a]    -- име на поле - тип 
  }

type Graph1 a = [GNode a] 

g::Graph1 Int 
g =
  [ GNode 1 [2, 3],
    GNode 2 [4, 1, 5],
    GNode 3 [4],
    GNode 4 [5],
    GNode 5 [4]
  ]

-- Връща броя наследници на даден връх
outDeg :: Eq a => a -> Graph1 a -> Int
outDeg node gr = length . adjacents $ head [ v | v <-gr, value v == node]

-- Връща броя родители на даден връх
inDeg :: Eq a => a -> Graph1 a -> Int
inDeg node gr = length [ v | v <- gr, node `elem` adjacents v]

-- Връща списък от родители 
inDegSpis :: Eq a => a -> Graph1 a -> [a]
inDegSpis node gr =  [value v | v <- gr, node `elem` adjacents v]

-- Проверява дали има ребро между два върха
edge :: Eq a => a -> a -> Graph1 a -> Bool
edge u v gr = v `elem` inDegSpis u gr

-- Проверява дали има път между два върха в ацикличен граф.
-- Бонус: да работи за графи с цикъли
path :: Eq a => Graph1 a -> a -> a -> Bool
path gr parent child 
    | parent == child = True
    | otherwise = 
        let children = adjacents $ head [ v | v <-gr, value v == parent]
            in not (null children) && any (\c -> path gr c child) children

gr1 :: (Eq a, Num a) => [(a, [a])]
gr1 =
  [ (1, [2, 3]),
    (2, [4, 1]),
    (3, [4]),
    (4, [5]),
    (5, [4])
  ]

-- Наско 
-- Проверява дали има път между два върха
-- TODO: възможно е да зациклим ако не пазим посетените върхове
path1 :: Eq a => [(a, [a])] -> a -> a -> Bool
path1 graph1 a b
  | a == b = True
  | otherwise =
      let succs = lookup a graph1
       in case succs of
            Nothing -> False
            Just children -> any (\c -> path1 graph1 c b) children

grcyc::Graph1 Int
grcyc =
  [ GNode 1 [2, 3],
    GNode 2 [4],
    GNode 3 [],
    GNode 4 [1, 5],
    GNode 5 []
  ]

findPath :: Eq a => Graph1 a -> a -> a ->[a]-> Bool
findPath gr parent node currPath 
    | parent == node = True
    | otherwise = 
        not (null notOccured) && any (\c -> findPath gr c node (currPath ++ [c]) ) notOccured 
            where notOccured =  filter (\x -> x `notElem` currPath) $ head [ adjacents v | v <-gr, value v == parent]
                  --children =  head [ adjacents v | v <-gr, value v == parent]

dfs :: Eq a => Graph1 a -> a -> a -> Bool
dfs gr parent node = findPath gr parent node [parent]