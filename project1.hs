import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]
type AdjList = [(City,[(City,Distance)])]

-- Helper functions 

addcity :: City -> [City] -> [City]   -- Function to filter the citymap in order to not have duplicates 
addcity city cities
    | city `elem` cities  = cities
    | otherwise           = city : cities

areEq :: City -> City ->  City -> City -> Bool -- True if pair of city_a and city_b is equivalent to pair city1 and city2
areEq city_a city_b city1 city2
    | (city_a == city1 && city_b == city2) || (city_a == city2 && city_b == city1) = True
    | otherwise = False

dfs :: RoadMap -> City -> [City] -> [City] -- Perform DFS to find all reachable cities
dfs roadmap city visited
    | city `elem` visited = visited
    | otherwise = exploreNeighbors (city : visited) (adjacent roadmap city)
  where
    exploreNeighbors visited [] = visited
    exploreNeighbors visited ((neighbor, _):neighbors) =
        exploreNeighbors (dfs roadmap neighbor visited) neighbors

createAdjList :: RoadMap -> AdjList -- Creates a new list of tuples with them containing each city and their respective edges
createAdjList roadmap = foldr addEdge [] roadmap
  where
    addEdge (city1, city2, dist) acc = addNeighbor city2 (city1, dist) (addNeighbor city1 (city2, dist) acc)

    addNeighbor :: City -> (City, Distance) -> AdjList -> AdjList
    addNeighbor city neighbor [] = [(city, [neighbor])]
    addNeighbor city neighbor ((c, neighbors):rest)
      | c == city = (c, neighbor : neighbors) : rest
      | otherwise = (c, neighbors) : addNeighbor city neighbor rest


minDirectDistance :: City -> City -> AdjList -> Int -- Function to find the minimum direct distance between two cities
minDirectDistance _ _ [] = 10^9 -- returns a large value if cities are not connected
minDirectDistance city1 city2 ((city, neighbors):rest)
    | city1 == city2 = 0
    | city == city1 =
        case filter (\(neighbor, _) -> neighbor == city2) neighbors of
            [] -> minDirectDistance city1 city2 rest
            distances -> minimum (map snd distances)
    | otherwise = minDirectDistance city1 city2 rest

intToCity :: Int -> [City] -> City
intToCity n cities
  | n < 0 || n >= length cities = "Error: Index out of bounds"
  | otherwise = cities !! n

-- Functions requested by the project

cities :: RoadMap -> [City] -- It takes a roadmap as argument and then returns a list with all the cities that are there present
cities [] = []                                                                        -- Case when the roadmap is empty
cities ( (city1 , city2 , _ ) : rest ) = addcity city1 (addcity city2 (cities rest))  -- Take the roadmap and only process the information related to the cities and not distances

areAdjacent :: RoadMap -> City -> City -> Bool -- True if there is a tuple in the Roadmap which uses the city1 and city2
areAdjacent [] _ _ = False
areAdjacent ((city_a, city_b, _):xs) city1 city2
    | areEq city_a city_b city1 city2 = True
    | otherwise = areAdjacent xs city1 city2

distance :: RoadMap -> City -> City -> Maybe Distance -- It returns a distance if there is a tuple which uses the city1 and city2
distance [] _ _ = Nothing
distance ((city_a, city_b, dist):xs) city1 city2
    | areEq city_a city_b city1 city2 = Just dist
    | otherwise = distance xs city1 city2

adjacent :: RoadMap -> City -> [(City,Distance)] -- ? is there only one road conecting two cities? any specific order?
adjacent roadmap city = foldr exctractCityDist [] roadmap
  where
    exctractCityDist (city_a, city_b, dist) xs
      | city_a == city = (city_b, dist) : xs
      | city_b == city = (city_a, dist) : xs
      | otherwise = xs

pathDistance :: RoadMap -> Path -> Maybe Distance -- Calculates the distance between two cities following a certain path, if that said path exists along roads (RoadMap)
pathDistance roads path
    | null path = Just 0                                                                          -- Case of empty path
    | length path == 1 = Just 0                                                                   -- Case of single city
    | otherwise =
      case distance roads startCity nextCity of                                                   -- Calculate the distance if there is a road
        Just dist ->
            case pathDistance roads (nextCity : tail (tail path)) of                              -- Recursive call for the rest of the path
                Just total -> Just (dist + total)                                                 -- Sum the distances of the road with the rest
                Nothing -> Nothing                                                                -- Marks the path as invalid
        Nothing -> Nothing                                                                        -- Marks the path invalid if there is no distance
  where
    startCity = head path                                                                         -- Selects the first city on the path
    nextCity = head (tail path)                                                                   -- Selects the next city on the path 

--I assum that in the roadmap there is only one triple for each edge
rome :: RoadMap -> [City]
rome [] = []
rome roadmap = map fst (maxOccurence (countOccurences (roadsFromCities roadmap) []) [])
  where
    roadsFromCities :: RoadMap -> [City]
    roadsFromCities [] = []
    roadsFromCities ((city1, city2,_):xs) = city1 : city2 : roadsFromCities xs

    countOccurences :: [City] -> [(City, Int)] -> [(City, Int)]
    countOccurences [] acc = acc
    countOccurences (city:xs) acc = countOccurences filteredXs ((city, 1 + sameCity):acc)
      where
        sameCity = length (filter (== city) xs)
        filteredXs = filter (/= city) xs

    maxOccurence :: [(City, Int)] -> [(City, Int)] -> [(City, Int)]
    maxOccurence [] acc = acc
    maxOccurence ((city, count):xs) [] = maxOccurence xs [(city,count)]
    maxOccurence ((city, count):xs) ((maxcity, maxcount):rest)
      | count > maxcount = maxOccurence xs [(city, count)]
      | count == maxcount = maxOccurence xs ((city, count):(maxcity, maxcount):rest)
      | otherwise = maxOccurence xs ((maxcity, maxcount):rest)

isStronglyConnected :: RoadMap -> Bool -- Check if the graph is strongly connected, by providing a roadmap
isStronglyConnected [] = False                    -- Returns false if dont exist a roadmap
isStronglyConnected cityMap =
    let nations = cities cityMap                  -- Get all unique cities
        start = head nations                      -- Start for example from the head of the list
        reachableCities = dfs cityMap start []    -- Perform DFS from the starting city
    in length reachableCities == length nations   -- Check if all cities are reachable

shortestPath :: RoadMap -> City -> City -> [Path] -- Find all the shortest paths starting from start to target when using the cityMap
shortestPath [] _ _ = []                                                                                      -- Case when the roadmap provided is empty
shortestPath cityMap start target
  | start == target = [[start]]                                                                               -- Case when the target and start are the same
  | otherwise = findShortestPaths [[start]] [] maxBound adjList
  where
    adjList = createAdjList cityMap                                                                           -- Create a list with adjacent cities of each city

    findShortestPaths :: [Path] -> [Path] -> Distance -> AdjList -> [Path]                                    -- Function that uses BFS to search each path while filtering based on the distance
    findShortestPaths [] shortestPaths _ _ = shortestPaths                                                    -- Case when there is no more paths to explore
    findShortestPaths (path:paths) shortestPaths shortestDist adjList =
      let currentCity = last path                                                                             -- Taking the value of the last element of the path which can be the target or not
          nextPaths = concatMap (expandPath path) (neighbors adjList currentCity)                             -- Step to create the rest of possibles paths that still need to be evaluated
      in
        if currentCity == target then                                                                         -- Checks if the last of that path is the target
          let pathDist = pathDistance cityMap path                                                            -- Calcalutes the distance of the current path
          in case pathDist of
            Just dist
              | dist < shortestDist -> findShortestPaths paths [path] dist adjList                            -- Case when the current path is shorter than the rest
              | dist == shortestDist -> findShortestPaths paths (path : shortestPaths) shortestDist adjList   -- Case when the path is equal cost than the rest
              | otherwise -> findShortestPaths paths shortestPaths shortestDist adjList                       -- Case when the current path is higher cost than the rest
            Nothing -> findShortestPaths paths shortestPaths shortestDist adjList                             -- Case when the current path is invalid
        else
          findShortestPaths (paths ++ nextPaths) shortestPaths shortestDist adjList                           -- Case when the current path doesnt end on the target

    expandPath :: Path -> (City, Distance) -> [Path]                                                          -- Function to add more cities to the path while keeping track of those that are already on it
    expandPath path (nextCity, edgeDist)
      | nextCity `elem` path = []                                                                             -- Part to check if they are in the path
      | otherwise = [path ++ [nextCity]]

    neighbors :: AdjList -> City -> [(City, Distance)]                                                        -- Function to get the list of the neighbors of a city when provided with the adjlist
    neighbors adjList city = case lookup city adjList of
      Just neighborsList -> neighborsList
      Nothing -> []


travelSales :: RoadMap -> Path
travelSales roadmap = 
    if minimum costs < 10^9 
    then intToCity start cities : findPath minIndex lastSubset 
    else []
  where
    adjlist = createAdjList roadmap
    cities = map fst adjlist
    n = length cities
    start = 0
    lastSubset = ((2^(n-1)) - 1) `Data.Bits.shiftL` 1

    memo = Data.Array.array ((0, 0), (n - 1, (1 `Data.Bits.shiftL` n) - 1)) 
           [((end, mask), dp (end, mask)) | end <- [0..n-1], mask <- [0..(1 `Data.Bits.shiftL` n) - 1]]

    costs = [memo Data.Array.! (i, lastSubset) + minDirectDistance (intToCity i cities) (intToCity start cities) adjlist | i <- [1..n-1]]
    minIndex = snd $ minimum $ zip costs [1..n-1]

    dp :: (Int, Int) -> Int
    dp (end, 0) = minDirectDistance (intToCity end cities) (intToCity start cities) adjlist
    dp (end, mask)
      | mask == Data.Bits.setBit 0 end = minDirectDistance (intToCity end cities) (intToCity start cities) adjlist
      | otherwise = minimum [memo Data.Array.! (j, Data.Bits.clearBit mask end) + minDirectDistance (intToCity j cities) (intToCity end cities) adjlist 
                             | j <- citiesInSubset mask, j /= end]

    citiesInSubset :: Int -> [Int]
    citiesInSubset subset = [i | i <- [1..n-1], Data.Bits.testBit subset i]

    findPath :: Int -> Int -> Path
    findPath end 0 = [intToCity end cities]
    findPath end mask
      | mask == Data.Bits.setBit 0 end = [intToCity end cities, intToCity 0 cities]
      | otherwise = intToCity end cities : findPath next (Data.Bits.clearBit mask end)
      where
        next = snd $ minimum [(memo Data.Array.! (j, Data.Bits.clearBit mask end) + minDirectDistance (intToCity j cities) (intToCity end cities) adjlist, j)
                             | j <- citiesInSubset mask, j /= end]

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

-- Some cities for tests
cTest1 :: City
cTest1 = "6"

cTest2 :: City
cTest2 = "10"