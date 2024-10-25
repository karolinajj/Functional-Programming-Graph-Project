import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
--import qualified Data.Maybe (isJust, mapMaybe)
--import qualified Data.Maybe as Data
-- {-# HLINT ignore "Use camelCase" #-}

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]
type AdjList = [(City,[(City,Distance)])]

-- Helper functions 

addcity :: City -> [City] -> [City]   -- Function to filter the citymap in order to not have duplicates (might not work when called by other functions hadn't tested that case)
addcity city cities
    | city `elem` cities  = cities
    | otherwise           = city : cities

areEq :: City -> City ->  City -> City -> Bool -- True if pair of city_a and city_b is equivalent to pair city1 and city2
areEq city_a city_b city1 city2
    | (city_a == city1 && city_b == city2) || (city_a == city2 && city_b == city1) = True
    | otherwise = False

isNotStartOrTarget :: City -> City -> City -> Bool -- Function to check that the city is neither the start or the end
isNotStartOrTarget start target city = city /= start && city /= target

constructPath :: City -> City -> [City] -> [City] -- Function to add a city at the start and a city at the end
constructPath start target path = start : path ++ [target]

generatePaths :: [City] -> City -> City -> [[City]] -- Function to generate all possible paths between two cities
generatePaths allCities start target = 
    let intermediateCities = filter (isNotStartOrTarget start target) allCities
        possibleIntermediatePaths = Data.List.permutations intermediateCities -- Generate permutations of the intermediate cities
        completePaths = map (constructPath start target) possibleIntermediatePaths -- Add the start and target cities to each path
    in completePaths

createPathDistanceTuple :: Path -> Distance -> (Path, Distance) -- Converts a path and distance into a tuple (path, distance)
createPathDistanceTuple path distance = (path, distance)

toPathDistance :: RoadMap -> Path -> Maybe (Path, Distance) -- Function which saves in a tuple the path with the respective cost of it exists
toPathDistance roads path = fmap (createPathDistanceTuple path) (pathDistance roads path)

dfs :: RoadMap -> City -> [City] -> [City] -- Perform DFS to find all reachable cities
dfs roadmap city visited
    | city `elem` visited = visited
    | otherwise = exploreNeighbors (city : visited) (adjacent roadmap city)
  where
    exploreNeighbors visited [] = visited
    exploreNeighbors visited ((neighbor, _):neighbors) =
        exploreNeighbors (dfs roadmap neighbor visited) neighbors

createAdjList :: RoadMap -> AdjList
createAdjList roadmap = foldr addEdge [] roadmap
  where
    addEdge (city1, city2, dist) acc = addNeighbor city2 (city1, dist) (addNeighbor city1 (city2, dist) acc)

    addNeighbor :: City -> (City, Distance) -> AdjList -> AdjList
    addNeighbor city neighbor [] = [(city, [neighbor])]
    addNeighbor city neighbor ((c, neighbors):rest)
      | c == city = (c, neighbor : neighbors) : rest
      | otherwise = (c, neighbors) : addNeighbor city neighbor rest

--Function to find the minimum direct distance between two cities
minDirectDistance :: City -> City -> AdjList -> Maybe Distance
minDirectDistance _ _ [] = Nothing
minDirectDistance city1 city2 ((city, neighbors):rest)
    | city == city1 = 
        case filter (\(neighbor, _) -> neighbor == city2) neighbors of
            [] -> minDirectDistance city1 city2 rest
            distances -> Just (minimum (map snd distances))
    | otherwise = minDirectDistance city1 city2 rest

cityToInt :: City -> [City] -> Int
cityToInt _ [] = -1
cityToInt city cities = cityToInt_acc city cities 0
  where
    cityToInt_acc _ [] _ = -1
    cityToInt_acc target (x:xs) acc
      | target == x = acc
      | otherwise = cityToInt_acc target xs (acc + 1)

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
    | null path = Just 0  -- Case of empty path
    | length path == 1 = Just 0  -- Case of single city
    | otherwise = 
      case distance roads startCity nextCity of  -- Calculate the distance if there is a road
        Just dist -> 
            case pathDistance roads (nextCity : tail (tail path)) of  -- Recursive call for the rest of the path
                Just total -> Just (dist + total)  -- Sum the distances of the road with the rest
                Nothing -> Nothing  -- Marks the path as invalid
        Nothing -> Nothing  -- Marks the path invalid if there is no distance
  where
    startCity = head path         -- Selects the first city on the path
    nextCity = head (tail path)   -- Selects the next city on the path 

--I assum that:
--1) in (city1, city2, dist) city1 /= city2
--2) in the roadmap there is only one triple for each edge
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
        sameCity = length(filter (== city) xs)
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

-- Still need to find a way to print vary options of path
-- shortestPath :: RoadMap -> City -> City -> [Path] -- Computes all shortest paths considering a certain roads (RoadMap) and start, target (City)
-- shortestPath roads start target
--     | start == target = [[start]]  -- If the start and target are the same
--     | otherwise =
--         let allCities = cities roads  -- Get all cities
--             candidatePaths = generatePaths allCities start target  -- Generate all paths to evaluate
--             validPathsWithDistances = Data.mapMaybe (toPathDistance roads) candidatePaths -- Validate the paths
            
--             directPathDistance = pathDistance roads [start, target]  -- Check for direct path from start to target
--             directPathWithDistance = case directPathDistance of
--                 Just dist -> [createPathDistanceTuple [start, target] dist]  -- Create a tuple with the direct path and distance
--                 Nothing -> []  -- No direct path, return empty 

--             allValidPathsWithDistances = validPathsWithDistances ++ directPathWithDistance -- Combine valid paths with the direct path if valid

--         in if null allValidPathsWithDistances
--            then []  -- If no valid paths exist
--            else
--                 let shortestDist = minimum (map snd allValidPathsWithDistances) -- Find the shortest distance 
--                in [path | (path, dist) <- allValidPathsWithDistances, dist == shortestDist]  -- Return all paths with the shortest distance


tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap -- unconnected graph
gTest4 = [("A", "B", 10), ("A", "C", 15), ("B", "C", 10), ("B", "D", 12), ("C", "D", 10)]
-- Some cities for tests
cTest1 :: City
cTest1 = "6"

cTest2 :: City
cTest2 = "10"