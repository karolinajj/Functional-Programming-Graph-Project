import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
{-# HLINT ignore "Use camelCase" #-}

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

cities :: RoadMap -> [City]
cities = undefined -- modifiy this line to implement the solution, for each exercise not solved, leave the function definition like this

areEq :: City -> City ->  City -> City -> Bool -- True if pair of city_a and city_b is equivalent to pair city1 and city2
areEq city_a city_b city1 city2
    | (city_a == city1 && city_b == city2) || (city_a == city2 && city_b == city1) = True
    | otherwise = False

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent ((city_a, city_b, _):xs) city1 city2
    | areEq city_a city_b city1 city2 = True
    | otherwise = areAdjacent xs city1 city2

distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((city_a, city_b, dist):xs) city1 city2
    | areEq city_a city_b city1 city2 = Just dist
    | otherwise = distance xs city1 city2

adjacent :: RoadMap -> City -> [(City,Distance)] -- ? is there only one road conecting two cities? any specific order?
adjacent roadmap city = foldr addCity [] roadmap
  where
    addCity (city_a, city_b, dist) xs
      | city_a == city = (city_b, dist) : xs
      | city_b == city = (city_a, dist) : xs
      | otherwise = xs

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

--wrong definition
rome :: RoadMap -> [City] -- ? what if city_a == city_b should we count it as a road?
rome [] = []
rome roadmap = map fst (rome_acc (sumFromCities (roadsFromCities roadmap) []) [])
  where
    roadsFromCities :: RoadMap -> [(City, Distance)]
    roadsFromCities [] = []
    roadsFromCities ((city1, city2, dist):xs) = (city1, dist) : (city2, dist) : roadsFromCities xs

    sumFromCities :: [(City, Distance)] -> [(City, Distance)] -> [(City, Distance)]
    sumFromCities [] acc = acc
    sumFromCities ((city, dist):xs) acc = sumFromCities filteredXs ((city, dist + sumDist sameCity):acc)
      where
        sameCity = filter (\(c, _) -> c == city) xs
        filteredXs = filter (\(c, _) -> c /= city) xs
        sumDist [] = 0
        sumDist ((_, d):rest) = d + sumDist rest

    rome_acc :: [(City, Distance)] -> [(City, Distance)] -> [(City, Distance)]
    rome_acc [] acc = acc
    rome_acc ((city, dist):xs) [] = rome_acc xs [(city, dist)]
    rome_acc ((city, dist):xs) ((maxcity, maxdist):rest)
        | dist > maxdist = rome_acc xs [(city, dist)]
        | dist == maxdist = rome_acc xs ((city, dist):(maxcity, maxdist):rest)
        | otherwise = rome_acc xs ((maxcity, maxdist):rest)

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

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