# PFL-project-1

## Group Members:

    Luis Fernandes     UP:202108770 
    Karolina Jedraszek UP:XXXXXXXXX 

## Distribution of work

The work was distributed equally (50/50).

1. cities :: RoadMap -> [City] <br>
Responsible: Luis 
2. areAdjacent :: RoadMap -> City -> City -> Bool <br>
Responsible: Karolina 
3. distance :: RoadMap -> City -> City -> Maybe Distance <br>
Responsible: Karolina 
4. adjacent :: RoadMap -> City -> [(City,Distance)]
Responsible: Karolina 
5. pathDistance :: RoadMap -> Path -> Maybe Distance <br>
Responsible: Luis
6. rome :: RoadMap -> [City] <br>
Responsible: Karolina 
7. isStronglyConnected :: RoadMap -> Bool <br>
Responsible: Luis
8. shortestPath :: RoadMap -> City -> City -> [Path] <br>
Responsible: Luis
9. travelSales :: RoadMap -> Path <br>
Responsible: Karolina 

## Process of work for shortestPath

For the shortestpath we followed this path to analyse and get the answer for the question. First we check if the start and the target are the same and so end the function there by returning the result [[start]]. <br>
In the case of those being different, first we create some variables those being, a list containing all the cities (allCities) using one of the earlier functions asked, then we create all the possibles paths that can exist by using a helper function called generatePaths which needs the roadmap and the starter and ending cities, with it we use a function from the list module to compute all the possible variations and then we add the start at the start and the target at the end of the list. <br>
With the list of possibles paths we then check to see if they are valid or not by using the pathDistance function mixed with another one to produce a list of tuples showing the (path,distance). We use a mapMaybe in order to be possible to deal with the case of Nothing without further more work. <br>
After we check if there is a direct path, which means if exists a edge which is the start <-> target (since the edges are undirected we only need to check if there is an edge which have start and target as cities).Depending on the value it might be added if existed following the tuple formula or empty tuple in case of returning nothing. <br>
Then we check if after all there is a path in said list of tuples. If no ends the function with a empty list. Otherwise, we first retreive the value of the lowest distance to a variable and then we filter the list with only the paths that have that value, discarding now the value of distance since its not expected at the end. <br>

## Process of work for travelSales
