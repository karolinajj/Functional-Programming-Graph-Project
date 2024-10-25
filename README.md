# PFL-project-1

## Group Members:

    Luis Fernandes     UP:202108770 
    Karolina Jedraszek UP:202402265 

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

So in order to do the shortestPath function we followed this procedure: <br>
First we check if the roadmap is empty, if yes end the function by returning empty. <br>
Then we get to the actual cases: We now check if the start and target cities provided are not the same, in the case they are we return [start]. <br>
On the case they are not we proced to call an helper function called findShortestPaths which takes a list of paths to start evaluating, a empty list where the result will be, a distance where we set to maxBound so when searching for the distance on the first case we dont accidently give a lower number than the case and lastly a new type that we created. <br>
This new type is: type AdjList = [(City,[(City,Distance)])], which basically creates a list of tuples where the first argument is a city1 and the second one is a list of tuples but of cities and their respecitve distance to the city1. <br>
Now this functions works using the BFS type to search for all the paths. It then starts creating the path and calculating the distance, once it reaches the target it then evaluates the distance with the value that was provided before when the function was called, this is only applied to the first path possible, with a distance that is. <br>
Then depending it can go in 2 ways whenever the path has a distance different that the current shortestDist it creates a new list with this path and deletes the other, or simply skips the path entirely, when its lower and higher than the value respectively. <br>
Once all paths are analysed we then return the list with all the paths that cost the lowest. 

## Process of work for travelSales
