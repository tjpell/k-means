rm(list=ls())
cat("\014")


set.seed(101)
myTestDf <- as.data.frame(matrix(rnorm(10000), ncol = 5))
myTestDf[2] <- myTestDf[2] + 5
myTestDf[3] <- myTestDf[3] - 0.1
myTestDf[4] <- myTestDf[4] + 1
myTestDf[5] <- myTestDf[5] - 3

myKmeans <- function(myScatterInput = myTestDf, nReps = 10, 
                     myClusterNum = 5, maxIter = 1000){
  
  # save this for later
  columns <- ncol(myScatterInput)
  labs <- factor(1 : myClusterNum)
  disty <- NULL
  dists <- NULL
  
  
  #initilize
  # start the factor col populate factor cols
  
  # optimize: make data structs and store in them, dont append.
  # matrix wise math
  
  bestGroups <- NULL
  
  #nreps will restart the whole set of operations
  for(rep in 1 : nReps){
    
    myScatterInput$centroid <- sample(labs, size = nrow(myScatterInput), replace = T)
    
    #myScatterInput$shortestDist <- 0
    centroidFrame <- myScatterInput %>% group_by(centroid) %>%
      summarise_all(mean) #start up the centroids
    
    # lets start a vector to keep track of the distances
    euclidDists <- NULL
    
    # initialize these: matrices for distances, old clusters for comparison
    oldClusters <- myScatterInput$centroid
    
    inputMatrix <- myScatterInput[ , 1 : columns]
    centroidMat <- centroidFrame[ , 2:columns + 1]
    
    # we are initialized, lets start iterating
    for (iter in 1 : maxIter){
      
      # take euclidean distance
      euclids <- rdist(inputMatrix, centroidMat)
      
      # which is  row min? assign the corresponding label in vect
      minDists <- apply(euclids, 1, min)
      newClusters <- factor(apply(euclids, 1, which.min), levels = labs)
      
      
      # recompute clusters
      
      newFrame <- data.frame(newClusters,inputMatrix)
      
      totalDist <- 0
      # did the centroid stabilize? Did we hit max iter?
      if (all(newClusters == oldClusters) | iter == maxIter){ 
        # get the sum euclid distance
        totalDist <- sum(minDists)
        euclidDists <- c(euclidDists, totalDist)
        break
      }
      # hold onto this for the next run
      oldClusters <- newClusters
      
      #recompute the centroids each time
      centroidFrame <- newFrame %>% group_by(newClusters) %>% summarise_all(mean)
      
      # if we lose a cluster, pick a new random centroid from the input
      if (nrow(centroidFrame) != myClusterNum){
        centroidFrame %<>% 
          rbind( c(labs[nrow(centroidFrame) + 1], 
                   unlist(inputMatrix[sample(nrow(inputMatrix), size = 1), ])))
      }
      
      # rebuild the matrix with how ever extra rows we lost
      centroidMat <- centroidFrame[ ,-1]
    }
    # this is the best grouping to happen after each iteration
    bestGrouping <- min(euclidDists)
    bestGroups <- c(bestGroups, bestGrouping)
  }
  print(min(bestGroups))
  
  #3D plot, if appropriate dimensional data
  if (columns == 3){
    threeDcolors <- c("#999999", "#E69F00", "#56B4E9", "#FFAEB9", "#00FFFF",
                      "#00FF7F", "#FFFF00","#CDC5BF", "#FF4500", "#8E388E",
                      "#121212", "#006400", "#104E8B")
    threeDcolors <- threeDcolors[as.numeric(newFrame$newClusters)]
    myPlot <- scatterplot3d(newFrame[,1:3], pch = 16, color=threeDcolors)
    print(myPlot)
  }

  # 2D plot, if appropriate dimensional data
  if (columns == 2){
    myPlot <- newFrame%>%ggplot(aes(x=newFrame[,2], y=newFrame[,3])) + geom_point(aes(color = newClusters))
    print(myPlot)
  }
}

myKmeans()


# TEST DATA 1
set.seed(101)
myScatterInput1 <- data_frame(myCol_01 = runif(100000, -1, 1))
myClusterNum1 <- 2

microbenchmark(myKmeans(myScatterInput = myScatterInput1, nReps = 10, myClusterNum = myClusterNum1, maxIter = 10000), times = 2)


# TEST DATA 2
set.seed(102)
myScatterInput2 <- data_frame(myCol_01 = runif(100000, -1, 1))
myClusterNum2 <- 4

myKmeans(myScatterInput = myScatterInput2, nReps = 10, myClusterNum = myClusterNum2, maxIter = 10000)
microbenchmark(myKmeans(myScatterInput = myScatterInput2, nReps = 10, myClusterNum = myClusterNum2, maxIter = 10000), times = 2)

set.seed(103)
myScatterInput3 <- data_frame(myCol_01 = runif(10000, -5, 20), 
                              myCol_02 = c(rnorm(3000, 20, 5), rnorm(5000, -4, 2), rnorm(2000, 40, 2)))

myClusterNum3 <- 3


print("Shortest euclidean distance was:")
myKmeans(myScatterInput = myScatterInput3, nReps = 10, myClusterNum = myClusterNum3, maxIter = 10000)
