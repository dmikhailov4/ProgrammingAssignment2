## The first function, makeVector creates a special "vector", which is really a list containing a function to
  ## 1.	set the value of the vector
  ## 2.	get the value of the vector
  ## 3.	set the value of the mean
  ## 4.	get the value of the mean
  
  makeVector <- function(x = numeric()) {
       m <- NULL
       set <- function(y) {
           x <<- y
           m <<- NULL
         }
       get <- function() x
       setmean <- function(mean) m <<- mean
       getmean <- function() m
       list(set = set, get = get,
                     setmean = setmean,
                     getmean = getmean)
     }
  ## The following function calculates the mean of the special "vector" 
    ## created with the above function. However, it first checks to see if 
    ## the mean has already been calculated. If so, it gets the mean from 
    ## the cache and skips the computation. Otherwise, it calculates the mean 
    ## of the data and sets the value of the mean in the cache 
    ## via the setmean function.
    
    cachemean <- function(x, ...) {
         m <- x$getmean()
         if(!is.null(m)) {
             message("getting cached data")
             return(m)
           }
         data <- x$get()
         m <- mean(data, ...)
         x$setmean(m)
         m
       }
    ## Write the following functions:
      ## Write the following functions:
      ##    makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
      
      ## makeCasheMatrix <- function(x = matrix()) {
      ##  
      ## }
      
      makeCacheMatrix <- function(x = matrix()) {
             inv <- NULL
             set <- function(y) {
                x <<- y
                 inv <<- NULL
                 }
            get <- function() x
             setinv <- function(inverse) inv <<- inverse
             getinv <- function() inv
             list(set = set, get = get, setinv = setinv, getinv = getinv)
         }
      ## Write the following functions:
        ##    cacheSolve: This function computes the inverse of the special "matrix" 
        ##    returned by makeCacheMatrix above. If the inverse has already been calculated 
        ##    (and the matrix has not changed), 
        ##    then the cacheSolve should retrieve the inverse from the cache.
        
        ## cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
         ## }
         
        cacheSolve <- function(x, ...) {
             ## Return a matrix that is the inverse of 'x'
                 inv <- x$getinv()
                 if(!is.null(inv)) {
                     message("getting cached result")
                     return(inv)
                   }
                 data <- x$get()
                 inv <- solve(data, ...)
                 x$setinv(inv)
                 inv
             }
        
        DMikhailov4_Matrix <- matrix(rnorm(100),10,10)
        
        DMikhailov4_Matrix
        ## [,1]        [,2]       [,3]       [,4]       [,5]        [,6]        [,7]       [,8]       [,9]       [,10]
        ## [1,] -1.20380057  0.64725560  0.1513869  1.3558439  0.8647405 -1.22652625 -0.53602224 -0.1094001 -1.1679404 -0.17984424
        ## [2,] -0.45210602 -1.15644524  0.4727908  0.5648885 -1.6849527 -0.08428157  0.82520119 -2.9592000 -0.5384584 -0.03220147
        ## [3,] -0.49143159  0.69893117  1.9392537  1.2906108 -0.2133592 -2.24480988  1.31037849  0.6838571  0.6137593 -0.31380173
        ## [4,] -0.21939219  0.14758961 -1.6564516 -0.6310631 -1.6200046  1.19399040  0.69904845  0.7196695 -1.3501297  2.02636165
        ## [5,]  0.05793218  0.89977169  2.0348687  1.8545302  1.0941461 -0.56086021 -0.02740148  1.0151563 -0.2916951  0.51870632
        ## [6,] -0.81166128 -0.28658096 -1.9755829  0.1481594  0.7163759 -1.12275600  0.24944015 -0.3206838  1.3316916  1.16818727
        ## [7,] -1.00058602  0.80314496  0.2708509 -0.7451945  0.3740378  1.68556337 -0.62046164 -1.3750855 -0.5370620 -0.41232650
        ## [8,] -1.14581978 -0.02432194  0.1710262 -0.0117935 -0.1213805 -0.51924875 -0.72930319  0.6223968 -1.4987895 -0.68756405
        ## [9,]  0.41990873  0.66908048  0.6263653  1.4135952  0.2401357 -0.47635403  0.76108558  2.3529085  0.3153125 -0.34165983
        ## [10,] -0.27355104 -0.61141650  0.1306810 -0.9580981  0.1455815  0.58184740 -0.27155639 -0.2330954 -0.5191823  0.92927720
        
        DMikhailov4_Matrix_Cache <- makeCacheMatrix(DMikhailov4_Matrix)
        
        cacheSolve(DMikhailov4_Matrix_Cache)
        ## getting cached result
        ## [,1]        [,2]        [,3]          [,4]        [,5]        [,6]         [,7]       [,8]       [,9]       [,10]
        ## [1,]  0.5681162 -0.19391269 -0.04339712  0.0274254094 -0.26652443 -0.52966157 -0.414889995 -0.7416201 -0.2135574  0.03204931
        ## [2,]  0.1523059 -0.34720289  0.28699553  0.2829303560  0.06167674 -0.18137366  0.157775260 -0.2579888 -0.5531545 -0.63326845
        ## [3,] -0.3915832  0.03358161  0.13681861 -0.0566418592  0.35006269 -0.00939701  0.056530950  0.2472384 -0.1777222  0.05417853
        ## [4,] -0.3162563  0.30496551 -0.35541098 -0.0002601025  0.53175352  0.29127250  0.009950357  0.3520877  0.1441423 -0.51514182
        ## [5,]  1.1683578 -0.10051588  0.00701104 -0.2764159288 -0.82221320 -0.30972961  0.046275756 -1.0192624  0.7399196  1.21447842
        ## [6,] -0.3078913  0.19994220 -0.27392620 -0.0283466625  0.16024707  0.14370506  0.330314976  0.1334796  0.4388886  0.05324186
        ## [7,]  1.3132711  0.07558795  0.32441096 -0.0844272271 -1.30684917 -0.47254742  0.143906923 -1.3007795  1.1539351  1.39959810
        ## [8,] -0.3545494 -0.04644819 -0.04919643  0.0175523752  0.14022783  0.13962329 -0.013083361  0.4047459  0.2017561 -0.01106443
        ## [9,] -1.1074594  0.05766236 -0.04526612 -0.0499748208  0.69901569  0.57464032  0.171230422  0.7060842 -0.3354092 -0.75611148
        ## [10,] -0.3177053 -0.04261159  0.01796438  0.2147867285  0.53343296  0.20290301 -0.095646518  0.1321806 -0.4435014 -0.10966946
