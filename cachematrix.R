## The function below creates a special "matrix" object that
## can cache its inverse.

## It consists of 4 steps:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix". It returns a
## matrix that is the inverse of 'x'.

## It first checks if the inverse has already been calculated.
## If this is the case, it will get the inverse from the cache and skip
## computation. Otherwise, it will calculate the inverse of the data and it
## will set the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("Getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Testing my code:

## Making a matrix

## > test_matrix <- matrix(c(1, 2, 2, 1), 2, 2)
## > test_matrix
## [,1] [,2]
## [1,]    1    2
## [2,]    2    1
## > my_matrix <- makeCacheMatrix(test_matrix)
## > my_matrix$get()
## [,1] [,2]
## [1,]    1    2
## [2,]    2    1
## > my_matrix$getinverse()
## NULL

## I don't get the inverse since it's not calculated and cached yet.
## Calculate the inverse:

## > cacheSolve(my_matrix)
## [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333

## Now it finds the inverse since it's cached by above function.

## > my_matrix$getinverse()
## [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
