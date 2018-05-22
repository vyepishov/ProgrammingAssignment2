## The functions below provide the functionality that allows to
## manage matrix contents and its cached inverse in efficient way
## (using caching the inverse).
## Examples of usage:
## > mat <- makeCacheMatrix(matrix(1:4, 2, 2))
## > print(mat$get())
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > print(cacheSolve(mat))
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > print(cacheSolve(mat))
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > print(mat$getInverse())
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > mat$set(matrix(c(0, 1, 0, 2, 1, 1, 2, 1, 2), 3, 3))
## > print(cacheSolve(mat))
## [,1] [,2] [,3]
## [1,] -0.5    1    0
## [2,]  1.0    0   -1
## [3,] -0.5    0    1
## etc...

## Makes list of the following operations (functions):
## 1. setting contents of matrix
## 2. getting contents of matrix,
## 3. setting inverse of matrix, and
## 4. getting inverse of matrix.
## Setting inverse of matrix should NOT be used explicitly by user.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}

## Gets cached inverse of matrix if available, otherwise
## computes the inverse and stores it in the cache before providing it.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
