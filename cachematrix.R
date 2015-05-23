## These functions are used to enhance the normal capabilities of a matrix by caching the inverse
## value of the matrix instead of having to calculate it fresh each time. The matrix must be
## invertible in order for these functions to be useful, please see the following article for more
## information: https://en.wikipedia.org/wiki/Invertible_matrix

## makeCacheMatrix() is a function that takes a matrix and returns a special matrix object that will
## return a cached version of the inverse of the matrix if it has already been calculated.

makeCacheMatrix <- function (x = matrix())
{
        ## Initialize the inverse of the matrix x to NULL
        inverseMatrix <- NULL

        ## Sets the special matrix object x to the passed in matrix y,
        ## and resets the inverse of x to NULL
        set <- function (y)
        {
                x <<- y
                inverseMatrix <<- NULL
        }

        ## Returns the special matrix object x
        get <- function () {return (x)}

        ## Sets the inverse of the matrix to the matrix i
        setInverse <- function (i)
        {
                inverseMatrix <<- i
        }

        ## Returns the cached inverse of the matrix x
        getInverse <- function ()
        {
                return (inverseMatrix)
        }

        ## Set up the special matrix object methods
        list (set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}


## cachSolve() is a function that is used along with makeCacheMatrix() to compute the inverse of a
## matrix and return a cached copy if it has already been calculated, otherwise it calculates and
## returns the inverse of the matrix.

cacheSolve <- function (x, ...)
{
        ## Try to get the inverse matrix if it has already been cached
        inverseMatrix <- x$getInverse()

        ## If a cached copy of the inverse of x was found, return it
        if (!is.null(inverseMatrix))
        {
                message("Fetching inverse from cache...")

                ## Return a matrix that is the inverse of 'x'
                return (inverseMatrix)
        }

        ## If the inverse was not found in the cache, calculate it from the data in x

        ## Get the data in x
        data <- x$get()

        ## Calculate the inverse of x
        inverseMatrix <- solve(data)

        ## Set the inverse of x in the cache
        x$setInverse(inverseMatrix)

        ## Return a matrix that is the inverse of 'x'
        return (inverseMatrix)
}