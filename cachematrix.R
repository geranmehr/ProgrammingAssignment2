# mohammadreza


##
## I simply set the input x as a matrix to sign it as my input
## and then set the solved value "sol" as a null
## then I have changed every reference to "mean" to "solve" 
makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL
        set <- function(y) {
                x <<- y
                sol <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) sol <<- solve
        getsolve <- function() sol
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Same here, changed "mean" to "solve" and "m" to "sol"
cacheSolve <- function(x, ...) {
        sol <- x$getsolve()
        if(!is.null(sol)) {
                message("getting inversed matrix")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data, ...)
        x$setsolve(sol)
        sol
}

#check the function

m <- matrix(rnorm(36),6,6)
inv_mat <- makeCacheMatrix(m)
cacheSolve(inv_mat)





