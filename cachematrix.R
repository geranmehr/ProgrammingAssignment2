# mohammadreza
# I choose the easy way, I mean I realized if I use a matrix instead of numeric input and then change every
# reference of mean and replace it by solve function, I can make a good function that able to inverse any matrix. 

#the first_step introduce the input as a matrix
#the make "sol" variable as a null data that will be fill by running our function and by setsolve function 
#"solve" would apply on sol and get the result
# 

makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL                     # initializing inverse data as a NULL
        set <- function(y) {
                x <<- y
                sol <<- NULL
        }
        get <- function() x     # it will get the matrix x
        setsolve <- function(solve) sol <<- solve  #calculate the inverse of the matrix
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





