## Esta función crea una matriz especial la cual tiene cuatro métodos
## que permite setear y obtener los valores de la matriz y de su inversa.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y = matrix()) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinv <- function(inverse_matrix = matrix()) {
                inv <<- inverse_matrix
        }
        getinv <- function() {
                inv
        } 
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Esta función obtiene la inversa de una matriz del tipo 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Retorna la inversa de la matriz 'x'
}