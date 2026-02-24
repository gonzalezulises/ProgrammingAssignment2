## Estas funciones permiten crear una matriz especial que almacena en caché
## su inversa, evitando recalcularla si la matriz no ha cambiado.

## makeCacheMatrix crea un objeto que almacena una matriz y su inversa en caché

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calcula la inversa de la matriz. Si ya fue calculada y la
## matriz no cambió, devuelve el valor en caché.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
