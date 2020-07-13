makecachematrix <- function(x = matrix()){
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setin <- function(inverse) i <<- inverse
        getin <- function() i
        list(set = set, get = get, setin = setin, getin = getin)
}

cachesolve <- function(x, ...){
        i <- x$getin()
        if(!is.null(i)){
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setin(i)
        i
}

a <- matrix(c(2,-5,-3,-1,3,2,3,1,3), 3, 3)
print(a)