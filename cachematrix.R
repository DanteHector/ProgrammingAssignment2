## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()){
	minv <- NULL
	set <- function(y){
		m <<- y
		minv <<- NULL
	}
	get <- function() m
	setInverse <- function(inverse) minv <<- inverse
	getInverse <- function() minv
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(m, ...){
	minv <- m$getInverse()
	if(!is.null(minv)) {
		message("getting cached data")
		return(minv)
	}
	data <- m$get()
	minv <- solve(data, ...)
	m$setInverse(minv)
	minv
}
