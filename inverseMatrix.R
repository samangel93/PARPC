makeCacheMatrix <- function(x = matrix()){
	rm <- NULL
	set <- function(y){
		x <<- y
		rm <<- NULL
	}
	
	get <- function() x

	setRMatrix <- function(solve) rm <<- solve
	
	getRMatrix <- function() rm

	getdim <- function() dim(x)

	list(set = set, get = get,
		setRMatrix = setRMatrix,
		getRMatrix = getRMatrix,
		getdim = getdim)
}

cacheSolve <- function(x, ...){
	if (x$getdim()[1] == x$getdim()[2]){
		rm <- x$getRMatrix()
		if (!is.null(rm)){
			message("getting cached data")
			return (rm)
		}
		data <- x$get()
		rm <- solve(data, ...)
		x$setRMatrix(rm)
		rm
	} else {
		message("x is not a square invertible matrix.")
	}
}
