makeCacheMatrix <- function(x=matrix()) {
	# create and save the matrix and the inverse
	i <- NULL
	# store the matrix in the parent enviromment 
	set <- function(y) {
		# save the matrix and reset the inverse
		x <<- y
		
		i <<- NULL
	}
	# call the matrix
	get <- function(){x} 
	# store the inverse in the parent enviromment with i
	setinversa <- function(inversa){i <<- inversa}
	#  call inverse
	getinversa <- function() {i}
	# create the list
	list(set = set, get = get,
	     setinversa = setinversa,
	     getinversa = getinversa)
	
}


cacheSolve <- function(x,...) {
	# check for see if we have the inverse
	i <- x$getinversa()
	# permite extraer el m
	if(!is.null(i)) {
		message("Getting cached data")
		return(i)
	}
	# if not, I have to calculate the inverse///// Getting the matrix
	matrix <- x$get()
	# calculate the matrix
	i <- solve(matrix)
	# store the matrix
	x$setinversa(i)
	# print
	i
	
}




