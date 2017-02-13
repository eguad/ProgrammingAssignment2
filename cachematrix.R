## these functions work together to preserve the result of 
## inverting a matrix


## makeCacheMatrix uses a function closure to provide methods
## for calculating and caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix())
{
	## initialize cache variable
	m <- NULL

	## store new matrix and reset cache
	set <- function(y)
	{
		x <<- y			## modify symbols in parent scope
		m <<- NULL		## reset cache
	}

	## simple accessors
	get <- function(){ x }
	setinv <- function(inv){ m <<- inv }
	getinv <- function(){ m }

	## return object with methods
	invisible(
		list(
			set = set,
			get = get,
			setinv = setinv,
			getinv = getinv
	))
}


## cacheSolve calculates the inverse of a cached matrix.  
## if this is the first time, the cache is updated

cacheSolve <- function(cm)
{
	## get cached value
	m <- cm$getinv()

	## see if inverse has already been cached
	if(!is.null(m))
	{
		message("getting cached data")
		return(m)
	}

	## calculate inverse from matrix
	data <- cm$get()
	m <- solve(data)

	## cache and return inverse
	cm$setinv(m)
	m
}
