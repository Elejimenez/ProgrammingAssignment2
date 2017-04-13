## Eleonora Jimenez
## 4/11/2017
## programmingAssignment2: calculating and storing the inverse of a matric

## function used to cache de inverse when it's calculated by solveCache and return it when it called by the same function
##test:
## my_matrix <- makeCacheMatrix(matrix(c(3,6,2,4), 2, 2))
## my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## my_matrix <- makeCacheMatrix(matrix(1:6, 3, 2))

makeCacheMatrix <- function(x = matrix()) {
	 i <- NULL 
	 set <- function(y) { 
                x <<- y
                i <<- NULL
	 }
        get <- function() x  
        setinverse <- function(solve) i <<- solve 
        getinverse <- function() i 
        list(set = set, get = get,setinverse = setinverse, getinverse = getinverse) 

}

#Calculates the inverse of the matrix if it's not i the cache or returns it using makeCacheMatrix
cacheSolve <- function(x, ...) {
	  i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        tryCatch(i <- solve(data, ...),
			error = function(msg){
			message("there has been the following error:")
			message(msg)
			return(NULL)
			},
			warning = function(msg){
			message("there has been the following warning:")
			message(msg)		
			x$setinverse(i)
        		return(i)
			},
        		finally ={
			x$setinverse(i)
			return(i)})

}