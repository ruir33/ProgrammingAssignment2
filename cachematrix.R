### Warning: Identation looks good on vi editor, but not on notepad ++ !  Sorry !

## And now function:   makeCacheMatrix
## ... Lets see it ! 

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Input parameter: x - a square invertible matrix
        ## returns: [a list containing functions]
        ##              1. set - set the matrix
        ##              2. get - get the matrix
        ##              3. setinv - set the inverse
        ##              4. getinv - get the inverse
        ##         this list is used as the input to cacheSolve()
	##
	## Because it uses <<- assignment operator, these variables are not exposed 
	## to the outside environment
        		
		
	# inv var is where we are going to store the result of inversion
        inv = NULL
		
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
		
	# Lets return the input matrix
        get = function() x
		
	# Lets set the inversed matrix
        setinv = function(inverse) inv <<- inverse 
		
	# Lets return the inversed matrix
        getinv = function() inv
		
		
	# Next, lets return a list with these functions,
	# so that it will be possible to use makeCacheMatrix like this:
	# x <- makeCacheMatrix(demo_matrix)
	# x$set(new_matrix) - to change matrix
	# x$get 			- to get the setted matrix
	# x$setinv 			- to set the inversed matrix
	# x$getinv 			- to get the inversed matrix
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}





## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Input parameter: x: output of makeCacheMatrix()  [set,get,setinv,getinv]
        ## return: inverse of the matrix input to makeCacheMatrix()
        
	# Lets get the inversed matrix from object x 
        inv = x$getinv()
        
		
        # if the inverse has already been calculated it wont be NULL,
	# and we can use the cache to speed up this process ...
        if (!is.null(inv)){
                # Ok ! Lets get it from the cache and skip the computation. 
                message("Getting cached data")
                return(inv)
        }
        
        # Not OK ! First time !  We still have to calculate the inverse !
	# Lets get the matrix object ...
        matrix.data = x$get()
	# Lets get it using r resources (solve function)
        inv = solve(matrix.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)

}




## 
## Steps used to test functions (on a 4x4 matrix)
##
## Just execute: [Please uncomment the 4 following lines!]
# test <- matrix(runif(16,1,100),4,4)
# testCached <- makeCacheMatrix(test)
# cacheSolve(testCached)
# cacheSolve(testCached)

### Test example:
#> test <- matrix(runif(16,1,100),4,4)
#> testCached <- makeCacheMatrix(test)
#> cacheSolve(testCached)
#             [,1]          [,2]         [,3]
#[1,] -0.044538322  0.0044934384  0.179601006
#[2,] -0.003153976  0.0126854106  0.004651675
#[3,]  0.010785248 -0.0001943551 -0.128180007
#[4,]  0.017334580 -0.0061822030  0.023975369
#             [,4]
#[1,] -0.074295743
#[2,] -0.003620191
#[3,]  0.066706872
#[4,] -0.012245038
#> cacheSolve(testCached)
#Getting cached data
#             [,1]          [,2]         [,3]
#[1,] -0.044538322  0.0044934384  0.179601006
#[2,] -0.003153976  0.0126854106  0.004651675
#[3,]  0.010785248 -0.0001943551 -0.128180007
#[4,]  0.017334580 -0.0061822030  0.023975369
#             [,4]
#[1,] -0.074295743
#[2,] -0.003620191
#[3,]  0.066706872
#[4,] -0.012245038

### END ###
