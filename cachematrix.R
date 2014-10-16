## The following functions are used to cache the inverse of a matrix so that it is available
## to be recalled at a later time.  
## The first function allows the user the set the value of a matrix and then searches for
## the value of the corresponding matrix inverse from the cached data.  
## The second function computes, caches and returns the inverse of the matrix, after first 
## checking the cached data to see if that particular inverse has been previously stored.

#Setting the Value of the Matrix/Getting the Value of the Inverse from the Cache
makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y,nrow,ncol){
                x<<-y
                s<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) s <<- solve
        getinverse<-function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#Computing, Caching and Returning the Inverse of a Matrix, after checking the Cached Data
cacheSolve <- function(x, ...) {
        s<-x$getinverse()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data<-x$get()
        s<-solve(data,...)
        x$setinverse(s)
        s
}


##Validation Example - Use these commands to verify the above formulas
test.matrix = makeCacheMatrix(matrix(c(1,2,3,6,8,9,12,15,10), nrow=3, ncol=3))  #Assign a matrix
test.matrix$get()         # Returns original matrix
test.matrix$getinverse()  # Returns a NULL since the inverse has not been computed and cached yet
cacheSolve(test.matrix)   # Computes, caches, and returns the matrix inverse
test.matrix$getinverse()  # Now this step returns the matrix inverse that has been stored in the previous step
cacheSolve(test.matrix)   # Returns the cached matrix inverse that was previously computed

test.matrix$set(matrix(c(0,10,7,3,6,8,12,19,11), nrow=3, ncol=3)) # Modify existing matrix
test.matrix$get()         # Returns the modified matrix
test.matrix$getinverse()  # Returns a NULL again because the original matrix has been modified
cacheSolve(test.matrix)   # Computes, caches, and returns new matrix inverse
test.matrix$getinverse()  # Returns the new matrix inverse that has been stored in the previous step
cacheSolve(test.matrix)   # Returns the new cached matrix inverse that was previously computed


##Validation Output
#> test.matrix = makeCacheMatrix(matrix(c(1,2,3,6,8,9,12,15,10), nrow=3, ncol=3))  #Assign a matrix
#> test.matrix$get()         # Returns original matrix
#[,1] [,2] [,3]
#[1,]    1    6   12
#[2,]    2    8   15
#[3,]    3    9   10
#> test.matrix$getinverse()  # Returns a NULL since the inverse has not been computed and cached yet
#NULL
#> cacheSolve(test.matrix)   # Computes, caches, and returns the matrix inverse
#[,1]       [,2]       [,3]
#[1,] -2.3913043  2.0869565 -0.2608696
#[2,]  1.0869565 -1.1304348  0.3913043
#[3,] -0.2608696  0.3913043 -0.1739130
#> test.matrix$getinverse()  # Now this step returns the matrix inverse that has been stored in the previous step
#[,1]       [,2]       [,3]
#[1,] -2.3913043  2.0869565 -0.2608696
#[2,]  1.0869565 -1.1304348  0.3913043
#[3,] -0.2608696  0.3913043 -0.1739130
#> cacheSolve(test.matrix)   # Returns the cached matrix inverse that was previously computed
#getting cached data
#[,1]       [,2]       [,3]
#[1,] -2.3913043  2.0869565 -0.2608696
#[2,]  1.0869565 -1.1304348  0.3913043
#[3,] -0.2608696  0.3913043 -0.1739130
#> 
#> test.matrix$set(matrix(c(0,10,7,3,6,8,12,19,11), nrow=3, ncol=3)) # Modify existing matrix
#> test.matrix$get()         # Returns the modified matrix
#[,1] [,2] [,3]
#[1,]    0    3   12
#[2,]   10    6   19
#[3,]    7    8   11
#> test.matrix$getinverse()  # Returns a NULL again because the original matrix has been modified
#NULL
#> cacheSolve(test.matrix)   # Computes, caches, and returns new matrix inverse
#[,1]  [,2]        [,3]
#[1,] -0.16380952  0.12 -0.02857143
#[2,]  0.04380952 -0.16  0.22857143
#[3,]  0.07238095  0.04 -0.05714286
#> test.matrix$getinverse()  # Returns the new matrix inverse that has been stored in the previous step
#[,1]  [,2]        [,3]
#[1,] -0.16380952  0.12 -0.02857143
#[2,]  0.04380952 -0.16  0.22857143
#[3,]  0.07238095  0.04 -0.05714286
#> cacheSolve(test.matrix)
#getting cached data
#[,1]  [,2]        [,3]
#[1,] -0.16380952  0.12 -0.02857143
#[2,]  0.04380952 -0.16  0.22857143
#[3,]  0.07238095  0.04 -0.05714286