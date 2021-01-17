##BACKGROUND INFO:
#cache is good when you are required to go through same computation multiple times for a large object (matrix, vector, data frame) and the contents of the large object are not changing with time. Can call values of computation again when needed with cache 

# <<- allows you to manage variables at different levels (parent function: makeCacheMatrix and closure function: set, get, getinverse, setinverse);

#closure function: function made in another function

#<- vs <<-: <- make assignments at current environemnt;  <<- can modify variables from parent levels 

#lexical scoping: managing variables at different levels 
##make special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){ #takes 
    i <- NULL #i is an empty object 
    set <- function(y){ #set the value of the matrix to y
        x <<- y   #assigns y to x in another environment
        i <<- NULL #assigns NULL to i in another environment 
    }
    get <- function() {x} #get value of matrix outside set function
    setinverse <- function(solve){i <<- solve} #set the value of the inverse
    getinverse <- function() {i} #get the value of the inverse
    list (set = set, get = get, setinverse = setinverse, getinverse= getinverse)#output lists with values from each closure function
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...){
    i <- x$getinverse() #return a matrix that is the inverse of x and assigns it to i 
    if(!is.null(i)){ #first need to check if inverse is calculated (not null)
        message("getting cached data") #displayed message
        return(i) #then get inverse from cache and escape computation
    }
    #otherwise compute value of matrix set inverse of matrix in cache
    data <- x$get() #data from get() of first function 
    i <- solve(data, ...) #get inverse of that data using solve() and assign to i
    x$setinverse(i) #set value of inverse in cache using setinverse() function 
    i
}

#testing functions
source("cachematrix.R")
pmatrix <- makeCacheMatrix(matrix(1:4,nrow= 2, ncol=2))
pmatrix$get()
pmatrix$getinverse() #outputs NULL, meaning the inverse of the matrix has not been cached 
cacheSolve(pmatrix) #calculates inverse of matrix
cacheSolve(pmatrix) #if called again, will get inverse of matrix from cached data and outputs the messages
pmatrix$getinverse() #no longer gives NULL but inverse 
