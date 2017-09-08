## The program calculates the inverse of a given matrix and store it in 
##an object created by the first function, indirectly, as a nested function's 
##output.
## As long as the matrix remains the same, the inverse is retained and produced 
##when needed.One can change the matrix which updates the inverse accordingly 

## This function creates a unique object that is a list of 4 functions with self evident names.
## The get inverse function acts as a variable retaining the value of the inverse (i)

makeCacheMatrix <- function(x = matrix()) {
    i_cached <- NULL
    setmatrix <- function(y)
    {
        x <<- y # The asignment operator pushes x to the parent environment
        i_cached <<- NULL
    }
    getmatrix <- function() 
        {return(x)}
    setinverse <- function(inverse) 
        {i_cached <<- inverse} #pushes i to the parent environment
    getinverse <- function() 
        {return(i_catched)}
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
         setinverse = setinverse, getinverse = getinverse)
}


## This function takes the object created above and 
##checks whether the inverse already exists in getinverse. 
## if it does, it returns that value otherwise it calculates the inverse 
## returns it and stores it in getinverse of the object.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse() 
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$getmatrix()
    inverse <- solve(data)
    x$setinverse(inverse)
    return(inverse)
}



#########################################################################
### This can also be done in a single function as below##################

#The nested functions are same as before. this function does both computing
## and getting the cached value.
matrixCache <- function(x = matrix())
{
    i <- NULL
    setmat <- function(y) # to feed a new matrix to the object
    {
        x <<- y
        i <<- NULL
    }
    getinv <- function() # this performs the function of cacheSolve in the previous code.
    {
        if(!(is.null(i)))
        {
            print('getting from cache')
        }
        else
        {
            print('solving')
            i <<- solve(x) 
        }
        return(i)
    }
    return(list(setmat = setmat, getinv = getinv))
} #To test, one has to create a matrix object and call the getinv function of the object.

## I am not completely satisified with the functions, however.
## Because, this is essentially the same thing as storing the inverse of a
##matrix in a variable and calling that variable everytime the inverse is 
##needed. introducing lexical scoping and additional functions just complicates
##the matters. I hope the reviewers can give me some insight!!!