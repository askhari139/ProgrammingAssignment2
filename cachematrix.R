## The program calculates the inverse of a given matrix and store it in 
##an object created by the first function, indirectly, as a nested function's 
##output.
## As long as the matrix remains the same, the inverse is retained and produced 
##when needed.One can change the matrix which updates the inverse accordingly 

## This function creates a unique object that is a list of 4 functions with self evident names.
## The get inverse function acts as a variable retaining the value of the inverse (i)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setmatrix <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inverse) i <<- inverse #pushes i to the parent environment
    getinverse <- function() i
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, 
         getinverse = getinverse)
}


## This function takes the object created above and 
##checks whether the inverse already exists in getinverse. 
## if it does, it returns that value otherwise it calculates the inverse 
## returns it and stores it in getinverse of the object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    j <- x$getinverse()
    if(!is.null(j)) {
        message("getting cached data")
        return(j)
    }
    data <- x$getmatrix()
    j <- solve(data)
    x$setinverse(j)
    j
}



#########################################################################
### This can also be done in a single function as below##################

#The nested functions are same as before. this function does both computing
## and getting the cached value.
matrixCache <- function(x = matrix())
{
    i <- NULL
    setmat <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    getinv <- function()
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
}

## I am not completely satisified with the functions, however.
## Because, this is essentially the same thing as storing the inverse of a
##matrix in a variable and calling that variable everytime the inverse is 
##needed. introducing lexical scoping and additional functions just complicates
##the matters. I hope the reviewers can give me some insight!!!