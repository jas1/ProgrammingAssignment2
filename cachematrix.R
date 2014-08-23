## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## ¿may be similar to clases ?
makeCacheMatrix <- function(originalMatrix = matrix()) {
    
    ## verifying X is a matrix
    if(!is.matrix(originalMatrix)){
        message("the value entered must be a matrix!")
        return("the value entered must be a matrix!")
    }
    
    ## initialize the variable where the inverse will be saved
    ## its null because it have not been calculated yet.
    inverseOfMatrix <- NULL

    ## accessing values
    get <- function(){
        ## returns the matrix as it was created
        originalMatrix  
    } 
    
    ## setting a new matrix
    set <- function(newSetMatrix) {
        ## saving the new matrix value replacing the old one
        originalMatrix <- newSetMatrix
        ## as value changed, invers is reset 
        inverseOfMatrix <- NULL
    }
    
    ## return the value of the inverse matrix
    getInverse <- function (){
        inverseOfMatrix
    }
    
    ## return the value of the inverse matrix
    setInverse <- function(inverseParam) {
        ## asign the inverse param to the inverse of this matrix.
        ## using the <<- operator because: 
        inverseOfMatrix <<- inverseParam
    }
    
    ## 'methods'/'functions' available on this(makeCacheMatrix) function
    list(
         set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(originalMatrix , ...) {
    
    ## pre req: should be a makeCacheMatrix
    
    if(!is.list(originalMatrix)){
        message("the value entered must be a makeCacheMatrix!")
        return("the value entered must be a makeCacheMatrix!")
    }
    
    ## Return a matrix that is the inverse of 'originalMatrix'
    ## 1st step retrive the inverse of the matrix
    inversedMatrix <- originalMatrix$getInverse()
    
    ## if inversed matrix its aready there, return the cached inverse
    if( !is.null(inversedMatrix) ) {
        ## leaving the message as the example of vector
        message("getting cached data")
        ## return the value of inversed matrix, 
        return(inversedMatrix)
        ## no furtrher code its executed if inversed matrix wasnt null.
    }
    
    ## not explicit, but, if inversed its null , ELSE, will calculate the inversedMatrix
    ## Get the original matrix from the object object
    tempVarMatrix <- originalMatrix$get()
    
    ## quick fix for not having errors that might not understand if not searching google/reading
    ## an inversed matrix CANNOT be applied on singular matrix
    ## to check if its singular, we check the determinant != 0
    matDeterminant =  det(tempVarMatrix)
    if(matDeterminant == 0){
        message("Singular Matrix: determinant == 0: matrix cannot be inversed.")
        return ("Singular Matrix: determinant == 0: matrix cannot be inversed.")
    }
    
    
    ## this is the way to ghet the matrix inverse in R
    ## Calculating the inverse using matrix multiplication
    ## very important the '%*%' operator
    ## asign the result ov the inversion to the inverse variable
    inversedMatrix <- solve(tempVarMatrix) %*% tempVarMatrix
    
    ## Set the inverse to the object
    originalMatrix$setInverse(inversedMatrix)
    
    ## Return the matrix inversion
    inversedMatrix
    
    ## may be an upgrade to this might be handling the chache inside the makeCacheMatrix, instead of having an external func.
    ## as its not required it will be not modified
}
