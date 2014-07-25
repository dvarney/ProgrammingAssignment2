#   INTRO TO ASSIGN/<<-
#       The special assignment operator, '<<-', is used to change the value 
#       associated with an object, in this case, a matrix, in an environment
#       beyond the current one. This operator looks back in enclosing envi-
#       ronments for an environment that contains the symbol it is seeking,
#       and replaces the value in that environment, with the value to the
#       right of the operator. If the global or top-level environment is
#       reached without finding the symbol, then the object is created and 
#       the value assigned to its location. The nice thing about this opera-
#       tor, one doesn't have to keep track of the environment.
#
#   This function creates an object that may be outside the current environ-
#   ment. It takes 1 parameter [that is resolved into a square matrix]:
#       1. a pre-defined N x N matrix
#       2. or a on-the-spot matrix(c(1,1,1,2,2,2,3,3,3), nrow=3, ncol=3)
#
#   USE:
#       IA <- cacheSolve(oA, 3), where "oA" is the matrix to invert
#       yy <- makeCacheMatrix(matrix(c(...))) this is a holding place
#       yy$set(IA), where IA is the inverted matrix
#       yy$get(), returns the [matrix] object 
#
#   WARNING:
#       if yy$get() is called before yy$set(...), than a "null matrix"
#       message will be returned as the new enviironment variable has
#       not been created
#
#   OUTPUT:
#       original matrix
#       inverted matrix
#       and possibly the identity matrix
#
#   RETURNS:
#       inverted matrix
#
#   NOTE:
#   As mentioned, need to creat a vector to become a N x N matrix:
#               mt <- c(4,0,0,1,0,0,1,0,0,2,2,0,0,0,0,1) 
#   then,       var <- cacheSolve(mt,4,"I"), else cacheSolve(mt,4)
#
makeCacheMatrix <- function(x=matrix())
    {
        #print(class(x)); print(length(x)); print(x)

        x <- NULL
        set <- function(y=matrix())         # create new env obj
        {
            x <<- y; 
            print("in SET, show input matrix"); print(x);          
        }
        
        get <- function()                   # retrieve the obj
        {
            if (!is.null(x))                # iff it's !NULL
            {
                print("in GET, show stored matrix"); print(x)
            }
            else
                message("null matrix!")     # if NULL, then ...
        }

        list(set=set, get=get)
    }


#   This function solves for the inverse of a square matrix. It takes 1-3
#   parameters:
#       1. a list of values, i.e., c(1,3,...) in column-order form, N x N
#       2. the size of the matrix, defaults to 2
#       3. identity matrix, if it should be printed, select="I"
#
#       WHERE,  matrix to invert(oA)    the inverted matrix(IA)
#                [,1] [,2] [,3]              [,1] [,2] [,3]
#           [1,]    1    2    3         [1,]  -40   16    9
#           [2,]    2    5    3         [2,]   13   -5   -3
#           [3,]    1    0    8         [3,]    5   -2   -1
#
#   USE:
#       IA <- cacheSolve(mt,4,"I"), returns the inverted and identity
#       IA <- cacheSolve(mt,4), returns inverted matrix
#
#   OUTPUT:
#       original matrix
#       inverted matrix
#       and possibly the identity matrix
#
#   RETURNS:
#       inverted matrix
#
#   NOTE:
#   need to creat a vector to become a N x N matrix:
#               e.g., mt <- c(4,0,0,1,0,0,1,0,0,2,2,0,0,0,0,1) 
#   then,       cacheSolve(mt,4,"I"), else cacheSolve(mt,4)
#
cacheSolve <- function(y=matrix(c(1,2,3,4),2,2), mSize=2, select="IA") 
    {
        # Return a matrix that is the inverse of 'x'

        A <- matrix(y, mSize, mSize)        # creat the matrix
        print(noquote(" "))
        print(sprintf("Original %d x %d matrix:", mSize, mSize))
        print(A)                            # and print
        
        print(noquote(" "))
        print("Inverted matrix:")
        IA <- solve(A)                      # do the inversion
        print(IA)                           # and print
                
        if (select == "I") 
        {
            print(noquote(" "));            # if requested,
            print("Resultant Identity matrix:") 
            I <- solve(A)%*%A               # get the identity
            print(trunc(I))                 # and print
        }
        return(IA)
    }


