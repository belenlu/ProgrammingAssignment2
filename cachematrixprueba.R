## This function calculates the inverse of a matrix and
## saves it to the cache. In this way, the next time
## you need calculate the inverse of a matrix, the previously
## saved value is returned instead of repeting the calculation

## This function creates a special matrix ( is really a list)
makeCacheMatrix <- function(x = matrix()) {
        ##create a matrix object x
        ##define the cache m
        m<-NULL
        set<-function(y){
        ##assign the input matrix y to the variable x in parent environment
        x<<y
        ##reinitialize m in the parent env
        m<<NULL
                }
        #return the matrix x
        get<-function() x
        #set the cache m to the inverse of x
        setinv<-function(inv) m<<-inv
        #return the cached inverse of x
        getinv<-function()m
        ##return a list. Each named element is a function.
        list(set=set,get=get,
             setinv=setinv,
             getinv=getinv)
         }


## This function calculates the inverse of the matrix created with the above function
## First it checks if the inverse has already been calculated. If so, it gets the inverse from the cache,
## and skips the calculation. If not, it calculates the matrix inverse and sets the value of the inverse
## in the cache by the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinv()
        ## if a cached value exists, return it
        if(!is.null(m)){
                message ("getting cached data")
                return(m)
                }
        ## otherwise, get the matrix, calculate the inverse and store it in the cache
        data<-x$get()
        m<-solve(data,...)
        x$setinv(m)
        ##return the inverse of the matrix
        m
}
