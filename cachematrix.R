## Code written in order to Coursera 2nd Programming Practice
## The idea is save time of proccessing getting cached data


## The makeCacheMatrix function creates a list with four functions:
## function set: set the value of matrix
## function get: get the value of matrix
## function set: setinv the value of matrix
## function get: getinv the value of matrix

makeCacheMatrix<- function(x=matrix()){
          #start a Null variable          
          inv<-NULL
          
          #set the matrix
          set <-function(y){            
                    x<<-y 
                    inv<<- NULL
          }
          
          #get the matrix
          get<-function() x
          
          #set the inverse of matrix
          setinv<- function(inverse) inv <<- inverse
          
          #get the inverse of matrix
          getinv <- function() inv
          list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The cacheSolve function calcule the inverse matrix, 
##but before it checks if it has already been inverted to get the result from cache
cacheSolve<-function(x,...){
          inv<- x$getinv()    #bring the inverse to the inv variable
          
          # check if the inverse has already been calculated
          if(!is.null(inv)){
                    message("Getting cached data")
                    return(inv)
          }
          #solve the inverse in case of the inverse is NULL
          data<- x$get()
          inv<- solve(data)
          x$setinv(inv)
          inv
}
