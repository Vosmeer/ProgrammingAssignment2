## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix checks if a given data vector can be coerced into a n x n matrix
## If not it will try to do so by dropping rows or columns
## If the matrix is square, it will create the inverse of the created matrix

## createSquareMatrix will create a square matrix based on the smallest dimension of the given matrix

createSquareMatrix<-function(x){
  if(ncol(x)>nrow(x)) 
    m<-x[,-c((1+nrow(x)):ncol(x))]
  else
    m<-x[-c((1+ncol(x)):nrow(x)),] 
}

## Calculates inverse of give matrix en stores in cache

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  m <- matrix(data=x,ncol=floor(sqrt(length(x))))
  if( ncol(m)!=nrow(m)) {warning('Matrix is not square, dropping columns/rows')
    createSquareMatrix(m)
    }

  makeCacheMatrix.result<<-list(m,solve(m))
  
}

## Function checks whether the given dataset is equal to the one in cache memory and, if so, retrieves
## the inverse from memory. If not, it calculates the inverse

cacheSolve <- function(x, ...) {
  m<-NULL
  m <- matrix(data=x,ncol=floor(sqrt(length(x))))
  if( ncol(m)!=nrow(m)) {warning('Matrix is not square, dropping columns/rows')
    createSquareMatrix(m)
  }
  
  newmatrix <- m
  oldmatrix <- makeCacheMatrix.result[[1]]
  
  if (identical(newmatrix,oldmatrix)==TRUE){
    message("New and old data sets are identical")
  
    if(!is.null(makeCacheMatrix.result[[2]])) {
      message("Getting cached data")
      cacheSolve.result<<-list(newmatrix,makeCacheMatrix.result[[2]])
      return(makeCacheMatrix.result[[2]])
    }
  }
  message("New and old data sets are not identical")
  cacheSolve.result<<-list(newmatrix,solve(newmatrix))
  cacheSolve.result[[2]]
}
