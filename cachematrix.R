#' @author Auro
#' The functions bellow create a group of helping functions, created by the \code{makeCacheMatrix},
#' which caches an inverse of a given matrix inside it.
#' Be sure of creating the cache struture, via \code{makeCacheMatrix}, before using the cache itself

#' This function stores the matrix whose inverse will be cached
#' @param cache.matrix The matrix that will be cached
#' @return A list with functions to manipulate the matrix and the inverse cache
makeCacheMatrix <- function(cache.matrix = matrix()) {
  solve <- NULL
  set <- function(matrix) {

    # The cache is reset only if the matrix was updated
    if(!all.equal(cache.matrix, matrix)) {
      cache.matrix <<- matrix
      solve <<- NULL
    }
  }

  get <- function() cache.matrix

  setSolve <- function(s) solve <<- s
  getSolve <- function() solve

  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


#' Retrieve the inverse of the given cached matrix (created by the \code{makeCacheMatrix} function)
#' @param cached.matrix
cacheSolve <- function(cached.matrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  solve <- cached.matrix$getSolve()

  if(!is.null(solve)) {
    message("Getting cached data")
    return(solve)
  }

  matrix <- cached.matrix$get()

  solve <- solve(matrix)

  cached.matrix$setSolve(solve)

  solve
}
