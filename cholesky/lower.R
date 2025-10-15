resolution_lower <- function(A, b, n) {

  xt <- c(0,0,0)

  X <- matrix(xt, nrow = 3, byrow = FALSE)

  X[1] <- b[1] / A[1,1]

  for (k in 2:n) {
    soma <- 0
    for (j in 1:(k-1)) {
      soma <- soma + A[k,j]*X[j]
    }
    X[k] <- (b[k] - soma)/A[k,k]
  }

  return(X)
}

at <- c(1,0,0,2,1,0,2,-3,1)
A <- matrix(at, nrow = 3, byrow = TRUE)
bt <- c(-3,-1,-23)
b <- matrix(bt, nrow = 3, byrow = FALSE)

print(resolution_lower(A,b,3))
