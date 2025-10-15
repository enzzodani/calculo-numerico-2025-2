
A_v <- c(10, 2, 1,
         1, 5, 1,
         2, 3, 10)

b_v <- c(7, -8, 6)

A <- matrix(A_v, nrow = 3, byrow = TRUE)
b <- b_v

cat("Matriz A:\n")
print(A)

cat("\nVetor b:\n")
print(b)



gauss_jacobi <- function(A, b, tol = 1e-4, max_iter = 100) {
  n <- length(b)
  
  x <- c(0.7, -1.6, 0.6)
  
  x_new <- rep(0, n)
  
  iter <- 0
  
  while (iter < max_iter) {
    for (i in 1:n) {
      sigma <- 0
      for (j in 1:n) {
        if (i != j) {
          sigma <- sigma + A[i, j] * x[j]
        }
      }
      
      x_new[i] <- (b[i] - sigma) / A[i, i]
    }
    
    if (max(abs(x_new - x)) < tol) {
      cat(paste("\nConvergência alcançada após", iter + 1, "iterações.\n"))
      return(x_new)
    }
    
    x <- x_new
    
    iter <- iter + 1
  }
  
  cat(paste("\nO método não convergiu após", max_iter, "iterações.\n"))
  return(x)
}


solucao <- gauss_jacobi(A, b)


cat("\nA solução do sistema é:\n")
print(solucao)

cat("\nVerificação (A * x):\n")
print(A %*% solucao)
