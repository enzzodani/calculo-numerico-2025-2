gauss_seidel <- function(A, b, tol = 1e-2, max_iter = 100) {
  n <- length(b)
  
  x <- c(0.7,-1.6,0.6)
  
  iter <- 0
  
  while (iter < max_iter) {
    # 1. Salva o vetor x da iteração anterior para o critério de parada
    x_old <- x
    
    for (i in 1:n) {
      sigma <- 0
      for (j in 1:n) {
        if (i != j) {
          sigma <- sigma + A[i, j] * x[j]
        }
      }
      
      # 2. Atualiza x[i] diretamente, em vez de um x_new[i]
      x[i] <- (b[i] - sigma) / A[i, i]
    }
    
    # 3. Compara o x atual com o x da iteração anterior (x_old)
    if (max(abs(x - x_old)) < tol) {
      cat(paste("\nConvergência alcançada após", iter + 1, "iterações.\n"))
      return(x)
    }
    
    iter <- iter + 1
  }
  
  cat(paste("\nO método não convergiu após", max_iter, "iterações.\n"))
  return(x)
}

# --- Script principal ---

# Definição da matriz A e vetor b
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

# Chamada da nova função
solucao <- gauss_seidel(A, b)

cat("\nA solução do sistema (Gauss-Seidel) é:\n")
print(solucao)

cat("\nVerificação (A * x):\n")
# Verifica se A*x se aproxima de b
print(A %*% solucao)
