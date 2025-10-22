# --- Função Genérica: Gauss-Jacobi ---
#
# Resolve o sistema Ax = b usando o método iterativo
# de Gauss-Jacobi.
#
# Argumentos:
#   A: Matriz de coeficientes
#   b: Vetor de termos independentes
#   x0: (Opcional) Chute inicial. Se NULL, usa vetor de zeros.
#   tol: Critério de parada (tolerância)
#   max_iter: Número máximo de iterações
# -----------------------------------------------

gauss_jacobi <- function(A, b, x0 = NULL, tol = 1e-4, max_iter = 100) {
  n <- length(b)
  
  # Define o chute inicial
  if (is.null(x0)) {
    x <- rep(0, n)
  } else {
    x <- x0
  }
  
  # Vetor para armazenar os resultados da iteração k
  x_new <- rep(0, n)
  
  # Matriz para salvar a evolução (primeira coluna é o chute inicial)
  evolucao <- matrix(x, ncol = 1)
  
  iter <- 0
  
  while (iter < max_iter) {
    for (i in 1:n) {
      sigma <- 0
      for (j in 1:n) {
        if (i != j) {
          # Usa os valores da iteração anterior (x)
          sigma <- sigma + A[i, j] * x[j]
        }
      }
      # Salva o novo valor em x_new
      x_new[i] <- (b[i] - sigma) / A[i, i]
    }
    
    # Salva a iteração na matriz de evolução
    evolucao <- cbind(evolucao, x_new)
    
    # Critério de parada (d = max|x_i^(k) - x_i^(k-1)|)
    if (max(abs(x_new - x)) < tol) {
      cat(paste("\nConvergência alcançada após", iter + 1, "iterações.\n"))
      
      # Adiciona nomes às colunas da evolução
      colnames(evolucao) <- paste0("k=", 0:(ncol(evolucao)-1))
      print("Evolução das aproximações:")
      print(evolucao)
      
      return(x_new)
    }
    
    # Atualiza o vetor x para a próxima iteração
    x <- x_new
    
    iter <- iter + 1
  }
  
  cat(paste("\nO método não convergiu após", max_iter, "iterações.\n"))
  return(x)
}

A_q4 <- matrix(c(
  10, -2, 0, -1, 0,
   1, 5, 2, 0, 1,
   0, 3, 6, -2, 0,
   0, 0, 3, 7, 2,
   1, -1, 0, 5, 10
), nrow = 5, byrow = TRUE) 

b_q4 <- c(-31, -3, -32, 12, 102)

x0_q4 <- c(0,0,0,0,0)

solucao_jacobi <- gauss_jacobi(A_q4, b_q4, x0 = x0_q4, tol = 0.0001)

cat("\nSolução (Jacobi):\n")
print(solucao_jacobi)
