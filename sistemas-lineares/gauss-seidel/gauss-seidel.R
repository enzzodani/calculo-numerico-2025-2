# --- Função Genérica: Gauss-Seidel ---
#
# Resolve o sistema Ax = b usando o método iterativo
# de Gauss-Seidel.
#
# Argumentos:
#   A: Matriz de coeficientes
#   b: Vetor de termos independentes
#   x0: (Opcional) Chute inicial. Se NULL, usa vetor de zeros.
#   tol: Critério de parada (tolerância)
#   max_iter: Número máximo de iterações
# -----------------------------------------------

gauss_seidel <- function(A, b, x0 = NULL, tol = 1e-4, max_iter = 100) {
  n <- length(b)
  
  # Define o chute inicial
  if (is.null(x0)) {
    x <- rep(0, n) #Caso não tenha chute inicial, usaremos 0
  } else {
    x <- x0
  }
  
  # Matriz para salvar a evolução (primeira coluna é o chute inicial)
  evolucao <- matrix(x, ncol = 1)
  
  iter <- 0
  
  while (iter < max_iter) {
    # 1. Salva o vetor x da iteração anterior para o critério de parada
    x_old <- x
    
    for (i in 1:n) {
      sigma <- 0
      for (j in 1:n) {
        if (i != j) {
          # AQUI ESTÁ A DIFERENÇA:
          # Usa os valores de x[j] já atualizados nesta iteração (se j < i)
          # ou os valores antigos (se j > i).
          sigma <- sigma + A[i, j] * x[j]
        }
      }
      
      # 2. Atualiza x[i] diretamente
      x[i] <- (b[i] - sigma) / A[i, i]
    }
    
    # Salva a iteração na matriz de evolução
    evolucao <- cbind(evolucao, x)
    
    # 3. Compara o x atual com o x da iteração anterior (x_old)
    if (max(abs(x - x_old)) < tol) {
      cat(paste("\nConvergência alcançada após", iter + 1, "iterações.\n"))
      
      # Adiciona nomes às colunas da evolução
      colnames(evolucao) <- paste0("k=", 0:(ncol(evolucao)-1))
      print("Evolução das aproximações:")
      print(evolucao)
      
      return(x)
    }
    
    iter <- iter + 1
  }
  
  cat(paste("\nO método não convergiu após", max_iter, "iterações.\n"))
  return(x)
}

# --- Exemplo de uso (Q4 da Prova) ---
A_q4 <- matrix(c(
  10, -2, 0, -1,
   1,  5, 2,  0,
   0,  3, 5, -2,
   0,  0, 3,  7
), nrow = 4, byrow = TRUE)

b_q4 <- c(11, 5, 12, -1)

# Chute inicial da prova
x0_q4 <- c(1, 1, 1, 1)

solucao_seidel <- gauss_seidel(A_q4, b_q4, x0 = x0_q4, tol = 0.001)

cat("\nSolução (Seidel):\n")
print(solucao_seidel)
# Resultado esperado (da prova): 1, 0, 2, -1
