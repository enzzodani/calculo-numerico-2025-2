# --- Script para Questão 4a: Verificação de Dominância Diagonal ---

# 1. Definir a Matriz A e o Vetor b da Questão 4
A_q4 <- matrix(c(
  10, -2, 0, -1, 0,
   1, 5, 2, 0, 1,
   0, 3, 6, -2, 0,
   0, 0, 3, 7, 2,
   1, -1, 0, 5, 10
), nrow = 5, byrow = TRUE) 

b_q4 <- c(-31, -3, -32, 12, 102)

cat("--- (a) Verificação de Dominância Diagonal ---\n")
cat("Matriz A:\n")
print(A_q4)

# Função para verificar a dominância diagonal
verificar_dominancia <- function(A) {
  n <- nrow(A)
  alphas <- numeric(n) # Vetor para armazenar os alphas
  is_dominant <- TRUE
  
  cat("\n--- Verificação Linha a Linha ---\n")
  
  # Loop por cada linha i
  for (i in 1:n) {
    diag_element <- abs(A[i, i])
    sum_off_diag <- 0
    
    # Loop por cada coluna j para somar os elementos fora da diagonal
    for (j in 1:n) {
      if (i != j) {
        sum_off_diag <- sum_off_diag + abs(A[i, j])
      }
    }
    
    # Calcula o alpha_i (como na resolução da prova) 
    alphas[i] <- sum_off_diag / diag_element
    
    # Imprime a verificação da linha
    cat(paste0("\nLinha ", i, ":\n"))
    cat(paste0("  |A[", i, ",", i, "]| = ", diag_element, "\n"))
    cat(paste0("  Soma dos outros elementos (abs): ", sum_off_diag, "\n"))
    cat(paste0("  alpha", i, " = ", sum_off_diag, " / ", diag_element, " = ", alphas[i], "\n"))
    
    # Verifica a condição
    if (diag_element < sum_off_diag) {
      cat("  CONDIÇÃO FALHA: |A[i,i]| < Soma dos outros\n")
      is_dominant <- FALSE
    } else {
      cat("  CONDIÇÃO OK: |A[i,i]| >= Soma dos outros\n")
    }
  }
  
  # Calcula o alpha máximo (como na prova) 
  alpha_max <- max(alphas)
  cat(paste0("\nalpha (máximo) = max(alphas) = ", alpha_max, "\n"))
  
  # Imprime a conclusão final
  cat("\n--- Conclusão ---\n")
  if (is_dominant) {
    cat("A matriz É diagonal dominante (critério de linhas).\n")
    if (alpha_max < 1) {
      cat("A matriz também é ESTRITAMENTE diagonal dominante.\n")
    } else {
      cat("A dominância não é estrita (pelo menos um alpha = 1).\n")
    }
  } else {
    cat("A matriz NÃO é diagonal dominante (critério de linhas).\n")
  }
  
  return(is_dominant)
}

# Executa a verificação
verificar_dominancia(A_q4)
