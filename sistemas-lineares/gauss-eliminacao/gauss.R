# --- Função Genérica: Eliminação de Gauss (Modo Detalhado) ---
#
# Resolve o sistema Ax = b e MOSTRA as matrizes
# intermediárias A(k)|b(k) a cada passo,
# conforme solicitado na prova.
#
# Argumentos:
#   A: Matriz de coeficientes
#   b: Vetor de termos independentes
#
# Retorna:
#   x: Vetor da solução
# -----------------------------------------------

solve_gauss_verbose <- function(A, b) {
  n <- nrow(A)
  
  # 1. Criar a matriz aumentada [A|b]
  Ab <- cbind(A, b)
  
  # --- Início da Parte (a) da Questão ---
  cat("--- (a) Apresentação das Matrizes Intermediárias ---\n")
  
  # Imprime a matriz inicial A(0)|b(0)
  cat("\n--- Matriz A(0)|b(0) ---\n")
  print(Ab)
  
  # 2. Fase de Eliminação (Triangularização)
  for (k in 1:(n - 1)) {
    # k é a coluna do pivô
    
    # Opcional: Checagem de pivô (se for 0, o método falha sem pivoteamento)
    if (Ab[k, k] == 0) {
      stop(paste("Pivô na coluna", k, "é zero. O método requer pivoteamento."))
    }
    
    for (i in (k + 1):n) {
      # i é a linha que será zerada
      
      # Calcula o multiplicador
      m <- Ab[i, k] / Ab[k, k]
      
      # Atualiza a linha i: L_i <- L_i - m * L_k
      Ab[i, ] <- Ab[i, ] - m * Ab[k, ]
    }
    
    # --- IMPRESSÃO INTERMEDIÁRIA ---
    # Imprime a matriz A(k)|b(k) ao final de cada passo k
    cat(paste0("\n--- Matriz A(", k, ")|b(", k, ") --- (Após zerar coluna ", k, ")\n"))
    print(Ab)
  }
  
  # A matriz final é A(n-1)|b(n-1)
  # A impressão acima já mostra a matriz final A(3)|b(3) no caso de n=4
  
  # --- Início da Parte (b) da Questão ---
  cat("\n\n--- (b) Resolução do sistema A(", n-1, ")x = b(", n-1, ") ---\n")
  
  # 3. Fase de Retrosubstituição
  
  # Cria o vetor de solução x
  x <- numeric(n)
  
  # Resolve para o último x[n]
  # (Ab[n, n] é o último pivô, Ab[n, n+1] é o último termo do vetor b)
  x[n] <- Ab[n, n + 1] / Ab[n, n]
  
  # Resolve para os demais x[i] (de n-1 até 1)
  if (n > 1) {
    for (i in (n - 1):1) {
      soma <- 0
      for (j in (i + 1):n) {
        soma <- soma + Ab[i, j] * x[j]
      }
      x[i] <- (Ab[i, n + 1] - soma) / Ab[i, i]
    }
  }
  
  cat("Sistema final (triangularizado):\n")
  print(Ab)
  
  cat("\nSolução (x):\n")
  return(x)
}

# --- Exemplo de uso (Q1 da Prova) ---
# Dados da Q1
A_q1 <- matrix(c(
   2, -2, -1,  3,
   1, -2,  2,  2,
  -5, -2, -8, -10,
  -2,  3,  2, -1
), nrow = 4, byrow = TRUE)

b_q1 <- c(-7, -14, 8, -1)

# Chamar a nova função "detalhada"
solucao_gauss_q1 <- solve_gauss_verbose(A_q1, b_q1)

print(solucao_gauss_q1)
# Resultado esperado (da prova): 10, 5, -1, -6
