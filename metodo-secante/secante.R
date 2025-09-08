# --- Método da Secante Recursivo ---

# @param funcao A função cuja raiz queremos encontrar.
# @param x0 O primeiro chute inicial.
# @param x1 O segundo chute inicial.
# @param erro_f A tolerância para o critério de parada |f(x)|.
# @param erro_x A tolerância para o critério de parada |x_{k+1} - x_k|.
# @param k (parâmetro interno) Contador de iterações para exibição.
metodo_secante <- function(funcao, x0, x1, erro_f, erro_x, k = 0) {
  
  # Imprime o cabeçalho na primeira chamada
  if (k == 0) {
    cat("k\t x_k\t\t |x_{k+1} - x_k|\t f(x_{k+1})\n")
    cat("------------------------------------------------------------------\n")
  }
  
  # Calcula o erro e o valor da função para exibição
  erro_atual_x <- abs(x1 - x0)
  fx1 <- funcao(x1)
  
  # Imprime a evolução
  cat(sprintf("%d\t %.8f\t %.8e\t %.8e\n", k, x1, erro_atual_x, fx1))
  
  # --- Bloco do Critério de Parada ---
  if (abs(fx1) < erro_f || erro_atual_x < erro_x) {
    if (abs(fx1) < erro_f) {
      cat("\nCritério de parada atingido: |f(x_k)| <", erro_f, "\n")
    } else {
      cat("\nCritério de parada atingido: |x_{k+1} - x_k| <", erro_x, "\n")
    }
    return(x1)
  }
  
  # --- Bloco da Recursão ---
  # Fórmula da Secante
  x2 <- (x0 * fx1 - x1 * funcao(x0)) / (fx1 - funcao(x0))
  
  # Chama a função novamente com os novos pontos
  return(metodo_secante(funcao, x1, x2, erro_f, erro_x, k + 1))
}

# Exemplo de uso

f_q3 <- function(x) {
  return(10 * exp(-x) - 1)
}

raiz_aproximada <- metodo_secante(
  funcao = f_q3,
  x0 = 0,
  x1 = 4,
  erro_f = 0,          # Não usamos este critério, então definimos como 0
  erro_x = 0.0001
)

print(paste("A raiz encontrada foi:", raiz_aproximada))
