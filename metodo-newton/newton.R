# --- Método de Newton Recursivo ---

# @param f A função cuja raiz queremos encontrar.
# @param df A função derivada de f.
# @param x0 O chute inicial para a raiz.
# @param erro A tolerância para o critério de parada |f(x)| < erro.
# @param k (parâmetro interno) Contador de iterações para exibição.
metodo_newton <- function(f, df, x0, erro, k = 0) {
  
  # Imprime o cabeçalho na primeira chamada
  if (k == 0) {
    cat("k\t x_k\t\t f(x_k)\n")
    cat("--------------------------------------------\n")
  }
  
  # Calcula o valor da função no ponto atual
  fx <- f(x0)
  
  # Imprime a evolução da aproximação a cada passo
  cat(sprintf("%d\t %.8f\t %.8e\n", k, x0, fx))
  
  # --- Bloco do Critério de Parada ---
  if (abs(fx) < erro) {
    cat("\nCritério de parada atingido: |f(x_k)| <", erro, "\n")
    return(x0)
  }
  
  # --- Bloco da Recursão ---
  # Calcula o próximo x usando a fórmula de Newton
  x_prox <- x0 - fx / df(x0)
  
  # Chama a função novamente com o novo chute e incrementa o contador
  return(metodo_newton(f, df, x_prox, erro, k + 1))
}

# Exemplo de uso

f_q2 <- function(x) {
  return(x^3 - 6*x^2 + 4*x + 12)
}

df_q2 <- function(x) {
  return(3*x^2 - 12*x + 4) # Derivada de f_q2
}

raiz_aproximada <- metodo_newton(
  f = f_q2,
  df = df_q2,
  x0 = -2,
  erro = 1e-6
)

print(paste("A raiz encontrada foi:", raiz_aproximada))
