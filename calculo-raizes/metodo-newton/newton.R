# --- Método de Newton Recursivo ---

# @param f A função cuja raiz queremos encontrar.
# @param df A função derivada de f.
# @param x0 O chute inicial para a raiz.
# @param erro A tolerância para o critério de parada |f(x)| < erro.
# @param k (parâmetro interno) Contador de iterações para exibição.
metodo_newton <- function(f, df, x0, erro_f, erro_x, k = 0) {
  
  # Imprime o cabeçalho na primeira chamada
  if (k == 0) {
    cat("k\t x_k\t\t x_k+1\t\t |f(x_k)|\t\t |f(x_k+1)|\n")
    cat("-----------------------------------------------------------------------------------\n")
  }
  
  # Calcula o valor da função no ponto atual
  fx <- f(x0)
  
  # --- Bloco da Recursão ---
  # Calcula o próximo x usando a fórmula de Newton
  x_prox <- x0 - fx / df(x0)

  fx_prox <- f(x_prox)
  
  # Imprime a evolução da aproximação a cada passo
  cat(sprintf("%d\t %.8f\t %.8e\t %.8e\t %.8e\n", k, x0, x_prox, abs(fx), abs(fx_prox)))

  # --- Bloco do Critério de Parada ---
  if (abs(fx_prox) < erro_f && abs(x_prox - x0) < erro_x) {
    cat("\nCritério de parada atingido: |f(x_k+1)| <", erro_f, " e |x_(k+1) - x_k| <", erro_x, "\n")
    return(x0)
  }
  
  # Chama a função novamente com o novo chute e incrementa o contador
  return(metodo_newton(f, df, x_prox, erro_f, erro_x, k + 1))
}

# Exemplo de uso

f_q2 <- function(x) {
  return(x^4 - 2*x^3 - 13*x^2 + 14*x + 24)
}

df_q2 <- function(x) {
  return(4*x^3 - 6*x^2 - 26*x + 14) # Derivada de f_q2
}

raiz_aproximada <- metodo_newton(
  f = f_q2,
  df = df_q2,
  x0 = -5,
  erro_f = 1e-5,
  erro_x = 1e-2
)

print(paste("A raiz encontrada foi:", raiz_aproximada))
