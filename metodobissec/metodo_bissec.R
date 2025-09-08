# --- Método da Bissecção Unificado e Recursivo ---

# @param minha_funcao A função cuja raiz queremos encontrar.
# @param a O limite inferior do intervalo.
# @param b O limite superior do intervalo.
# @param erro A tolerância para o critério de parada.
# @param criterio O critério de parada a ser usado: "intervalo" ou "funcao".
# @param k (parâmetro interno) Contador de iterações para exibição.
metodo_bisseccao <- function(minha_funcao, a, b, erro, criterio = "intervalo", k = 0) {
  
  # Imprime o cabeçalho na primeira chamada
  if (k == 0) {
    cat("k\t a\t\t b\t\t x_medio\t f(x_medio)\n")
    cat("------------------------------------------------------------------\n")
  }
  
  # Calcula o ponto médio
  x_medio <- (a + b) / 2
  fx_medio <- minha_funcao(x_medio)
  
  # Imprime a evolução da aproximação a cada passo
  cat(sprintf("%d\t %.8f\t %.8f\t %.8f\t %.8e\n", k, a, b, x_medio, fx_medio))
  
  # --- Bloco do Critério de Parada ---
  if (criterio == "funcao") {
    if (abs(fx_medio) < erro) {
      cat("\nCritério de parada atingido: |f(x_medio)| <", erro, "\n")
      return(x_medio)
    }
  } else { # Critério é "intervalo"
    if ((b - a) < erro) {
      cat("\nCritério de parada atingido: (b - a) <", erro, "\n")
      return(x_medio)
    }
  }
  
  # --- Bloco da Recursão ---
  if (minha_funcao(a) * fx_medio < 0) {
    return(metodo_bisseccao(minha_funcao, a, x_medio, erro, criterio, k + 1))
  } else {
    return(metodo_bisseccao(minha_funcao, x_medio, b, erro, criterio, k + 1))
  }
}


# Exemplo prova 
f_q4 <- function(x) {
  return(2*x^3 - 10*x^2 + 4*x + 12)
}

raiz_aprox <- metodo_bisseccao(f_q4, a = 4, b = 4.5, erro = 0.001, criterio = "funcao")

print(paste("A raiz encontrada foi: ", raiz_aprox))
