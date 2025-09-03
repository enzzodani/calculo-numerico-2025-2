source('fpoly.R')

metodo_bissec_i_rec <- function(coefs, a, b, epsilon) {
  
  if (fpoly(coefs, a) * fpoly(coefs, b) >= 0) {
    stop("O intervalo [a, b] não é válido! f(a) e f(b) devem ter sinais opostos.")
  }
  
  
  if ((b - a) < epsilon) {
    return((a + b) / 2)
  } 
  
  x_medio <- (a + b) / 2
  
  if (fpoly(coefs, x_medio) == 0) {
    return(x_medio)
  }
  
  if (fpoly(coefs, a) * fpoly(coefs, x_medio) < 0) {
    return(metodo_bissec_i_rec(coefs, a, x_medio, epsilon))
  } else {
    return(metodo_bissec_i_rec(coefs, x_medio, b, epsilon))
  }
}

# Nossos coeficientes
meus_coefs <- c(log10(x),-1)

# Chama a versão recursiva
raiz_recursiva <- metodo_bissec_i_rec(coefs = meus_coefs, a = 0, b = 1, epsilon = 0.001)

# Mostra o resultado
print(raiz_recursiva)

