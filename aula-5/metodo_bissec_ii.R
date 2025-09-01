source('fpoly.R')

metodo_bissec_ii_rec <- function(coefs, a, b, epsilon) {
  if (fpoly(coefs, a) * fpoly(coefs, b) >= 0) {
    stop("O intervalo [a, b] não é válido!")
  }
  
  x_medio <- (a + b) / 2
  
  if (abs(fpoly(coefs, x_medio)) < epsilon) {
    return(x_medio)
  }
  
    if (fpoly(coefs, a) * fpoly(coefs, x_medio) < 0) {
    return(metodo_bissec_ii_rec(coefs, a, x_medio, epsilon))
  } else {
    return(metodo_bissec_ii_rec(coefs, x_medio, b, epsilon))
  }
}

# Nossos coeficientes
meus_coefs <- c(3, -9, 0, 1)

# Chama a versão recursiva
raiz_recursiva <- metodo_bissec_ii_rec(coefs = meus_coefs, a = 0, b = 1, epsilon = 0.001)

# Mostra o resultado
print(raiz_recursiva)

