funcao <- function() {
  return(4*cos(x) - exp(x))
}

funcao_derivada <- function() {
  return(-4 * sin(x) - exp(x))
}


metodo_newton <- function(funcao, funcao_derivada, ponto, e) {
  while(funcao(ponto) >= 0.01) {
    divisao <- funcao(x)/funcao_derivada(x)

    ponto <- ponto - divisao
  }

  return(ponto)
}

metodo_newton(funcao(), funcao_derivada(), )
