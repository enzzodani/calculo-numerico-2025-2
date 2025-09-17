f_q2 <- function(x) {
  return(x^4 - 2*x^3 - 13*x^2 + 14*x + 24)
}

for (i in -10:10) {
  print(paste("A função no ponto ", i, " vale ", f_q2(i)))
  print(paste("A função no ponto ", i+0.5, " vale ", f_q2(i+0.5)))
  sinal <- f_q2(i)*f_q2(i+0.5)

  if (sinal == 0) {
    print(paste("A raiz eh ", i, " ou ", i+0.5))
  } else {
    if (sinal < 0) {
      print(paste("A raiz esta entre ", i, " e ", i+0.5))
    }
  }
}

