f_q2 <- function(x) {
  return(x^3 - 6*x^2 + 4*x + 12)
}

for (i in -10:10) {
  sinal <- f_q2(i)*f_q2(i+1)

  if (sinal == 0) {
    print(paste("A raiz eh ", i, " ou ", i+1))
  } else {
    if (sinal < 0) {
      print(paste("A raiz esta entre ", i, " e ", i+1))
    }
  }
}

