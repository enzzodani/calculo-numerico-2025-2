f_q2 <- function(x) {
  return(exp(x/2)-5)
}

for (i in -10:10) {
  print(paste("A função no ponto ", i, " vale ", f_q2(i)))
  print(paste("A função no ponto ", i+1, " vale ", f_q2(i+1)))
  sinal <- f_q2(i)*f_q2(i+1)

  if (sinal == 0) { # Se for 0, entao a raiz é um dos pontos
    print(paste("-----------------------------------------------"))
    print(paste("A raiz eh ", i, " ou ", i+1))
    print(paste("-----------------------------------------------"))
  } else {
    if (sinal < 0) { # Se o sinal for negativo, de acordo com bolzano, a raiz esta entre os intervalos
      print(paste("-----------------------------------------------"))
      print(paste("A raiz esta entre ", i, " e ", i+1))
      print(paste("-----------------------------------------------"))
    }
  }
}


