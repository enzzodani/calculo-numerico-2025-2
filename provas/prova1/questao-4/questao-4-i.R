f_q2 <- function(x) {
  return(5000*((1+x)^24) - 8000)
}

for (i in -1:1) {
  print(paste("A função no ponto ", i, " vale ", f_q2(i)))
  print(paste("A função no ponto ", i+0.1, " vale ", f_q2(i+0.1)))
  sinal <- f_q2(i)*f_q2(i+0.1)

  if (sinal == 0) { # Se for 0, a raiz esta em um dos pontos
    print(paste("A raiz eh ", i, " ou ", i+0.1))
  } else {
    if (sinal < 0) { # Se o sinal for negativo, de acordo com bolzano, a raiz esta entre os intervalos
      print(paste("A raiz esta entre ", i, " e ", i+0.1))
    }
  }
}


