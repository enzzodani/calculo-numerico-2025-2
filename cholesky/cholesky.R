
at <- c(1,2,2,2,5,1,2,1,14)
bt <- c(-3,-1,-23)

A <- matrix(at, nrow = 3, byrow = TRUE)
b <- matrix(bt, nrow = 3, byrow = FALSE)


resolution_lower <- function(A, b, n) {

  xt <- c(0,0,0)

  X <- matrix(xt, nrow = 3, byrow = FALSE)

  X[1] <- b[1] / A[1,1]

  for (k in 2:n) {
    soma <- 0
    for (j in 1:(k-1)) {
      soma <- soma + A[k,j]*X[j]
    }
    X[k] <- (b[k] - soma)/A[k,k]
  }

  return(X)
}

y <- resolution_lower(A,b,3)

fatoracao_cholesky <- function(A, n) {
  # Cria uma matriz L preenchida com zeros
  L <- matrix(0, nrow = n, ncol = n)

  for (j in 1:n) {
    # --- Cálculo do elemento da diagonal L[j,j] ---
    soma_diag <- 0
    if (j > 1) {
      for (k in 1:(j-1)) {
        soma_diag <- soma_diag + L[j,k]^2
      }
    }
    L[j,j] <- sqrt(A[j,j] - soma_diag)

    # --- Cálculo dos elementos abaixo da diagonal na coluna j ---
    if (j < n) {
      for (i in (j+1):n) {
        soma_off_diag <- 0
        if (j > 1) {
          for (k in 1:(j-1)) {
            soma_off_diag <- soma_off_diag + L[i,k]*L[j,k]
          }
        }
        L[i,j] <- (A[i,j] - soma_off_diag) / L[j,j]
      }
    }
  }
  return(L)
}
