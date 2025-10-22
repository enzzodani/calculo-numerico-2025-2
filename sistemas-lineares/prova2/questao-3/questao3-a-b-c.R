# --- Funções Genéricas: Fatoração de Cholesky ---
#
# Este conjunto de funções resolve um sistema Ax = b
# pelo método de Cholesky.
# ----------------------------------------------------

# 1. Função de Fatoração (A = G * G^T)
fatoracao_cholesky <- function(A) {
  n <- nrow(A)
  G <- matrix(0, nrow = n, ncol = n)
  
  for (j in 1:n) {
    # --- Cálculo do elemento da diagonal G[j,j] ---
    soma_diag <- 0
    if (j > 1) {
      for (k in 1:(j - 1)) {
        soma_diag <- soma_diag + G[j, k]^2
      }
    }
    # Checagem de estabilidade (A[j,j] - soma_diag deve ser > 0)
    termo_diag <- A[j, j] - soma_diag
    if (termo_diag <= 0) {
      stop("A matriz não é positiva definida.")
    }
    G[j, j] <- sqrt(termo_diag)
    
    # --- Cálculo dos elementos abaixo da diagonal na coluna j ---
    if (j < n) {
      for (i in (j + 1):n) {
        soma_off_diag <- 0
        if (j > 1) {
          for (k in 1:(j - 1)) {
            soma_off_diag <- soma_off_diag + G[i, k] * G[j, k]
          }
        }
        G[i, j] <- (A[i, j] - soma_off_diag) / G[j, j]
      }
    }
  }
  return(G)
}

# 2. Função de Substituição Direta (Resolve Ly = b)
forward_substitution <- function(L, b) {
  n <- nrow(L)
  y <- numeric(n)
  
  for (i in 1:n) {
    soma <- 0
    if (i > 1) {
      for (j in 1:(i - 1)) {
        soma <- soma + L[i, j] * y[j]
      }
    }
    y[i] <- (b[i] - soma) / L[i, i]
  }
  return(y)
}

# 3. Função de Retrosubstituição (Resolve Ux = y)
back_substitution <- function(U, y) {
  n <- nrow(U)
  x <- numeric(n)
  
  for (i in n:1) {
    soma <- 0
    if (i < n) {
      for (j in (i + 1):n) {
        soma <- soma + U[i, j] * x[j]
      }
    }
    x[i] <- (y[i] - soma) / U[i, i]
  }
  return(x)
}


A_q3 <- matrix(c(
  6, 2, 1, 0, 0,
  2, 5, 2, 1, 0,
  1, 2, 6, 2, 1,
  0, 1, 2, 5, 2,
  0, 0, 1, 2, 4
), nrow = 5, byrow = TRUE)

b_q3 <- c(-52, 4, -5, 18, 8)

# Passo (a): Apresente G e G^T
cat("\n--- (a) Fatoração de Cholesky ---\n")
G <- fatoracao_cholesky(A_q3)
Gt <- t(G)

cat("Matriz G:\n")
print(G)
cat("\nMatriz G^T:\n")
print(Gt)

# Passo (b): Resolva Gy = b
cat("\n--- (b) Solução de Gy = b (vetor y) ---\n")
y <- forward_substitution(G, b_q3)
print(y)

# Passo (c): Resolva G^T x = y
cat("\n--- (c) Solução de G^T x = y (vetor x) ---\n")
x <- back_substitution(Gt, y)
print(x)
