# --- Script de Fatoração LU (4x4) ---
#
# Instrução: Edite as matrizes A e B abaixo com os
# valores do seu problema.
# -----------------------------------------------

# --- DEFINA SEU SISTEMA AQUI ---
# Exemplo: Q1 da Prova
A_v <- c(
   2, -2, -1,  3,
   1, -2,  2,  2,
  -5, -2, -8, -10,
  -2,  3,  2, -1
)
B_v <- c(-7, -14, 8, -1)
# --------------------------------

# Matrizes iniciais
A <- matrix(A_v, nrow = 4, byrow = TRUE)
B <- matrix(B_v, nrow = 4, byrow = FALSE)
n <- 4

cat("--- Matrizes Originais ---\n")
cat("Matriz A (A0):\n"); print(A)
cat("\nVetor B:\n"); print(B)

# --- PASSO 1: Zerar a primeira coluna (k=1) ---
m21 <- A[2, 1] / A[1, 1]
m31 <- A[3, 1] / A[1, 1]
m41 <- A[4, 1] / A[1, 1]

M0t <- c(1, 0, 0, 0,
        -m21, 1, 0, 0,
        -m31, 0, 1, 0,
        -m41, 0, 0, 1)
M0 <- matrix(M0t, nrow = n, byrow = TRUE)

A1 <- M0 %*% A
B1 <- M0 %*% B

cat("\n--- Após Passo 1 ---\n")
cat("M0:\n"); print(M0)
cat("A1:\n"); print(A1)

# --- PASSO 2: Zerar a segunda coluna (k=2) ---
m32 <- A1[3, 2] / A1[2, 2]
m42 <- A1[4, 2] / A1[2, 2]

M1t <- c(1, 0, 0, 0,
         0, 1, 0, 0,
         0, -m32, 1, 0,
         0, -m42, 0, 1)
M1 <- matrix(M1t, nrow = n, byrow = TRUE)

A2 <- M1 %*% A1
B2 <- M1 %*% B1

cat("\n--- Após Passo 2 ---\n")
cat("M1:\n"); print(M1)
cat("A2:\n"); print(A2)

# --- PASSO 3: Zerar a terceira coluna (k=3) ---
m43 <- A2[4, 3] / A2[3, 3]

M2t <- c(1, 0, 0, 0,
         0, 1, 0, 0,
         0, 0, 1, 0,
         0, 0, -m43, 1)
M2 <- matrix(M2t, nrow = n, byrow = TRUE)

# U é a matriz triangular superior final
# Y é o vetor de resultados final (solução de Ly = b)
U <- M2 %*% A2
Y <- M2 %*% B2

cat("\n--- Após Passo 3 (Final) ---\n")
cat("M2:\n"); print(M2)
cat("U (A3):\n"); print(U)
cat("Y (B3):\n"); print(Y)

# --- Montagem da Matriz L ---
L <- matrix(0, nrow = n, ncol = n)
diag(L) <- 1
L[2, 1] <- m21
L[3, 1] <- m31
L[4, 1] <- m41
L[3, 2] <- m32
L[4, 2] <- m42
L[4, 3] <- m43

cat("\n--- Matrizes L e U ---\n")
cat("L:\n"); print(L)
cat("U:\n"); print(U)
# Verificação (L %*% U deve ser igual a A)
cat("Verificação (L %*% U):\n"); print(L %*% U)

# --- Resolução do Sistema ---

# (b) Solução de Ly = b
cat("\n--- (b) Solução de Ly = b (vetor Y) ---\n")
print(Y)

# (c) Solução de Ux = y (Retrosubstituição)
x <- numeric(n)

x[4] <- Y[4] / U[4, 4]
x[3] <- (Y[3] - U[3, 4] * x[4]) / U[3, 3]
x[2] <- (Y[2] - U[2, 3] * x[3] - U[2, 4] * x[4]) / U[2, 2]
x[1] <- (Y[1] - U[1, 2] * x[2] - U[1, 3] * x[3] - U[1, 4] * x[4]) / U[1, 1]

cat("\n--- (c) Solução de Ux = y (vetor x) ---\n")
print(x)
