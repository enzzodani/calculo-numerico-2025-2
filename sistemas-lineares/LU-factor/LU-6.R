# --- Script de Fatoração LU (6x6) ---
#
# Instrução: Edite as matrizes A e B abaixo com os
# valores do seu problema.
# -----------------------------------------------

# --- DEFINA SEU SISTEMA AQUI ---
# Exemplo: Matriz 6x6 aleatória
A_v <- c(
   2,  1,  0,  0,  0,  0,
  -1,  3,  1,  0,  0,  0,
   0, -1,  4,  1,  0,  0,
   0,  0, -1,  5,  1,  0,
   0,  0,  0, -1,  6,  1,
   0,  0,  0,  0, -1,  7
)
B_v <- c(1, 2, 3, 4, 5, 6)
# --------------------------------

# Matrizes iniciais
A <- matrix(A_v, nrow = 6, byrow = TRUE)
B <- matrix(B_v, nrow = 6, byrow = FALSE)
n <- 6

cat("--- Matrizes Originais ---\n")
cat("Matriz A (A0):\n"); print(A)
cat("\nVetor B:\n"); print(B)

# --- PASSO 1: Zerar a primeira coluna (k=1) ---
m21 <- A[2, 1] / A[1, 1]
m31 <- A[3, 1] / A[1, 1]
m41 <- A[4, 1] / A[1, 1]
m51 <- A[5, 1] / A[1, 1]
m61 <- A[6, 1] / A[1, 1]

M0t <- c(1, 0, 0, 0, 0, 0,
        -m21, 1, 0, 0, 0, 0,
        -m31, 0, 1, 0, 0, 0,
        -m41, 0, 0, 1, 0, 0,
        -m51, 0, 0, 0, 1, 0,
        -m61, 0, 0, 0, 0, 1)
M0 <- matrix(M0t, nrow = n, byrow = TRUE)

A1 <- M0 %*% A
B1 <- M0 %*% B

cat("\n--- Após Passo 1 ---\n")
cat("M0:\n"); print(M0)
cat("A1:\n"); print(A1)

# --- PASSO 2: Zerar a segunda coluna (k=2) ---
m32 <- A1[3, 2] / A1[2, 2]
m42 <- A1[4, 2] / A1[2, 2]
m52 <- A1[5, 2] / A1[2, 2]
m62 <- A1[6, 2] / A1[2, 2]

M1t <- c(1, 0, 0, 0, 0, 0,
         0, 1, 0, 0, 0, 0,
         0, -m32, 1, 0, 0, 0,
         0, -m42, 0, 1, 0, 0,
         0, -m52, 0, 0, 1, 0,
         0, -m62, 0, 0, 0, 1)
M1 <- matrix(M1t, nrow = n, byrow = TRUE)

A2 <- M1 %*% A1
B2 <- M1 %*% B1

cat("\n--- Após Passo 2 ---\n")
cat("M1:\n"); print(M1)
cat("A2:\n"); print(A2)

# --- PASSO 3: Zerar a terceira coluna (k=3) ---
m43 <- A2[4, 3] / A2[3, 3]
m53 <- A2[5, 3] / A2[3, 3]
m63 <- A2[6, 3] / A2[3, 3]

M2t <- c(1, 0, 0, 0, 0, 0,
         0, 1, 0, 0, 0, 0,
         0, 0, 1, 0, 0, 0,
         0, 0, -m43, 1, 0, 0,
         0, 0, -m53, 0, 1, 0,
         0, 0, -m63, 0, 0, 1)
M2 <- matrix(M2t, nrow = n, byrow = TRUE)

A3 <- M2 %*% A2
B3 <- M2 %*% B2

cat("\n--- Após Passo 3 ---\n")
cat("M2:\n"); print(M2)
cat("A3:\n"); print(A3)

# --- PASSO 4: Zerar a quarta coluna (k=4) ---
m54 <- A3[5, 4] / A3[4, 4]
m64 <- A3[6, 4] / A3[4, 4]

M3t <- c(1, 0, 0, 0, 0, 0,
         0, 1, 0, 0, 0, 0,
         0, 0, 1, 0, 0, 0,
         0, 0, 0, 1, 0, 0,
         0, 0, 0, -m54, 1, 0,
         0, 0, 0, -m64, 0, 1)
M3 <- matrix(M3t, nrow = n, byrow = TRUE)

A4 <- M3 %*% A3
B4 <- M3 %*% B3

cat("\n--- Após Passo 4 ---\n")
cat("M3:\n"); print(M3)
cat("A4:\n"); print(A4)

# --- PASSO 5: Zerar a quinta coluna (k=5) ---
m65 <- A4[6, 5] / A4[5, 5]

M4t <- c(1, 0, 0, 0, 0, 0,
         0, 1, 0, 0, 0, 0,
         0, 0, 1, 0, 0, 0,
         0, 0, 0, 1, 0, 0,
         0, 0, 0, 0, 1, 0,
         0, 0, 0, 0, -m65, 1)
M4 <- matrix(M4t, nrow = n, byrow = TRUE)

# U é a matriz triangular superior final
# Y é o vetor de resultados final (solução de Ly = b)
U <- M4 %*% A4
Y <- M4 %*% B4

cat("\n--- Após Passo 5 (Final) ---\n")
cat("M4:\n"); print(M4)
cat("U (A5):\n"); print(U)
cat("Y (B5):\n"); print(Y)

# --- Montagem da Matriz L ---
L <- matrix(0, nrow = n, ncol = n)
diag(L) <- 1
L[2, 1] <- m21; L[3, 1] <- m31; L[4, 1] <- m41; L[5, 1] <- m51; L[6, 1] <- m61
L[3, 2] <- m32; L[4, 2] <- m42; L[5, 2] <- m52; L[6, 2] <- m62
L[4, 3] <- m43; L[5, 3] <- m53; L[6, 3] <- m63
L[5, 4] <- m54; L[6, 4] <- m64
L[6, 5] <- m65

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

x[6] <- Y[6] / U[6, 6]
x[5] <- (Y[5] - U[5, 6] * x[6]) / U[5, 5]
x[4] <- (Y[4] - U[4, 5] * x[5] - U[4, 6] * x[6]) / U[4, 4]
x[3] <- (Y[3] - U[3, 4] * x[4] - U[3, 5] * x[5] - U[3, 6] * x[6]) / U[3, 3]
x[2] <- (Y[2] - U[2, 3] * x[3] - U[2, 4] * x[4] - U[2, 5] * x[5] - U[2, 6] * x[6]) / U[2, 2]
x[1] <- (Y[1] - U[1, 2] * x[2] - U[1, 3] * x[3] - U[1, 4] * x[4] - U[1, 5] * x[5] - U[1, 6] * x[6]) / U[1, 1]

cat("\n--- (c) Solução de Ux = y (vetor x) ---\n")
print(x)
