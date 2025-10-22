# --- Script de Fatoração LU (3x3) ---
#
# Instrução: Edite as matrizes A e B abaixo com os
# valores do seu problema.
# -----------------------------------------------

# --- DEFINA SEU SISTEMA AQUI ---
A_v <- c(3, 2, 4,
         1, 1, 2,
         4, 3, -2)
B_v <- c(1, 2, 3)
# --------------------------------

# Matrizes iniciais
A <- matrix(A_v, nrow = 3, byrow = TRUE)
B <- matrix(B_v, nrow = 3, byrow = FALSE)
n <- 3

cat("--- Matrizes Originais ---\n")
cat("Matriz A (A0):\n"); print(A)
cat("\nVetor B:\n"); print(B)

# --- PASSO 1: Zerar a primeira coluna (k=1) ---
m21 <- A[2, 1] / A[1, 1]
m31 <- A[3, 1] / A[1, 1]

M0t <- c(1, 0, 0, 
        -m21, 1, 0,
        -m31, 0, 1)
M0 <- matrix(M0t, nrow = n, byrow = TRUE)

A1 <- M0 %*% A
B1 <- M0 %*% B

cat("\n--- Após Passo 1 ---\n")
cat("M0:\n"); print(M0)
cat("A1:\n"); print(A1)


# --- PASSO 2: Zerar a segunda coluna (k=2) ---
m32 <- A1[3, 2] / A1[2, 2]

M1t <- c(1, 0, 0, 
         0, 1, 0,
         0, -m32, 1)
M1 <- matrix(M1t, nrow = n, byrow = TRUE)

# U é a matriz triangular superior final
# Y é o vetor de resultados final (solução de Ly = b)
U <- M1 %*% A1
Y <- M1 %*% B1

cat("\n--- Após Passo 2 (Final) ---\n")
cat("M1:\n"); print(M1)
cat("U (A2):\n"); print(U)
cat("Y (B2):\n"); print(Y)


# --- Montagem da Matriz L ---
L <- matrix(0, nrow = n, ncol = n)
diag(L) <- 1
L[2, 1] <- m21
L[3, 1] <- m31
L[3, 2] <- m32

cat("\n--- Matrizes L e U ---\n")
cat("L:\n"); print(L)
cat("U:\n"); print(U)
# Verificação (L %*% U deve ser igual a A)
cat("Verificação (L %*% U):\n"); print(L %*% U)

# --- Resolução do Sistema ---

# (b) Solução de Ly = b
# O vetor Y calculado (Y = M1 %*% M0 %*% B) já é a solução de Ly=b
cat("\n--- (b) Solução de Ly = b (vetor Y) ---\n")
print(Y)

# (c) Solução de Ux = y (Retrosubstituição)
x <- numeric(n)

x[3] <- Y[3] / U[3, 3]
x[2] <- (Y[2] - U[2, 3] * x[3]) / U[2, 2]
x[1] <- (Y[1] - U[1, 2] * x[2] - U[1, 3] * x[3]) / U[1, 1]

cat("\n--- (c) Solução de Ux = y (vetor x) ---\n")
print(x)
