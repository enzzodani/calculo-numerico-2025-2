# Vetor com os elementos
A_v <- c(3, -2, 1,
         1, 2, -1,
         2, 1, 3)

B_v <- c(9, -5, 6)

# Matrizes iniciais
A <- matrix(A_v, nrow = 3, byrow = TRUE)
B <- matrix(B_v, nrow = 3, byrow = FALSE)

cat("Matriz A original:\n")
print(A)
cat("\nVetor B original:\n")
print(B)

# --- PASSO 1: Zerar a primeira coluna ---
m21 <- A[2, 1] / A[1, 1]
m31 <- A[3, 1] / A[1, 1]

M <- c(1, 0, 0, -m21, 1, 0, -m31, 0, 1)
M0 <- matrix(M, nrow = 3, byrow = TRUE)

# Aplica a transformação
A1 <- M0 %*% A
M0b <- M0 %*% B

cat("\nMatriz A após a primeira eliminação (A1):\n")
print(A1)

# --- PASSO 2: Zerar a segunda coluna ---
m32 <- A1[3, 2] / A1[2, 2]
M1l <- c(1, 0, 0, 0, 1, 0, 0, -m32, 1)
M1 <- matrix(M1l, nrow = 3, byrow = TRUE)

# Aplicar M1 em A1 para obter U e em M0b para obter Y
U <- M1 %*% A1
Y <- M1 %*% M0b

cat("\nMatriz triangular superior final (U):\n")
print(U)
cat("\nVetor transformado final (Y):\n")
print(Y)


x <- numeric(3) # Vetor para armazenar a solução x1, x2, x3

# Resolvendo para x3
x[3] <- Y[3] / U[3, 3]

# Resolvendo para x2
x[2] <- (Y[2] - U[2, 3] * x[3]) / U[2, 2] 

# Resolvendo para x1
x[1] <- round((Y[1] - U[1, 2] * x[2] - U[1, 3] * x[3]) / U[1, 1]) 

cat("\nSolução final (vetor x):\n")
print(x)
