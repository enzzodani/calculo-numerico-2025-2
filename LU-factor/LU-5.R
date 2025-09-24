# Vetor com os elementos para uma matriz 5x5 
A_v <- c(2, -3, 4, 5, -6,
         1, 2, -1, 3, 4,
         3, -4, 1, 2, -1,
         5, 2, -2, 4, 3,
         -2, 3, 2, -1, 5)

# Vetor de resultados com 5 elementos
B_v <- c(-21, 29, -2, 30, 24)

# Matrizes iniciais 
A <- matrix(A_v, nrow = 5, byrow = TRUE)
B <- matrix(B_v, nrow = 5, byrow = FALSE)

cat("--- Matrizes Originais ---\n")
cat("Matriz A original:\n")
print(A)
cat("\nVetor B original:\n")
print(B)

# --- PASSO 1: Zerar a primeira coluna abaixo do pivô A[1,1] ---
m21 <- A[2,1] / A[1,1]
m31 <- A[3,1] / A[1,1]
m41 <- A[4,1] / A[1,1]
m51 <- A[5,1] / A[1,1]

M0t <- c(1,0,0,0,0, -m21,1,0,0,0, -m31,0,1,0,0, -m41,0,0,1,0, -m51,0,0,0,1)
M0 <- matrix(M0t, nrow = 5, byrow = TRUE) 

A1 <- M0 %*% A
B1 <- M0 %*% B

cat("\n--- Após Passo 1 ---\n")
print(A1)


# --- PASSO 2: Zerar a segunda coluna abaixo do pivô A1[2,2] ---
m32 <- A1[3,2] / A1[2,2]
m42 <- A1[4,2] / A1[2,2]
m52 <- A1[5,2] / A1[2,2]

M1t <- c(
    1, 0, 0, 0, 0,
    0, 1, 0, 0, 0,
    0, -m32, 1, 0, 0,
    0, -m42, 0, 1, 0,
    0, -m52, 0, 0, 1
)
M1 <- matrix(M1t, nrow = 5, byrow = TRUE) 

A2 <- M1 %*% A1
B2 <- M1 %*% B1

cat("\n--- Após Passo 2 ---\n")
print(A2)


# --- PASSO 3: Zerar a terceira coluna abaixo do pivô A2[3,3] ---
m43 <- A2[4,3] / A2[3,3]
m53 <- A2[5,3] / A2[3,3]

M2t <- c(
    1,0,0,0,0,
    0,1,0,0,0,
    0,0,1,0,0,
    0,0,-m43,1,0,
    0,0,-m53,0,1
)
M2 <- matrix(M2t, nrow = 5, byrow = TRUE) 

A3 <- M2 %*% A2
B3 <- M2 %*% B2

cat("\n--- Após Passo 3 ---\n")
print(A3)


# --- PASSO 4: Zerar a quarta coluna abaixo do pivô A3[4,4] ---
m54 <- A3[5,4] / A3[4,4] 

M3t <- c(
    1,0,0,0,0,
    0,1,0,0,0,
    0,0,1,0,0,
    0,0,0,1,0,
    0,0,0,-m54,1
)
M3 <- matrix(M3t, nrow = 5, byrow = TRUE) 

# U é a matriz triangular superior final e Y é o vetor de resultados final
U <- M3 %*% A3 
Y <- M3 %*% B3 

cat("\n--- Resultados Finais da Eliminação ---\n")
cat("Matriz triangular superior final (U):\n")
print(U)
cat("\nVetor transformado final (Y):\n")
print(Y)


# --- PASSO FINAL: Retrosubstituição ---
x <- numeric(5) # Vetor para armazenar a solução x1, x2, x3, x4, x5

x[5] <- Y[5] / U[5,5]
x[4] <- (Y[4] - U[4,5] * x[5]) / U[4,4]
x[3] <- (Y[3] - U[3,4] * x[4] - U[3,5] * x[5]) / U[3,3]
x[2] <- (Y[2] - U[2,3] * x[3] - U[2,4] * x[4] - U[2,5] * x[5]) / U[2,2]
x[1] <- (Y[1] - U[1,2] * x[2] - U[1,3] * x[3] - U[1,4] * x[4] - U[1,5] * x[5]) / U[1,1]

cat("\n--- Solução ---\n")
cat("Solução final (vetor x):\n")
print(x)
