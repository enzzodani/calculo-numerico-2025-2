# Vetor com os elementos
A <- c(3, 2, 4,
       1, 1, 2,
       4, 3, -2)

# Cria a matriz A0, preenchendo por coluna (padrão do R)
A0 <- matrix(A, nrow = 3, byrow = TRUE)

# Calcula os multiplicadores
m21 <- A0[2, 1] / A0[1, 1]
m31 <- A0[3, 1] / A0[1, 1]

# Vetor com os elementos da matriz de eliminação
M <- c(1, 0, 0, -m21, 1, 0, -m31, 0, 1)

# Cria a matriz M0, preenchendo por linhas
M0 <- matrix(M, nrow = 3, byrow = TRUE)

# Realiza a multiplicação de matrizes
A1 <- M0 %*% A0

m32 <- A1[3,2]/A1[2,2]

M1l <- c(1,0,0,0,1,0,0,-m32,1)

M1 <- matrix(M1l, nrow = 3, byrow = TRUE)

A2 <- M1 %*% A1 # Matriz triangular superior

L0 <- c(1,0,0,m21,1,0,m31,m32,1)

L <- matrix(L0, nrow = 3, byrow = TRUE)

print(L%*%A2)

