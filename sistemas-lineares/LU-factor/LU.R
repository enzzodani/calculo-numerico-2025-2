A0 <- c(2,-3,4,5,-6,1,2,-1,3,4,3,-4,1,2,-1,5,2,-2,4,3,-2,3,2,-1,5)
A <- matrix(A0, nrow = 5, byrow = TRUE)

m21 <- A[2,1] / A[1,1]
m31 <- A[3,1] / A[1,1]
m41 <- A[4,1] / A[1,1]
m51 <- A[5,1] / A[1,1]

M0t <- c(1,0,0,0,0, -m21,1,0,0,0, -m31,0,1,0,0, -m41,0,0,1,0, -m51,0,0,0,1)

M0 <- matrix(M0t, nrow = 5, byrow = TRUE)

A1 <- M0 %*% A

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

A3 <- M2%*%A2

m54 <- A3[5,4] / A3[5,5]


M3t <- c(
        1,0,0,0,0,
        0,1,0,0,0,
        0,0,1,0,0,
        0,0,0,1,0,
        0,0,0,-m54,1
)

M3 <- matrix(M3t, nrow = 5, byrow = TRUE)

A4 <- M3 %*% A3

L <- matrix(0, nrow = 5, ncol = 5) # Cria uma matriz 5x5 de zeros
diag(L) <- 1                      # Coloca 1s na diagonal

# Preenche com os multiplicadores
L[2,1] <- m21; L[3,1] <- m31; L[4,1] <- m41; L[5,1] <- m51
L[3,2] <- m32; L[4,2] <- m42; L[5,2] <- m52
L[4,3] <- m43; L[5,3] <- m53
L[5,4] <- m54

print(L%*%A4)

