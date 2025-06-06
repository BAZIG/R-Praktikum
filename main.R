#R-Praktikum Semesterprojekt (Teil 1)
# Name: [Baptiste Fabian Zigmann Weiß]
# Matrikelnummer: [2414242]


#Aufgabe 1: Vektoren und Matrizen
vector_ac <- c(1, 4, 2, 4, 2)
vector_b <- 2^seq(1, 5, by = 1)
vector_c <- seq(1, 5, by = 1)^2

vector_ac
vector_b
vector_c

max_element <- max(vector_c)
position <- which.max(vector_c)

max_element
position

matrix_e <- cbind(vector_ac, vector_b, vector_c)
dim(matrix_e)
matrix_e

colnames(matrix_e) <- c("Matrikel", "HochAufZwei", "HochZwei")

matrix_g <- matrix_e[, -1]
matrix_g

P <- matrix(1:100, nrow = 10, ncol = 10, byrow = FALSE)
Q <- matrix(1 / (1:100), nrow = 10, ncol = 10, byrow = TRUE)

addition <- P + Q
subtraction <- P - Q
multiplication <- P %*% Q
hadamard_product <- P * Q

addition
