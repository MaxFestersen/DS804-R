# 6-7 Matrices in R -------------------------------------------------------

# >(a) --------------------------------------------------------------------
# Create a 2 x 2 matrix A by row binding vectors using the rbind() command.
v1 <- (c(5,1))
v2 <- (c(1,-6))
matrixA<-rbind(v1,v2)
matrixA

# >(b) --------------------------------------------------------------------
# Nullify matrix A by adding another matrix that you define.
nul <- (c(0,0))
matrixA*nul

# >(c) --------------------------------------------------------------------
# Double all the values in the original matrix
# A by multiplication with another matrix that you define.
v3 <- (c(2,2))
v4 <- (c(2,2))
matrixB<-rbind(v3,v4)

matrixA*matrixB

