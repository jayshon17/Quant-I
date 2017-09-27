# I'm editing my file, this is a github's test.
rm(list = ls())
# Set working directory 
setwd("D:/documents/Dropbox/NYU/Semester I/Quant I/PS 2/")
# Loading necessary packages
install.packages("MASS")
install.packages("Matrix")
library(MASS)
library(Matrix)
#==================================================================
# Bivariate normal distribution
rm(list = ls())
# Setting different levels for cov(x_1,x_2)
cov<-c(0.5,0.01,0.99,-0.99)
count=0
for(i in cov) {
  count=count+1
  # Bivariate normal distribution, 5000 obs with
  bivn <- mvrnorm(10000, mu = c(0, 0), Sigma = matrix(c(1, i, i, 1), 2))
  # Two dimensional kernel estimate
  bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)
  # Contour plots 
  contour(bivn.kde)
  # 3D density plot
  persp(bivn.kde, phi = 45, theta = 30)
}
#==================================================================
# Simulating CLT
rm(list = ls())
# Set the random seed
set.seed(1234567)
# Random variables
N<-c(10,30,100,1000) ;  # Sample size
R<-500 ; # 500 repetitions
# Initialize Bernoulli, Binomial and Cauchy
BR <- numeric(R)
BN <- numeric(R)
CU <- numeric(R)
# Repeat R times
for(i in N) {
  for(j in 1:R) {
    # Bernoulli
    BR_temp<-rbinom(i,1,0.5)
    BR[j]<-mean(BR_temp) ; #Storing the averages
    # Binomial
    BN_temp<-rbinom(i,5,0.5)
    BN[j]<-mean(BN_temp) ; #Storing the averages
    # Bernoulli
    CU_temp<-rcauchy(i)
    CU[j]<-mean(CU_temp) ; #Storing the averages
  }
  # Bernoulli kernel density plot 
  plot(density(BR))
  # Binomial kernel density plot 
  plot(density(BN))
  # Cauchy kernel density plot 
  plot(density(CU))
}
#==================================================================
# Matrix converging to singular
rm(list = ls())
# Random variables
V<-c(0,10,20,30,40,50,60,70,80,90,100,100000) 
# First column vector
FC <- matrix(c(1,0), nrow=2, , ncol=1)
# Varying the second column vector
for(i in V) {
SC<-matrix(c(i,1), nrow=2, , ncol=1)
A<-cbind(FC,SC)
print(solve(A))
}
#==================================================================
# Exercises with matrices
rm(list = ls())
# Invers of matrix (1,2,3,1)
solve(matrix(c(1,2,3,1), nrow=2, , ncol=2))
# Inverse of matrix (1,2,-1,-2). Columns and rows linearly dependent
solve(matrix(c(1,2,-1,-2), nrow=2, , ncol=2))
# Multiplyin M by N
M <- matrix(c(1,2,2,1,1,0), nrow=2, ncol=3)
N <- matrix(c(0,2,1,3,1,4), nrow=3, ncol=2)
M%*%N
# Quadratic form 
v=matrix(c(1,2))
A <- matrix(c(1,2,0,1), nrow=2, ncol=2)
t(v)%*%A%*%v
# 3X3 matrix
A<-rankMatrix(matrix(c(1,2,3,1,0,0,0,1,0), nrow=3, ncol=3))
IA<-solve(matrix(c(1,2,3,1,0,0,0,1,0), nrow=3, ncol=3))
IIA<-solve(IA)
IIA%*%IA
