#install package mvtnorm, do this in R-sutdio
 install.packages("mvtnorm")
#load library mvtnorm, again just check box in R-S
 library("mvtnorm")
                                        #First set seed, choose your own
set.seed(121696)
smat<-matrix(c(1,.5,.5,1),2)
data<-rmvnorm(100,sigma =smat)
x<-data[,1]
eps<-data[,2]
y<-3+7*x+eps
reg<-lm(y~x)
regtest<-lm(x~reg$resid)
summary.lm(regtest)
summary.lm(lm(x~eps))

epsilon <- rnorm(100, mean = 1, sd = 1)
smat_n<-matrix(c(1,.5,.5,1),2)
data_n<-rmvnorm(100,sigma =smat_n)

x_1<-data_n[,1]
x_2<-data_n[,2]

a <- 3
b <- 2
c <- 2
y_n <- a + b*x_1+c*x_2+epsilon
reg_yxx <- lm(y_n~x_1+x_2)
regyx <- lm(y_n~x_1)
regxx <- lm(x_2~x_1)

summary(reg_yxx)
summary(lm(regyx$residuals~regxx$residuals))

regxxnew <- lm(x_1~x_2)
regyxnew <- lm(y_n~x_2)
summary(reg_yxx)
summary(lm(regyxnew$residuals~regxxnew$residuals))
#4================================================================

x_mat <- matrix(c((c(rep(1,100))),x_1,x_2),100,3)
b_calc <- solve(t(x_mat)%*%x_mat)%*%t(x_mat)%*%y_n

b_calc
#5===============================================================
# But. It was.. Significant..
summary(lm(y_n~x_1+x_2-1))
summary(lm(y_n~x_1+x_2))
#5b
y_n_cen <- y_n - mean(y_n)
x_1_cen <- x_1 - mean(x_1)
x_2_cen <- x_2 - mean(x_2)
summary(lm(y_n_cen~x_1_cen+x_2_cen-1))
summary(lm(y_n~x_1+x_2))
