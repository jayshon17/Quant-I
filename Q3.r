set.seed(1234567)
b1hatc30<-numeric(1000)
b1hatcse30<-numeric(1000)
x30<-rnorm(30)
for(j in 1:1000){
  uc30<-rcauchy(30,0,1)
yc30<- 1+x30+uc30
bhatc30<-coef(lm(yc30~x30))
sec30<-summary(lm(yc30~x30))$coefficients["x30","Std. Error"]
b1hatc30[j]<-bhatc30["x30"]
b1hatcse30[j]<-sec30
}

#The average of estimated Beta 1
mean(b1hatc30)

#The SD of estimated Beta 1
sd(b1hatc30)

#The average of SEs
mean(b1hatcse30)