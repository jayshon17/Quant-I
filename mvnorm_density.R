# lets first simulate a bivariate normal sample
#MASS IS A COOL LIBRARY OF THINGS DONE IN RIPLEY'S MODERN APPLIED
#STARTITICS WITH (R AND)S
library(MASS)
#This draws a bivariate normal (here 5000 (10,000 numbers divided by
#2 obs, leave mu at zero, leave the first and fourth terms of Sigma,
#change second and third (off diagonal), keeping symmetric, and if you
#make bigger than 1 or less than -1 your will get an error that your
#Sigma is not positive definite, leave 1000 and 2 args alone
bivn <- mvrnorm(10000, mu = c(0, 0), Sigma = matrix(c(1, .5, .5, 1), 2))
# now we do a kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)
# now plot your results
#First do contour then do 3d (perspecitve plot, args of persp are the
#viewing angle)
contour(bivn.kde)
persp(bivn.kde, phi = 45, theta = 30)

