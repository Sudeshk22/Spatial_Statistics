##Draw a random sample from uniform(0,1)
runif(1)

#Question2 
#Draw a randm vector (u1, u2) from uniform over the unit square
runif(2, min = o
      , max = 1)

# question 3
#select N = 100 random location from the uniform distribution over the unit square using 
# R and form a (N* 2) matrix S
N <- 100
loc <- runif(100*2)
S <- matrix(loc, N , 2)

# option 2 
S <- cbind(runif(N), runif(N))

# Question 3 
# calculate the distance(Euclidean) matrix from S
Dist <- matrix(0,100,100)
for (i in 1:100){
  for (j in : 100){
    Dist[i,j] <- sqrt(sum(S[i, ]- S[j, ])^2)
  }
  return (Dist)
}
## Do some correction

# optoion 2
library(fields)
Euc_dist_mat <- rdist(S)


# Question5
#Simulate a N- diemensional realisation from a zero - mean Gaussian process defined over unit square
# with covariance kernel K(s,s') = 2 del(s = s')

library(MASS)
n = 100
mvrnorm(1 , 0, 2* diag(N))
# OFF DIAGONAL ELEMENT are zero so y1, y2, y3 ... are independent so we can use univariate normal
N <- 100
uni <- rnorm(100, 0, 1)
mutlivariate_norm <- sqrt(2)*uni 

# other option 
N <- 100
Y <- rnorm(N, 0, sd = sqrt(2))
Y


# Question 6
# Draw a simple spatial map of simulated data vector 
library(plot3D)
scatter2D(x = S[,1], y = S[,2], colvar = Y, pch = 15)


# Question 7
# Draw a spatial map of the simulated data vector using ggplot2
library(ggplot2)
p <- ggplot( ) +  geom_point(aes(x = S[, 1], y = S[, 2], color = Y))
p

# Question 8 
# Simulate a N- diemensiobnal realisation from a zero - mean Gaussian process
# over the unit square , with covariance kernel K(s, s') = 2 + 0.5 del(s = s')
N <- 100
sigma <- 2 + 0.5 * diag(N)
sigma
Y <- c(t(chol(sigma)) %*% rnorm(N))
Y

# Question 9 
#repeate the same for 2500
N <- 2500
sigma <- 2 + 0.5 * diag(N)
sigma
Y <- c(t(chol(sigma)) %*% rnorm(N))
Y

# Question 10 
# measure the computation time using package tictoc
library(tictoc)
tic()
N <- 2500
sigma <- 2 + 0.5 *diag(N)
Y <- c(t(chol(sigma)) %*% rnorm(N))
toc()
# 4.86 sec elapsed

#####################################################
# feasibility Questions 
# Question 11
# Simulate a similar N- diemesional realisation using a hierarchical structure 
# Same as ques of quiz 1 question3
# Y(s) <- W + e(s)
#k(s, s') = cov(w, w) + cov(e(s), e(s'))

N <- 2500
W <- sqrt(2) * rnorm(1)
Y <- W + sqrt(0.5) * rnorm(N)
sigma <- var(W) + 0.5 * diag(N)
sigma

#Question 12 
# measure the tme using tictoc
tic()
N <- 2500
W <- sqrt(2) * rnorm(1)
Y <- W + sqrt(0.5) * rnorm(N)
sigma <- var(W) + 0.5 * diag(N)
toc()

#Question 13 
# let's go back to our N = 100 , and bring the old S and its distance matrix back 
# Simulate a N- diemensional realisation from a zero -mean gaussian process over 
# the unit square , with covariance kernel K(s, s') = exp(-mod(s - s')/ 0.1)
library(fields)
Euc_dist_mat <- rdist(S)
sigma <- exp(-Euc_dist_mat/0.1)
Y <- c(t(chol(sigma)) %*% rnorm(N))

# Another option 
dist_mat <- dist(S)
sigma <- exp(-dist_mat/0.1)
Y <- c(t(chol(sigma)) %*% rnorm(N))
# there is a mistake in this 
       
#Question 14

library(geoR)
phi = 0.1
nu <- 5
Sigma <- matern(d, phi, nu)
Y <- c(t(chol(Sigma)) %*% rorm(N))


##Question15
N <- 100
mu <- 2 + 0.5 * S[,1] + 2* S[, 2]

Y <- mu + rnorm(N, 0, sd = sqrt(2))
Y

#Question 16
# creating if y ~ MVN (xB , sigma )
sx <- S[, 1]
sy <- S[, 2]
X <- cbind(1, sx , sy) 
beta <- c(2, 0.5)
sigma <- 2* diag(N)

# Question 17 
# from a uniform grid of 11^2 locations {(x, y) : x,y belongs {0.1, 0.2,,,,,1}}
# over a unit square 
#points <- runif(11^2, min = 0, max = 1)

S <- expand.grid(x =  seq(0,1, 0.1), 
                 y =  seq(0,1, 0.1))
S <- as.matrix(S)




#########################################
library(fields)
N <- 1000
S <- cbind(runif(N), runif(N))
d <- rdist(S)
mu <- 0.5 + 0.5*S[, 1] + 0.5 * S[, 2]
covmat <- exp(-d/ 0.1)
X <- mu + t(chol(covmat)) %*% rnorm(N)
Y <- rbinom(N, 1, pnorm(X))
library(plot3D)
scatter2D(x = S[, 1], S[, 2], colvar = X, pch= 15)
scatter2D(x = S[, 1], S[, 2], colvar = pnorm(X), pch = 15)
scatter2D(x = S[, 1], S[, 2], colvar = Y, pch = 15)