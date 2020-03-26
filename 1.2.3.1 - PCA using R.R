#Ensure reproducible results
set.seed(1337)


#Multivariate Gaussian Sampling Process
my_rmvnorm=function(mu,Sigma){
  r = length(mu)
  L = t(chol(Sigma)) 
  Z = rnorm(r)
  
  return(L %*% Z + mu)
}


#Set the parameters of the bivariate normal distribution
N <- 200
mu <- c(2, 3)
sigma <- rbind(c(1.0, 0.75),c(0.75, 1.0))

#Get 200 samples from a bivariate normal distribution N(mu, sigma) and add each sample as a new column in matrix X
X = matrix(0,nrow=2,ncol=N)
for(i in 1:N){
  X[,i] = my_rmvnorm(mu, sigma)
}

#Read off x and y data from sample matrix and plot it, showing mean (Fig 1a)
x <- X[1,]
y <- X[2,]

plot(x, y, main = "Random Sample from multivariate normal distribution. (N = 200)")
abline(v=2)
abline(h=3)

#Centre the data at the mean and plot it, showing new mean at (0, 0) (Fig 1b)
x_hat <- x - mean(x)
y_hat <- y - mean(y)

plot(x_hat, y_hat, main = "Random Sample (N=200) from multivariate normal distribution centred at origin.")
abline(v=0)
abline(h=0)

#Calculate the covariance matrix of the data and calculate the eigenvectors (as columns of a matrix) of and eigenvalues cov_matrix
cov_matrix <- cov(cbind(x_hat, y_hat))
print(cov_matrix)

eigenvectors <- eigen(cov_matrix)$vectors
print(eigenvectors)

eigenvalues <- eigen(cov_matrix)$values
print(eigenvalues)


#Define a function that plots the direction of principal axes and use it to create a new plot of the standardised data with principal axes shown.
plot_principal_axis <- function(eigenvector, colour) {
  #print(eigenvector[2]/eigenvector[1]) - their product is -1 so the lines are in fact perpendicular!
  
  abline(0, eigenvector[2]/eigenvector[1], col=colour)
  return(NULL)
}

plot(x_hat, y_hat, main = "Random Sample (N=200) with principal axes")
plot_principal_axis(eigenvectors[, 1], "blue")
plot_principal_axis(eigenvectors[, 2], "red")

#Next we generate a screeplot. The fraction of the variance captured in the kth principal component is lambda_k/(sum(lambda))
percentage_explained <- eigenvalues/sum(eigenvalues)
barplot(percentage_explained, xlab = "Principal Component", 
        ylab = "Percentage of Variance Explained", ylim = c(0,1),
        main = "Screeplot of PCA on Bivariate Normal Sample (N=200)")

#Now we simply rotate the data such that the first principal component aligns with the x-axis and the second aligns with the y-axis.
r <- t(eigenvectors) %*% rbind(x_hat, y_hat) #this fancy operator is matrix multiplcation in R

plot(r[1, ], r[2, ], main = "Random Sample (N=200) with principal axes",
     xlab = "Direction of First Principal Component", ylab = "Direction of Second Principal Component")
abline(v=0, col="red")
abline(h=0, col="blue")

#To reduce the dimensionality to 1D, we would simply 'project' all points along the red axis onto y=0.
plot(r[1, ], replicate(200, 0), main = "1D Representation of Bivariate Normal Sample",
     xlab = "First Principal Component", ylab="", yaxt = "n")
