#Wishart - Surmise Approx
mat <- function(){ 
  tmp <- matrix(c(rnorm(n = 1),
                      rnorm(n = 1 , sd = 1/sqrt(2)),
                      rnorm(n = 1, sd = 1/sqrt(2)),
                      rnorm(n = 1)),
                ncol = 2)
  return(tmp)  
}

n <- 10000
s <- numeric(n)

for(i in 1:n){
  evals <- eigen(mat())$values
  s[i] <- abs(diff(evals))
}

plot(density(s))


#Semi-Circular Law Approx
m <- 100
mat <- function(){ 
  tmp <- matrix(NA, ncol = m, nrow = m)
  diag(tmp) <- rnorm(m)
  tmp[upper.tri(tmp)] <- rnorm(m *(m-1)/2, sd = 1/sqrt(2))
  tmp[lower.tri(tmp)] <- t(tmp)[lower.tri(tmp)]
  return(tmp)
}
n <- 100
res <- numeric(n*m)
for(i in 1:n){
  evals <- eigen(mat())$values
  res[(m*(i-1)+1):(n*m)] <- evals
}
plot(density(res))
abline(v = sqrt(2*m), lty = 2, col = "red")
abline(v = -sqrt(2*m), lty = 2, col = "red")

