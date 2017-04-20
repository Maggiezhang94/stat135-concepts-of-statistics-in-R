#c) Use bootstrap to find the approximate standard deviation of the mle:
require(stats4)
computeMLE <- function(x){
  a <- 2*x[3] + x[2]
  b <- 2*(sum(x))
  theta_hat <- a / b
  return(theta_hat)
  n <- sum(x)
}

n <- 10 + 68 + 112
theta_hat <- computeMLE(c(10, 68, 112))
prob = c( (1 - theta_hat)^2, 2*theta_hat*(1 - theta_hat), theta_hat^2)

B <- 1000
boot_samps <- rmultinom(B, n, prob)
boot_mle <- apply(boot_samps, 2, computeMLE)
sd(boot_mle)
# e) Form 99% confidence intervals using bootstrap:
ci_99 <- 2*theta_hat - quantile(boot_mle, probs = c(0.995, 0.005))