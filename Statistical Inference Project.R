##Define variables
n <- 40
lambda <- 0.2
nsim <- 1000
###Sample means

simmeans <- rowMeans(matrix(rexp(n*nsim, lambda), nsim, n))
hist(simmeans, breaks = 50, col = "red")
lines(density(simmeans))
?hist

lvals <- seq(0.1, .9, by = 0.05)
coverage <- sapply(lvals, function(p) {
  phats <- rbinom(nsim, prob = p, size = n)/n
  ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
  ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
  mean(ll < p & ul > p)
})

plot(lvals, coverage, type = "l", xlab = "Lambda Values", 
     main = "Evaluating the 95% confidence intervals for Lambda")
abline(h = 0.95)
qqnorm(simmeans)
qqline(simmeans, probs=c(0.25,0.75))
?qqnorm
set.seed?
?set.seed
