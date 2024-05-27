# Function to get the payoff once
optionPayoff <- function(n, sigma = 0.5) {
  # input 
  # n: number of periods
  

  # given:
  rf <- 0
  S <- S0 <- 100
  
  u <- exp(sigma / sqrt(n))
  d <- 1 / u
  rnprob <- (1-d)/(u-d)
  cumulativeMean <- S0
  
  # vector with True if stock moves up, False otherwise
  movements <- sample(c(T, F), n, replace = TRUE, prob = c(0.5, 0.5))
  
  for (i in 1:n) {
    S <- ifelse(movements[i], S*u, S*d)
    cumulativeMean <- (cumulativeMean * (i - 1) + S) / i
  }
  
  return(max(S - cumulativeMean, 0))
}


# sigma = 0.5
# optionPayoff(9999)


# 1.
monteCarlo <- function(N, n = 100, sigma = 0.5) {
  # input 
  # N: sample size
  # n: number of periods
  # sigma: volatility in asset
  
  payoffs <- c()
  
  # payoffMeans <- optionPayoff(n, sigma)
  for (i in 1:N) {
    payoffs <- c(payoffs, optionPayoff(n, sigma))
  }
  
  price <- mean(payoffs)
  st_dev <- sd(payoffs)
  
  numOfPositives <- sum(payoffs > 0)
  
  
  cat("We observed the average price to be:", price,
      "\nand the standard deviation of: ", st_dev,
      "\nin total: ", numOfPositives, "of the payoffs were positive\n\n")
  
  return(c(price, st_dev, numOfPositives))
}

results <- monteCarlo(1e5)

price <- results[1]
st_dev <- results[2]
positives <- results[3]


pie(c(positives, 1e5 - positives), 
    c("Number of positive payoffs", "Number of payoffs of 0"),
    clockwise = TRUE, col=c("green", "red")) # 1e5 - positives


# 2. 95% confidence interval:
# price + c(-1, 1) * qnorm(.975) * st_dev / sqrt(1e4)

confidence_interval <- function(xbar, n, sigma, confidenceLevel = 0.95) {
  alpha <- 1 - confidenceLevel
  tails <- qnorm(1 - alpha / 2) * sigma / sqrt(n)
  return(xbar + c(-tails, tails))
}

confidence_interval(price, 1e5, st_dev)


# 3.

# given sigma and the confidence level we can calculate the sample size easily 
# if the CI should be 2 wide.
samplesize <- function(sigma, confidenceLevel = 0.95) {
  alpha <- 1 - confidenceLevel
  
  res <- (qnorm(1 - alpha / 2) * sigma)^2
  cat("We found the optimal sample size to be: ", res, "\n")
  res
}

optimalSampleSize <- samplesize(st_dev)

cat("With the optimal sample size we ran the Monte Carlo simulation again and got:\n")
results <- monteCarlo(optimalSampleSize)

# z_(alpha / 2) * sigma / sqrt(n) = 1
# <=>
# n = (z_(alpha / 2) * sigma)^2


# 4.

# Barplot: for each sigma - price bar, sigma
{
  a <- setNames(monteCarlo(optimalSampleSize, sigma = 0.1)[c(1, 2)], "                    0.1")
  b <- setNames(monteCarlo(optimalSampleSize, sigma = 0.5)[c(1, 2)], "                    0.5")
  c <- setNames(monteCarlo(optimalSampleSize, sigma = 1)[c(1, 2)], "                    1")
  d <- setNames(monteCarlo(optimalSampleSize, sigma = 1.5)[c(1, 2)], "                    1.5")
  
  sigmas <- c("0.5", "0.1", "1", "1.5")
  barplot(c(a, b, c, d), col = rep(c("red", "blue"), 4), xlab="sigma", main = "Price & standard deviation with different sigmas")
  legend("topleft", c("Price", "Standard deviation"), col=c("red", "blue"), lwd=3)
}


