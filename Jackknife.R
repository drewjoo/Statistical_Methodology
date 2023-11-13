# Jackknife

# My data
x <- c(0.9836458, 0.98679346, 0.97652966, 0.9875398, 0.96775347, 0.98156935, 0.9777201, 0.97770274, 0.9868976, 0.97849494,
       0.96807307, 0.9782228, 0.97763, 0.97853947, 0.9845965, 0.9834155, 0.97115946, 0.98483765, 0.97235817, 0.9765884)

y <- c(0.7436851, 0.7385785, 0.7498541, 0.7495987, 0.74609363, 0.74981433, 0.7506356, 0.7440469, 0.75910604, 0.764551,
       0.7503272, 0.75200033, 0.7489456, 0.7673101, 0.74372, 0.7473218, 0.74083483, 0.7557458, 0.7614375, 0.74430144)

# Observed difference in means
observed_diff <- mean(x) - mean(y)

# Jackknife resampling function
jackknife_diff <- function(data) {
  n <- length(data)
  diffs <- numeric(n)
  
  for (i in 1:n) {
    sample_data <- data[-i]
    diffs[i] <- mean(sample_data)
  }
  
  return(diffs)
}

# Jackknife resampling for x and y
jackknife_diff_x <- jackknife_diff(x)
jackknife_diff_y <- jackknife_diff(y)

# Calculate the standard errors
se_x <- sd(jackknife_diff_x) * sqrt(length(x) - 1)
se_y <- sd(jackknife_diff_y) * sqrt(length(y) - 1)

# Calculate the confidence interval
confidence_interval <- observed_diff + c(-1.96 * se_x, 1.96 * se_y)

# Print the results
cat("Observed Difference in Means:", observed_diff, "\n") # 0.228608 
cat("95% Confidence Interval for the Difference in Means:", confidence_interval, "\n") #0.2259204 0.2320985 

# 0 does not exist between CI(Confidence Interval)
# There is a significant difference between the mean of the x group and the mean of the y group.

