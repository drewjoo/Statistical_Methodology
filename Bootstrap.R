# Bootstrap

# PKG
library(boot)

# My data
x <- c(0.9836458, 0.98679346, 0.97652966, 0.9875398, 0.96775347, 0.98156935, 0.9777201, 0.97770274, 0.9868976, 0.97849494,
       0.96807307, 0.9782228, 0.97763, 0.97853947, 0.9845965, 0.9834155, 0.97115946, 0.98483765, 0.97235817, 0.9765884)

y <- c(0.7436851, 0.7385785, 0.7498541, 0.7495987, 0.74609363, 0.74981433, 0.7506356, 0.7440469, 0.75910604, 0.764551,
       0.7503272, 0.75200033, 0.7489456, 0.7673101, 0.74372, 0.7473218, 0.74083483, 0.7557458, 0.7614375, 0.74430144)

#################################
## Method1 : Using the Boot PKG##
#################################

# Observed difference in means
observed_diff <- mean(x) - mean(y)

# Bootstrap function
bootstrap_diff <- function(data, indices) {
  x_sample <- data$x[indices]
  y_sample <- data$y[indices]
  return(mean(x_sample) - mean(y_sample))
}

# Combine the data
data_combined <- data.frame(x = x, y = y)

# Set the seed for reproducibility
set.seed(123)

# Perform bootstrap resampling
boot_result <- boot(data_combined, statistic = bootstrap_diff, R = 10000)

# Calculate the confidence interval
confidence_interval <- boot.ci(boot_result, type = "bca")$bca[, c(4, 5)]

# Print the results
cat("Observed Difference in Means:", observed_diff, "\n") # 0.228608 
cat("95% Confidence Interval for the Difference in Means:", confidence_interval, "\n") # 0.2242345 0.2326718 

# 0 does not exist between CI(Confidence Interval)
# There is a significant difference between the mean of the x group and the mean of the y group.




#########################################
## Method2 : Without using the Boot PKG##
#########################################

# Observed difference in means
observed_diff <- mean(x) - mean(y)

# Number of bootstrap samples
num_samples <- 10000

# Bootstrap resampling
bootstrap_diffs <- numeric(num_samples)

for (i in 1:num_samples) {
  # Resample with replacement
  x_bootstrap <- sample(x, replace = TRUE)
  y_bootstrap <- sample(y, replace = TRUE)
  
  # Calculate the difference in means for the bootstrap sample
  bootstrap_diffs[i] <- mean(x_bootstrap) - mean(y_bootstrap)
}

# Calculate the confidence interval
confidence_interval <- quantile(bootstrap_diffs, c(0.025, 0.975))

# Print the results
cat("Observed Difference in Means:", observed_diff, "\n") # 0.228608 
cat("95% Confidence Interval for the Difference in Means:", confidence_interval, "\n") # 0.2242902 0.2326756