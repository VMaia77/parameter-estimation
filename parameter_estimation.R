library(stats4)
library(plotly) # 3D Surface Plot

# Sum of Squares

# Data
x <- as.matrix(cbind(c(1, 2, 3, 4, 5, 6, 7)))
y <- as.matrix(c(115, 233, 317, 377, 411, 711, 917))

# Plotting data
plot(y ~ x)

# Linear regression without intercept
lm_r <- lm(y ~ x - 1)
abline(lm_r, col = "red")

# Matricial operations
solve(t(x) %*% x) %*% t(x) %*% x  # Identity matrix

solve(t(x) %*% x) %*% t(x) %*% y  # Solving for coefficients

# Loss function calculation
sq_function <- expression((y - x * b) ^ 2)

# Derivative of the loss function with respect to b
D(sq_function, "b")

# Setting derivative to zero to find the b minimizing the sum of squares function
slope_b <- sum(x * y) / sum(x^2)
slope_b

# All situations above change slightly when there is an intercept!

# Maximum Likelihood

# Assuming distribution, homogeneous errors
# Note:
log(500 * 3 * 7000) == log(500) + log(3) + log(7000)  # Will return TRUE

# Linear model with intercept
lm1 <- lm(y ~ x)
lm1

# Step 1: Create the loss function to be minimized
# Loss function: Deviance (-2 times sum of log likelihoods).
# By minimizing deviance, we maximize likelihood

deviance_loss <- function(par) {
  a.par <- par[1]  # Current intercept b0
  b.par <- par[2]  # Current slope b1
  err.sigma <- par[3]  # Current standard deviation of error

  # If the current standard deviation of error is invalid (i.e., negative),
  # return a high deviance to avoid that point
  if (err.sigma < 0) {
    deviance <- 1e7
  }

  # If the current standard deviation of error is valid (i.e., > 0),
  # calculate the deviance
  if (err.sigma > 0) {
    likelihoods <- dnorm(y, mean = x * b.par + a.par, sd = err.sigma)
    log.likelihoods <- log(likelihoods)
    deviance <- -2 * sum(log.likelihoods)
  }
  return(deviance)
}

# Use an optimization function to find parameter values
# that minimize the deviance function for this data
parameter_fits_optim <- optim(c(0, 0, 50),  # Initial values to start the algorithm
                              fn = deviance_loss, hessian = TRUE)

parameter_fits_optim$par
summary(lm1)
# Very similar results

# Residual Standard Error (Like Standard Deviation)
k <- length(lm1$coefficients) - 1  # Subtract one to ignore intercept
n <- length(lm1$residuals)

# Calculate predictions and residuals
predsml <- apply(x, 2, function(x) parameter_fits_optim$par[1] + parameter_fits_optim$par[2] * x)
residml <- (y - predsml) ^ 2 

# Calculate Residual Standard Error
SSEml <- sum(residml)
sqrt(SSEml / (n - (1 + k)))

# Maximum Likelihood function
negloglik_function <- function(theta0, theta1, sigma) {
  mu <- theta0 + x * theta1
  likelihoods <- dnorm(y, mean = mu, sd = sigma)
  log_likelihoods <- -sum(log(likelihoods))
  return(log_likelihoods)
}

# Note: Maximum Likelihood often underestimates variance, especially in small samples;
# restricted maximum likelihood deals with this

# Optimizer is used to find the minimum of the negative log-likelihood
parameter_fits_mle <- mle(minuslog = negloglik_function, start = list(theta0 = 0, theta1 = 0, sigma = 50))

# Compare
summary(parameter_fits_mle)
parameter_fits_optim$par
summary(lm1)

# Source an external file named "app_likelihood.R" and run a Shiny app
source("app_likelihood.R")
shinyApp(ui = ui, server = server)

x3d <- rnorm(100, 1.6, 1)
y3d <- rnorm(100, -1.7, 1)

kd <- MASS::kde2d(x3d, y3d, n = 50)
fig <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()

# Layout settings
fig <- fig %>% layout(
  title = "Lik space",
  scene = list(
    xaxis = list(title = "B0"),
    yaxis = list(title = "B1"),
    zaxis = list(title = "Likelihood")
  )
)

# Display the plot
fig

# Save 3D surface plot as HTML and open it
htmlwidgets::saveWidget(fig, file = "3dsurf.html")
system("open 3dsurf.html")
