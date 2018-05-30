# Include libraries
require(ggplot2)
require(MASS)

egfr_logistic <- read_csv("M:/CKD/MCKC Best trigger/data/egfr_logistic.csv")

data <- data.frame(egfr_logistic)

# Plot the distribution -- visually
# The answer appears to be b/t 12 and 14

ggplot(data = data, aes(x = mean_eGFR22)) +
  geom_bar(aes(color = as.factor(event)))

# Fit a glm model, specifying the binomial distribution
my.glm <- glm(as.factor(event)~mean_eGFR22, data = data, family = binomial)
b0 <- coef(my.glm)[[1]]
b1 <- coef(my.glm)[[2]]

# See what the probability function looks like
lr <- function(x, b0, b1) {
  prob <- 1 / (1 + exp(-1*(b0 + b1*x)))
  return(prob)                  
}

# The line appears to cross 0.5 just above 12.5
x <- -0:50
y <- lr(x, b0, b1)
lr.val <- data.frame(x, y)
ggplot(lr.val, aes(x = x, y = y)) +
  geom_line()

# The inverse of this function computes the threshold for a given probability
inv.lr <- function(p, b0, b1) {
  x <- (log(p / (1 - p)) - b0)/b1
  return(x)
}

# With the betas from this function, we get  12.88333 with se=0.05060167
inv.lr(0.5, b0, b1)

# Or, feeding the glm model into dose.p from MASS, we get the same answer
dose.p(my.glm, p = 0.5)



###############################################################################
# Plot the distribution -- visually
# The answer appears to be b/t 13 and 15

ggplot(data = data, aes(x = mean_eGFR11)) +
  geom_bar(aes(color = as.factor(event)))

# Fit a glm model, specifying the binomial distribution
my.glm <- glm(as.factor(event)~mean_eGFR11, data = data, family = binomial)
b0 <- coef(my.glm)[[1]]
b1 <- coef(my.glm)[[2]]

# See what the probability function looks like
lr <- function(x, b0, b1) {
  prob <- 1 / (1 + exp(-1*(b0 + b1*x)))
  odds=b0 + b1*x
  return(odds) 
}

# The line appears to cross 0.5 just above 12.5
x <- -5:95
y <- lr(x, b0, b1)
lr.val <- data.frame(x, y)
ggplot(lr.val, aes(x = x, y = y)) +
  geom_line()

# The inverse of this function computes the threshold for a given probability
inv.lr <- function(p, b0, b1) {
  x <- (log(p / (1 - p)) - b0)/b1
  return(x)
}

# With the betas from this function, we get 14.27603  with se=0.05424586
inv.lr(0.5, b0, b1)

# Or, feeding the glm model into dose.p from MASS, we get the same answer
dose.p(my.glm, p = 0.5)

###############################################################################

kfr_logistic <- read_csv("M:/CKD/MCKC Best trigger/data/dds_kfr_logistic.csv")

data <- data.frame(kfr_logistic)
summary(data)

ggplot(data = data, aes(x = mean_KFR_2yr)) +
  geom_bar(aes(color = as.factor(event)))

# Fit a glm model, specifying the binomial distribution
my.glm <- glm(as.factor(event)~mean_KFR_2yr, data = data, family = binomial)
b0 <- coef(my.glm)[[1]]
b1 <- coef(my.glm)[[2]]

# See what the probability function looks like
lr <- function(x, b0, b1) {
  prob <- 1 / (1 + exp(-1*(b0 + b1*x)))
  # odds=b0 + b1*x
  return(prob) 
}

# The line appears to cross 0.5 just above 12.5
x <- seq(0,1,0.001)
y <- lr(x, b0, b1)
lr.val <- data.frame(x, y)
ggplot(lr.val, aes(x = x, y = y)) +
  geom_line()

# The inverse of this function computes the threshold for a given probability
inv.lr <- function(p, b0, b1) {
  x <- (log(p / (1 - p)) - b0)/b1
  return(x)
}

# With the betas from this function, we get 14.27603  with se=0.05424586
inv.lr(0.5, b0, b1)

# Or, feeding the glm model into dose.p from MASS, we get the same answer
dose.p(my.glm, p = 0.5)
