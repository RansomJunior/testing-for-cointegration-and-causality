
# Granger causality test
gc.test <- function(x, y) {
  mod <- lm(y ~ x)
  u1 <- residuals(mod)
  mod <- lm(y ~ x + lag(y, -1))
  u2 <- residuals(mod)
  f.stat <- ((sum(u1^2) - sum(u2^2))/2) / (sum(u2^2)/(length(y) - 3))
  p.val <- 1 - pf(f.stat, 1, length(y) - 3)
  list(F.statistic = f.stat, p.value = p.val)
}

# Generate some dummy data
set.seed(123)
data <- data.frame(x = rnorm(100), y = rnorm(100))

# Run Granger causality test
gc.test(data$x, data$y)

library(lmtest)

# Generate some dummy data
set.seed(123)
x <- rnorm(100)
y <- x + rnorm(100)

# Run Granger causality tests
grangertest(x ~ y, order = 1)
grangertest(y ~ x, order = 1)


# Granger Causality Test - Phillips
gctest1 <- lm(y ~ x, data = dummy_data)
summary(grangertest(gctest1, order = 2))

# Granger Causality Test - Johansen
gctest2 <- VAR(dummy_data, p = 2, type = "const")
summary(causality(gctest2, cause = "x"))

# Granger Causality Test - Engle-Granger
gctest3 <- lm(y ~ x, data = dummy_data)
summary(ur.ers(gctest3, type = "DF"))
