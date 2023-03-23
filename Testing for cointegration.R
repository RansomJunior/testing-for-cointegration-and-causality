# Check for cointegration using Johansen test
jotest <- ca.jo(data, type = "trace", K = 2)
summary(jotest)

# Check for cointegration using Engle-Granger test
egtest <- summary(lm(data$y ~ data$x))
summary(egtest)

# Engle-Granger test for cointegration
eg <- engle.granger(x, y)
summary(eg)

# Phillips-Quliaris test for cointegration
pq <- ca.po.test(cbind(x, y), lag = "long", demean = "constant", type = "Pz")
summary(pq)

# Johansen's procedure for cointegration
joh <- ca.jo(cbind(x, y), type = "trace", ecdet = "const", K = 2, spec = "longrun")
summary(joh)

# Diagnostic error tests
# Normality test
e <- residuals(joh)
shapiro.test(e)

# Heteroscedasticity test
bptest(e ~ x + y)

# Serial correlation test
dwtest(joh)

# Misspecification test
coeftest(joh)
