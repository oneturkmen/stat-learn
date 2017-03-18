
# 4.3.R1 --------------------------------------
# x1 = hours studies
# x2 = undergrad gpa
#  y = receive an A

beta0 <- -6
beta1 <- 0.05
beta2 <- 1

m_x1 <- 40
m_x2 <- 3.5

log_regr <- beta0 + beta1*m_x1 + beta2*m_x2

px <- (exp(log_regr))/(1 + exp(log_regr))
print(px)


# 4.3.R2 ------------------------------------
given_p <- 0.5
# as we are given p(log_regr) = 0.5, log of (p/(1-p)) will be 0.
# therefore, we have:
m_x1_r2 <- -(beta0 + beta2*m_x2)/beta1




