library(survival)

# load source function
source(here::here("source","06_EM.R"))

# load data
dat = veteran

y <- dat$time
delta <- as.numeric(dat$status == 1)

result <- em_exp(y, delta)

# calculate CI
result$lambda + c(-1,1) * 1.96 * result$se

# fit AFT
fit = survreg(Surv(time, status)~1, dist = "weibull", data = dat)

# fitted lambda
exp(coef(fit))
# 95% CI
exp(confint(fit))
