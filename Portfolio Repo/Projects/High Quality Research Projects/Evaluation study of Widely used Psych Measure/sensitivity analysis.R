# SCratchpad for sensitivity analyses of simple multiple regressions

library(tidyverse)
library(paramtest)
library(lme4)
library(pwr)
set.seed(123)


# Sensitivity analysis for bivariate regression (coping motives and use)

lm_test <- function(simNum, N, b1, b0=0, xm=0, xsd=1) {
  x <- rnorm(N, xm, xsd)
  y <- rnorm(N, b0 + b1*x, sqrt(1 - b1^2))  # var. approx. 1 after accounting
  # for explained variance by x
  model <- lm(y ~ x)
  
  # pull output from model
  est <- coef(summary(model))['x', 'Estimate']
  se <- coef(summary(model))['x', 'Std. Error']
  p <- coef(summary(model))['x', 'Pr(>|t|)']
  
  return(c(xm=mean(x), xsd=sd(x), ym=mean(y), ysd=sd(y), est=est, se=se, p=p,
           sig=(p < .05)))
}

# we vary N at 200 and 300; we are also setting coefficient of x predicting
# y to be approx. .15 across all simulations
power_lm <- grid_search(lm_test, params=list(N=c(250), b1 = c(.1,.15,.2,.3)), n.iter=5000, output='data.frame',
                        parallel='snow', ncpus=2)
test <- results(power_lm) %>%
  group_by(N.test, b1.test) %>%
  summarise(power=mean(sig))




### sensitivity analysis for multiple regression (coping motives regressed on demographics and disengagement strategies)


lm_test_multiple <- function(simNum, N, b1, b2, b3, b4, b0=0, x1m=0, x1sd=1,
                             x2m=0, x2sd=1, x3m=0, x3sd=1,  x4m=0, x4sd=1) {
  
  x1 <- rnorm(N, x1m, x1sd)
  x2 <- rnorm(N, x2m, x2sd)
  x3 <- rnorm(N, x3m, x3sd)
  x4 <- rnorm(N, x4m, x4sd)
  
  yvar <- sqrt(1 - b1^2 - b2^2 - b3^2 - b4^2)  # residual variance
  y <- rnorm(N, b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4, yvar)
  model <- lm(y ~ x1 + x2 + x3 + x4)
  
  # pull output from model 
  est_x1 <- coef(summary(model))['x1', 'Estimate']
  p_x1 <- coef(summary(model))['x1', 'Pr(>|t|)']
  sig_x1 <- p_x1 < .05
  est_x2 <- coef(summary(model))['x2', 'Estimate']
  p_x2 <- coef(summary(model))['x2', 'Pr(>|t|)']
  sig_x2 <- p_x2 < .05
  est_x3 <- coef(summary(model))['x3', 'Estimate']
  p_x3 <- coef(summary(model))['x3', 'Pr(>|t|)']
  sig_x3 <- p_x3 < .05
  est_x4 <- coef(summary(model))['x4', 'Estimate']
  p_x4 <- coef(summary(model))['x4', 'Pr(>|t|)']
  sig_x4 <- p_x4 < .05
  
  
  
  return(c(est_x1=est_x1, p_x1=p_x1, sig_x1=sig_x1, est_x2=est_x2, p_x2=p_x2,
           sig_x2=sig_x2, est_x3=est_x3, p_x3=p_x3, sig_x3=sig_x3, est_x4=est_x4,
           p_x4=p_x4, sig_x4=sig_x4))
}

# Keeping N constant at 250; varying coefficient of x1 = c(.1,.15,.2), coefficient of
# x2 = .2, and coefficien of interaction = .2
power_lm_int <- grid_search(lm_test_multiple, params=list(N=c(250), 
                                                          b1=c(.1,.15,.2), 
                                                          b2 = c(.1,.15,.2), 
                                                          b3 = c(.1,.15,.2),
                                                          b4 = c(.1,.15,.2)),
                            n.iter=750, output='data.frame', parallel='snow', ncpus=2)
test2 <- results(power_lm_int) %>%
  group_by(N.test, b1.test, b2.test, b3.test) %>%
  summarise(
    power_x1=mean(sig_x1),
    power_x2=mean(sig_x2),
    power_x3=mean(sig_x3),
    power_x4=mean(sig_x4))

test2