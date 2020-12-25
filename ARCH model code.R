data("euro", package="POE5Rdata") #14.8 (a, c)
View(euro)
euro.ts <- ts(euro, start=c(1988,1), end=c(2004,12), frequency = 12)
r <- euro.ts[,"r"]
plot(r)
library(tseries)
adf.test(r)

library(dynlm)


library(broom)

library(knitr)

ymean <- dynlm(r~1) #mean equation.
kable(tidy(ymean))

e <- resid(ymean)
e2 <- e^2

arch <- dynlm(e2~L(e2)) #first-order
kable(tidy(arch))
sumarch <- summary(arch)
(lm <- (length(r)-1)*sumarch$r.squared)
alpha <- 0.05
(Chicr <- qchisq(1-alpha, 1)) #we have arch-effects
h <- arch$fitted.values
plot(h)