# logitExample1.R
educ <- c(0,0,1,1)
psex <- c(0,1,0,1)
f    <- c(873,1190,533,1208)
# OLS regression
mod.0 <- glm(psex~educ, weight=f)
summary(mod.0)
#  robust se's 
library(sandwich)
cbind(mod.0$coef, sqrt(diag(vcovHC(mod.0))))
# logistic regression
mod.1 <- glm(psex~educ, weight=f, family='binomial')
summary(mod.1)
library(margins)
summary(margins(mod.1))
phat <- fitted(mod.1)
mean(phat[educ==0])
mean(phat[educ==1])
ci <- confint(mod.1)
# transform to OR 
exp(cbind(OR=coef(mod.1), ci))
# difference in proportions test
p0 <- 873/(873 + 1190)
p1 <- 533/(533 + 1208)
n0 <- (873 + 1190)
n1 <- (533 + 1208)
p  <- (873 + 533)/(sum(f))
sep <- sqrt(p*(1-p)*(1/n0 + 1/n1))
z  <- (p0 - p1)/sep
#test: H0: p0 - p1
p.z <- 2 * pnorm(-abs(z))
# CI
l.d <- (p1 - p0) - 1.96 * sep
u.d <- (p1 - p0) + 1.96 * sep
cbind(z,p.z, l.d, u.d)
# chi-square tests
# make individual-level data
dat.exp <- data.frame(y=rep(psex,f), x=factor(rep(educ,f)))
mod.1a <- glm(y~x, family='binomial', data=dat.exp)
# dp/dx
mx <- margins(mod.1a, data=dat.exp)
summary(mx, by_factor=TRUE)
cplot(mod.1a, what="prediction")

chisq.test(dat.exp$y,dat.exp$x)
# or
M <- as.table(rbind(c(f[1:2]), c(f[3:4])))
dimnames(M) <- list(educ=c("Lo","Hi"),
                    psex=c("No","Yes"))
chisq.test(M) # 
# a useful library
library(dplyr)
tab.yx <- dat.exp %>%
  group_by(y, x) %>%
  tally(sort=TRUE)
#
# alternative input
educ <- c(0,1)
y    <- c(1190, 1208)
n    <- c(2063,1741)
mod.2 <- glm(cbind(y,(n-y))~educ, family='binomial')
summary(mod.2)



