require(foreign)
require(ggplot2)
require(VGAM)
require(boot)
dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ztp.dta")
dat
dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ztp.dta")

dat <- within(dat,{
  hmo <- factor(hmo)
  died <- factor(died)
})

summary(dat)
ggplot(dat, aes(stay)) +
  geom_histogram() +
  scale_x_log10() +
  facet_grid(hmo ~ died, margins=TRUE, scales="free_y")
ggplot(dat, aes(factor(age), stay)) +
  geom_violin() +
  geom_jitter(size=1.5) +
  scale_y_log10() +
  stat_smooth(aes(x = age, y = stay, group=1), method="loess")
ggplot(dat, aes(age, fill=died)) +
  geom_histogram(binwidth=.5, position="fill") +
  facet_grid(hmo ~ ., margins=TRUE)
m1 <- vglm(stay ~ age + hmo + died, family = posnegbinomial(), data = dat)
summary(m1)
output <- data.frame(resid = resid(m1)[, 1], fitted = fitted(m1))
ggplot(output, aes(fitted, resid)) + geom_jitter(position = position_jitter(width = 0.25), 
                                                 alpha = 0.5) + stat_smooth(method = "loess")
ggplot(output, aes(fitted, resid)) +
  geom_jitter(position=position_jitter(width=.25), alpha=.5) +
  stat_quantile(method="rq")

output <- within(output, {
  broken <- cut(fitted, hist(fitted, plot=FALSE)$breaks)
})

ggplot(output, aes(broken, resid)) +
  geom_boxplot() +
  geom_jitter(alpha=.25)
m2 <- vglm(formula = stay ~ age + hmo + died, family = pospoisson(), data = dat)
m2
## change in deviance
(dLL <- 2 * (logLik(m1) - logLik(m2)))
## p-value, 1 df---the overdispersion parameter
pchisq(dLL, df = 1, lower.tail = FALSE)
dput(round(coef(m1),3))
f <- function(data, i, newdata) {
  require(VGAM)
  m <- vglm(formula = stay ~ age + hmo + died, family = posnegbinomial(),
            data = data[i, ], coefstart = c(2.408, 0.569, -0.016, -0.147, -0.218))
  mparams <- as.vector(t(coef(summary(m))[, 1:2]))
  yhat <- predict(m, newdata, type = "response")
  return(c(mparams, yhat))
}

## newdata for prediction
newdata <- expand.grid(age = 1:9, hmo = factor(0:1), died = factor(0:1))
newdata$yhat <- predict(m1, newdata, type = "response")

set.seed(10)
res <- boot(dat, f, R = 1200, newdata = newdata, parallel = "snow", ncpus = 4)
## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              basicLL = basic[4], basicLL = basic[5]))
}))

## add row names
row.names(parms) <- names(coef(m1))
## print results
parms

## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              basicLL = basic[4], basicLL = basic[5]))
}))

## add row names
row.names(expparms) <- names(coef(m1))
## print results
expparms
ggplot(newdata, aes(x = age, y = yhat, colour = hmo))  +
  geom_point() +
  geom_line() +
  facet_wrap(~ died)
## get the bootstrapped percentile CIs
yhat <- t(sapply(10 + (1:nrow(newdata)), function(i) {
  out <- boot.ci(res, index = i, type = c("perc"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5]))
}))

## merge CIs with predicted values
newdata <- cbind(newdata, yhat)

## graph with CIs
ggplot(newdata, aes(x = age, y = yhat, colour = hmo, fill = hmo))  +
  geom_ribbon(aes(ymin = pLL, ymax = pUL), alpha = .25) +
  geom_point() +
  geom_line() +
  facet_wrap(~ died)

