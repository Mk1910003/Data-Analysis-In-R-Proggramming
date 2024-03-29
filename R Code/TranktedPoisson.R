#install.packages("VGAM")
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
m1 <- vglm(stay ~ age + hmo + died, family = pospoisson(), data = dat)
summary(m1)
output <- data.frame(resid = resid(m1), fitted = fitted(m1))
ggplot(output, aes(fitted, resid)) +
  geom_jitter(position=position_jitter(width=.25), alpha=.5) +
  stat_smooth(method="loess")

output <- within(output, {
  broken <- cut(fitted, hist(fitted, plot=FALSE)$breaks)
})

ggplot(output, aes(broken, resid)) +
  geom_boxplot() +
  geom_jitter(alpha=.25)
dput(round(coef(m1),3))
f <- function(data, i) {
  require(VGAM)
  m <- vglm(formula = stay ~ age + hmo + died, family = pospoisson(),
            data = data[i, ], coefstart = c(2.436, -0.014, -0.136, -0.204))
  as.vector(t(coef(summary(m))[, 1:2]))
}

set.seed(10)
res <- boot(dat, f, R = 1200, parallel = "snow", ncpus = 4)

## print results
res
## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              basicLL = basic[4], basicLL = basic[5]))
}))

## add row names
row.names(parms) <- names(coef(m1))
## print results
parms
## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              basicLL = basic[4], basicLL = basic[5]))
}))

## add row names
row.names(expparms) <- names(coef(m1))
## print results
expparms
newdata <- expand.grid(age = 1:9, hmo = factor(0:1), died = factor(0:1))
newdata$yhat <- predict(m1, newdata, type = "response")

ggplot(newdata, aes(x = age, y = yhat, colour = hmo))  +
  geom_point() +
  geom_line() +
  facet_wrap(~ died)
## function to return predicted values
fpred <- function(data, i, newdata) {
  require(VGAM)
  m <- vglm(formula = stay ~ age + hmo + died, family = pospoisson(),
            data = data[i, ], coefstart = c(2.436, -0.014, -0.136, -0.204))
  predict(m, newdata, type = "response")
}

## set seed and run bootstrap with 1,200 draws
set.seed(10)
respred <- boot(dat, fpred, R = 1200, newdata = newdata,
                parallel = "snow", ncpus = 4)

## get the bootstrapped percentile CIs
yhat <- t(sapply(1:nrow(newdata), function(i) {
  out <- boot.ci(respred, index = i, type = c("perc"))
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

