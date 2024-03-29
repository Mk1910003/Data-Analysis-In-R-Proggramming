require(ggplot2)
require(pscl)
require(MASS)
require(boot)
zinb <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})

summary(zinb)
## histogram with x axis in log10 scale
ggplot(zinb, aes(count, fill = camper)) +
  geom_histogram() +
  scale_x_log10() +
  facet_grid(camper ~ ., margins=TRUE, scales="free_y")
m1 <- zeroinfl(count ~ child + camper | persons,
               data = zinb, dist = "negbin")
summary(m1)
m0 <- update(m1, . ~ 1)

pchisq(2 * (logLik(m1) - logLik(m0)), df = 3, lower.tail=FALSE)
dput(round(coef(m1, "count"), 4))
dput(round(coef(m1, "zero"), 4))
f <- function(data, i) {
  require(pscl)
  m <- zeroinfl(count ~ child + camper | persons,
                data = data[i, ], dist = "negbin",
                start = list(count = c(1.3711, -1.5152, 0.879), zero = c(1.6028, -1.6663)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
(res <- boot(zinb, f, R = 1200, parallel = "snow", ncpus = 4))
## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 9, 11), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaUL = bca[5]))
}))

## add row names
row.names(parms) <- names(coef(m1))
## print results
parms
## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaUL = bca[5]))
}))

## add row names
row.names(expparms) <- names(coef(m1))
## print results
expparms
newdata1 <- expand.grid(0:3, factor(0:1), 1:4)
colnames(newdata1) <- c("child", "camper", "persons")
newdata1$phat <- predict(m1, newdata1)

ggplot(newdata1, aes(x = child, y = phat, colour = factor(persons))) +
  geom_point() +
  geom_line() +
  facet_wrap(~camper) +
  labs(x = "Number of Children", y = "Predicted Fish Caught")




