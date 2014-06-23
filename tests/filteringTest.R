
library(signal)
production = wheatRaw.dt[areaName == "the United States of America",
    productionImputed]


lowPass <- butter(2, 1/3, type="low")
production.low <- filter(lowPass, production)

midPass <- butter(2, c(1/3, 1/2), type = "pass")
production.mid <- filter(midPass, production)

highPass <- butter(2, 1/2, type="high")
production.high <- filter(highPass, production)


par(mfrow = c(4, 1), mar = c(0, 2, 0, 0))
plot(production, col = "black", type = "l", lwd = 3)
## lines(production.low + production.high, col = "red")
plot(production.low, col="black", pch=20)
plot(production.mid, col="black", pch=20)
plot(production.high, col="black", pch=20)

par(mfrow = c(3, 1), mar = c(0, 2, 0, 0))
production.fit = lm(production ~ production.low + production.mid +
    production.high)
summary(production.fit)
plot(production, ylim = c(0, max(production, fitted(production.fit))))
points(fitted(production.fit), pch = 19, col = "red")

areaHarvested = wheatRaw.dt[areaName == "the United States of America",
    areaHarvestedImputed]
areaHarvested.fit = lm(areaHarvested ~ production.low + production.mid +
    production.high)
summary(areaHarvested.fit)
plot(areaHarvested,
     ylim = c(0, max(areaHarvested, fitted(areaHarvested.fit))))
points(fitted(areaHarvested.fit), pch = 19, col = "red")


yield = wheatRaw.dt[areaName == "the United States of America",
    yieldImputed]
yield.fit = lm(yield ~ production.low + production.mid +
    production.high)
summary(yield.fit)
plot(yield,
     ylim = c(0, max(yield, fitted(yield.fit))))
points(fitted(yield.fit), pch = 19, col = "red")



usa.dt = wheatRaw.dt[areaName == "the United States of America",
    list(productionFit, areaHarvestedFit, yieldFit)]

band = seq(0, 1, length = 3)
for(i in 1:(length(band) - 1)){
    usa.dt[, c(paste0("band", i)) :=  filter(butter(2, c(band[i], band[i + 1]), type = "pass"), productionFit)]
}



par(mfrow = c(3, 1), mar = c(0, 2, 0, 0))
production.fit = lm(as.formula(paste0("productionFit ~ ",
    paste0(grep("band", colnames(usa.dt), value  = TRUE), collapse = " + "))),
    data = usa.dt)
summary(production.fit)
plot(production, ylim = c(0, max(production, fitted(production.fit))))
points(fitted(production.fit), pch = 19, col = "red")

areaHarvested.fit = lm(as.formula(paste0("areaHarvestedFit ~ ",
    paste0(grep("band", colnames(usa.dt), value  = TRUE), collapse = " + "))),
    data = usa.dt)
summary(areaHarvested.fit)
plot(areaHarvested,
     ylim = c(0, max(areaHarvested, fitted(areaHarvested.fit))))
points(fitted(areaHarvested.fit), pch = 19, col = "red")
areaHarvested.spline = lm(areaHarvestedFit ~ bs(productionFit, df = 10), data = usa.dt)
summary(areaHarvested.spline)
points(fitted(areaHarvested.spline), col = "blue", pch = 19)

yield.fit = lm(as.formula(paste0("yieldFit ~ ",
    paste0(grep("band", colnames(usa.dt), value  = TRUE), collapse = " + "))),
    data = usa.dt)
summary(yield.fit)
plot(yield,
     ylim = c(0, max(yield, fitted(yield.fit))))
points(fitted(yield.fit), pch = 19, col = "red")
yield.spline = lm(yieldFit ~ bs(productionFit, df = 10), data = usa.dt)
summary(yield.spline)
points(fitted(yield.spline), col = "blue", pch = 19)
