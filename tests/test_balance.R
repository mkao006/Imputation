context("Balance functions")

aCodes = unique(okrapd$areaCode)[1:2]
data = okrapd[1:8,]
params = defaultImputationParameters()
params$flagTable = rbind(params$flagTable, data.frame(
    flagObservationStatus = c("*", "F"),
    flagObservationWeights = c(.3, .4)))
assignParameters(params)

test_that("balanceAreaHarvested works as it should", {
    #Test all 8 combinations of missing values:
    data[,productionValue := c(NA, NA, NA, NA, 1, 2, 3, 4)]
    data[,areaHarvestedValue := c(NA, NA, 1, 2, NA, NA, 3, 4)]
    data[,yieldValue := c(NA, 1, NA, 2, NA, 3, NA, 4)]
    balanceAreaHarvested(imputationParameters = params, data = data)
    # Balancing one observation:
    expect_that(data[6,areaHarvestedValue], equals(2/3))
    # Other observations are not balanced:
    expect_true(all(is.na(data[c(1, 2, 5), areaHarvestedValue])))
    expect_that(data[c(3, 4, 7, 8), areaHarvestedValue], equals(1:4))
})

test_that("balanceProduction works as it should", {
    #Test all 8 combinations of missing values:
    data[,productionValue := c(NA, NA, NA, NA, 1, 2, 3, 4)]
    data[,areaHarvestedValue := c(NA, NA, 1, 2, NA, NA, 3, 4)]
    data[,yieldValue := c(NA, 1, NA, 2, NA, 3, NA, 4)]
    balanceProduction(imputationParameters = params, data = data)
    # Balancing one observation:
    expect_that(data[4,productionValue], equals(4))
    # Other observations are not balanced:
    expect_true(all(is.na(data[1:3, productionValue])))
    expect_that(data[5:8, productionValue], equals(1:4))    
})