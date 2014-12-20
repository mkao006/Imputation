context("Run on simple dataset")
library(splines)

# Create a small dataset for testing:
aCodes = unique(okrapd$areaCode)[1:2]
data = okrapd[areaCode %in% aCodes,]
data[20:34,productionValue:=NA]
columnNames = defaultColumnNames()
flagTable = rbind( faoswsFlagTable, data.frame(
    flagObservationStatus = c("*", "F"),
    flagObservationWeights = c(.3, .4) ) )

# Testing imputeYield is slow:
# test_that("imputeYield on test dataset", {
#     naYield = is.na(data[,yieldValue])
#     imputeYield(columnNames = columnNames, imputationFlag = "I",
#         newMethodFlag = "I", data = data, flagTable = flagTable)
#     expect_that( data[naYield, yieldValue], is_a("numeric") )
# })

test_that("imputeProduction on test dataset", {
    imputeProduction(columnNames = defaultColumnNames(),
        newMethodFlag = "I", data = data, flagTable = flagTable)
    expect_that( data[20:34, productionValue], is_a("numeric") )
})
