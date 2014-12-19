context("Testing functions")
library(faoswsFlag)

# Define some simple data objects:
columnNames = c(productionValue              = "productionValue",
                productionObservationFlag    = "productionObservationFlag",
                productionMethodFlag         = "productionMethodFlag",
                areaHarvestedValue           = "areaHarvestedValue",
                areaHarvestedObservationFlag = "areaHarvestedObservationFlag",
                areaHarvestedMethodFlag      = "areaHarvestedMethodFlag",
                yieldValue                   = "yieldValue",
                yieldObservationFlag         = "yieldObservationFlag",
                yieldMethodFlag              = "yieldMethodFlag",
                yearValue                    = "yearValue",
                byKey                        = "byKey")
data = data.table(productionValue              = 1:10,
                  productionObservationFlag    =
                      sample(faoswsFlagTable[,1], size = 10, replace = TRUE),
                  productionMethodFlag         = 
                      sample(faoswsFlagTable[,1], size = 10, replace = TRUE),
                  areaHarvestedValue           = 1:10,
                  areaHarvestedObservationFlag = 
                      sample(faoswsFlagTable[,1], size = 10, replace = TRUE),
                  areaHarvestedMethodFlag      = 
                      sample(faoswsFlagTable[,1], size = 10, replace = TRUE),
                  yieldValue                   = 1:10,
                  yieldObservationFlag         = 
                      sample(faoswsFlagTable[,1], size = 10, replace = TRUE),
                  yieldMethodFlag              = 
                      sample(faoswsFlagTable[,1], size = 10, replace = TRUE),
                  yearValue                    = 2000+1:10,
                  byKey                        = rep(1,10))

### testColumnNames()
test_that("testColumnNames works", {
    expect_that( testColumnNames(columnNames = columnNames, data = data),
        is_a("NULL") )
})

test_that("testColumnNames fails when expected", {
    setnames(data, old = "productionValue", new = "prodValue" )
    expect_that( testColumnNames(columnNames = columnNames, data = data),
        throws_error("The following columns do not exist in data") )
    columnNames[1] = "prodValue"
    expect_that( testColumnNames(columnNames = columnNames, data = data),
        is_a("NULL") )
    names(columnNames)[1] = "prodValue"
    expect_that( testColumnNames(columnNames = columnNames, data = data),
        throws_error("The following elements do not exist in columnNames") )
})

### testFlagTable()
test_that("testFlagTable works when expected", {
    expect_that( testFlagTable(flagTable = faoswsFlagTable,
        data = data, columnNames = columnNames),
        is_a("NULL") )
})

test_that("testFlagTable fails when expected", {
    data[10,productionObservationFlag:="Invalid Value"]
    expect_that( testFlagTable(flagTable = faoswsFlagTable,
        data = data, columnNames = columnNames),
        throws_error("Some observation flags are not in the flag table!") )
})