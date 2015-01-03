context("Remove functions")
rm(list = ls())

processingParameters = defaultProcessingParameters()
assignParameters(processingParameters)

test_that("remove0M", {
    # yield test
    data = okrapd[1:10,]
    data[,c("yieldValue", "yieldFlag"):=list(0, "M")]
    remove0M(data = data, value = "yieldValue", flag = "yieldFlag")
    expect_true(all(is.na(data[,yieldValue])))
    # production test
    data = okrapd[1:10,]
    data[,c("productionValue", "productionFlag"):=list(0, "M")]
    remove0M(data = data, value = "productionValue", flag = "productionFlag")
    expect_true(all(is.na(data[,productionValue])))
    # areaHarvested test
    data = okrapd[1:10,]
    data[,c("areaHarvestedValue", "areaHarvestedFlag"):=list(0, "M")]
    remove0M(data = data, value = "areaHarvestedValue",
        flag = "areaHarvestedFlag")
    expect_true(all(is.na(data[,areaHarvestedValue])))
    # Error should be thrown if missing values aren't 0 or NA
    data = okrapd[1:10,]
    data[,c("yieldValue", "yieldFlag"):=list(100, "M")]
    expect_that(remove0M(data = data, value = "yieldValue",
        flag = "yieldFlag"),
        throws_error("Some missing values are not 0 or NA!"))
})

test_that("removeImputation", {
    flagValues = rep(" ", 10)
    flagValues[sample(10, size=4)] = "E"
    # yield test
    data = okrapd[1:10,]
    data[,yieldFlag:=flagValues]
    removeImputation(data = data, value = "yieldValue", flag = "yieldFlag")
    expect_true(all(is.na(data[flagValues=="E", yieldValue])))
    expect_that(data[flagValues=="E", yieldFlag], equals(rep("M", 4)))
    # production test
    data = okrapd[1:10,]
    data[,productionFlag:=flagValues]
    removeImputation(data = data, value = "productionValue",
        flag = "productionFlag")
    expect_true(all(is.na(data[flagValues=="E", productionValue])))
    expect_that(data[flagValues=="E", productionFlag], equals(rep("M", 4)))
    # areaHarvested test
    data = okrapd[1:10,]
    data[,areaHarvestedFlag:=flagValues]
    removeImputation(data = data, value = "areaHarvestedValue",
        flag = "areaHarvestedFlag")
    expect_true(all(is.na(data[flagValues=="E", areaHarvestedValue])))
    expect_that(data[flagValues=="E", areaHarvestedFlag], equals(rep("M", 4)))
})

test_that("removeNoInfo", {
    
})

test_that("removeZeroConflict", {
    data = okrapd[1:10,]
    data[,areaHarvestedValue := c(rep(0,5), rep(100,5))]
    data[,productionValue := c(0,0,100,100,100,0,0,100,100,100)]
    removeZeroConflict(data = data)
    expect_true(all(is.na(data[6:7, areaHarvestedValue])))
    expect_that(data[6:7, areaHarvestedFlag], equals(c("M","M")))
    expect_true(all(is.na(data[3:5, productionValue])))
    expect_that(data[3:5, productionFlag], equals(c("M","M","M")))
    expect_true(all(is.na(data[3:7, yieldValue])))
    expect_that(data[3:7, yieldFlag], equals(rep("M", 5)))
})
