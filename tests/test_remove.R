context("Remove functions")

test_that("remove0M", {
    # yield test
    data = okrapd[1:10,]
    data[,c("yieldValue", "yieldFlag"):=list(0, "M")]
    remove0M(data = data, value = "yieldValue", flag = "yieldFlag",
        naFlag = "M")
    expect_true( all( is.na( data[,yieldValue] ) ) )
    # production test
    data = okrapd[1:10,]
    data[,c("productionValue", "productionFlag"):=list(0, "M")]
    remove0M(data = data, value = "productionValue", flag = "productionFlag",
        naFlag = "M")
    expect_true( all( is.na( data[,productionValue] ) ) )
    # areaHarvested test
    data = okrapd[1:10,]
    data[,c("areaHarvestedValue", "areaHarvestedFlag"):=list(0, "M")]
    remove0M(data = data, value = "areaHarvestedValue",
        flag = "areaHarvestedFlag", naFlag = "M")
    expect_true( all( is.na( data[,areaHarvestedValue] ) ) )
    # Error should be thrown if missing values aren't 0 or NA
    data = okrapd[1:10,]
    data[,c("yieldValue", "yieldFlag"):=list(100, "M")]
    expect_that( remove0M(data = data, value = "yieldValue",
        flag = "yieldFlag", naFlag = "M"),
        throws_error("Some missing values are not 0 or NA!") )
})

test_that("removeImputation", {
    flagValues = rep(" ", 10)
    flagValues[sample(10, size=4)] = "T"
    # yield test
    data = okrapd[1:10,]
    data[,yieldFlag:=flagValues]
    removeImputation(data = data, value = "yieldValue", flag = "yieldFlag",
        imputedFlag = "T", naFlag = "M")
    expect_true(all(is.na( data[flagValues=="T", yieldValue] ) ) )
    expect_that(data[flagValues=="T", yieldFlag], equals(rep("M", 4)))
    # production test
    data = okrapd[1:10,]
    data[,productionFlag:=flagValues]
    removeImputation(data = data, value = "productionValue",
        flag = "productionFlag", imputedFlag = "T", naFlag = "M")
    expect_true(all(is.na( data[flagValues=="T", productionValue] ) ) )
    expect_that(data[flagValues=="T", productionFlag], equals(rep("M", 4)))
    # areaHarvested test
    data = okrapd[1:10,]
    data[,areaHarvestedFlag:=flagValues]
    removeImputation(data = data, value = "areaHarvestedValue",
        flag = "areaHarvestedFlag", imputedFlag = "T", naFlag = "M")
    expect_true(all(is.na( data[flagValues=="T", areaHarvestedValue] ) ) )
    expect_that(data[flagValues=="T", areaHarvestedFlag], equals(rep("M", 4)))
})

test_that("removeNoInfo", {
    
})

test_that("removeZeroConflict", {
    data = okrapd[1:10,]
    data[,areaHarvestedValue := c(rep(0,5), rep(100,5))]
    data[,productionValue := c(0,0,100,100,100,0,0,100,100,100)]
    removeZeroConflict(columnNames = defaultColumnNames(), data = data)
    expect_true(all(is.na( data[6:7, areaHarvestedValue] ) ) )
    expect_that( data[6:7, areaHarvestedFlag], equals(c("M","M")) )
    expect_true(all(is.na( data[3:5, productionValue] ) ) )
    expect_that( data[3:5, productionFlag], equals(c("M","M","M")) )
    expect_true(all(is.na( data[3:7, yieldValue] ) ) )
    expect_that( data[3:7, yieldFlag], equals(rep("M", 5)) )
})