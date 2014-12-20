context("columnNames")
allUsedNames = names(defaultColumnNames())
suppressWarnings(rm(list = allUsedNames))

test_that("defaultColumnNames returns character vector", {
    expect_that(defaultColumnNames(), is_a("character"))
})

test_that("defaultColumnNames are as expected (if this fails because of an updated
    definition of defaultColumnNames, then the test file will need to be updated).",{
    expect_that(names(defaultColumnNames()), equals(c("productionValue",
        "productionObservationFlag", "productionMethodFlag",
        "areaHarvestedValue", "areaHarvestedObservationFlag",
        "areaHarvestedMethodFlag", "yieldValue", "yieldObservationFlag",
        "yieldMethodFlag", "yearValue", "byKey")))
})

test_that("Assignment to correct environment", {
    f = function(){
        assignColumnNames( defaultColumnNames(), environment() )
        print(productionValue)
    }
    expect_that( f(), prints_text("productionValue") )
    #Function should assign to f's environment, not global:
    expect_that( productionValue, throws_error("object 'productionValue' not found") )
})

test_that("All expected names are assigned, and no more", {
    currentObjects = ls(envir = environment())
    assignColumnNames(defaultColumnNames())
    expect_true( exists("productionValue") )
    expect_true( exists("productionObservationFlag") )
    expect_true( exists("productionMethodFlag") )
    expect_true( exists("yieldValue") )
    expect_true( exists("yieldObservationFlag") )
    expect_true( exists("yieldMethodFlag") )
    expect_true( exists("areaHarvestedValue") )
    expect_true( exists("areaHarvestedObservationFlag") )
    expect_true( exists("areaHarvestedMethodFlag") )
    expect_true( exists("byKey") )
    expect_true( exists("yearValue") )
    expect_true( all( sapply(allUsedNames, exists, envir = environment()) ) )
    newObjects = setdiff( ls(envir = environment()), currentObjects )    
    expect_true( setequal( newObjects, c(allUsedNames, "currentObjects") ) )
})
