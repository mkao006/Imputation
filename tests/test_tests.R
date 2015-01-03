context("Testing functions")
library(faoswsFlag)

params = defaultImputationParameters()
params$flagTable = data.frame(
    flagObservationStatus = c("", "T", "E", "I", "M", "*", "F"),
    flagObservationWeights = c(1, .8, .75, .5, .4, .3, 0),
    stringsAsFactors = FALSE)
assignParameters(params)

### ensureData()
test_that("ensureData works", {
    # ensureData() should run without any problems. However, nothing is
    # returned, printed, etc. so just ensure that it takes less than a minute.
    expect_that(ensureData(okrapd), takes_less_than(amount = 60))
})

test_that("ensureData fails when expected", {
    data = copy(okrapd)
    setnames(data, old = "productionValue", new = "prodValue")
    expect_that(ensureData(data = data),
        throws_error("The following columns do not exist in data"))
    reassignGlobalVariable("productionValue", "prodValue")
    # Now it should work again
    expect_that(ensureData(data = data), takes_less_than(amount = 60))
    reassignGlobalVariable("productionValue", "productionValue")
})

### ensureFlagTable()
test_that("ensureFlagTable works when expected", {
    data = copy(okrapd)
    expect_that(ensureFlagTable(flagTable = flagTable,
        data = data),
        is_a("NULL"))
})

test_that("ensureFlagTable fails when expected", {
    data = copy(okrapd)
    data[[productionObservationFlag]][10] = "Invalid Value"
    expect_that(ensureFlagTable(flagTable = flagTable,
        data = data),
        throws_error("Some observation flags are not in the flag table!"))
})

test_that("Default parameters pass checks", {
    ensureImputationParameters(defaultImputationParameters())
    ensureProcessingParameters(defaultProcessingParameters())
})