context("Computing Errors")
rm(list = ls())

test_that("MSE matches manual case", {
    data = okrapd[1:10,]
    epsilon = rnorm(10)
    data$productionValue = 1:10 + epsilon
    params = defaultImputationParameters()
    params$errorType = "raw"
    params$flagTable = data.frame(
        flagObservationStatus = c("", "T", "E", "I", "M", "*", "F"),
        flagObservationWeights = c(1, .8, .75, .5, 0, .6, .7),
        stringsAsFactors = F)
    assignParameters(params)
    error = computeErrorRate(data = data, fit = 1:10,
                             imputationParameters = params)
    expect_that(abs(epsilon), equals(error))
})

test_that("LOOCV matches manual case", {
    data = okrapd[1:10,]
    data$productionValue = rnorm(10)
    reassignGlobalVariable("errorType", "loocv")
    error = computeErrorRate(data = data, fit = 1:10,
        model = allDefaultModels()[["defaultMean"]], cvGroup = 1:10)
    x = data$productionValue
    mu = mean(x)
    n = length(x)
    manualCalculation = sapply(1:n, function(i){
        crossValidatedMu = mu*n/(n-1) - x[i]/(n-1)
        error = abs(x[i] - crossValidatedMu)
    })
    expect_that(manualCalculation, equals(error))
})

test_that("Function errors out if arguments are invalid", {
    data = okrapd[1:10,]
    reassignGlobalVariable("errorType", "raw")
    # fit must be supplied if errorType = "raw"
    expect_that(computeErrorRate(data = data),
        throws_error('argument "fit" is missing'))
    reassignGlobalVariable("errorType", "loocv")
    # model must be supplied if errorType = "loocv"
    expect_that(computeErrorRate(data = data, fit = 1:10),
        throws_error('model, "ensembleModel"'))
    # cvGroup must be supplied if errorType = "loocv"
    expect_that(computeErrorRate(data = data, fit = 1:10,
            model = allDefaultModels()[["defaultMean"]]),
        throws_error('argument "cvGroup" is missing'))
    # cvGroup must be valid
    expect_that(computeErrorRate(data = data, fit = 1:10,
            model = allDefaultModels()[["defaultMean"]],
            cvGroup = rep(1,10)),
        throws_error('length\\(unique\\(cvGroup\\)\\)'))
})