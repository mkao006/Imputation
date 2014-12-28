context("Computing Errors")

test_that("MSE matches manual case", {
    data = okrapd[1:10,]
    epsilon = rnorm(10)
    data$productionValue = 1:10 + epsilon
    error = computeErrorRate(data = data, columnNames = defaultColumnNames(),
        value = "productionValue", flag = "productionFlag", fit = 1:10,
        errorType = "raw")
    expect_that( abs(epsilon), equals(error) )
})

test_that("LOOCV matches manual case", {
    data = okrapd[1:10,]
    data$productionValue = rnorm(10)
    error = computeErrorRate(data = data, columnNames = defaultColumnNames(),
        value = "productionValue", flag = "productionFlag", fit = 1:10,
        errorType = "loocv", model = allDefaultModels()[["defaultMean"]],
        cvGroup = 1:10)
    x = data$productionValue
    mu = mean(x)
    n = length(x)
    manualCalculation = sapply(1:n, function(i){
        crossValidatedMu = mu*n/(n-1) - x[i]/(n-1)
        error = abs(x[i] - crossValidatedMu)
    })
    expect_that( manualCalculation, equals(error) )
})

test_that("Function errors out if arguments are invalid", {
    data = okrapd[1:10,]
    cNames = defaultColumnNames()
    # fit must be supplied if errorType = "raw"
    expect_that( computeErrorRate(data = data, columnNames = cNames,
            value = "productionValue", flag = "productionFlag",
            errorType = "raw"),
        throws_error('argument "fit" is missing') )
    # model must be supplied if errorType = "loocv"
    expect_that( computeErrorRate(data = data, columnNames = cNames,
            value = "productionValue", flag = "productionFlag",
            errorType = "loocv"),
        throws_error('Cannot perform leave-one-out cross-validation') )
    # cvGroup must be supplied if errorType = "loocv"
    expect_that( computeErrorRate(data = data, columnNames = cNames,
            value = "productionValue", flag = "productionFlag",
            model = allDefaultModels()[["defaultMean"]],
            errorType = "loocv"),
        throws_error('argument "cvGroup" is missing') )
    # cvGroup must be valid
    expect_that( computeErrorRate(data = data, columnNames = cNames,
            value = "productionValue", flag = "productionFlag",
            model = allDefaultModels()[["defaultMean"]],
            errorType = "loocv", cvGroup = rep(1,10)),
        throws_error('cvGroup must have at least two unique values!') )
})