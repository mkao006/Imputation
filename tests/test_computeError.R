context("Computating Errors")

test_that("MSE matches manual case", {
    x = 1:10
    epsilon = rnorm(10)
    error = computeErrorRate(x = x + epsilon, fit = x,
        errorType = "raw")
    expect_that( abs(epsilon), equals(error) )
})

test_that("LOOCV matches manual case", {
    x = rnorm(10)
    error = computeErrorRate( x = x, fit = rep(mean(x), length(x)),
        model = defaultMean, errorType = "loocv" )
    mu = mean(x)
    n = length(x)
    manualCalculation = sapply(1:n, function(i){
        crossValidatedMu = mu*n/(n-1) - x[i]/(n-1)
        error = abs(x[i] - crossValidatedMu)
    })
    expect_that( manualCalculation, equals(error) )
})