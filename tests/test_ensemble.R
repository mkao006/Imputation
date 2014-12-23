context("Ensemble models")

test_that("Imputation returns numeric", {
    x = c(rep(10, 5), rep(NA, 5))
    expect_that( ensembleImpute(x), is_a("numeric"))
    expect_true( length(ensembleImpute(x))==10 )
})

test_that("Model range is respected", {
    x = c(rep(NA, 5), 1:10, rep(NA, 5))
    fit = ensembleImpute(x, errorType = "raw",
        modelExtrapolationRange = rep(0, length(allDefaultModels())))
    expect_that( length(na.omit(fit)), equals(10) )
    fit = ensembleImpute(x, errorType = "loocv",
        modelExtrapolationRange = rep(1, length(allDefaultModels())))
    expect_that( length(na.omit(fit)), equals(12) )
    fit = ensembleImpute(x, errorType = "raw",
        modelExtrapolationRange = rep(Inf, length(allDefaultModels())))
    expect_that( length(na.omit(fit)), equals(20) )
})

test_that("Arguments work", {
    x = c(1:5, NA, rep(8,5))
    # Using is_a("numeric") as the check, but really we just want to ensure
    # that the ensembleImpute call doesn't generate any errors.
    expect_that( ensembleImpute(x, maximumWeights = .8, errorType = "raw",
            ensembleModel = allDefaultModels()[1:4])
        ,is_a("numeric"))
    expect_that( ensembleImpute(x, maximumWeights = .6, errorType = "loocv",
            ensembleModel = allDefaultModels()[5:9],
            errorFunction = function(x) mean(abs(x)))
        ,is_a("numeric"))
    expect_that( ensembleImpute(x, restrictWeights = F, errorType = "raw",
            ensembleModel = allDefaultModels(),
            errorFunction = function(x) max(x))
        ,is_a("numeric"))
    })

test_that("Maximum weight argument respected", {
    # Linear model should fit very well
    x = c(1:10, NA) + rnorm(11, sd=.01)
    fits = computeEnsembleFit(x)
    weights = computeEnsembleWeight(x, fits = fits, errorType = "loocv",
        ensembleModel = allDefaultModels())
    expect_true( max(weights) <= .70001)  #.00001 for numerical tol. 
    weights = computeEnsembleWeight(x, fits = fits, errorType = "loocv",
        ensembleModel = allDefaultModels(), maximumWeights = .5)
    expect_true( max(weights) <= .50001)  #.00001 for numerical tol.
})

# test_that("extendSimpleModel is the same as running models individually", {
#     data = copy(okrapd)
#     remove0M(data = data, value = "productionValue", flag = "productionFlag")
#     extendSimpleModel( data = data, columnNames = defaultColumnNames(),
#         model = defaultLoess, variable = "production", newColumnName = "test" )
#     for(aCode in unique(data$areaCode) ){
#         simpleOutput = defaultLoess( data[areaCode==aCode, productionValue] )
#         names(simpleOutput) = NULL
#         expect_that( simpleOutput, equals( data[areaCode==aCode, test] ) )
#     }
# })