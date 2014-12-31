context("default* models")

test_that("Returns list", { 
    expect_that(allDefaultModels(), is_a("list"))
})

test_that("All elements are of class ensembleModel", {
    type = sapply(allDefaultModels(), function(x) is(x)[1])
    expect_true(all(type=="ensembleModel"))
})

test_that("getDefaultRange works for all default models", {
    range = getDefaultRange(allDefaultModels())
    expect_that(range, is_a("numeric"))
    expect_true(length(range)==length(allDefaultModels()))
})

models = allDefaultModels()
countryCommodityFilter =
    sapply(models, function(x) x@level == "countryCommodity")
modelsCC = models[countryCommodityFilter]
commodityFilter = sapply(models, function(x) x@level == "commodity")
modelsC = models[commodityFilter]

test_that("Country-Commodity models return vectors of appropriate length", {
    x = 1:10
    for(model in modelsCC)
        expect_that(length(model@model(x)), equals(10))
})

test_that("Results are all NA or all numeric for Country-Commodity models", {
    x = c(abs(rnorm(5)), rep(NA,10))
    for(model in modelsCC)
        expect_true(length(na.omit(model@model(x))) == 15 | 
                    length(na.omit(model@model(x))) == 0)
})