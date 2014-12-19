context("default* models")

test_that("Returns list", {
    expect_that( allDefaultModels(), is_a("list") )
})

test_that("All elements are functions", {
    type = sapply( allDefaultModels(), function(x){ is(x)[1] } )
    expect_true( all(type=="function") )
})

models = allDefaultModels()

test_that("Models return vectors of appropriate length", {
    x = 1:10
    for(model in models)
        expect_that( length( model(x) ), equals(10) )
})

test_that("Results are all NA or all numeric", {
    x = c(abs(rnorm(5)), rep(NA,10))
    for(model in models)
        expect_true( length( na.omit(model(x)) ) == 15 | 
                     length( na.omit(model(x)) ) == 0  )
})

test_that("getDefaultRange works for all default models", {
    range = getDefaultRange( allDefaultModels() )
    expect_that(range, is_a("numeric") )
    expect_true(length(range)==length(allDefaultModels()))
})