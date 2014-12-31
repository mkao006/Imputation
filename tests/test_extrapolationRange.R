context("Extrapolation Range")

test_that("Extrapolation Range is as expected", {
    expect_that(getObservedExtrapolationRange(c(NA,NA,0,NA,NA)),
                equals(c(2,1,0,1,2)))
    expect_that(getObservedExtrapolationRange(c(NA,NA,0,NA,1,NA,2,NA,NA)),
                equals(c(2,1,0,0,0,0,0,1,2)))
    expect_that(getObservedExtrapolationRange(c(0,NA,1,NA,2,NA,NA)),
                equals(c(0,0,0,0,0,1,2)))
    expect_that(getObservedExtrapolationRange(c(1, rep(NA, 10), 2)),
                equals(rep(0, 12)))
})

