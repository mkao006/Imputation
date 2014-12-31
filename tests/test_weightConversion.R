context("Weight Conversion")
weightVector = c(.2, .2, .3, .1, .2)
range = c(Inf, 0, Inf, Inf, 1)
x = c(NA, NA, NA, 1:10, NA, 12, NA, 13, NA)
weightMatrix = cbind(c(1/3, 1/3, 1/4, rep(.2,14), 1/4),
                     c(0  , 0  , 0  , rep(.2,14), 0  ),
                     c(1/2, 1/2, 3/8, rep(.3,14), 3/8),
                     c(1/6, 1/6, 1/8, rep(.1,14), 1/8),
                     c(0  , 0  , 1/4, rep(.2,14), 1/4))

test_that("Weight vector converts to matrix and back", {
    matrix = weightVectorToMatrix(x = x, w = weightVector,
        modelExtrapolationRange = range)
    vector = weightMatrixToVector(weightMatrix = matrix)
    expect_that(vector, equals(weightVector))
})

test_that("Weight matrix converts to vector and back", {
    vector = weightMatrixToVector(weightMatrix = weightMatrix)
    matrix = weightVectorToMatrix(x = x, w = vector,
        modelExtrapolationRange = range)
    expect_that(matrix, equals(weightMatrix))
})

test_that("Equal weight vector yields equal weight matrix", {
    range = c(Inf, 0, 1, 2, 3)
    weightVector = rep(1,5)
    matrix = weightVectorToMatrix(x = x, w = weightVector,
        modelExtrapolationRange = range)
    expect_true(all(matrix[4:17,]==.2))
    expect_true(all(matrix[c(1:3,18),2] == 0))
    expect_true(all(matrix[1:2,3] == 0))
    expect_that(matrix[1,4], equals(0))
})
