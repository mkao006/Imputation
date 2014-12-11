##' Convert the weights vector to a matrix
##' 
##' The default weights vector assumes that all models are valid for all
##' imputation points.  However, some imputations may be extrapolations that
##' are well outside the range of the data, and in this case not all models
##' will be valid.  Thus, this function takes the weights vector and adjusts it
##' for such observations.  This adjustment requires the ensembleModel to be a
##' list of lists.  Each list element should have the model function as well as
##' the "extrapolation range" of that function, i.e. how far it can be outside
##' the range of the data to be valid.  Values of Inf are acceptable.
##' 
##' Note that the extrapolation range was not a parameter in previous versions
##' of the code.  Thus, if that element is not present, the model assumes the
##' range is infinite.  This ensures backwards-compatibility.
##'
##' @param x A numeric vector to be imputed.
##' @param w The vector of weights, computed based on the errors of the models.
##' @param ensembleModel A list of the different models used to construct the
##' ensemble.  Each element of this list should be the function that was used
##' to fit the model.
##' @param modelExtrapolationRange A numeric vector specifying the valid range
##' of extrapolation for each model.  This vector must be the same length as
##' ensembleModel, as it's i-th element gives the extrapolation range for the
##' i-th element of ensembleModel.
##' 
##' @return A matrix of weights of dimension Txm, where T=number of time steps
##' and m=number of models.  The (i,j) element of this matrix, then, is the
##' weight that the j-th model should receive at the i-th time step.
##' 
##' @export

weightVectorToMatrix = function( x, w, ensembleModel, modelExtrapolationRange){
    ### Verify inputs match assumptions:
    stopifnot( length(w) == length(ensembleModel) )
    stopifnot( length(modelExtrapolationRange)==length(ensembleModel) )
    
    ### Run the function:
    firstNaCount = min( which( !is.na(x) ) ) - 1
    lastNaCount = length(x) - max( which( !is.na(x) ) )
    #Vector of the distance to the nearest observed point (for extrapolations)
    observedExtrapolationRange = c(firstNaCount:0
        ,rep(0, length(x)-firstNaCount-lastNaCount-2), 0:lastNaCount )
    validRangeMatrix = sapply( modelExtrapolationRange,
        function(x) observedExtrapolationRange <= x)
    weightMatrix = matrix(w, nrow=length(x), ncol=length(w), byrow = TRUE)
    weightMatrix = weightMatrix * validRangeMatrix
    rowTotals = apply(weightMatrix, 1, sum)
    weightMatrix = weightMatrix * 1/rowTotals
    weightMatrix
}