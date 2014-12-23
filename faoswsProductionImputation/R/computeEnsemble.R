##' Function to combine the ensembles
##'
##' @param fits A list of fitted values from models.
##' @param weights A vector for the weights of each model.
##' @export


computeEnsemble = function(fits, weights){
    
    ### Data quality checks
    # Rearrange elements of fits if needed
    fits = fits[names(weights)]
    stopifnot(all(names(weights) == names(fits)))
    stopifnot(length(fits) == ncol(weights))
    if(!all(sapply(fits, length) == nrow(weights)))
        stop("Length of fits do not match nrow(weights)!")
    
    fitsMatrix = matrix(unlist(fits), ncol = length(fits))
    fitsMatrix[is.na(fitsMatrix)] = 0
    weightedFit = fitsMatrix * weights
    apply(weightedFit, 1, sum)
}
