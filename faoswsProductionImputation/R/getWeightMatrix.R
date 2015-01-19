##' Get Weight Matrix
##' 
##' Initially, a weight is computed for each model and byKey.  However, some
##' models are not valid for some observations (as certain models are limited
##' in how far they can extrapolate outside the range of the data).  Thus, the
##' final weight for each ensemble model at each observation will depend on
##' that models performance for that byKey group as well as if that model is
##' valid at that point.
##' 
##' This function creates a weight matrix to use in constructing the final
##' ensemble.  If F is a nxk matrix (n = number of observations, k = number of
##' models) containing the fitted models, then this function constructs W,
##' another nxk matrix of weights.  The final ensemble estimate for observation
##' i can be computed by sum(F[i,]*W[i,]).
##'
##' @param data The data.table containing the data.
##' @param value The column name of data which contains the variable being
##' imputed.
##' @param byKey The column name of data which specifies the grouping
##' (typically the areaCode).
##' @param yearValue The column name of data which specifies the year.
##' @param w The weights data.table, typically as produced in
##' computeEnsembleWeight.  There should be at least three columns: byKey,
##' model, and weight.  Weight gives the model weight for model within the
##' byKey group.
##' @param ensembleModels The list of ensemble models used.  Each element of
##' this list should be of class ensembleModel.  This is used to determine
##' the extrapolation range of each model in the ensemble.
##' 
##' @return A matrix of weights that can be multiplied by the fitted models to
##' give the imputed values.  Rows corresponding to non-missing values in data
##' have values of NA.
##' 
##' @export
##' 

getWeightMatrix = function(data, w, imputationParameters = NULL){

    ### Data Quality Checks
    if(!exists("parametersAssigned") || !parametersAssigned)
        stopifnot(!is.null(imputationParameters))
    if(!is.null(imputationParameters))
        assignParameters(imputationParameters)
    if(!ensuredData)
        ensureData(data = data)
    if(!ensuredFlagTable)
        ensureFlagTable(flagTable = flagTable, data = data)
    stopifnot(is(w, "data.table"))
    # w should have one row for each model at each byKey level
    stopifnot(nrow(w) == length(unique(data[[byKey]])) *
                  length(ensembleModels))
    
    ### Run the function:
    setnames(data, old = c(imputationValueColumn, byKey, yearValue),
             new = c("imputationValueColumn", "byKey", "yearValue"))
    data[, extrapolationRange := 
             getObservedExtrapolationRange(imputationValueColumn), by = byKey]
    weightMatrix = merge(data[,.(byKey, imputationValueColumn, yearValue,
                               extrapolationRange)],
                          w, by = "byKey", all = TRUE, allow.cartesian = TRUE)
    # Set data back to it's original state
    data[, extrapolationRange := NULL]
    setnames(data, old = c("imputationValueColumn", "byKey", "yearValue"),
             new = c(imputationValueColumn, byKey, yearValue))
    # Set weights to 0 that are outside of extrapolationRange
    range = sapply(ensembleModels, function(model){
        model@extrapolationRange
    })
    weightMatrix[, allowedRange := range[model]]
    weightMatrix[allowedRange < extrapolationRange, weight := 0]
    # Renormalize weights so all columns add to 1
    weightMatrix[, weight := weight / sum(weight), by = list(byKey, yearValue)]
    weightMatrix = dcast.data.table(weightMatrix, byKey + yearValue ~ model,
                                     value.var = "weight")
    weightMatrix[, c("byKey", "yearValue") := list(NULL, NULL)]
    weightMatrix[!is.na(data[[imputationValueColumn]])] = NA
    return(weightMatrix)
}
