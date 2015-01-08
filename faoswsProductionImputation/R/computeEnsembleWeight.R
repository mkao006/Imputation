##' Function to compute the weights of the ensemble models
##'
##' @param data A data.table containing the data.
##' @param cvGroup A vector of the same length as nrow(data).  Entries of the
##' vector should be integers from 1 to the number of cross-validation groups
##' (typically 10).  This should be randomly assigned, and is usually created
##' by ensembleImpute.
##' @param fits The fitted values from the models.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @export
##' 

computeEnsembleWeight = function(data, cvGroup, fits,
                                 imputationParameters = NULL){
    
    ### Data Quality Checks
    if(!exists("parametersAssigned") || !parametersAssigned){
        stopifnot(!is.null(imputationParameters))
        assignParameters(imputationParameters)
    }
    if(!ensuredData)
        ensureData(data = data)
    if(!ensuredFlagTable)
        ensureFlagTable(flagTable = flagTable, data = data)
    if(!all(lapply(fits, length) == nrow(data)))
        stop("All elements of fits must have the same length as nrow(x)!")
    stopifnot(all(names(fits) == names(ensembleModels)))
    if(is.null(names(fits)))
        names(fits) = paste("Model", 1:length(fits), sep="_")
    counts = data[, sum(!is.na(get(imputationValueColumn))), by = byKey]
    if(min(counts[, V1]) == 0)
        stop("Some countries have no data.  Have you ran removeNoInfo?")
    
    ### Compute errors from each model
    error = lapply(1:length(fits),
        FUN = function(i){
            out = computeErrorRate(data = data, cvGroup = cvGroup,
                                   fit = fits[[i]],
                                   model = ensembleModels[[i]])
            out = data.table( model = names(fits)[i],
                              byKey = data[[byKey]],
                              missingValue =
                                  is.na(data[[imputationValueColumn]]),
                              year = data[[yearValue]],
                              error = out,
                              fit = fits[[i]])
        })
    error = do.call("rbind", error)
    # Sometimes, a model will fail in the leave-out-one cross-validation.  If
    # that happens, we'll get a missing value (NA) for that error.  To work
    # around this, assign that NA to the highest error for that observation.
    # In other words, assumme the model that failed did as poor as possible.
    error[, error := ifelse(is.na(error), max(error, na.rm = TRUE), error),
          by = c("byKey", "year")]
    if(error[!(missingValue), max(abs(error))] == Inf)
        stop("Infinite error observed!  This may have been created because of
        no valid models for some time/country.  Is defaultMean() included
        in the ensembleModels?  That may fix this error.  Or, you may get this
        error if errorType == 'loocv' and a time series has only one valid
        value (in which case errors are not possible to compute).  Consider
        including a model like defaultMixedModel.")
    # If the fit failed, we can't use this model.  Assign error of Inf.
    error[, modelFailed := anyNA(fit), by = list(byKey, model)]
    error[(modelFailed), error := NA]
    
    ### Create the weights data.table using the errors
    # Aggregate the errors using the provided error function, applying to each
    # byKey group and model individually.
    weights = error[!(missingValue), errorFunction(error),
                    by = list(byKey, model)]
    setnames(weights, old = "V1", new = "averageError")
    if(errorType == "raw"){
        # Perfect fits can give really small errors but actually be overfitting
        # the data.  To prevent that, set small errors to the mean error.  But,
        # if all errors are less than 1e-3, change weights to uniform (in this
        # case, averageErrorByKey will be NA).
        ## NOTE (Michael): Maybe change this to uniform weight
        weights[, averageErrorByKey :=
            mean(averageError[averageError > 1e-3], na.rm = TRUE),
            by = byKey]
        weights[, averageError := ifelse(averageError < 1e-3 &
                !is.na(averageErrorByKey), averageErrorByKey,
            averageError), by = byKey]
        weights[, averageError := ifelse(averageError < 1e-3,
            1, averageError), by = byKey]
    } else if(errorType == "loocv"){
        # Really small errors will cause 1/error^2 to be Inf, and this
        # gives weights of all 0, NA, NaN.  Prevent that by limiting how
        # small the errors can be:
        weights[, averageError := ifelse(averageError < 1e-16,
            mean(averageError[averageError > 1e-16], na.rm = TRUE),
            averageError), by = byKey]
    }
    weights[, weight := (1/averageError^2) /
                sum(1/averageError^2, na.rm = TRUE),
            by = byKey]
    
    ### Apply adjustments to weights (NA->0, reduce below maximumWeights)
    weights[is.na(weight), weight := 0]
    if(restrictWeights & any(weights > maximumWeights)){
        # Assign weights exceeding the threshold a new value.  We want this new
        # value to be maximumWeights, but after reassigning this weight we have
        # to re-normalize the weights so they sum to 1.  The (1-weight)/(1-max)
        # factor ensures the final weight for this maximum case will be
        # maximumWeights.
        weights[weight > maximumWeights,
                weight := maximumWeights * (1 - weight) / (1 - maximumWeights)]
        # Re-normalize the weights so they sum to 1:
        weights[, weight := weight / sum(weight), by = byKey]
    }
    
    ### Convert weights to a matrix
    weights = getWeightMatrix(data = data, w = weights)
    weights
}
