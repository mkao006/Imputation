##' Function to compute the weights of the ensemble models
##'
##' @param data A data.table containing the data.
##' @param columnNames See the same argument at ?imputeProductionDomain.
##' @param value The column name of data which contains the values to be
##' imputed.
##' @param flag The column name of data which contains the flag describing the
##' status of value.
##' @param ensembleModels A list of the models fit to data.  Each element
##' should be of class ensembleModel.
##' @param cvGroup A vector of the same length as nrow(data).  Entries of the
##' vector should be integers from 1 to the number of cross-validation groups
##' (typically 10).  This should be randomly assigned, and is usually created
##' by ensembleImpute.
##' @param fits The fitted values from the models.
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].  Note that this is enforced when computing the weight
##' vector (average performance for each model).  Once the weight vector is
##' converted to a weight matrix, certain models may not be allowed to
##' extrapolate.  Since a smaller subset of models is valid at that point, it's
##' possible that a weight for one of these models may exceed the maximum
##' weight.
##' @param errorType Specifies what type of error to compute.  Currently, "raw"
##' and "loocv" are implemented.  If "raw", then error is computed as the
##' difference between the model and observed data.  "loocv" performs
##' leave-one-out cross-validation to determine the predictive error of the
##' model.  "loocv" is more rigorous but much slower.
##' @param errorFunction Function taking a vector of errors and returning a
##' positive value.  Smaller values should indicate a better fit to the data.
##' The default is the mean-squared error (so f(x)=mean(x^2)), but other
##' functions (such as the median absolute deviation, or f(x)=median(abs(x)))
##' can be used.  Note: NA values will be removed prior to calling this
##' function.  Also, the errors are all positive, so the median absolute
##' deviation can also be computed with f(x)=median(x).
##' 
##' @export
##' 

computeEnsembleWeight = function(data, columnNames, value, flag,
    ensembleModels, cvGroup, fits, restrictWeights = TRUE,
    maximumWeights = 0.7, errorType = "raw",
    errorFunction = function(x) mean(x^2)){
    
    ### Data quality checks
    ensureData(data = data, columnNames = columnNames)
    if(!all(lapply(fits, length) == nrow(data)))
        stop("All elements of fits must have the same length as nrow(x)!")
    if(!is.null(ensembleModels))
        stopifnot(all(names(fits) == names(ensembleModels)))
    if(is.null(names(fits)))
        names(fits) = paste("Model", 1:length(fits), sep="_")
    stopifnot(errorType %in% c("raw", "loocv"))
    stopifnot(maximumWeights <= 1 & maximumWeights >= 0.5)
    if(errorType=="loocv" & is.null(ensembleModel))
        stop("ensembleModels must be provided if errorType='loocv'")
    assignColumnNames(columnNames = columnNames)
    # Ensure all time series have at least one valid observation
    # Using yieldValue directly in next line causes an error if yieldValue is a
    # column name of data, so create variable y.
    y = yieldValue
    counts = data[, sum(!is.na(get(y))), by = byKey]
    if(min(counts[, V1]) == 0)
        stop("Some countries have no data.  Have you ran removeNoInfo?")
    # Verify errorFunction
    stopifnot(is(errorFunction, "function"))
    stopifnot(length(errorFunction(1:10))==1)
    stopifnot(is.numeric(errorFunction(1:10)))
    
    ### Compute errors from each model
    error = lapply(1:length(fits),
        FUN = function(i){
            out = computeErrorRate(data = data, columnNames = columnNames,
                value = value, flag = flag, model = ensembleModels[[i]],
                cvGroup = cvGroup, fit = fits[[i]], errorType = errorType)
            out = data.table( model = names(fits)[i],
                              byKey = data[[byKey]],
                              missingValue = is.na(data[[value]]),
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
          by = list(byKey, year)]
    if(error[!(missingValue), max(abs(error))] == Inf)
        stop("Infinite error observed!  This may have been created because of
        no valid models for some time/country.  Is defaultMean() included
        in the ensembleModels?  That may fix this error.")
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
    weights = getWeightMatrix( data = data, value = value, byKey = byKey,
        yearValue = yearValue, w = weights, ensembleModels = ensembleModels )
    weights
}
