##' Mixed Model for Imputation
##'
##' This function imputes missing values through linear mixed model.
##'
##' @param data The data.table object containing the data.
##' @param value Which variable should be imputed?  This should be one of the
##' column names of data corresponding to productionValue, yieldValue, or
##' areaHarvestedValue.
##' @param flag Column name (of data) of the flag variable corresponding to the value
##' argument, i.e. "productionObservationFlag", "yieldObservationFlag", or
##' "areaHarvestedObservationFlag".
##' @param columnNames See the same argument at ?imputeProductionDomain.
##' @param maxdf The maximum degree of freedom for the spline.
##' @param weights The weights for the observation
##' @param modelFormula Formula specifying how the dependent variable (value)
##' depends on the other columns of data.  Should be a valid mixed model
##' formula, as it will be passed to lmer (R's mixed model function).
##' 
##' @return Returns a vector of the estimated/imputed values.  If a value
##' existed in the original data, then an NA is returned in that location.
##' 
##' @export
##' 

defaultMixedModel = function(data, value, flag, columnNames, maxdf = 5,
    weights = NULL, modelFormula){
    
    ### Data Quality Checks
    stopifnot(is(data, "data.table"))
    stopifnot(c(value, flag) %in% colnames(data))
    testColumnNames(columnNames = columnNames, data = data)
    assignColumnNames(columnNames = columnNames, environment = environment())
    if(!is.null(weights)){
        stopifnot(length(weights) == nrow(data))
        stopifnot(min(weights) >= 0)
    }
    if(!missing(modelFormula))
        stopifnot(is(modelFormula, "formula"))
    
    setnames(x = data, old = c(value, flag, yearValue),
             new = c("value", "flag", "year"))
    if(missing(modelFormula)){
        modelFormula =
            as.formula(paste0("value ~ -1 + (1 + year|",
            byKey, ")"))
        ## print(modelFormula)
        model = try(
            lmer(formula = modelFormula, data = data,
                 ## weights = data[, productionValue],
                 weights = weights,
                 REML = FALSE)
            )
                    
        predictError = function(x, y, newdata){
            yhat = predict(x, newdata = newdata)
            amse = sum((yhat - y)^2,na.rm = TRUE)/
                length(na.omit(y))
            amse
        }

        benchmarkError = bootMer(model,
            FUN = function(x){
                predictError(x = x, y = data$value,
                             newdata = data)
            }, nsim = 100)
        
        if(!inherits(model, "try-error")){
            for(i in 2:maxdf){
                ## cat("proposing df:", i, "\n")
                newModelFormula =
                    as.formula(paste0("value ~ -1 + (1 + bs(year, df = ",
                                      i, ", degree = 1)|", byKey, ")"))
                ## print(newModelFormula)
                newModel = try(
                    lmer(formula = newModelFormula,
                         data = data,
                         ## weights = data[, productionValue],
                         weights = weights,
                         REML = FALSE)
                    )
                if(!inherits(newModel, "try-error")){

                    newModelError = bootMer(newModel,
                        FUN = function(x){
                            predictError(x = x, y = data$value,
                                         newdata = data)
                        }, nsim = 100)
                    ## cat("old:", mean(benchmarkError$t), "\n")
                    ## cat("new:", mean(newModelError$t), "\n")
                    if(mean(benchmarkError$t) > mean(newModelError$t)){
                        modelFormula = newModelFormula
                        model = newModel
                        benchmarkError = newModelError
                    } else {
                        cat("Model with", i - 1,
                            "degree of freedom is selected\n")
                        break
                    }                   
                    
                    ## m = bootMer(model, FUN = function(x)
                    ##     as.numeric(logLik(x)), nsim = 100)
                    ## nm = bootMer(newModel, FUN = function(x)
                    ##     as.numeric(logLik(x)), nsim = 100)
                    ## if(quantile(-2 * m$t + 2 * nm$t, prob = 0.05) < 0){
                    ##     break
                    ## } else {
                    ##     yieldFormula = newYieldFormula
                    ##     model = newModel
                    ## }
                }
            }
        }
    } else {
        model = try(
            lmer(formula = modelFormula, data = data,
                 ## weights = data[, productionValue],
                 weights = weights,
                 REML = FALSE)
            )
    }
                
    if(!inherits(model, "try-error")){
        ## Impute the data with lme.
        modelFit = predict(model, newdata = data, allow.new.levels = TRUE)
    }

    setnames(x = data,
             old = c("value", "flag", "year"),
             new = c(value, flag, yearValue))
    return(modelFit)
}
