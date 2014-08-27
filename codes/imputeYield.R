##' Function to impute yield
##'
##' This function imputes the yield through linear mixed model
##'
##' @param formula The formula to pass to the linear mixed model.
##' @param yieldObservationFlag The observation flag of yield.
##' @param imputationFlag Flag value for new imputation values.
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param data The data
##' @param weights The weights for the observation
##' @param byKey The unique key identifier.
##'
##' @export
##' 


imputeYield = function(formula, yieldObservationFlag, imputationFlag,
    flagTable = faoswsFlagTable, data, weights = NULL, byKey){
    response = all.vars(formula)[1]
    setnames(x = data, old = c(response, yieldObservationFlag),
             new = c("response", "yieldObservationFlag"))

    yieldMissingIndex = is.na(data[, response])
    
    newFormula = update(formula, response ~ .)
    model =
        try(
            lmer(formula = newFormula,
                 data = data,
                 weights = weights)
            )

    if(!inherits(model, "try-error")){
        ## Impute the data with lme.
        data[yieldMissingIndex,
             response := predict(model, newdata = .SD,
                          allow.new.levels = TRUE)]
        
        ## Remove negative value from data.
        data[response <= 0, response := as.numeric(NA)]
        

    }
    ## Reimpute with naive imputation for those values that were
    ## negative, or if the model failed.
    data[, response := defaultNaive(response), by = byKey]
    data[yieldMissingIndex & !is.na(response),
         yieldObservationFlag := imputationFlag]

    setnames(x = data,
             old = c("response", "yieldObservationFlag"),
             new = c(response, yieldObservationFlag))
}
