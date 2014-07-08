##' Function to impute yield
##'
##' This function imputes the yield through linear mixed model
##'
##' @param formula The formula to pass to the linear mixed model.
##' @param data The data
##' @param weights The weights for the observation
##' @param index The keys for performing naive imputation if linear
##' mixed model fails.
##'
##' @export
##' 

imputeYield = function(formula, data, weights = NULL, index){

    dataCopy = copy(data)
    response = all.vars(formula)[1]
    setnames(dataCopy, response, "response")

    ## Fit the model
    newFormula = update(formula, response ~.)
    model =
        try(
            lmer(formula = newFormula, data = dataCopy,
                 weights = weights)
            )

    if(!inherits(model, "try-error")){
        ## Impute the data with lme.
        dataCopy[is.na(response),
                 response := predict(model, newdata = .SD,
                              allow.new.levels = TRUE)]
        
        ## Remove negative value from data.
        dataCopy[response <= 0, response := as.numeric(NA)]
        
        ## Reimpute with naive imputation for those values that were
        ## negative.
        dataCopy[, response := naiveImputation(response), by = index]
        imputedYield = unlist(dataCopy[, response])
        
    } else {
        imputedYield = data[, response, with = FALSE]
    }
    
    imputedYield
}
