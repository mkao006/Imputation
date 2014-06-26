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
imputeYield = function(formula, data, weights, index){
    model = try(lmer(formula = formula, data = data, weights = weights))
    response = all.vars(formula)[1]
    
    if(!inherits(model, "try-error")){
        missingString = paste0("is.na(", response, ")")
        predictionString =
            paste0(response, " := predict(model, newdata = .SD, allow.new.levels = TRUE)")

        ## Impute the data
        data[eval(parse(text = missingString)),
             eval(parse(text = predictionString))]

        ## Remove negative value from data
        data[eval(parse(text = paste0(response, "<= 0"))),
             eval(parse(text = paste0(response, " := as.numeric(NA)")))]

        ## Impute with naive imputation for those values that were
        ## negative

        data[, eval(parse(text = paste0(response, " := naiveImputation(",
                         response, ")"))),
                    by = index]
    }
    data
}
