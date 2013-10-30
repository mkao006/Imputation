##' This function performs imputation by linear mixed model
##'
##'
##' @param formula See the formula of lme4
##' @param groupVar The grouped effect of the model, the mean are
##' computed based on this formula
##' @param countryVar The variable which defines the country, which act as the conditional variable in the random effect.
##' @param data the data.frame or data.table containing the data
##' @param n.iter The number of iteration for the EM-algorithm for
##' estimating the grouped average effect.
##' @param tol The tolerance, stopping criteria for the likelihood.
##' @param EMverbose logical, whether the likelihood in the EM step should be returned.
##'
##' @seealso \code{\link{FAOProductionImpute}}
##' @export


meanlme4 = function(formula, groupVar, countryVar, data,
    n.iter, tol, EMverbose = TRUE){
    require(lme4)
    
    ## Initialization
    dataCopy = copy(data.table(data))
    ## ll = vector(mode = "numeric", length = 1 + n.iter)
    ll = rep(NA, length = 1 + n.iter)
    ll[1] = -Inf
    
    ## Extract the response variable
    y = as.character(nlme:::getResponseFormula(formula))[2]
    
    ## Compute the grouped mean
    dataCopy[, eval(parse(text = paste0("groupedMean := mean(",
                              y, "na.rm = TRUE)"))), by = groupVar]
    
    ## Update the formula to include the group mean
    mean.formula = update(formula, paste0(". ~ . + (groupedMean|",
        countryVar, ")"))
    
    ## Fit the model
    init.fit = try(
        do.call("lmer",
                list(formula = mean.formula, data = dataCopy)
                ),
        silent = TRUE
        )
    
    ## Impute the missing value
    dataCopy[, eval(parse(text =
                          paste0("missInd := is.na(", y, ")"))), ]
    dataCopy[, eval(parse(text = paste0("imputedValue := ", y)))]
    dataCopy[missInd == TRUE, imputedValue :=
             predict(init.fit, dataCopy[missInd == TRUE, ])]    
    
    ## Start EM mean imputation
    EM.formula = update(mean.formula, imputedValue ~ .)
    for(i in 1:n.iter){
        if(i == n.iter)
            print("maximum iteration reached, model may have not converged")
        
        dataCopy[, groupedMean := mean(imputedValue), by = groupVar]
        
        EMMean.fit = try(
            do.call("lmer",
                    list(formula = EM.formula,
                         data = dataCopy, REML = FALSE)
                    ),
            silent = TRUE
            )
        if(EMverbose)
            cat("Iteration", i, ":", logLik(EMMean.fit), "\n")
        
        if(is.finite(logLik(EMMean.fit))){            
            ll[i + 1] = logLik(EMMean.fit)
            if(ll[i + 1] - ll[i] > tol){
                dataCopy[missInd == TRUE,
                         imputedValue := predict(EMMean.fit,
                                          dataCopy[missInd == TRUE, ])]
            } else {
                break
            }
        } else {
            break
        }   
    }
    
    ## Return the model
    list(model = update(EMMean.fit, REML = TRUE),
         groupedMean = dataCopy[, groupedMean], ll = ll)
}
