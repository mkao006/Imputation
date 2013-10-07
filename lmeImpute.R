##' This function performs imputation by linear mixed model
##'
##' There are three imputation method associated with the function,
##' the function will select whether between the null model excluding
##' the grouped average fixed effect and the full model.
##'
##' @param fixed The fixed effect of the model excluding the grouping variable.
##' @param random The random effect of the model
##' @param group The grouped effect of the model, the mean are
##' computed based on this formula
##' @param Data the data.frame or data.table containing the data
##' @param n.iter The number of iteration for the EM-algorithm for
##' estimating the grouped average effect.
##' @param tol The tolerance, stopping criteria for the likelihood.
##'
##' @seealso \code{\link{FAOProductionImpute}}
##' @export


lmeImpute = function(fixed, random,
  groupVar = c(year, region), Data, n.iter = 1000, tol = 1e-6){    

  require(nlme)
  
  old.class = class(Data)
  Data = data.table(Data)

  ## Initialization
  imputeVar = as.character(fixed[[2]])
  condVar = as.character(random[[2]][[3]])
  setkeyv(Data, cols = c(condVar, groupVar))
  null.ll = -Inf
  naive.AIC = Inf
  lme.ll = rep(NA, n.iter + 1)
  lme.ll[1] = -Inf  
  missInd = is.na(Data[, eval(parse(text = imputeVar))])

  ## Naive imputation
  Data[, eval(parse(text = paste0("naiveImp := naiveImp(",
                      imputeVar, ")"))), by = condVar]
  
  ## Null model without grouped average effects
  null.fit = try(
    do.call("lme",
            list(fixed = fixed, random = random, na.action = na.omit,
                 data = Data, method = "ML")),
    silent = TRUE
    )

  if(is.finite(logLik(null.fit))){
    null.ll = logLik(null.fit)
    Data[, lmeImp := predict(null.fit, Data, level = 1)]
  }
  

  ## lme with grouped average effects
  Data[, lmeMeanImp := naiveImp]
  
  for(j in 1:n.iter){
    if(j == n.iter)
      print("maximum iteration reached, model may have not converged")

    Data[, groupAverage := mean(lmeMeanImp, na.rm = TRUE), by = groupVar]

    ## Move grouped average from fixed to random effect
    random = as.formula(paste("~ -1 + ", random[[2]][[2]],
      " + groupAverage|", random[[2]][[3]]))

    fit <<- try(
      do.call("lme",
              list(fixed = fixed, random = random,
                   na.action = na.omit, data = Data, method = "ML")),
      silent = TRUE
      )
    if(is.finite(logLik(fit))){
      fit.ll = logLik(fit)
      if(fit.ll - lme.ll[j] > tol){
        Data[!is.na(groupAverage),
             lmeMeanImp := predict(fit, Data[!is.na(groupAverage), ],
                          level = 1)]
        lme.ll[j + 1] = fit.ll
      } else {
        break
      }
    } else {
      break
    }
  }

  ## Select imputation method
  bestImp = c("naiveImp", "lmeImp", "lmeMeanImp")[
    which.min(c(naive.AIC, AIC(null.fit), AIC(fit)))]  
  Data[, eval(parse(text = paste0("impValue := ", bestImp)))]
  class(Data) = old.class

  if(!inherits(fit, "try-error")){
      final.fit = fit
  } else {
      final.fit = fit
  }

  list(imputed = Data, method = bestImp, model = final.fit)
}
