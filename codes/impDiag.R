##' A function to perform a suite of diagnostic plots for the linear
##' mixed model and the fit of the imputation.
##'
##' @param object The Fitted object from \code{swsImputation}
##' @param savePlots Logical, whether the plots should be saved as pdf
##' file.
##' @param yieldObsVar The column name of the observed yield.
##' @param countryVar The column representing the country.
##' @param file Optional name for the pdf.
##'
##' @seealso \code{impFit}
##' @export

impDiag = function(object, savePlots = FALSE, yieldObsVar, countryVar,
    file){

    if(savePlots){
        if(missing(file))
            file = "imputationDiagnostic.pdf"
        pdf(file, width = 15, height = 12)
        par(ask = FALSE)
    } else {
        par(ask = TRUE)
    }

    ## Diagnose random coefficients
    print(dotplot(ranef(object$model$model, condVar = TRUE),
                  main = "Caterpillar plot of random coefficients"))

    ## Diagnose normality of random coefficients
    print(qqmath(ranef(object$model$model, condVar=TRUE),
                 main = "QQ-plot of random coefficients"))

    ## Checking the fitted value with the observed value
    tmp = as.formula(paste0(yieldObsVar, " ~ fittedYield|", countryVar))
    print(xyplot(tmp, data = object$imputed,
                 panel = function(x, y) {
                     panel.xyplot(x, y)
                     panel.abline(a = 0, b = 1)
                 },
                 aspect = "fill", type = c("g", "p"),
                 xlab = "Fitted value", ylab = "Observed value")
          )

    ## Checking the fitted value with the observed value
    tmp = as.formula(paste0(yieldObsVar, " + fittedYield ~ Year|",
        countryVar))
    print(xyplot(tmp, data = object$imputed,
                 aspect = "fill", type = c("g", "l"), auto.key = TRUE,
                 ylab = "Fitted value VS Observed", xlab = "Year")
          )
    
    ## Check the residuals with the observed value
    tmp = as.formula(paste0("res ~ fittedYield|", countryVar))
    print(xyplot(tmp, data = object$imputed,
                 type = c("g", "p"), auto.key = TRUE,
                 panel = function(x, y){
                     panel.xyplot(x, y)
                     panel.abline(h = 0)
                 },
                 xlab = "Fitted value", ylab = "Residuals")
          )
    

    ## Examine normality of residuals
    tmp = as.formula(paste0("~ res|", countryVar))
    print(qqmath(tmp, data = object$imputed,
                 prepanel = prepanel.qqmathline,
                 panel = function(x, ...) {
                     panel.qqmathline(x, ...)
                     panel.qqmath(x, ...)
                 },
                 xlab = "Theoretical Quantile",
                 ylab = "Observed Quantile",
                 main = "QQ-plot of residuals by country")
          )

    if(savePlots){
        graphics.off()
        cat("plots saved as '", file, "'\n")
    } else {
        par(ask = FALSE)
    }
}

