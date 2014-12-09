##' The default LOESS model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export

defaultLoess = function(x){
    time = 1:length(x)
    T = length(x)
    n.obs = length(na.omit(x))
    if( n.obs < 5)
        return( rep(NA, length.out=T) )
    ## Need to check this span
    span = ifelse(n.obs/T >= 0.5, 0.3, ifelse(n.obs >= 10, 0.75, 1))
    loessFit = tryCatch({
        fit = predict(loess(formula = x ~ time,
            control = loess.control(surface = "direct"),
            span = span, degree = 1),
            newdata = data.frame(time))
        list(fit=fit, failure="none")}
        ,silent = TRUE
        ,warning = function(w){
            fit = predict(loess(formula = x ~ time,
                control = loess.control(surface = "direct"),
                span = span, degree = 1),
                newdata = data.frame(time))
            failure = lapply(w, grepl, pattern="(pseudoinverse |
                    reciprocal condition number |
                    neighborhood radius 1 |
                    other near singularities)" )
            failure = do.call("any", failure)
            if(failure)
                return(list(fit=fit, failure="warning"))
            else
                return(list(fit=fit, failure="none"))
        }
        ,error = function(e){
            return( list(fit=rep(NA,length(x)), failure="error") )
        } )
    loessFit = switch(loessFit$failure
           ,error = rep(NA, T)
           ,warning = loessFixedEquivalentNumberParams(x)
           ,none = loessFit$fit )
    loessFit[loessFit<0] = 0
    loessFit
}
