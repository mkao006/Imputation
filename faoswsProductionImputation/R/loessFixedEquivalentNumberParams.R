##' LOESS model with fixed Equivalent Number of Parameters
##'
##' In some cases, the model given by defaultLoess() will fail to fit the data
##' well and will issue warnings.  In this case, the problem is that too much
##' flexibility is allowed in the loess model.  This function reduces that
##' flexibility by restricting the equivalent number of parameters, and thus
##' leads to a more stable fit.
##'
##' @param x A numeric vector to be imputed.
##' @param enp.target See ?loess.  This value specifies the equivalent number
##' of parameters in the loess fit, and it can be specified instead of span.
##' 
##' @return A numeric vector of the same length as x, but with the model
##' estimates instead of the original data.
##' 
##' @export

loessFixedEquivalentNumberParams = function(x, enp.target=sum(!is.na(x))/2 ){
    time = 1:length(x)
    T = length(x)
    n.obs = length(na.omit(x))
    if( n.obs < 5)
        return( rep(NA, length.out=T) )
    loessFit = try(predict(loess(formula = x ~ time,
            control = loess.control(surface = "direct"),
            enp.target = enp.target, degree = 1),
            newdata = data.frame(time))
        ,silent = TRUE )
    if(!inherits(loessFit, "try-error")){
        loessFit[loessFit < 0] = 0
    } else {
        loessFit = rep(NA, T)
    }
    loessFit
}