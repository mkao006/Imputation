##' Returns all default models
##'
##' This is a convenience function that returns a list of all the default
##' models.
##'
##' @return A list of all the default model functions. 
##' 
##' @export
##' 

allDefaultModels = function(){
    return( list(defaultMean = defaultMean,
        defaultLm = defaultLm, defaultExp = defaultExp,
        defaultLogistic = defaultLogistic, defaultLoess = defaultLoess,
        defaultSpline = defaultSpline, defaultArima = defaultArima,
        defaultMars = defaultMars, defaultNaive = defaultNaive) )
}
