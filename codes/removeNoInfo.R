##' This function removes data with no information
##'
##' The information contained within a set of index is calculated
##' based on whether a non-zero value exist. If the data contains only
##' zero and missing value then it is considered to contain no
##' imformation for imputation.
##'
##' @param data The data
##' @param productionValue The value of the production.
##' @param productionSymb The flag/symbol of production.
##' @param index The unique keys which identify groups.
##'
##' @export
##' 

removeNoInfo = function(data, productionSymb, productionValue, index){
    data[, info :=
         rep(containInfo(productionSymb, productionValue),
             NROW(.SD)),
         by = index]
    
    data = data[info == TRUE, ]
    data[, info := NULL]
    data
}
