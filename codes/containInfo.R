##' Function to determin whether the data contains any information.
##'
##' If the data contains only of missing value or zero then it is
##' marked as no information.
##'
##' @param data The data.
##' @param productionValue The column name corresponding to the
##' production value.
##' @param productionSymb The column name which contains the flag of
##' the production value.
##'
##' @export

containInfo = function(productionSymb, productionValue){
    ifelse(all(productionSymb == "M") |
           sum(productionValue, na.rm = TRUE) == 0,
           FALSE, TRUE)
}
