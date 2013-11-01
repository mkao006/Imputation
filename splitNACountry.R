##' This function splits the data into two data.frame depending on
##' whether the country contain any data.
##'
##' @param valueVar The column name for the variable of interest.
##' @param countryVar The column name of the country.
##' @param data the name of the data
##'
##' @export

splitNACountry = function(valueVar, countryVar, data){
    evalText = paste0("missProp := sum(is.na(", valueVar, "))/length(",
        valueVar, ")")
    data[, eval(parse(text = evalText)), by = countryVar]
    list(emptyData = data[missProp == 1, ],
         nonEmptyData = data[missProp != 1, ])
}
