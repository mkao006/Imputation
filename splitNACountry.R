##' This function splits the data into two data.frame depending on
##' whether the country contain any data.
##'
##' @param value The column name for the variable of interest.
##' @param country The column name of the country.
##' @param data the name of the data
##'
##' @export

splitNACountry = function(value, country, data){
    evalText = paste0("missProp := sum(is.na(", value, "))/length(",
        value, ")")
    data[, eval(parse(text = evalText)), by = country]
    list(emptyData = data[missProp == 1, ],
         nonEmptyData = data[missProp != 1, ])
}
