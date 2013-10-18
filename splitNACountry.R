splitNACountry = function(value, country, data){
    evalText = paste0("missProp := sum(is.na(", value, "))/length(",
        value, ")")
    data[, eval(parse(text = evalText)), by = country]
    list(emptyData = data[missProp == 1, ],
         nonEmptyData = data[missProp != 1, ])
}
