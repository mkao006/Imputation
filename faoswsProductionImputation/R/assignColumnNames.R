##' Assign Column Names
##'
##' This function takes the values of columnNames and assigns them to the 
##' variables defined by names(columnNames), respectively.  This assignment is
##' done in the environment passed to this function by env.  Not all variables
##' value/name pairs are assigned, however; only the variables needed by the
##' functions of this package (execute defaultColumnNames() to see this list).
##'
##' This function is used in combination with the columnNames argument to load
##' variables into the environment of a particular function.  That function can
##' then access all these variables (which describe the columns of data)
##' without creating global variables.
##' 
##' @param columnNames A named character vector describing which variables
##' (the names) map to which columns of data (the values).
##' @param data The dataset being analysed.  This is used to ensure that all
##' elements of columnNames are in fact in data.
##' @param environment The environment where the column name variables should be 
##' assigned.  Typically, this will be the environment of the function which
##' called this function.  See examples.
##'
##' @return No value is returned.  However, values are assigned in the
##' environment which called this function.
##'
##' @examples
##' f = function(){
##'     assignColumnNames( defaultColumnNames, okrapd, environment() )
##'     print(productionValue)
##' }
##' #productionValue gets assigned a character value from defaultColumnNames:
##' f()
##' #However, the assignment is only within f().  productionValue doesn't exist
##' #in the global environment:
##' exists("productionValue")
##' 
##' @export
##' 

assignColumnNames = function( columnNames, data,
    environment = parent.frame(1) ){
    for(variables in c("productionValue", "productionObservationFlag",
        "productionMethodFlag", "areaHarvestedValue",
        "areaHarvestedObservationFlag", "areaHarvestedMethodFlag",
        "yieldValue", "yieldObservationFlag", "yieldMethodFlag", "yearValue",
        "byKey" ) ){
        assign( x = variables, value = columnNames[variables],
            envir = environment )
    }
}