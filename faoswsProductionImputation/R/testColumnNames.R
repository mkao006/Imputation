##' Assign Column Names
##'
##' In addition, this function ensures that the columnNames argument is valid.
##' In particular, that means that all elements of columnNames are valid column
##' names in data.  Additionally, this function also ensures that all of the
##' necessary columns are provided (necessary for other functions in this
##' package).
##' 
##' @param columnNames A named character vector describing which variables
##' (the names) map to which columns of data (the values).
##' @param data The dataset being analysed.  This is used to ensure that all
##' elements of columnNames are in fact in data.
##'
##' @return No value is returned.  However, an error will be raised if the
##' columnNames vector is invalid.
##'
##' @export
##' 

testColumnNames = function(columnNames, data){
    ### Check that all values of columnNames are in colnames(data)
    missingColumns = ! columnNames %in% colnames(data)
    if( any(missingColumns) )
        stop(paste0("The following columns do not exist in data (but should, as they're in columnNames):\n",
            paste(columnNames[missingColumns], collapse="\n")))

    ### Check that names(columnNames) has all required names
    requiredColumns = c("productionValue", "productionObservationFlag", 
        "productionMethodFlag", "areaHarvestedValue",
        "areaHarvestedObservationFlag", "areaHarvestedMethodFlag",
        "yieldValue", "yieldObservationFlag", "yieldMethodFlag", "yearValue",
        "byKey" )
    missingNames = ! requiredColumns %in% names(columnNames)
    if( any(missingNames) )
        stop(paste0("The following elements do not exist in columnNames (and are required):\n",
            paste(requiredColumns[missingNames], collapse="\n")))
}