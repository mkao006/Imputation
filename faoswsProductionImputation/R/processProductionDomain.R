##' This is a wrapper for all the data manipulation step before the
##' preparation of the imputation.
##'
##' @param data The data
##' @param columnNames See columnNames argument at ?imputeProductionDomain.
##' @param removePriorImputation logical, whether prior imputation
##' should be removed.
##' @param removeConflictValues logical, whether conflict area
##' harvested value and production should be removed.
##' @param imputedFlag Flag value corresponding to values from prior
##' imputation, ignored if removePriorImputation is FALSE.
##' @param naFlag Flag value for missing values.
##' @export
##' 

processProductionDomain = function(data, columnNames,
    removePriorImputation = TRUE, removeConflictValues = TRUE,
    imputedFlag = "E",  naFlag = "M"){
    
    ### Ensure inputs are as expected (and assign columnNames variables)
    stopifnot( is(data, "data.table") )
    stopifnot( is.logical( c(removePriorImputation, removeConflictValues) ) )
    testColumnNames( columnNames = columnNames, data = data )
    assignColumnNames( columnNames = columnNames, environment = environment() )
        
    if(removePriorImputation){
        removeImputation(data = data,
                         value = areaHarvestedValue,
                         flag = areaHarvestedObservationFlag,
                         imputedFlag = imputedFlag,
                         naFlag = naFlag)
        
        removeImputation(data = data,
                         value = productionValue,
                         flag = productionObservationFlag,
                         imputedFlag = imputedFlag,
                         naFlag = naFlag)

        removeImputation(data = data,
                         value = yieldValue,
                         flag = yieldObservationFlag,
                         imputedFlag = imputedFlag,
                         naFlag = naFlag)
    }

    remove0M(data = data,
             value = areaHarvestedValue,
             flag = areaHarvestedObservationFlag,
             naFlag = naFlag)
    
    remove0M(data = data,
             value = productionValue,
             flag = productionObservationFlag,
             naFlag = naFlag)
    
    remove0M(data = data,
             value = yieldValue,
             flag = yieldObservationFlag,
             naFlag = naFlag)        

    if(removeConflictValues){
        removeZeroConflict(columnNames, data = data)
    }

    removeNoInfo(data = data,
                 value = yieldValue,
                 flag = yieldObservationFlag,
                 byKey = byKey)
    # removeNoInfo assigns the new data.table to the variable "data" in the
    # environment of this function.  Thus, to ensure "data" is returned to the
    # caller of this function, assign the data.table to the calling environment.
    # This should be removed/fixed once row deletion by reference is
    # implemented for data.table, see
    # http://stackoverflow.com/questions/10790204/how-to-delete-a-row-by-reference-in-r-data-table
    dataTableName = as.character(match.call()$data)
    assign(x = dataTableName, value = data, envir = parent.frame(1))
}   
