##' This is a wrapper for all the data manipulation step before the
##' preparation of the imputation.
##'
##' @param data The data
##' 
##' @export
##' 

processProductionDomain = function(data, processingParameters){
    
    ### Data Quality Checks
    if(!ensuredProcessingParameters)
        ensureProcessingParameters(processingParameters = processingParameters)
    if(!ensuredProductionData)
        ensureProductionData(data = data)
        
    if(removePriorImputation){
        removeImputation(data = data,
                    value = processingParameters$areaHarvestedValue,
                    flag = processingParameters$areaHarvestedObservationFlag,
                    processingParameters = processingParameters)
        
        removeImputation(data = data,
                    value = processingParameters$areaHarvestedValue,
                    flag = processingParameters$areaHarvestedObservationFlag,
                    processingParameters = processingParameters)
        
        removeImputation(data = data,
                    value = processingParameters$areaHarvestedValue,
                    flag = processingParameters$areaHarvestedObservationFlag,
                    processingParameters = processingParameters)
    }

    remove0M(data = data,
             value = processingParameters$areaHarvestedValue,
             flag = processingParameters$areaHarvestedObservationFlag,
             naFlag = processingParameters$naFlag)
    
    remove0M(data = data,
             value = processingParameters$areaHarvestedValue,
             flag = processingParameters$areaHarvestedObservationFlag,
             naFlag = processingParameters$naFlag)
    
    remove0M(data = data,
             value = processingParameters$areaHarvestedValue,
             flag = processingParameters$areaHarvestedObservationFlag,
             naFlag = processingParameters$naFlag)
    
    if(removeConflictValues){
        removeZeroConflict(data = data,
                           processingParameters = processingParameters)
    }

    removeNoInfo(data = data,
                 value = processingParameters$yieldValue,
                 flag = processingParameters$yieldObservationFlag,
                 processingParameters = processingParameters)
    # removeNoInfo assigns the new data.table to the variable "data" in the
    # environment of this function.  Thus, to ensure "data" is returned to the
    # caller of this function, assign the data.table to the calling environment.
    # This should be removed/fixed once row deletion by reference is
    # implemented for data.table, see
    # http://stackoverflow.com/questions/10790204/how-to-delete-a-row-by-reference-in-r-data-table
    dataTableName = as.character(match.call()$data)
    assign(x = dataTableName, value = data, envir = parent.frame(1))
}   
