##' Ensure Imputation Data
##'
##' This function is designed to ensure that the provided dataset is valid.  In
##' particular, it coerces column types: all values are coerced to numeric (
##' instead of integer, which can cause problems) and all flags are coerced to
##' character (instead of logical, which occurs if the flag is set to NA).
##' Also, it ensures data is a data.table.
##' 
##' Names for the columns of this data.table should exist in the calling
##' environment (and likely in the global environment).  For example, a
##' character variable called productionValue should specify the column name of
##' data which corresponds to the production value.  Other variables should
##' also exist, such as parametersAssigned and ensuredData.  These will all
##' typically be assigned by calling the assignParameters function with an
##' argument of either defaultImputationParameters().
##'
##' @param data A data.table containing the data.
##'
##' @export
##' 

ensureImputationData = function(data, imputationParameters){
    
    ### Make sure all column name variables exist in data
    columnNames = c(imputationParameters$imputationValueColumn,
                    imputationParameters$imputationFlagColumn,
                    imputationParameters$imputationMethodColumn,
                    imputationParameters$yearValue,
                    imputationParameters$byKey)
    missingColumns = ! columnNames %in% colnames(data)
    if( any(missingColumns) )
        stop("The following columns do not exist in data but should (or the",
             "parameters in the global environment should be corrected):\n\t",
             paste(columnNames[missingColumns], collapse="\n\t"))
    
    ### Coerce columns to appropriate type:
    for(name in c(imputationParameters$imputationValueColumn)){
        expr = substitute(x := as.numeric(data[[x]]), list(x = name))
        data[, eval(expr)]
    }
    for(name in c(imputationParameters$imputationFlagColumn,
                  imputationParameters$imputationMethodColumn)){
        expr = substitute(x := as.character(data[[x]]), list(x = name))
        data[, eval(expr)]
    }
        
    ### Globally assign ensuredData so data will not need to be ensured again
    if(exists("ensuredImputationData"))
        reassignGlobalVariable("ensuredImputationData", TRUE)
    else
        ensuredImputationData <<- TRUE
}