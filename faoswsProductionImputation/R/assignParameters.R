##' Assign Parameters 
##' 
##' This function takes the values of a parameter list and assigns them to the 
##' variables defined by names(parameterList), see defaultImputationParameters
##' and defaultProcessingParameters for examples.  This assignment is done in
##' the environment passed to this function by env.  Not all variables
##' value/name pairs are assigned, however; only the variables needed by the
##' functions of this package (execute defaultColumnNames() to see this list).
##' 
##' This function is used in combination with the columnNames argument to load
##' variables into the environment of a particular function.  That function can
##' then access all these variables (which describe the columns of the dataset)
##' without creating global variables.
##' 
##' @param parameterList A named character vector with the variables that
##' should be assigned.
##' @param environment The environment where the column name variables should be 
##' assigned.  Defaults to the global environment.
##'
##' @return No value is returned.  However, values are assigned to the passed
##' environment.  Since values are assigned in the global environment,
##' lockBinding is also called on these variables to ensure they are not easily
##' changed.  If the variables need to be changed, unlockBinding can be called
##' before reassigning.
##' 
##' @export
##' 

assignParameters = function(parameterList, environment = .GlobalEnv){
    
    ### Data Quality Checks
    stopifnot(is(parameterList, "list"))
    stopifnot(is(names(parameterList), "character"))
    stopifnot(is(environment, "environment"))
    if("productionValue" %in% parameterList){
        type = "processing"
    } else {
        type = "imputation"
    }
    
    ## If variable is provided and type = "imputation", create imputation cols.
    if("variable" in parameterList & type == "imputation"){
        if(parameterList$variable == "production"){
            parameterList$imputationValueColumn = "productionValue"
            parameterList$imputationFlagColumn = "productionFlag"
            parameterList$imputationMethodColumn = "productionFlag2"
        } else if(parameterList$variable == "yield"){
            parameterList$imputationValueColumn = "yieldValue"
            parameterList$imputationFlagColumn = "yieldFlag"
            parameterList$imputationMethodColumn = "yieldFlag2"
        } else if(parameterList$variable == "seed"){
            parameterList$imputationValueColumn = "seedValue"
            parameterList$imputationFlagColumn = "seedFlag"
            parameterList$imputationMethodColumn = "seedFlag2"
        } else {
            stop("No implementation for current value of variable!")
        }
        parameterList$variable = NULL 
    }
    
    imputationParameters = c("yearValue", "byKey", "ensembleModels",
        "restrictWeights", "maximumWeights", "plotImputation", "errorType",
        "errorFunction", "groupCount", "missingFlag", "imputationFlag",
        "newMethodFlag", "flagTable", "ensuredData", "ensuredFlagTable",
        "parametersAssigned", "imputationValueColumn", "imputationFlagColumn",
        "imputationMethodColumn")
    processingParameters = c("productionValue", "productionObservationFlag",
        "productionMethodFlag", "yieldValue", "yieldObservationFlag",
        "yieldMethodFlag", "areaHarvestedValue",
        "areaHarvestedObservationFlag", "areaHarvestedMethodFlag",
        "yearValue", "byKey", "removePriorImputation", "removeConflictValues",
        "imputedFlag", "naFlag", "ensuredData", "ensuredFlagTable",
        "parametersAssigned")
    if(type == "imputation"){
        # Don't allow variables other than those expected to be assigned
        stopifnot(setequal(names(parameterList), imputationParameters))
        ensureImputationParameters(parameterList)
    } else {
        stopifnot(setequal(names(parameterList), processingParameters))
        ensureProcessingParameters(parameterList)
    }
    
    parameterList$parametersAssigned = TRUE
    for(var in names(parameterList)){
        # Unlock binding so variable can be reassigned, if applicable
        if(exists(var, .GlobalEnv))
            unlockBinding(var, .GlobalEnv)
        assign(x = var, value = parameterList[[var]],
            envir = environment)
        lockBinding(var, .GlobalEnv)
    }
}