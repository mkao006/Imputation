##' Assign Parameters 
##' 
##' This function takes the values of a parameter list and assigns them to the 
##' variables defined by names(parameterList), see defaultImputationParameters
##' and defaultProcessingParameters for examples.  This assignment is done in
##' the environment passed to this function by env.  All value/name pairs are
##' assigned, as a check is made to ensure the parameters in parameterList are
##' exactly the ones needed for the processing or imputation.
##' 
##' This function is used in combination with the parameterList argument to load
##' variables into a particular environment.  Functions can then access all
##' these variables (which describe the columns of the dataset) without needing
##' to pass long lists through.
##' 
##' @param parameterList A named character vector with the variables that
##' should be assigned.
##' @param environment The environment where the column name variables should be 
##' assigned.  Defaults to the global environment.
##'
##' @return No value is returned.  However, values are assigned to the passed
##' environment.  Since values are typically assigned in the global environment,
##' lockBinding is also called on these variables to ensure they are not easily
##' changed.  If the variables need to be changed, unlockBinding can be called
##' before reassigning.  Alternatively, one may use reassignGlobalVariable.
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
    if("variable" %in% names(parameterList) & type == "imputation"){
        if(parameterList$variable == "production"){
            parameterList$imputationValueColumn = "Value_measuredElement_5510"
            parameterList$imputationFlagColumn =
                "flagObservationStatus_measuredElement_5510"
            parameterList$imputationMethodColumn =
                "flagMethod_measuredElement_5510"
        } else if(parameterList$variable == "yield"){
            parameterList$imputationValueColumn = "Value_measuredElement_5416"
            parameterList$imputationFlagColumn =
                "flagObservationStatus_measuredElement_5416"
            parameterList$imputationMethodColumn =
                "flagMethod_measuredElement_5416"
        } else if(parameterList$variable == "seed"){
            parameterList$imputationValueColumn = "areaSownRatio"
            parameterList$imputationFlagColumn =
                "flagObservationStatus_measuredElement_5212"
            parameterList$imputationMethodColumn =
                "flagMethod_measuredElement_5212"
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
