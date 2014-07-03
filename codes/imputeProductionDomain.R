##' This function imputes hte whole production domain.
##'
##' The function will impute production, area harvested and yield at
##' the same time.
##'
##' @param data The data
##' @param productionVar The name of the production variable.
##' @param areaHarvestedVar The name of the area harvested variable.
##' @param yieldVar The name of the yield variable.
##' @param productionObservationFlag The observation flag of production.
##' @param areaHarvestedObservatoinFlag The observation flag of area
##' harvested.
##' @param yieldObservationFlag The observation flag of yield.
##' @param index The unique key identifier.
##'
##' @export
##' 

imputeproductionDomain = function(data, productionVar, areaHarvestedVar,
    yieldVar, productionObservationFlag, areaHarvestedObservationFlag,
    yieldObservationFlag, index){


    ## Convert flags to weights
    data[, c(productionWeight,
             areaHarvestedWeight,
             yieldWeight) :=
         list(eval(parse(text = paste0("flag2weight(",
                             productionObservationFlag, ")"))),
              eval(parse(text = paste0("flag2weight(",
                             areaHarvestedObservationFlag, ")"))),
              eval(parse(text = paste0("flag2weight(",
                             yieldObservationFlag, ")")))
              )]

    ## Create missing index
    productionMissIndex = data[, productionVar, with = FALSE]
    areaHarvestedMissIndex = data[, areaHarvestedVar, with = FALSE]
    yieldMissIndex = data[, yieldVar, with = FALSE]

    ## Step (1): Impute yield first
    yieldImputed =
        imputedYield(formula = yieldFormula, data = data,
                     ## NOTE (Michael): Need to add in weights here
                     index = index)

    ## Modify yield flag
    ##
    ## NOTE (Michael): This is currently hard coded, maybe we can
    ##                 change this.
    yieldImputed[yieldMissIndex &
                 !is.na(eval(parse(text = yieldVar))),
                 eval(parse(text = paste0(yieldObservationStatusFlag,
                                " := I")))]

    ## Step (2): Calculate production if area harvested and yield
    ##           exists.
    yieldImputed[productionMissIndex,
                 eval(parse(text = paste0(productionVar, " := ",
                                areaHarvestedVar, " * ", yieldVar)))]

    ## Calculate the balanced production flag
    yieldImputed[productionMissIndex &
                 !is.na(eval(parse(text = productionVar))),
                 eval(parse(text = paste0(productionObservationStatusFlag,
                                " := aggregateObservationStatusFlag(",
                                areaHarvestedObservationStatusFlag, ", ",
                                yieldObservationStatusFlag, ")")))]
    
    ## Step (3): Impute production    
    productionImputed = imputeProduction(data = yieldImputed,
        productionVar = productionVar, index = index)

    ## Modify imputed production flag
    productionImputed[productionMissIndex &
                      !is.na(eval(parse(text = productionVar))),
                      eval(parse(text =
                                 paste0(productionObservationStatusFlag,
                                        " := I")))]

    ## Step (4): Balance area harvested
    allImputed =
        productionImputed[, eval(parse(text =
                                       paste0(areaHarvstedVar, " : =",
                                              productionVar, "/",
                                              yieldVar)))]

    ## Modify balanced area harvested flag
    allImputed[areaHarvestedMissIndex &
                 !is.na(eval(parse(text = areaHarvestedVar))),
                 eval(parse(text =
                            paste0(areaHarvestedObservationStatusFlag,
                                   " := aggregateObservationStatusFlag(",
                                   productionObservationStatusFlag,
                                   ", ",
                                   yieldObservationStatusFlag, ")")))]

    allImputed
}
