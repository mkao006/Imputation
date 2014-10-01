## This is the step by step tutorial and essentially for the one step
## wrapper function for the integration with the R API of the new
## statistical working system.

library(faoswsProductionImputation)
library(faoswsFlag)
library(faoswsExtra)
library(data.table)
## library(splines)
## library(lme4)
## library(earth)
## library(forecast)
library(lattice)

okra.dt = data.table(read.csv(file = "Okra.csv",
    stringsAsFactors = FALSE))

okra.dt[, productionFlag2 := ""]
okra.dt[, areaHarvestedFlag2 := ""]
okra.dt[, yieldFlag2 := ""]

## First of all, we need a table to map the observation flags. This is
## because when we perform a computation of a new variable based on
## two other variables we need to compute the corresponding flag at
## the same time. Take the computation of yield for example, the
## observation derived from production and area harvested collected
## and thus the observation flag would be based on the two observed
## flag. Detailed information is beyond the scope of this
## documentation but can be obtained from the faoswsFlag package.

## Write a documentation on the flag in the flag package.
swsOldFlagTable = rbind(faoswsFlagTable,
    data.frame(flagObservationStatus = c("*", "F"),
               flagObservationWeights = c(0.9, 0.6)))
swsOldFlagTable[swsOldFlagTable$flagObservationStatus == "E",
                "flagObservationWeights"] = 0.55


## The first step of the imputation is to remove any undesirable
## imputation performed previously. Even for the same methodology,
## prior imputation will change as we receive more information about
## the future. This step is however optional and depends on the
## judgement of the analyst.

## Remove prior imputation for production
table(okra.dt$productionFlag)
removeImputation(data = okra.dt,
                 value = "productionValue",
                 flag = "productionFlag",
                 imputedFlag = "E",
                 naFlag = "M")
table(okra.dt$productionFlag)


## Now remove prior imputation for area harvested
table(okra.dt$areaHarvestedFlag)
removeImputation(data = okra.dt,
                 value = "areaHarvestedValue",
                 flag = "areaHarvestedFlag",
                 imputedFlag = "E",
                 naFlag = "M")
table(okra.dt$areaHarvestedFlag)


## After removing prior imputation, the next optional step is to
## replace zero values with flag M. This is the method which the old
## system represent a missing value.


remove0M(data = okra.dt,
         value = "productionValue",
         flag = "productionFlag",
         naFlag = "M")

remove0M(data = okra.dt,
         value = "areaHarvestedValue",
         flag = "areaHarvestedFlag",
         naFlag = "M")

## The final optional step is to compute the yield if it is not
## available.

computeYield(productionValue = "productionValue",
             productionObservationFlag = "productionFlag",
             areaHarvestedValue = "areaHarvestedValue",
             areaHarvestedObservationFlag = "areaHarvestedFlag",
             yieldValue = "yieldValue",
             yieldObservationFlag = "yieldFlag",
             yieldMethodFlag = "yieldFlag2",
             newMethodFlag = "",
             flagTable = swsOldFlagTable,
             data = okra.dt)


## Maybe talk about removing conflict values

## Now we have to subset data which contains no info.
okra.dt =
    removeNoInfo(data = okra.dt,
                 flag = "yieldFlag",
                 value = "yieldValue",
                 byKey = "areaCode")
    

## Now we are ready to perform the imputation. The yield is imputed
## first. The function can take user specific formula to feed into the
## linear mixed model, or the default is the spline model outlined in
## the documentation. In this documentation we will use the default
## model as specified in the methodological documentation. The maxdf
## arguement specify the maximum number of degree of freedom to be
## tested.

imputeYield(yieldValue = "yieldValue",
            yieldObservationFlag = "yieldFlag",
            yieldMethodFlag = "yieldFlag2",
            yearValue = "year",
            imputationFlag = "I",
            newMethodFlag = "",
            maxdf = 5,
            byKey = "areaCode",
            data = okra.dt)


## After the imputation of yield we can proceed to imute the
## porduction. The imputeProduction function actually consists of two
## steps, first balance production and then impute the production
## based on ensemble. After the yield is computed we can compute the
## production provided both area harvested and yield exist. Then the
## ensemble model is applied to impute the remaining missing
## production.


## The component model need to take a vector of values and return the
## fitted values. If the model failes, return NA for all
## observation. Shown below is the default logitstic model fitted, the
## model will return a vector of NA if there are no observations at
## both tail. It is the analyst's job to ensure the component model
## returns sensible value. For example, negative values are
## non-sensicle for production, and in the current implementation
## negative values are replaced with zero.
defaultLogistic = function (x){
    time = 1:length(x)
    xmax = max(x, na.rm = TRUE)
    x.scaled = x/xmax
    logisticModel = glm(formula = x.scaled ~ time, family = "binomial")
    logisticFit = predict(logisticModel,
                          newdata = data.frame(time = time), 
                          type = "response") * xmax
    midpoint = -coef(logisticModel)[1]/coef(logisticModel)[2]
    if (length(na.omit(x[time < midpoint])) < 1 |
        length(na.omit(x[time > midpoint])) < 1) 
        logisticFit = rep(NA, length(x))
    logisticFit
}


## To perform the imputation, we will need the ensemble models in a
## list. Here we list the default model used.
myModel = list(defaultMean, defaultLm, defaultExp,
        defaultLogistic, defaultLoess, defaultSpline, defaultArima,
        defaultMars, defaultNaive)

bahrainExample = okra.dt[areaName == "Bahrain", productionValue]

modelFits =
    lapply(myModel,
           FUN = function(x, value) x(value),
           value = bahrainExample)

modelWeights =
    computeEnsembleWeight(x = bahrainExample,
                          fits = modelFits,
                          restrictWeights = TRUE,
                          maximumWeights = 0.7)

ensembleFit = computeEnsemble(modelFits, modelWeights)

ensembleFit =
    ensembleImpute(x = bahrainExample,
                   restrictWeights = TRUE,
                   maximumWeights = 0.7,
                   ensembleModel = myModel,
                   plot = TRUE)


## An optional arguement for ensemble weight is available. You can
## specify whether to restrict the weights of a single model. In this
## example, the default restricts the weight and set the maximum
## weight of a model can take to 0.7.
imputeProduction(productionValue = "productionValue",
                 productionObservationFlag = "productionFlag",
                 productionMethodFlag = "productionFlag2",
                 areaHarvestedValue = "areaHarvestedValue",
                 areaHarvestedObservationFlag = "areaHarvestedFlag",
                 yieldValue = "yieldValue",
                 yieldObservationFlag = "yieldFlag",
                 newMethodFlag = "",
                 data = okra.dt,
                 ensembleModel = myModel,
                 restrictWeights = TRUE,
                 maximumWeights = 0.7,
                 byKey = "areaCode",
                 flagTable = swsOldFlagTable)

## Finally, after both production and yield are imputed, the area
## harvested can be ontained as a balance.
balanceAreaHarvested(productionValue = "productionValue",
                     productionObservationFlag = "productionFlag",
                     areaHarvestedValue = "areaHarvestedValue",
                     areaHarvestedObservationFlag = "areaHarvestedFlag",
                     areaHarvestedMethodFlag = "areaHarvestedFlag2",
                     yieldValue = "yieldValue",
                     yieldObservationFlag = "yieldFlag",
                     newMethodFlag = "",
                     data = okra.dt,
                     flagTable = swsOldFlagTable)

## The one step wrapper function performs all the steps outlined in
## this documentation.
system.time(
    {
        imputedOkra.dt =
            imputeProductionDomain(data = okra.dt,
                                   productionValue = "productionValue",
                                   areaHarvestedValue =
                                       "areaHarvestedValue",
                                   yieldValue = "yieldValue",
                                   yearValue = "year",
                                   productionObservationFlag =
                                       "productionFlag",
                                   areaHarvestedObservationFlag =
                                       "areaHarvestedFlag",
                                   yieldObservationFlag = "yieldFlag",
                                   productionMethodFlag =
                                       "productionFlag2",
                                   areaHarvestedMethodFlag =
                                       "areaHarvestedFlag2",
                                   yieldMethodFlag = "yieldFlag2",
                                   flagTable = swsOldFlagTable,
                                   removePriorImputation = TRUE,
                                   removeConflictValues = TRUE,
                                   imputedFlag = "E",
                                   imputationFlag = "I",
                                   newMethodFlag = "",
                                   naFlag = "M",
                                   maxdf = 5,
                                   byKey = "areaCode",
                                   restrictWeights = TRUE,
                                   maximumWeights = 0.7,
                                   ensembleModel = myModel)
    }
    )
                       
                       
xyplot(yieldValue ~ year|paste0(areaName, " (", areaCode, ")"),
       data = imputedOkra.dt, group = yieldFlag == "I",
       auto.key = TRUE)
