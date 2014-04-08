library(FAOSTAT)
library(lattice)
library(lme4)
library(data.table)
library(splines)
source("../support_functions/ensembleImpute.R")
source("../support_functions/computeYield.R")
dataPath = "../sua_data"
dataFile = dir(dataPath)


## NOTE (Michael): Need to check how we are going to handle
##                 non-imputed value with SWS.


## Read the data
pdf(file = "test.pdf", width = 30, height = 20)
for(i in dataFile){
    myFile = paste0(dataPath, "/", i)
    wheat.dt = data.table(read.csv(myFile, stringsAsFactors = FALSE))

    ## if anything is missing , then we don't impute
    if(NCOL(wheat.dt) != 7)
        next
    ## append regional and country information
    regionTable.dt =
        data.table(FAOregionProfile[!is.na(FAOregionProfile$FAOST_CODE),
                                    c("FAOST_CODE", "UNSD_SUB_REG",
                                      "UNSD_MACRO_REG")])
    setnames(regionTable.dt,
             old = c("FAOST_CODE", "UNSD_SUB_REG", "UNSD_MACRO_REG"),
             new = c("areaCode", "unsdSubReg", "unsdMacroReg"))
    countryNameTable.dt =
        data.table(FAOcountryProfile[!is.na(FAOcountryProfile$FAOST_CODE),
                                     c("FAOST_CODE", "ABBR_FAO_NAME")])
    countryNameTable.dt[FAOST_CODE == 357,
                        ABBR_FAO_NAME := "Taiwan and China"]
    countryNameTable.dt[FAOST_CODE == 107, ABBR_FAO_NAME := "Cote d'Ivoire"]
    countryNameTable.dt[FAOST_CODE == 284, ABBR_FAO_NAME := "Aland Islands"]
    countryNameTable.dt[FAOST_CODE == 279, ABBR_FAO_NAME := "Curacao"]
    countryNameTable.dt[FAOST_CODE == 182, ABBR_FAO_NAME := "Reunion"]
    countryNameTable.dt[FAOST_CODE == 282,
                        ABBR_FAO_NAME := "Saint Barthelemy"]
    setnames(countryNameTable.dt,
             old = c("FAOST_CODE", "ABBR_FAO_NAME"),
             new = c("areaCode", "areaName"))

    ## final data frame for processing
    wheatRaw.dt = merge(merge(wheat.dt, regionTable.dt, by = "areaCode"),
        countryNameTable.dt, by = "areaCode")
    wheatRaw.dt[, yieldValue :=
                computeYield(productionValue, areaHarvestedValue)]

    ## Make corrections
    ##
    ## NOTE (Michael): Need to check with the new SWS on this. What if
    ##                 some of the errors are official?
    wheatRaw.dt[areaHarvestedValue == 0 & productionValue != 0,
                areaHarvestedValue := as.numeric(NA)]
    wheatRaw.dt[areaHarvestedValue != 0 & productionValue == 0,
                productionValue := as.numeric(NA)]


    ## Create variable for imputation
    wheatRaw.dt[productionSymb != "M" & productionValue != 0,
                productionFit := productionValue]
    wheatRaw.dt[areaHarvestedSymb != "M" & areaHarvestedValue != 0,
                areaHarvestedFit := areaHarvestedValue]
    wheatRaw.dt[, yieldFit := computeYield(productionFit, areaHarvestedFit)]
    yieldMissIndex = which(is.na(wheatRaw.dt$yieldFit))

    ## First stage of imputation
    wheatRaw.dt[, productionImputed :=
                as.numeric(ensembleImpute(productionFit)),
                by = "areaName"]
    wheatRaw.dt[, areaHarvestedImputed :=
                as.numeric(ensembleImpute(areaHarvestedFit)),
                by = "areaName"]
    wheatRaw.dt[, yieldImputed :=
                as.numeric(computeYield(productionImputed,
                                        areaHarvestedImputed))]

    ## Create the weights for spline regression
    wheatRaw.dt[productionSymb %in% c(" ", "*", ""),
                productionWeight := as.numeric(1)]
    wheatRaw.dt[productionSymb == "M",
                productionWeight := as.numeric(0.25)]
    wheatRaw.dt[!(productionSymb %in% c(" ", "*", "", "M")),
                productionWeight := as.numeric(0.5)]
    wheatRaw.dt[areaHarvestedSymb %in% c(" ", "*", ""),
                areaHarvestedWeight := as.numeric(1)]
    wheatRaw.dt[areaHarvestedSymb == "M",
                areaHarvestedWeight := as.numeric(0.25)]
    wheatRaw.dt[!(areaHarvestedSymb %in% c(" ", "*", "", "M")),
                areaHarvestedWeight := as.numeric(0.5)]
    wheatRaw.dt[, yieldWeight := productionWeight * areaHarvestedWeight]


    ## Impute yield
    yieldModel = try(lmer(log(yieldImputed) ~
        (year|unsdSubReg) + (1|areaCode), data = wheatRaw.dt))
    if(!inherits(yieldModel, "try-error")){
        wheatRaw.dt[yieldMissIndex,
                    yieldImputed := exp(predict(yieldModel,
                                 newdata = wheatRaw.dt[yieldMissIndex, ],
                                     allow.new.levels = TRUE))]
    } else {
        wheatRaw.dt[, yieldImputed := ensembleImpute(yieldImputed)]
    }

    ## wheatRaw.dt[, yieldImputed2 :=
    ##             exp(predict(loess(log(yieldImputed) ~
    ##                               year + log(productionImputed), data = .SD),
    ##                         newdata = .SD)),
    ##             by = "areaCode"]
    ## wheatRaw.dt[, residualArea := productionImputed/yieldImputed2]
    ## wheatRaw.dt[, areaHarvestedImputed2 :=
    ##             exp(predict(loess(log(residualArea) ~
    ##                               year + log(productionImputed), data = .SD),
    ##                         newdata = .SD)),
    ##             by = "areaCode"]


    ## NOTE(Michael): Do not attempt any imputation if we have nothing
    ##                in the past 20 years.
    for(j in unique(wheatRaw.dt$areaName)){
        try(wheatRaw.dt[areaName == j,
                    yieldImputed2 :=
                    exp(predict(lm(log(yieldImputed) ~
                                   year + bs(log(productionImputed)),
                                   weights = yieldWeight)))]
            )
        try(wheatRaw.dt[, residualArea := productionImputed/yieldImputed2])
        try(wheatRaw.dt[areaName == j,
                        areaHarvestedImputed2 :=
                        exp(predict(lm(log(residualArea) ~
                                       year + bs(log(productionImputed)),
                                       weights = areaHarvestedWeight)))]
            )
    }




    ## Compute the final imputation
    wheatRaw.dt[yieldWeight == 1, yieldImputed2 := yieldValue]
    wheatRaw.dt[areaHarvestedWeight == 1,
                areaHarvestedImputed2 := areaHarvestedValue]
    wheatRaw.dt[productionWeight == 1 & areaHarvestedWeight != 1,
                areaHarvestedImputed2 := residualValue]
    wheatRaw.dt[, productionImputed2 := areaHarvestedImputed2 * yieldImputed2]

    ## Plot the results
    productionPlot =
        xyplot(productionImputed2 + productionImputed + productionValue ~
               year|areaName, data = wheatRaw.dt, type = c("g", "l"),
               auto.key = TRUE, scales = list(y = "free"),
               main = gsub("SUA\\.csv", "", i))
    print(productionPlot)

    areaHarvestedPlot = xyplot(areaHarvestedImputed2 + areaHarvestedImputed +
        areaHarvestedValue + residualArea ~
        year|areaName, data = wheatRaw.dt, type = c("g", "l"),
        auto.key = TRUE, scales = list(y = "free"),
        main = gsub("SUA\\.csv", "", i))
    print(areaHarvestedPlot)

    yieldPlot =
        xyplot(yieldImputed2 + yieldImputed + yieldValue ~ year|areaName,
               data = wheatRaw.dt, type = c("g", "l"), auto.key = TRUE,
               main = gsub("SUA\\.csv", "", i))
    print(yieldPlot)

}
dev.off()







xyplot(productionValue ~ year|areaName, wheatFinal.dt,
       type = c("g", "l"))

test = wheatRaw.dt[areaName == "Union of Soviet Socialist Republic",
    productionValue]
test[test == 0] = NA

ensembleImpute(test, plot = TRUE)


test = wheatRaw.dt[areaName == "Taiwan and China", productionValue]
test[test == 0] = NA

lines(predict(lm(test ~ bs(test)[, 1:2])), col = "purple")

wt = wavDWT(test)


ensembleImpute(test, plot = TRUE)


test = wheatRaw.dt[areaName == "Australia", productionValue]
test[test == 0] = NA

ensembleImpute(test, plot = TRUE)





test = wheatRaw.dt[areaName == "Union of Soviet Socialist Republic",
    list(year, productionImputed, yieldImputed)]

test[, newYield :=
     exp(predict(loess(log(yieldImputed) ~ year + log(productionImputed),
                   data = test[productionImputed != 0, ]),
             newdata = test))]




