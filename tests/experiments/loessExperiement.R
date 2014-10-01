library(forecast)
library(FAOSTAT)
library(lattice)
library(lme4)
library(data.table)
library(splines)
source("../support_functions/ensembleImpute.R")
source("../support_functions/computeYield.R")
dataPath = "../sua_data"
dataFile = dir(dataPath)

## dataFile = dataFile[1:10]
## NOTE (Michael): Need to check how we are going to handle
##                 unimputed value with SWS.
##
## NOTE (Michael): Try to implement wavelet decomposition oppose to
##                 spline decomposition.

start.time = Sys.time()
sameScale = FALSE
outFile = ifelse(sameScale, "sameScale.pdf", "individualScale.pdf")
pdf(file = outFile, width = 30, height = 20)
for(i in dataFile){
    ## Read the data
    myFile = paste0(dataPath, "/", i)
    wheat.dt = data.table(read.csv(myFile, stringsAsFactors = FALSE))

    ## if anything is missing , then we don't impute
    if(NCOL(wheat.dt) != 7)
        next

    ## Experiementing the extrapolation
    predict.dt = data.table(expand.grid(areaCode = unique(wheat.dt$areaCode),
        year = c(2012:2013)))
    predict.dt[, itemCode := unique(wheat.dt$itemCode)]
    predict.dt[, areaHarvestedValue := as.numeric(NA)]
    predict.dt[, productionValue := as.numeric(NA)]
    predict.dt[, areaHarvestedSymb := as.character("M")]
    predict.dt[, productionSymb := as.character("M")]
    wheat.dt = rbind(wheat.dt, predict.dt, use.names = TRUE)


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

    ## Remove country which contains no information
    hasInfo = function(data, productionSymb, productionValue){
        ifelse(all(data[, productionSymb, with = FALSE] == "M") |
               sum(data[, productionValue, with = FALSE], na.rm = TRUE) == 0,
               FALSE, TRUE)
    }
    wheatRaw.dt[,info := hasInfo(.SD, "productionSymb", "productionValue"),
                by = "areaName"]
    wheatRaw.dt = wheatRaw.dt[info == TRUE, ]

    if(NROW(wheatRaw.dt) < 1)
        next
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
    wheatRaw.dt[productionSymb %in% c("", " ", "*") & productionValue == 0,
                productionFit := productionValue]
    wheatRaw.dt[areaHarvestedSymb != "M" & areaHarvestedValue != 0,
                areaHarvestedFit := areaHarvestedValue]
    wheatRaw.dt[areaHarvestedSymb %in% c("", " ", "*") &
                areaHarvestedValue == 0,
                areaHarvestedFit := areaHarvestedValue]
    wheatRaw.dt[, yieldFit := computeYield(productionFit, areaHarvestedFit)]
    yieldMissIndex = which(is.na(wheatRaw.dt$yieldFit))

    ## First stage of imputation
    wheatRaw.dt[, productionAreaHarvestedRatio :=
                mean(computeYield(productionFit, areaHarvestedFit),
                     na.rm = TRUE), by = "areaName"]
    wheatRaw.dt[is.na(areaHarvestedFit) & !is.na(productionFit),
                areaHarvestedFit :=
                productionFit/productionAreaHarvestedRatio]
    wheatRaw.dt[!is.na(areaHarvestedFit) & is.na(productionFit),
                productionFit :=
                areaHarvestedFit * productionAreaHarvestedRatio]
    wheatRaw.dt[, productionImputed :=
                as.numeric(ensembleImpute(productionFit)),
                by = "areaName"]
    wheatRaw.dt[, areaHarvestedImputed :=
                as.numeric(ensembleImpute(areaHarvestedFit)),
                by = "areaName"]
    ## wheatRaw.dt[, yieldImputed :=
    ##             as.numeric(computeYield(productionImputed,
    ##                                     areaHarvestedImputed))]
    ## wheatRaw.dt[, yieldImputed :=
    ##             as.numeric(ensembleImpute(yieldFit)),
    ##             by = "areaName"]

    ## Create the weights for spline regression
    wheatRaw.dt[productionSymb %in% c(" ", "*", "", "\\"),
                productionWeight := as.numeric(1)]
    wheatRaw.dt[productionSymb == "M",
                productionWeight := as.numeric(0.25)]
    wheatRaw.dt[!(productionSymb %in% c(" ", "*", "", "M")),
                productionWeight := as.numeric(0.5)]
    wheatRaw.dt[areaHarvestedSymb %in% c(" ", "*", "", "\\"),
                areaHarvestedWeight := as.numeric(1)]
    wheatRaw.dt[areaHarvestedSymb == "M",
                areaHarvestedWeight := as.numeric(0.25)]
    wheatRaw.dt[!(areaHarvestedSymb %in% c(" ", "*", "", "M")),
                areaHarvestedWeight := as.numeric(0.5)]
    wheatRaw.dt[, yieldWeight := productionWeight * areaHarvestedWeight]

    ## Impute yield

    ## yieldModelFull = try(lmer(log(yieldFit) ~ (1 + log(year)|areaName),
    ##     data = wheatRaw.dt,
    ##     weights = yieldWeight))

    ## wheatRaw.dt[, yieldImputed := as.numeric(NA)]
    ## if(!inherits(yieldModelFull, "try-error") & !grepl("Nes", i)){
    ##     wheatRaw.dt[yieldMissIndex,
    ##                 yieldImputed := exp(predict(yieldModelFull,
    ##                              newdata = .SD,
    ##                                  allow.new.levels = TRUE))]
    ## } else {
    ##     wheatRaw.dt[, yieldImputed := ensembleImpute(yieldImputed),
    ##                 by = "areaName"]
    ## }


    wheatRaw.dt[, yieldImputed := yieldFit]
    yieldModelRecent = try(lmer(log(yieldFit) ~ (1 + log(year)|areaName),
        data = wheatRaw.dt[year >= 2000],
        weights = yieldWeight))

    if(!inherits(yieldModelRecent, "try-error") & !grepl("Nes", i)){
        wheatRaw.dt[intersect(yieldMissIndex, which(year >= 2000)),
                    yieldImputed := exp(predict(yieldModelRecent,
                                     newdata = .SD,
                                     allow.new.levels = TRUE))]
    } else {
        wheatRaw.dt[, yieldImputed := yieldFit]
    }

    yieldModelFull = try(lmer(log(yieldImputed) ~ (1 + log(year)|areaName),
        data = wheatRaw.dt, weights = yieldWeight))


    if(!inherits(yieldModelFull, "try-error") & !grepl("Nes", i)){
        wheatRaw.dt[intersect(yieldMissIndex, which(year < 2000)),
                    yieldImputed := exp(predict(yieldModelFull,
                                 newdata = .SD,
                                     allow.new.levels = TRUE))]
    } else {
        wheatRaw.dt[, yieldImputed := ensembleImpute(yieldImputed),
                    by = "areaName"]
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

    ## NOTE(Michael): Spline fails when a certain range of the data is
    ##                constant

    wheatRaw.dt[, yieldImputed2 := as.numeric(NA)]
    wheatRaw.dt[, areaHarvestedImputed2 := as.numeric(NA)]
    for(j in unique(wheatRaw.dt$areaName)){
        yield.bsFit = try(exp(predict(lm(log(yieldImputed) ~
            bs(log(productionImputed), df = 3),
            weights = yieldWeight, data = wheatRaw.dt[areaName == j, ]))))
        if(inherits(yield.bsFit, "try-error") | length(yield.bsFit) !=
           length(wheatRaw.dt[areaName == j, yieldImputed])){
            wheatRaw.dt[areaName == j, yieldImputed2 := yieldImputed]
        } else {
            wheatRaw.dt[areaName == j, yieldImputed2 := yield.bsFit]
        }
        try(wheatRaw.dt[, residualArea := productionImputed/yieldImputed2])
        wheatRaw.dt[productionWeight != 1,
                    residualArea := areaHarvestedImputed]

        areaHarvested.bsFit = try(exp(predict(lm(log(residualArea) ~
            bs(log(productionImputed), df = 3),
            weights = areaHarvestedWeight,
            data = wheatRaw.dt[areaName == j, ]))))
        if(inherits(areaHarvested.bsFit, "try-error") |
           length(areaHarvested.bsFit) !=
           length(wheatRaw.dt[areaName == j, areaHarvestedImputed])){
            wheatRaw.dt[areaName == j,
                        areaHarvestedImputed2 := areaHarvestedImputed]
        } else {
            wheatRaw.dt[areaName == j,
                        areaHarvestedImputed2 := areaHarvested.bsFit]
        }

        ## try(wheatRaw.dt[areaName == j,
        ##                 areaHarvestedImputed2 :=
        ##                 exp(predict(lm(log(residualArea) ~
        ##                                bs(log(productionImputed), df = 5),
        ##                                weights = areaHarvestedWeight)))]
        ##     )
        ## if(all(is.na(wheatRaw.dt[areaName == j, areaHarvestedImputed2])))
        ##     wheatRaw.dt[areaName == j, areaHarvestedImputed2 := residualArea]
    }




    ## Compute the final imputation
    wheatRaw.dt[yieldWeight == 1 & productionValue != 0 &
                areaHarvestedImputed2 != 0, yieldImputed2 := yieldValue]
    wheatRaw.dt[areaHarvestedWeight == 1,
                areaHarvestedImputed2 := areaHarvestedValue]
    wheatRaw.dt[productionWeight == 1 & areaHarvestedWeight != 1,
                areaHarvestedImputed2 := residualArea]
    wheatRaw.dt[, productionImputed2 := areaHarvestedImputed2 * yieldImputed2]

    if(sameScale){
        ## Plot the results
        productionPlot =
            xyplot(productionImputed2 + productionValue ~
                   year|areaName, data = wheatRaw.dt, type = c("g", "l"),
                   auto.key = TRUE,
                   main = gsub("SUA\\.csv", "", i))
        print(productionPlot)

        areaHarvestedPlot =
            xyplot(areaHarvestedImputed2 + areaHarvestedValue ~
                   year|areaName, data = wheatRaw.dt, type = c("g", "l"),
                   auto.key = TRUE,
                   main = gsub("SUA\\.csv", "", i))
        print(areaHarvestedPlot)


        yieldPlot =
            xyplot(yieldImputed2 + yieldValue ~
                   year|areaName, data = wheatRaw.dt, type = c("g", "l"),
                   auto.key = TRUE,
                   main = gsub("SUA\\.csv", "", i))
        print(yieldPlot)

    } else {

        productionPlot =
            xyplot(productionImputed2 + productionValue ~
                   year|areaName, data = wheatRaw.dt, type = c("g", "l"),
                   auto.key = TRUE, scales = list(y = "free"),
                   main = gsub("SUA\\.csv", "", i))
        print(productionPlot)

        areaHarvestedPlot = xyplot(areaHarvestedImputed2 +
            areaHarvestedValue ~
            year|areaName, data = wheatRaw.dt, type = c("g", "l"),
            auto.key = TRUE, scales = list(y = "free"),
            main = gsub("SUA\\.csv", "", i))
        print(areaHarvestedPlot)

        yieldPlot =
            xyplot(yieldImputed2 + yieldValue ~ year|areaName,
                   data = wheatRaw.dt, type = c("g", "l"), auto.key = TRUE,
                   main = gsub("SUA\\.csv", "", i))
        print(yieldPlot)

    }
}
dev.off()
Sys.time() - start.time

## Check yield model for kiwi fruit

## Check the rounding error in places where production and area
## harvested are close to zero.






test = wheatRaw.dt[areaName == "Ukraine", productionFit]
test[test == 0] = NA
ensembleImpute(test, plot = TRUE)

test = wheatRaw.dt[areaName == "Chile",
    areaHarvestedFit]
test[test == 0] = NA
ensembleImpute(test, plot = TRUE)


xyplot(productionValue ~ year|areaName, wheatFinal.dt,
       type = c("g", "l"))

test = wheatRaw.dt[areaName == "Union of Soviet Socialist Republic",
    productionFit]
test[test == 0] = NA

ensembleImpute(test, plot = TRUE)



test = wheatRaw.dt[areaName == "Taiwan and China", productionValue]
test[test == 0] = NA

lines(predict(lm(test ~ bs(test)[, 1:2])), col = "purple")

wt = wavDWT(test)


ensembleImpute(test, plot = TRUE)
T = 1:length(test)
lines(predict(loess(test ~ T, control = loess.control(surface = "direct")),
              data.frame(T)), col = "gold", lwd = 2)

test = wheatRaw.dt[areaName == "Australia", productionValue]
test[test == 0] = NA

ensembleImpute(test, plot = TRUE)





test = wheatRaw.dt[areaName == "Union of Soviet Socialist Republic",
    list(year, productionImputed, yieldImputed)]

test[, newYield :=
     exp(predict(loess(log(yieldImputed) ~ year + log(productionImputed),
                   data = test[productionImputed != 0, ]),
             newdata = test))]





