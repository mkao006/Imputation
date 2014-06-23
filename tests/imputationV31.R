library(reshape2)
library(earth)
library(forecast)
library(FAOSTAT)
library(lattice)
library(lme4)
library(data.table)
library(splines)
source("../support_functions/ensembleImpute.R")
source("../support_functions/computeYield.R")
source("../support_functions/naiveImputation.R")
dataPath = "../sua_data"
dataFile = dir(dataPath)

## NOTE (Michael): Need to check how we are going to handle
##                 unimputed value with SWS.
##

sameScale = FALSE
outFile = ifelse(sameScale, "sameScaleImputation.pdf",
    "individualScaleImputation.pdf")


start.time = Sys.time()
pdf(file = outFile, width = 30, height = 20)
for(i in dataFile){
    ## Read the data
    myFile = paste0(dataPath, "/", i)
    commodity.dt = data.table(read.csv(myFile, stringsAsFactors = FALSE))

    ## if anything is missing , then we don't impute
    if(NCOL(commodity.dt) != 7)
        next

    ## Experiementing the extrapolation
    predict.dt = data.table(expand.grid(areaCode =
        unique(commodity.dt$areaCode),
        year = c(2012:2013)))
    predict.dt[, itemCode := unique(commodity.dt$itemCode)]
    predict.dt[, areaHarvestedValue := as.numeric(NA)]
    predict.dt[, productionValue := as.numeric(NA)]
    predict.dt[, areaHarvestedSymb := as.character("M")]
    predict.dt[, productionSymb := as.character("M")]
    commodity.dt = rbind(commodity.dt, predict.dt, use.names = TRUE)


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
    commodityRaw.dt = merge(merge(commodity.dt, regionTable.dt,
        by = "areaCode"), countryNameTable.dt, by = "areaCode")
    commodityRaw.dt[, yieldValue :=
                computeYield(productionValue, areaHarvestedValue)]

    ## Remove country which contains no information
    hasInfo = function(data, productionSymb, productionValue){
        ifelse(all(data[, productionSymb, with = FALSE] == "M") |
               sum(data[, productionValue, with = FALSE], na.rm = TRUE) == 0,
               FALSE, TRUE)
    }
    commodityRaw.dt[,info :=
                    hasInfo(.SD, "productionSymb", "productionValue"),
                    by = "areaName"]
    commodityRaw.dt = commodityRaw.dt[info == TRUE, ]

    if(NROW(commodityRaw.dt) < 1)
        next
    ## Make corrections
    ##
    ## NOTE (Michael): Need to check with the new SWS on this. What if
    ##                 some of the errors are official?
    commodityRaw.dt[areaHarvestedValue == 0 & productionValue != 0,
                areaHarvestedValue := as.numeric(NA)]
    commodityRaw.dt[areaHarvestedValue != 0 & productionValue == 0,
                productionValue := as.numeric(NA)]


    ## Create variable for imputation
    commodityRaw.dt[, productionFit :=
                ifelse(productionSymb == "M" &
                       productionValue == 0, NA, productionValue)]
    commodityRaw.dt[, areaHarvestedFit :=
                ifelse(areaHarvestedSymb == "M" &
                       areaHarvestedValue == 0, NA, areaHarvestedValue)]
    commodityRaw.dt[, yieldFit :=
                    computeYield(productionFit, areaHarvestedFit)]
    yieldMissIndex = which(is.na(commodityRaw.dt$yieldFit))

    ## Create weights
    commodityRaw.dt[productionSymb %in% c(" ", "*", "", "\\"),
                productionWeight := as.numeric(1)]
    commodityRaw.dt[productionSymb == "M",
                productionWeight := as.numeric(0.25)]
    commodityRaw.dt[!(productionSymb %in% c(" ", "*", "", "M")),
                productionWeight := as.numeric(0.5)]
    commodityRaw.dt[areaHarvestedSymb %in% c(" ", "*", "", "\\"),
                areaHarvestedWeight := as.numeric(1)]
    commodityRaw.dt[areaHarvestedSymb == "M",
                areaHarvestedWeight := as.numeric(0.25)]
    commodityRaw.dt[!(areaHarvestedSymb %in% c(" ", "*", "", "M")),
                areaHarvestedWeight := as.numeric(0.5)]
    commodityRaw.dt[, yieldWeight := productionWeight * areaHarvestedWeight]


    ## First stage of imputation (yield)
    commodityRaw.dt[, yieldImputed := yieldFit]

    ## yieldModelRecent = try(lmer(yieldFit ~ (1 + year|areaName),
    ##     data = commodityRaw.dt[year >= 2000],
    ##     weights = yieldWeight))

    ## if(!inherits(yieldModelRecent, "try-error") & !grepl("Nes", i)){
    ##     commodityRaw.dt[intersect(yieldMissIndex, which(year >= 2000)),
    ##                 yieldImputed := predict(yieldModelRecent,
    ##                                  newdata = .SD,
    ##                                  allow.new.levels = TRUE)]
    ## } else {
    ##     commodityRaw.dt[, yieldImputed := yieldFit]
    ## }

    ## yieldModelFull = try(lmer(yieldImputed ~ (1 + year|areaName),
    ##     data = commodityRaw.dt, weights = yieldWeight))

    ## if(!inherits(yieldModelFull, "try-error") & !grepl("Nes", i)){
    ##     commodityRaw.dt[intersect(yieldMissIndex, which(year < 2000)),
    ##                 yieldImputed := exp(predict(yieldModelFull,
    ##                              newdata = .SD,
    ##                                  allow.new.levels = TRUE))]
    ## } else {
    ##     commodityRaw.dt[, yieldImputed := ensembleImpute(yieldImputed),
    ##                 by = "areaName"]
    ## }


    ## yieldModelFull = try(lmer(log(yieldFit) ~
    ##     (1 + bs(year, degree = 1, df = 3)|areaName),
    ##     data = commodityRaw.dt, weights = yieldWeight))


    ## if(!inherits(yieldModelFull, "try-error") & !grepl("Nes", i)){
    ##     commodityRaw.dt[,
    ##                 yieldImputed := exp(predict(yieldModelFull,
    ##                              newdata = .SD,
    ##                                  allow.new.levels = TRUE))]
    ## } else {
    ##     commodityRaw.dt[, yieldImputed := ensembleImpute(yieldFit),
    ##                 by = "areaName"]
    ## }


    yieldModelFull = try(lmer(yieldFit ~
        (1 + bs(year, degree = 1, df = 5)|areaName),
        data = commodityRaw.dt, weights = yieldWeight))


    if(!inherits(yieldModelFull, "try-error") & !grepl("Nes|other", i)){
        commodityRaw.dt[,
                    yieldImputed := predict(yieldModelFull,
                                 newdata = .SD,
                                     allow.new.levels = TRUE)]
    } else {
        commodityRaw.dt[, yieldImputed := ensembleImpute(yieldFit),
                    by = "areaName"]
    }

    ## NOTE (Michael): Check why some values are NA

    ## Remove values that are negative and impute with naive imputation.
    if(any(commodityRaw.dt$yieldImputed < 0, na.rm = TRUE)){
        cat(i, file = "commodityWithNegativeYield.txt", append = TRUE)
        commodityRaw.dt[yieldImputed < 0, yieldImputed := as.numeric(NA)]
        commodityRaw.dt[, yieldImputed := naiveImputation(yieldImputed),
                        by = "areaName"]
    }

    commodityRaw.dt[!is.na(yieldFit), yieldImputed := yieldFit]

    ## Second stage (impute production if area harvested is available)
    commodityRaw.dt[!is.na(areaHarvestedFit) & is.na(productionFit),
                productionFit := areaHarvestedFit * yieldFit]

    ## Third stage (impute production if neither exist)
    commodityRaw.dt[, productionImputed :=
                as.numeric(ensembleImpute(productionFit)),
                by = "areaName"]

    ## Fourth stage (balance the area harvested)
    commodityRaw.dt[, areaHarvestedImputed := productionImputed/yieldImputed]

    if(sameScale){
        ## Plot the results
        productionPlot =
            xyplot(productionImputed + productionValue ~
                   year|areaName, data = commodityRaw.dt, type = c("g", "l"),
                   auto.key = TRUE,
                   main = gsub("SUA\\.csv", "", i))
        print(productionPlot)

        areaHarvestedPlot =
            xyplot(areaHarvestedImputed + areaHarvestedValue ~
                   year|areaName, data = commodityRaw.dt, type = c("g", "l"),
                   auto.key = TRUE,
                   main = gsub("SUA\\.csv", "", i))
        print(areaHarvestedPlot)


        yieldPlot =
            xyplot(yieldImputed + yieldValue ~
                   year|areaName, data = commodityRaw.dt, type = c("g", "l"),
                   auto.key = TRUE,
                   main = gsub("SUA\\.csv", "", i))
        print(yieldPlot)

    } else {

        productionPlot =
            xyplot(productionImputed + productionValue ~ year|areaName,
                   data = commodityRaw.dt, auto.key = TRUE,
                   K = commodityRaw.dt$productionImputed, subscripts = TRUE,
                   scales = list(y = "free"),
                   prepanel = function(..., K, subscripts){
                       list(ylim = c(0, max(c(0, K[subscripts]),
                                na.rm = TRUE)))
                       },
                   panel = function(...){
                       panel.xyplot(...,
                                    type = c("g", "l"))
                   },
                   main = gsub("SUA\\.csv", "", i))
        print(productionPlot)

        areaHarvestedPlot =
            xyplot(areaHarvestedImputed + areaHarvestedValue ~ year|areaName,
                   data = commodityRaw.dt, auto.key = TRUE,
                   K = commodityRaw.dt$areaHarvestedImputed,
                   subscripts = TRUE,
                   scales = list(y = "free"),
                   prepanel = function(..., K, subscripts){
                       list(ylim = c(0, max(c(0, K[subscripts]),
                                na.rm = TRUE)))
                       },
                   panel = function(...){
                       panel.xyplot(...,
                                    type = c("g", "l"))
                   },
                   main = gsub("SUA\\.csv", "", i))
        print(areaHarvestedPlot)

        yieldPlot =
            xyplot(yieldImputed + yieldValue ~ year|areaName,
                   data = commodityRaw.dt, auto.key = TRUE,
                   K = commodityRaw.dt$yieldImputed,
                   ## subscripts = TRUE, scales = list(y = "free"),
                   ## prepanel = function(..., K, subscripts){
                   ##     list(ylim = c(0, max(c(0, K[subscripts]),
                   ##              na.rm = TRUE)))
                   ##     },
                   panel = function(...){
                       panel.xyplot(...,
                                    type = c("g", "l"))
                   },
                   main = gsub("SUA\\.csv", "", i))
        print(yieldPlot)

    }

    ## try({
    ##     tmp = commodityRaw.dt[, list(areaName, year, productionValue,
    ##         productionSymb, productionImputed, areaHarvestedValue,
    ##         areaHarvestedSymb, areaHarvestedImputed,
    ##         yieldValue, yieldImputed)]
    ##     tmp[, productionValue := paste0(productionValue, productionSymb)]
    ##     tmp[, productionValue := gsub("NAM", "0M", productionValue)]
    ##     tmp[, areaHarvestedValue :=
    ##         paste0(areaHarvestedValue, areaHarvestedSymb)]
    ##     tmp[, areaHarvestedValue := gsub("NAM", "0M", areaHarvestedValue)]
    ##     tmp[, c("productionSymb", "areaHarvestedSymb") := NULL]
    ##     tmp[, yieldValue := as.character(round(yieldValue))]
    ##     tmp[, productionImputed := as.character(round(productionImputed))]
    ##     tmp[, areaHarvestedImputed :=
    ##         as.character(round(areaHarvestedImputed))]
    ##     tmp[, yieldImputed := as.character(yieldImputed)]
    ##     tmp2 = dcast(melt(tmp, id.vars = c("areaName", "year")),
    ##         areaName + variable ~ year)

    ##     write.csv(tmp2,
    ##               paste0("C:/Users/kao/tmp/imputation_output_for_nicolas/",
    ##                      i), row.names = FALSE, na = "")
    ## })

}
dev.off()
end.time = Sys.time()
end.time - start.time

