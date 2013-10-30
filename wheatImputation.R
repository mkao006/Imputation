########################################################################
## Title: Script to impute wheat data
## Date: 2013-06-12
########################################################################

source("wheatDataManipulation.R")
source("naiveImpute.R")
source("swsImputation.R")
source("meanlme4.R")
source("splitNACountry.R")
library(lme4)

## Take only data from 1980 for linearity
wheatPrep.dt = wheatPrep.dt[Year >= 1993, ]


## Imputation
## ---------------------------------------------------------------------
imputed.lst = swsImputation(data = wheatPrep.dt, area = "valueArea",
  prod = "valueProd", yield = "valueYield", country = "FAOST_NAME",
  region = "UNSD_SUB_REG", year = "Year", tol = 1e-3)

imputed.dt = imputed.lst$imputed


## Percentage of missing value imputed
NROW(imputed.dt[is.na(valueProd) & !is.na(imputedProd), ])/
  NROW(imputed.dt[is.na(valueProd), ])

NROW(imputed.dt[is.na(valueArea) & !is.na(imputedArea), ])/
  NROW(imputed.dt[is.na(valueArea), ])


## Diagnosis and plots
## ---------------------------------------------------------------------
dotplot(ranef(imputed.lst$model$model, condVar = TRUE,
              whichel = "FAOST_NAME"))

qqmath(ranef(imputed.lst$model$model, condVar=TRUE,
             whichel = "FAOST_NAME"))

## Checking the fitted value with the observed value
xyplot(valueYield ~ fittedYield|FAOST_NAME, data = imputed.dt,
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(a = 0, b = 1)
       },
       aspect = "fill", type = c("g", "p"),
       xlab = "fitted value", ylab = "observed value")

## Examine the marginal distribution of imputed yield
histogram(~ imputedYield | as.character(Year) + is.na(valueYield),
          data = imputed.dt, aspect = "fill", breaks = 10)


xyplot(valueYield + fittedYield ~ Year | FAOST_NAME,
       data = imputed.dt, fill = "aspect", layout = c(15, 9),
       type = c("g", "l"), auto.key = TRUE)

xyplot(res ~ fittedYield|FAOST_NAME,
       data = imputed.dt, layout = c(15, 9),
       type = c("g", "p"), auto.key = TRUE,
       panel = function(x, y){
           panel.xyplot(x, y)
           panel.abline(h = 0)
           },
       xlab = "fitted value", ylab = "residuals")

qqmath(~ res | FAOST_NAME, data = imputed.dt,
       prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
           panel.qqmathline(x, ...)
           panel.qqmath(x, ...)
       })


## Check all individual imputation
pdf(file = "checkWheatImputation.pdf", width = 10)
for(i in unique(imputed.dt$FAOST_CODE)){
  tmp = imputed.dt[FAOST_CODE == i, ]

  myCountry =
    FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE == i),
                      "FAO_TABLE_NAME"]
  myItem = with(FAOmetaTable, unique(itemTable[itemTable$itemCode ==
    unique(imputed.dt$itemCode), "itemName"]))
  par(mfrow = c(4, 1), mar = c(2.1, 4.1, 3.1, 2.1))
  try({
    ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
    with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Production",
                   main = paste0(myCountry, " (", i, ") - ",
                     myItem, " (", unique(imputed.dt$itemCode), ")"),
                   cex = 2))
    with(tmp[is.na(valueProd), ],
         points(Year, imputedProd, col = "blue", pch = 19))
  })
  
  try({
    ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
    with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Area",
                   cex = 2))
    with(tmp[is.na(valueArea),],
         points(Year, imputedArea, col = "blue", pch = 19))
  })
  
  
  try({
    ymax = max(tmp[, list(valueYield, imputedYield, groupedMean)],
      na.rm = TRUE) * 1.2
    with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Yield",
                    cex = 2))
    with(tmp[!is.na(valueYield), ],
         points(Year, fittedYield, col = "red", pch = 19))
    with(tmp[is.na(valueYield), ],
         points(Year, imputedYield, col = "blue", pch = 19))
  })
  try({
    with(tmp, plot(Year, groupedMean, ylab = "Average Yield",
                   ylim = c(0, ymax), type = "b"))
  })
  
}
graphics.off()
system("evince checkWheatImputation.pdf&")


## Check the sub-regional fit
pdf(file = "wheatYieldSubregion.pdf", width = 11)
print(ggplot(data = imputed.dt,
             aes(x = Year, y = valueYield)) +
      geom_line(aes(col = factor(FAOST_CODE))) +
      geom_point(aes(col = factor(FAOST_CODE)), size = 1) +  
      scale_color_manual(values = rep("gold",
                           length(unique(imputed.dt$FAOST_CODE)))) +
      geom_line(aes(x = Year, y = groupedMean), col = "steelblue",
                alpha = 0.5) +
      facet_wrap(~UNSD_SUB_REG, ncol = 4) +
      labs(x = NULL, y = NULL,
      title = "Yield of Wheat by sub-region with estimated average") + 
      theme(legend.position = "none"))
graphics.off()
system("evince wheatYieldSubregion.pdf&")


## Illustration of the impuation process.
## pdf(file = "wheatImputationStep.pdf", width = 10)
## i = 2
## tmp = imputed.dt[FAOST_CODE == i, ]

## ## Step (1): Raw data
## myCountry = FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
##   i), "FAO_TABLE_NAME"]
## myItem = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
##   unique(imputed.dt$itemCode), "itemName"])
## par(mfrow = c(4, 1), mar = c(2.1, 4.1, 3.1, 2.1))
## title = "Step (1): Raw data - "
## try({
##   ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Production",
##                  main = paste0(title, myCountry, " (", i, ") - ",
##                    myItem, " (", unique(imputed.dt$itemCode), ")"),
##                  cex = 2))
## })

## try({
##   ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
##   with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Area",
##                  cex = 2))
## })


## try({
##   ymax = max(tmp[, list(valueYield, imputedYield, avgValue)],
##     na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "n",
##                  col = "black", xlab = "", ylab = "Yield",
##                  cex = 2))
## })
## try({
##   with(tmp, plot(Year, avgValue, ylab = "Average Yield",
##                  ylim = c(0, ymax), type = "n"))
## })



## ## Step (2): Compute Implied Yield
## title = "Step (2) Compute implied yield - "
## try({
##   ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Production",
##                  main = paste0(title, myCountry, " (", i, ") - ",
##                    myItem, " (", unique(imputed.dt$itemCode), ")"),
##                  cex = 2))
## })

## try({
##   ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
##   with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Area",
##                  cex = 2))
## })


## try({
##   ymax = max(tmp[, list(valueYield, imputedYield, avgValue)],
##     na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Yield",
##                  cex = 2))
## })
## try({
##   with(tmp, plot(Year, avgValue, ylab = "Average Yield",
##                  ylim = c(0, ymax), type = "n"))
## })


## ## Step (3): Impute the yield
## title = "Step (3) Impute the yield - "
## try({
##   ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Production",
##                  main = paste0(title, myCountry, " (", i, ") - ",
##                    myItem, " (", unique(imputed.dt$itemCode), ")"),
##                  cex = 2))
## })
## try({
##   ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
##   with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Area",
##                  cex = 2))
## })
## try({
##   ymax = max(tmp[, list(valueYield, imputedYield, avgValue)],
##     na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Yield",
##                  cex = 2))
##   with(tmp[is.na(valueYield), ],
##        points(Year, imputedYield, col = "blue", pch = 19))
## })
## try({
##   with(tmp, plot(Year, avgValue, ylab = "Average Yield",
##                  ylim = c(0, ymax), type = "b"))
## })



## ## Step (4): Compute Area and production where available
## title = "Step (4): Compute area and production where available"
## try({
##   ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Production",
##                  main = paste0(title, myCountry, " (", i, ") - ",
##                    myItem, " (", unique(imputed.dt$itemCode), ")"),
##                  cex = 2))
##   with(tmp[is.na(valueProd) & !is.na(valueArea), ],
##        points(Year, imputedProd, col = "blue", pch = 19))
## })
## try({
##   ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
##   with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Area",
##                  cex = 2))
##   with(tmp[is.na(valueArea) & !is.na(valueProd),],
##        points(Year, imputedArea, col = "blue", pch = 19))
## })
## try({
##   ymax = max(tmp[, list(valueYield, imputedYield, avgValue)],
##     na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Yield",
##                  cex = 2))
##   with(tmp[is.na(valueYield), ],
##        points(Year, imputedYield, col = "red", pch = 19))
## })
## try({
##   with(tmp, plot(Year, avgValue, ylab = "Average Yield",
##                  ylim = c(0, ymax), type = "b"))
## })


## ## Step (5): Impute Area with naive imputation
## title = "Step (5): Impute area with naive imputation - "
## try({
##   ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Production",
##                  main = paste0(title, myCountry, " (", i, ") - ",
##                    myItem, " (", unique(imputed.dt$itemCode), ")"),
##                  cex = 2))
##   with(tmp[is.na(valueProd) & !is.na(valueArea), ],
##        points(Year, imputedProd, col = "blue", pch = 19))
## })
## try({
##   ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
##   with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Area",
##                  cex = 2))
##   with(tmp[is.na(valueArea) & !is.na(valueProd),],
##        points(Year, imputedArea, col = "red", pch = 19))
##   with(tmp[is.na(valueArea) & is.na(valueProd),],
##        points(Year, imputedArea, col = "blue", pch = 19))  
## })
## try({
##   ymax = max(tmp[, list(valueYield, imputedYield, avgValue)],
##     na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Yield",
##                  cex = 2))
##   with(tmp[is.na(valueYield), ],
##        points(Year, imputedYield, col = "red", pch = 19))
## })
## try({
##   with(tmp, plot(Year, avgValue, ylab = "Average Yield",
##                  ylim = c(0, ymax), type = "b"))
## })


## ## Step (6): Impute the final production
## title = "Step (6): Impute the production - "
## try({
##   ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Production",
##                  main = paste0(title, myCountry, " (", i, ") - ",
##                    myItem, " (", unique(imputed.dt$itemCode), ")"),
##                  cex = 2))
##   with(tmp[is.na(valueProd), ],
##        points(Year, imputedProd, col = "blue", pch = 19))
## })
## try({
##   ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
##   with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Area",
##                  cex = 2))
##   with(tmp[is.na(valueArea) & !is.na(valueProd),],
##        points(Year, imputedArea, col = "red", pch = 19))
##   with(tmp[is.na(valueArea) & is.na(valueProd),],
##        points(Year, imputedArea, col = "red", pch = 19))  
## })
## try({
##   ymax = max(tmp[, list(valueYield, imputedYield, avgValue)],
##     na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Yield",
##                  cex = 2))
##   with(tmp[is.na(valueYield), ],
##        points(Year, imputedYield, col = "red", pch = 19))
## })
## try({
##   with(tmp, plot(Year, avgValue, ylab = "Average Yield",
##                  ylim = c(0, ymax), type = "b"))
## })

## graphics.off()
## system("evince wheatImputationStep.pdf&")


