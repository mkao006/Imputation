########################################################################
## Title: Test the full series of Rice production for case study
## Date: 2013-04-30
########################################################################

library(data.table)
library(reshape2)
library(FAOSTAT)
library(nlme)
library(zoo)

## Run the data manipulation
source("lmeEMImpute.R")
source("computeYield.R")
source("fullImputation.R")
source("naiveImpute.R")

## Data preperation
## ---------------------------------------------------------------------

## Read and manipulate the data
fc.df = read.csv(file = "fullRice.csv", header = TRUE,
  stringsAsFactors = FALSE)
mfc.df = melt(fc.df, id.var = c("Area.Code", "Area.Name",
                       "Item.Code", "Element.Code"))
ind = sapply(regexpr("_", mfc.df$variable), function(x) x[[1]])
mfc.df$Year = substring(mfc.df$variable, ind + 1)
mfc.df$type = substring(mfc.df$variable, 1, ind - 1)
mfc.df$variable = NULL
mfc.df$Element.Code = ifelse(mfc.df$Element.Code == 31, "valueArea",
  "valueProd")
cmfc.df = dcast(mfc.df, Area.Code + Area.Name + Item.Code + Year ~ Element.Code +
  type, value.var = "value")
colnames(cmfc.df) = c("FAOST_CODE", "FAOST_NAME", "itemCode", "Year",
          "valueArea", "symbArea", "valueProd", "symbProd")
cmfc.df$Year = as.integer(cmfc.df$Year)
cmfc.df$valueArea = as.numeric(cmfc.df$valueArea)
cmfc.df$valueProd = as.numeric(cmfc.df$valueProd)

## Save the original value
cmfc.df$ovalueArea = cmfc.df$valueArea
cmfc.df$ovalueProd = cmfc.df$valueProd

## Use data only from 1990
cmfc.df = subset(cmfc.df, Year >= 1990)

## Replace values with symbol T or duplicated F with NA
cmfc.df[which(cmfc.df$symbArea == "T"), "valueArea"] = NA
cmfc.df[which(cmfc.df$symbProd == "T"), "valueProd"] = NA
cmfc.df[which(cmfc.df$symbArea == "E"), "valueArea"] = NA
cmfc.df[which(cmfc.df$symbProd == "E"), "valueProd"] = NA

## Replace all duplicated F
## cmfc.df[which(duplicated(cmfc.df[, c("FAOST_CODE", "itemCode", "symbArea",
##                                  "valueArea")]) & cmfc.df$symbArea == "F"),
##          "valueArea"] = NA
## cmfc.df[which(duplicated(cmfc.df[, c("FAOST_CODE", "itemCode", "symbProd",
##                                  "valueProd")]) & cmfc.df$symbProd == "F"),
##          "valueProd"] = NA

cmfc.df[which(duplicated(cmfc.df[, c("FAOST_CODE", "itemCode", "symbArea")]) &
              cmfc.df$symbArea == "F"),
         "valueArea"] = NA
cmfc.df[which(duplicated(cmfc.df[, c("FAOST_CODE", "itemCode", "symbProd")]) &
              cmfc.df$symbProd == "F"),
         "valueProd"] = NA


## Remove error where area or production are zero while the other is not
cmfc.df[which(cmfc.df$valueArea == 0 & cmfc.df$valueProd != 0),
           "valueArea"] = NA
cmfc.df[which(cmfc.df$valueArea != 0 & cmfc.df$valueProd == 0),
           "valueProd"] = NA

## Compute the implied yield
cmfc.df$valueYield = with(cmfc.df, computeYield(valueProd, valueArea))
cmfc.df$ovalueYield = with(cmfc.df, computeYield(ovalueProd, ovalueArea))


## Merge with regional and subregional
cmfc.df = arrange(merge(cmfc.df,
  FAOregionProfile[, c("FAOST_CODE", "UNSD_SUB_REG_CODE",
  "UNSD_MACRO_REG_CODE")], all.x = TRUE), FAOST_CODE, itemCode, Year)
## This is a hack for the region until Fillipo fixes the countryprofile.
cmfc.df = merge(cmfc.df,
  na.omit(unique(FAOregionProfile[, c("UNSD_MACRO_REG_CODE", "UNSD_MACRO_REG")])),
  all.x = TRUE)
cmfc.df = merge(cmfc.df,
  na.omit(unique(FAOregionProfile[, c("UNSD_SUB_REG_CODE", "UNSD_SUB_REG")])),
  all.x = TRUE)
cmfc.df$UNSD_SUB_REG_CODE = NULL
cmfc.df$UNSD_MACRO_REG_CODE = NULL

## Remove country which does not have a sub-region, they are typically
## former countries
cmfc.df = cmfc.df[!is.na(cmfc.df$UNSD_SUB_REG), ]

## Western Asia is removed because there is no information
## final.dt = data.table(cmfc.df[cmfc.df$UNSD_SUB_REG != "Western Asia", ])
final.dt = data.table(cmfc.df)
final.dt[, UNSD_MACRO_REG := factor(UNSD_MACRO_REG)]
final.dt[, UNSD_SUB_REG := factor(UNSD_SUB_REG)]
setkeyv(final.dt, c("FAOST_CODE", "FAOST_NAME", "Year"))

## Number of official and semi officail data
final.dt[symbArea %in% c(" ", "*"), sum(!is.na(ovalueArea))]
final.dt[symbProd %in% c(" ", "*"), sum(!is.na(ovalueProd))]


## NOTE (Michael): We see no evidence of the change distribution
##                 dependent on the missing values.
## final.dt[, dvalueYield := c(NA, diff(valueYield)), by = "FAOST_CODE"]
## final.dt[, missBin := sum(is.na(valueYield))/length(valueYield) > 0.2,
##          by = "FAOST_CODE"]
## ggplot(data = final.dt, aes(x = dvalueYield)) +
##   geom_histogram(binwidth = 0.1) +
##   facet_wrap(~missBin, scales = "free_y")



## Imputation
## ---------------------------------------------------------------------
imputed.dt = fullImputation(final.dt, area = "valueArea", prod = "valueProd",
  yield = "valueYield", country = "FAOST_CODE",
  group = "UNSD_SUB_REG", year = "Year",  commodity = "itemCode")




## Simulation
## ---------------------------------------------------------------------

n.sim = 5000
sim.df = data.frame(propSim = runif(n.sim, 1e-05, 1 - 1e-05),
  propReal = rep(NA, n.sim), MAPE = rep(NA, n.sim), method = rep(NA, n.sim))
for(i in 1:n.sim){
  print(paste0("Simulation Number: ", i))
  tmp.dt = final.dt
  prop = sim.df[i, "propSim"]
  ## set.seed(587)
  simMissArea = sample(which(tmp.dt$symbArea %in% c(" ", "*")),
    length(which(tmp.dt$symbArea %in% c(" ", "*"))) * prop)
  tmp.dt[, simArea := valueArea]
  tmp.dt[simMissArea, "simArea"] = NA
  ## set.seed(587)
  simMissProd = sample(which(tmp.dt$symbProd %in% c(" ", "*")),
    length(which(tmp.dt$symbProd %in% c(" ", "*"))) * prop)
  tmp.dt[, simProd := valueProd]
  tmp.dt[simMissProd, "simProd"] = NA 
  tmp.dt[, simYield := computeYield(simProd, simArea)]
  sim.df[i, "propReal"] = tmp.dt[, sum(is.na(simYield))/length(simYield)]  
  impSim.dt = try(fullImputation(tmp.dt, area = "simArea", prod = "simProd",
    yield = "simYield", country = "FAOST_CODE",
    group = "UNSD_SUB_REG", year = "Year",  commodity = "itemCode"))
  if(!inherits(impSim.dt, "try-error")){
    sim.df[i, "method"] = unique(impSim.dt[, yieldMethodology])
    ## Check the MAPE of the imputation
    sim.df[i, "MAPE"] = 
      impSim.dt[1:nrow(impSim.dt) %in% simMissProd & ovalueProd != 0 &
                !is.na(imputedProd),
                sum(abs((ovalueProd - imputedProd)/(ovalueProd)))/
                length(simMissProd)]
  } else {
    sim.df[i, c("MAPE", "method")] = NA
  }
}

subSim.df = sim.df[sim.df$MAPE <= 100, ]

with(subSim.df[subSim.df$method == "LME", ], plot(propReal, MAPE, xlim = c(0, 1),
              ylim = c(range(subSim.df$MAPE, na.rm = TRUE))))
with(subSim.df[subSim.df$method != "LME", ], points(propReal, MAPE, col = "red"))

pdf(file = "riceSimulationResult.pdf", width = 10)
with(subSim.df[subSim.df$MAPE < 1.0, ], plot(propReal, MAPE, xlim = c(0, 1),
          ylim = c(range(MAPE, na.rm = TRUE)),
     xlab = "Missing proportion in yield",
     ylab = "Mean absolute percentage error"))
with(subSim.df[subSim.df$MAPE < 1.0 & !is.na(subSim.df$MAPE), ],
     lines(lowess(propReal, MAPE, f = 0.2), col = "red", lwd = 2), xlim = c(0, 1),
           ylim = c(range(MAPE, na.rm = TRUE)))
abline(h = seq(0, 1, by = 0.05), lty = 2, col = "grey50")
graphics.off()
system("evince riceSimulationResult.pdf&")


with(subSim.df[subSim.df$method == "LME", ], plot(propSim, MAPE, xlim = c(0, 1),
              ylim = c(range(subSim.df$MAPE, na.rm = TRUE))))
with(subSim.df[subSim.df$method != "LME", ], points(propSim, MAPE, col = "red"))


pdf(file = "checkRiceSim.pdf", width = 10)
for(i in unique(imputed.dt$FAOST_CODE)){
  tmp = impSim.dt[FAOST_CODE == i, ]

  myCountry = FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
    i), "FAO_TABLE_NAME"]
  myItem = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
    unique(imputed.dt$itemCode), "itemName"])
  par(mfrow = c(3, 1), mar = c(2.1, 4.1, 3.1, 2.1))
  try({
    ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
    with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Production",
                   main = paste0(myCountry, " (", i, ") - ",
                     myItem, " (", unique(imputed.dt$itemCode), ")"),
                   cex = 2))
    with(tmp[is.na(simProd), ],
         points(Year, imputedProd, col = "blue", pch = 19))
  })
  
  try({
    ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
    with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Area",
                   cex = 2))
    with(tmp[is.na(simArea),],
         points(Year, imputedArea, col = "blue", pch = 19))
  })
  
  
  try({
    ymax = max(tmp[, list(valueYield, imputedYield)], na.rm = TRUE) * 1.2
    with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Yield",
                    cex = 2))
    with(tmp[!is.na(valueYield), ],
         points(Year, imputedYield, col = "red", pch = 19))
    with(tmp[is.na(simYield), ],
         points(Year, imputedYield, col = "blue", pch = 19))    
  })
  
}
graphics.off()
system("evince checkRiceSim.pdf&")



## Percentage of missing value imputed
NROW(imputed.dt[is.na(valueProd) & !is.na(imputedProd), ])/
  NROW(imputed.dt[is.na(valueProd), ])

NROW(imputed.dt[is.na(valueArea) & !is.na(imputedArea), ])/
  NROW(imputed.dt[is.na(valueArea), ])

## Remove yield where area or produciton are zero
imputed.dt[imputedProd == 0 | imputedArea == 0, imputedYield := as.numeric(NA)]


## Examination and plots
## ---------------------------------------------------------------------

pdf(file = "checkRiceImputation.pdf", width = 10)
for(i in unique(imputed.dt$FAOST_CODE)){
  tmp = imputed.dt[FAOST_CODE == i, ]

  myCountry = FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
    i), "FAO_TABLE_NAME"]
  myItem = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
    unique(imputed.dt$itemCode), "itemName"])
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
    ymax = max(tmp[, list(valueYield, imputedYield, avgValue)],
      na.rm = TRUE) * 1.2
    with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Yield",
                    cex = 2))
    with(tmp[!is.na(valueYield), ],
         points(Year, imputedYield, col = "red", pch = 19))
    with(tmp[is.na(valueYield), ],
         points(Year, imputedYield, col = "blue", pch = 19))
  })
  try({
    with(tmp, plot(Year, avgValue, ylab = "Average Yield",
                   ylim = c(0, ymax), type = "b"))
  })
  
}
graphics.off()
system("evince checkRiceImputation.pdf&")



pdf(file = "riceYieldSubregion.pdf", width = 11)
print(ggplot(data = imputed.dt,
             aes(x = Year, y = valueYield)) +
      geom_line(aes(col = factor(FAOST_CODE))) +
      geom_point(aes(col = factor(FAOST_CODE)), size = 1) +  
      scale_color_manual(values = rep("gold",
                           length(unique(final.dt$FAOST_CODE)))) +
      ## geom_line(aes(x = Year, y = avgYield), col = "black", alpha = 0.5) +      
      geom_line(aes(x = Year, y = avgValue), col = "steelblue", alpha = 0.5) +
      ## geom_smooth(method = "lm") + 
      ## geom_line(aes(x = Year, y = avgValue), col = "blue", alpha = 0.5) +      
      facet_wrap(~UNSD_SUB_REG, ncol = 4) +
      labs(x = NULL, y = NULL,
      title = "Yield of Rice by sub-region with average and least square line") + 
      theme(legend.position = "none"))
graphics.off()
system("evince riceYieldSubregion.pdf&")



print(ggplot(data = final.dt[!UNSD_SUB_REG %in% c("Western Asia", "Micronesia"), ],
             aes(x = Year, y = valueArea)) +
      geom_line(aes(col = factor(FAOST_CODE))) +
      geom_point(aes(col = factor(FAOST_CODE)), size = 1) +  
      scale_color_manual(values = rep("gold",
                           length(unique(final.dt$FAOST_CODE)))) +
      geom_smooth(method = "lm") + 
      facet_wrap(~UNSD_SUB_REG, ncol = 4, scales = "free_y") +
      labs(x = NULL, y = NULL, title = "Yield of Rice by sub-region") + 
      theme(legend.position = "none"))





## ## Plots for presentation
## ## ---------------------------------------------------------------------
## plot.df = melt(final.dt[symbArea %in% c(" ", "*") & symbProd %in% c(" ", "*"),
##   list(FAOST_CODE, FAOST_NAME, Year, UNSD_SUB_REG,
##   valueArea, valueProd, valueYield)],
##   id.var = c("FAOST_CODE", "FAOST_NAME", "Year", "UNSD_SUB_REG"))
## plot.df$variable = factor(gsub("value", "", plot.df$variable),
##   levels = c("Prod", "Area", "Yield"))

## tmp = data.table(plot.df[plot.df$variable == "Prod", ])
## tmp[ ,avgValue := mean(value, na.rm = TRUE), by = c("Year", "UNSD_SUB_REG")]

## ggplot(data = tmp,
##        aes(x = Year, y = value)) +
##   geom_line(aes(col = factor(FAOST_CODE)), alpha = 0.3) +
##   scale_color_manual(values = rep("black", length(unique(plot.df$FAOST_CODE)))) +
##   geom_line(aes(x = Year, y = avgValue), col = "blue") +
##   facet_wrap(~UNSD_SUB_REG, ncol = 4, scales = "free_y") + 
##   theme(legend.position = "none") +
##   labs(x = NULL, y = NULL,
##        title = "Area and Yield series of Rice on original scale")

## pdf(file = "riceIdentityBreakDown.pdf", width = 10, height = 5)
## ggplot(data = plot.df[which(plot.df$value > 0), ],
##        aes(x = Year, y = log(value))) +
##   geom_line(aes(col = factor(FAOST_CODE)), alpha = 0.3) +
##   scale_color_manual(values = rep("black", length(unique(plot.df$FAOST_CODE)))) +
##   facet_wrap(~variable, ncol = 1) +
##   theme(legend.position = "none") +
##   labs(x = NULL, y = NULL,
##        title = "Relation of Rice production, area and yield on log scale")
## graphics.off()
## system("evince riceIdentityBreakDown.pdf&")

## pdf(file = "riceAreaYield.pdf", width = 10, height = 5)
## ggplot(data = plot.df[plot.df$variable != "Prod", ],
##        aes(x = Year, y = value)) +
##   geom_line(aes(col = factor(FAOST_CODE)), alpha = 0.3) +
##   scale_color_manual(values = rep("black", length(unique(plot.df$FAOST_CODE)))) +
##   facet_wrap(~variable, ncol = 1, scales = "free_y") +
##   theme(legend.position = "none") +
##   labs(x = NULL, y = NULL,
##        title = "Area and Yield series of Rice on original scale")
## graphics.off()
## system("evince riceAreaYield.pdf&")


## ## Check the missing mechanism
## pdf(file = "riceAreaMiss.pdf", width = 10)
## sparsityHeatMap(data = data.frame(final.dt[symbArea %in% c(" ", "*"), ]),
##                 country = "FAOST_NAME", year = "Year",
##                 var = "valueArea", ncol = 3)
## graphics.off()

## pdf(file = "riceProdMiss.pdf", width = 10)
## sparsityHeatMap(data = data.frame(final.dt[symbProd %in% c(" ", "*"), ]),
##                   country = "FAOST_NAME", year = "Year",
##                 var = "valueProd", ncol = 3)
## graphics.off()
