########################################################################
## Title: Test the full series of Wheat production for case study
## Date: 2013-04-30
########################################################################

library(data.table)
library(reshape2)
library(FAOSTAT)
library(nlme)
library(zoo)

## Run the data manipulation
source("na.approx2.R")
source("diffv.R")
source("lmeImpute.R")
source("lmeEMImpute.R")
source("checkSparsity.R")
source("computeYield.R")
source("randomImp.R")


## Data preperation
## ---------------------------------------------------------------------

## Read and manipulate the data
fc.df = read.csv(file = "fullWheat.csv", header = TRUE,
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

## Only use data from 1980
## cmfc.df = cmfc.df[cmfc.df$Year >= 1980, ]

## Save the original value
cmfc.df$ovalueArea = cmfc.df$valueArea
cmfc.df$ovalueProd = cmfc.df$valueProd

## Replace values with symbol T or duplicated F with NA
cmfc.df[which(cmfc.df$symbArea == "T"), "valueArea"] = NA
cmfc.df[which(cmfc.df$symbProd == "T"), "valueProd"] = NA
cmfc.df[which(cmfc.df$symbArea == "E"), "valueArea"] = NA
cmfc.df[which(cmfc.df$symbProd == "E"), "valueProd"] = NA

## ## Replace all duplicated F
## cmfc.df[which(duplicated(cmfc.df[, c("FAOST_CODE", "itemCode", "symbArea")]) &
##               cmfc.df$symbArea == "F"), "valueArea"] = NA
## cmfc.df[which(duplicated(cmfc.df[, c("FAOST_CODE", "itemCode", "symbProd")]) &
##               cmfc.df$symbProd == "F"), "valueProd"] = NA

cmfc.df[which(duplicated(cmfc.df[, c("FAOST_CODE", "itemCode", "symbArea",
                                 "valueArea")]) & cmfc.df$symbArea == "F"),
         "valueArea"] = NA
cmfc.df[which(duplicated(cmfc.df[, c("FAOST_CODE", "itemCode", "symbProd",
                                 "valueProd")]) & cmfc.df$symbProd == "F"),
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

checkSparsity(final.dt)

## We see no evidence of the change distribution dependent on the
## missing values.
final.dt[, dvalueYield := c(NA, diff(valueYield)), by = "FAOST_CODE"]
final.dt[, missBin := sum(is.na(valueYield))/length(valueYield) > 0.2,
         by = "FAOST_CODE"]
ggplot(data = final.dt, aes(x = dvalueYield)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~missBin, scales = "free_y")

## Carry out the imputation
## ---------------------------------------------------------------------
## lmeImpute(Data = final.dt, value = "valueYield", country = "FAOST_CODE",
##           group = "UNSD_SUB_REG", year = "Year", commodity = "itemCode")
## setnames(final.dt, old = "imputedValue", new = "imputedYield")

lmeEMImpute(final.dt, "valueYield", "FAOST_CODE", "UNSD_SUB_REG", "Year",
            "itemCode")

## Create the impute column
## final.dt[!is.na(valueYield), imputedYield := valueYield]
setnames(final.dt, old = "estValue", new = "imputedYield")
final.dt[!is.na(valueYield), imputedYield := as.numeric(NA)]
checkSparsity(final.dt)

## Create the imputed column for area and production
final.dt[, imputedArea := valueArea]
final.dt[, imputedProd := valueProd]

## Impute area and production if the other one exist
final.dt[is.na(imputedArea) & !is.na(imputedProd),
         imputedArea := imputedProd/imputedYield]
final.dt[!is.na(imputedArea) & is.na(imputedProd),
         imputedProd := imputedArea * imputedYield]

## Impute area with linear interpolation and last observation carry
## forward.
final.dt[, imputedArea := na.locf(na.locf(na.approx2(imputedArea), na.rm = FALSE),
             fromLast = TRUE, na.rm = FALSE),
         by = c("FAOST_CODE", "itemCode")]


## Impute the remaining production
final.dt[is.na(imputedProd), imputedProd := imputedArea * imputedYield]

checkSparsity(final.dt)

## Percentage of missing value imputed
NROW(final.dt[is.na(valueProd) & !is.na(imputedProd), ])/
  NROW(final.dt[is.na(valueProd), ])

NROW(final.dt[is.na(valueArea) & !is.na(imputedArea), ])/
  NROW(final.dt[is.na(valueArea), ])

final.dt[, avgYield := mean(valueYield, na.rm = TRUE),
         by = c("Year", "UNSD_SUB_REG")]

## Remove yield where area or produciton are zero
final.dt[imputedProd == 0 | imputedArea == 0, imputedYield := as.numeric(NA)]


## Examination and plots
## ---------------------------------------------------------------------

pdf(file = "checkWheatImputation.pdf", width = 10)
## for(i in c(32, 45, 46, 61, 74, 144, 181, 215, 251)){
for(i in unique(final.dt$FAOST_CODE)){
  tmp = final.dt[FAOST_CODE == i, ]

  myCountry = FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
    i), "FAO_TABLE_NAME"]
  myItem = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
    unique(final.dt$itemCode), "itemName"])
  par(mfrow = c(3, 1), mar = c(2.1, 4.1, 3.1, 2.1))
  try({
    ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
    with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Production",
                   main = paste0(myCountry, " (", i, ") - ",
                     myItem, " (", unique(final.dt$itemCode), ")"),
                   cex = 2))
    with(tmp, points(Year, imputedProd, col = "red", pch = 19))
  })
  
  try({
    ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
    with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Area",
                   cex = 2))
    with(tmp, points(Year, imputedArea, col = "red", pch = 19))
  })
  
  
  try({
    ymax = max(tmp[, list(valueYield, imputedYield)], na.rm = TRUE) * 1.2
    with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Yield",
                    cex = 2))
    with(tmp, points(Year, imputedYield, col = "red", pch = 19))
    with(tmp, points(Year, fittedValue, col = "blue", pch = 19))    
  })
  
}
graphics.off()
system("evince checkWheatImputation.pdf&")



pdf(file = "wheatYieldSubregion.pdf", width = 11)
print(ggplot(data = final.dt,
             aes(x = Year, y = valueYield)) +
      geom_line(aes(col = factor(FAOST_CODE))) +
      geom_point(aes(col = factor(FAOST_CODE)), size = 1) +  
      scale_color_manual(values = rep("gold",
                           length(unique(final.dt$FAOST_CODE)))) +
      geom_line(aes(x = Year, y = avgYield), col = "black", alpha = 0.5) +      
      geom_line(aes(x = Year, y = avgValue), col = "steelblue", alpha = 0.5) +
      ## geom_smooth(method = "lm") + 
      ## geom_line(aes(x = Year, y = avgValue), col = "blue", alpha = 0.5) +      
      facet_wrap(~UNSD_SUB_REG, ncol = 4) +
      labs(x = NULL, y = NULL,
      title = "Yield of Wheat by sub-region with average and least square line") + 
      theme(legend.position = "none"))
graphics.off()
system("evince wheatYieldSubregion.pdf&")



print(ggplot(data = final.dt[!UNSD_SUB_REG %in% c("Western Asia", "Micronesia"), ],
             aes(x = Year, y = valueArea)) +
      geom_line(aes(col = factor(FAOST_CODE))) +
      geom_point(aes(col = factor(FAOST_CODE)), size = 1) +  
      scale_color_manual(values = rep("gold",
                           length(unique(final.dt$FAOST_CODE)))) +
      geom_smooth(method = "lm") + 
      facet_wrap(~UNSD_SUB_REG, ncol = 4, scales = "free_y") +
      labs(x = NULL, y = NULL, title = "Yield of Wheat by sub-region") + 
      theme(legend.position = "none"))





## Plots for presentation
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
##        title = "Area and Yield series of Wheat on original scale")

## pdf(file = "wheatIdentityBreakDown.pdf", width = 10, height = 5)
## ggplot(data = plot.df[which(plot.df$value > 0), ],
##        aes(x = Year, y = log(value))) +
##   geom_line(aes(col = factor(FAOST_CODE)), alpha = 0.3) +
##   scale_color_manual(values = rep("black", length(unique(plot.df$FAOST_CODE)))) +
##   facet_wrap(~variable, ncol = 1) +
##   theme(legend.position = "none") +
##   labs(x = NULL, y = NULL,
##        title = "Relation of Wheat production, area and yield on log scale")
## graphics.off()

## pdf(file = "wheatAreaYield.pdf", width = 10, height = 5)
## ggplot(data = plot.df[plot.df$variable != "Prod", ],
##        aes(x = Year, y = value)) +
##   geom_line(aes(col = factor(FAOST_CODE)), alpha = 0.3) +
##   scale_color_manual(values = rep("black", length(unique(plot.df$FAOST_CODE)))) +
##   facet_wrap(~variable, ncol = 1, scales = "free_y") +
##   theme(legend.position = "none") +
##   labs(x = NULL, y = NULL,
##        title = "Area and Yield series of Wheat on original scale")
## graphics.off()



## ## Check the missing mechanism
## pdf(file = "wheatAreaMiss.pdf", width = 10)
## sparsityHeatMap(data = data.frame(final.dt[symbArea %in% c(" ", "*"), ]),
##                 country = "FAOST_NAME", year = "Year",
##                 var = "valueArea", ncol = 3)
## graphics.off()

## pdf(file = "wheatProdMiss.pdf", width = 10)
## sparsityHeatMap(data = data.frame(final.dt[symbProd %in% c(" ", "*"), ]),
##                   country = "FAOST_NAME", year = "Year",
##                 var = "valueProd", ncol = 3)
## graphics.off()









                
