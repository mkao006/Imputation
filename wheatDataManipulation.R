########################################################################
## Title: Test the full series of Wheat production for case study
## Date: 2013-04-30
########################################################################

library(data.table)
library(reshape2)
library(FAOSTAT)

## Run the data manipulation
source("computeYield.R")

## Data preperation
## ---------------------------------------------------------------------

## Read and manipulate the data
wheatSUA = read.csv(file = "wheatSUA.csv", header = TRUE,
  stringsAsFactors = FALSE)
save(wheatSUA, file = "wheatSUA.RData")
mfc.df = melt(wheatSUA, id.var = c("Area.Code", "Area.Name",
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
cmfc.df = subset(cmfc.df, Year >= 1975)

## Replace values with symbol T or duplicated F with NA
cmfc.df[which(cmfc.df$symbArea == "T"), "valueArea"] = NA
cmfc.df[which(cmfc.df$symbProd == "T"), "valueProd"] = NA
cmfc.df[which(cmfc.df$symbArea == "E"), "valueArea"] = NA
cmfc.df[which(cmfc.df$symbProd == "E"), "valueProd"] = NA

## Replace all duplicated F
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
cmfcReg.df = arrange(merge(cmfc.df,
  FAOregionProfile[, c("FAOST_CODE", "UNSD_MACRO_REG", "UNSD_SUB_REG")],
  all.x = TRUE), FAOST_CODE, itemCode, Year)

## Remove country which does not have a sub-region, they are typically
## former countries
cmfcReg.df = cmfcReg.df[!is.na(cmfcReg.df$UNSD_SUB_REG), ]

## Set factor and names
wheatPrep.dt = data.table(cmfcReg.df)
wheatPrep.dt[, UNSD_MACRO_REG := factor(UNSD_MACRO_REG)]
wheatPrep.dt[, UNSD_SUB_REG := factor(UNSD_SUB_REG)]
setkeyv(wheatPrep.dt, c("FAOST_CODE", "FAOST_NAME", "Year"))

## Number of official and semi officail data
wheatPrep.dt[symbArea %in% c(" ", "*"), sum(!is.na(ovalueArea))]
wheatPrep.dt[symbProd %in% c(" ", "*"), sum(!is.na(ovalueProd))]


wheatImputeExample = data.frame(subset(wheatPrep.dt,
  select = c("FAOST_NAME", "Year", "valueArea", "valueProd",
    "valueYield", "UNSD_SUB_REG", "UNSD_MACRO_REG")))
save(wheatImputeExample, file = "wheatImputeExample.RData")
