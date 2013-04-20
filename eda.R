########################################################################
## Title: Exploratory data analysis of the imputation data
## Date:  2013-04-10
########################################################################


library(plyr)
library(reshape2)
library(XML)
library(FAOSYB)
library(reshape2)
library(zoo)



## Commodity group data take from the SAS script
##---------------------------------------------------------------------

## TODO (Michael): Check which commodity group does 461 belong to
##                 (currently in fruits and other_crops), otherwise it
##                 creates duplicates. We will put it in fruits for now.
tmp = c(cereals = c(15, 27, 44, 75, 56, 71, 79, 83, 89, 101, 92, 94,
                    97, 103, 108),
        pulses = c(176, 187, 191, 195, 201, 181, 197, 210, 205,
                   203, 211),
        roots_tub = c(125, 116, 122, 137, 135, 136, 149),
        treenuts = c(221, 223, 217, 225, 222, 220, 224, 216, 226, 234),
        oilcrops = c(236, 242, 260, 249, 277, 305, 310, 256, 263, 265,
                     267, 270, 275, 280, 289, 292, 296, 299, 328, 329,
                     333, 336, 339),
        veg = c(388, 402, 403, 406, 426, 407, 393, 358, 372, 397, 417,
                414, 423, 420, 366, 367, 399, 449, 401, 373, 394, 446,
                430, 378, 567, 568, 463),
        fruit = c(486, 489, 577, 569, 574, 572, 571, 603, 490, 495, 507,
                  497, 512, 560, 600, 515, 521, 523, 526, 530, 531, 534,
                  536, 544, 547, 552, 554, 558, 550, 549, 592, 587, 542,
                  541, 591, 619),
        stim_beve = c(656, 661, 667, 671, 674),
        spices = c(687, 689, 692, 693, 698, 702, 711, 720, 723),
        tobacco = c(826),
        othr_crops = c(459, 461, 677),
        rubber = c(837, 836),
        leaves = c(748, 754),
        fibres = c(767, 780, 788, 800, 809, 821, 782, 987, 773, 1185, 789),
        othr_meat = c(867, 947, 1035, 977, 1017, 1127, 1097, 1108, 1111,
                      1163, 1166),
        poultry_meat = c(1058, 1069, 1073, 1080, 1141),
        milk = c(882, 951, 982, 1020, 1130), eggs = c(1062, 1091),
        lvstk_prds = c(886, 901),
        sugar = c(162, 163, 1182),
        veg_oils = c(36, 60, 237, 244, 257, 258, 252, 261, 264, 266, 268,
          271, 276, 281, 290, 293, 297, 331, 334, 337, 340, 1242, 278, 307, 313),
        alch_bev = c(564, 51, 66, 82, 86)
  )

comGrp.df = melt(tmp)
comGrp.df$Group = gsub("[0-9]", "", rownames(comGrp.df))
comGrp.df$includeGroup = ifelse(comGrp.df$Group %in% c("lvstk_prds", "sugar",
  "veg_oils", "alch_bev"), FALSE, TRUE)
row.names(comGrp.df) = NULL
comGrp.dt = data.table(comGrp.df)
rm(comGrp.df)
setnames(comGrp.dt, old = colnames(comGrp.dt),
         new = c("itemCode", "comGroup", "includeGroup"))


## Read and process the test data
## ---------------------------------------------------------------------
raw.dt = data.table(read.csv("nicola_ws_request_12yrs.csv", header = TRUE,
    stringsAsFactors = FALSE))
setnames(raw.dt, old = c("ItemGroup", "AREA", "ITEM", "ELE"),
         new = c("itemGroup", "FAOST_CODE", "itemCode", "elementCode"))

## Remove test area and excluded countries 
raw.dt = subset(raw.dt, !(FAOST_CODE %in% c(351, 296, 357, 76, 245, 246, 246, 43,
    298)))
FAOcountryProfile[FAOcountryProfile$FAOST_CODE %in%
                  c(351, 296, 357, 76, 245, 246, 246, 43, 298), "LAB_NAME"]

## Not sure what is column is used, and it is a mixture of numeric and strings.
raw.dt$itemGroup = NULL


## Manipulate the data before processing
mraw.df = melt(raw.dt, c("FAOST_CODE", "itemCode", "elementCode"))
mraw.df$type = ifelse(grepl("F", mraw.df$variable), "symb", "value")
mraw.df$variable = as.numeric(gsub("[^0-9]", "", mraw.df$variable))
cmraw.df = dcast(mraw.df, FAOST_CODE + itemCode + variable ~ type + elementCode)
colnames(cmraw.df) = c("FAOST_CODE", "itemCode", "Year",
        "symbArea", "symbProd", "valueArea", "valueProd")
cmraw.df$valueProd = as.numeric(cmraw.df$valueProd)
cmraw.df$valueArea = as.numeric(cmraw.df$valueArea)
cmraw.df$symbArea = gsub("[[:space:]]", "", cmraw.df$symbArea)
cmraw.df$symbProd = gsub("[[:space:]]", "", cmraw.df$symbProd)
cmraw.df$symbYield = with(cmraw.df, ifelse(symbArea == "" & symbProd == "",
    "Y", "N"))
cmraw.df[is.na(cmraw.df$symbYield), "symbYield"] = "N"


## Create the final data frame with country and regional codes and
## names and also the commodity group.
process.df = merge(cmraw.df, comGrp.dt, all.x = TRUE)

## These items does not have commodity group from the file given by Hans.
sort(unique(process.df[is.na(process.df$comGroup), "itemCode"]))
unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode %in%
                              sort(unique(process.df[is.na(process.df$comGroup),
                                                     "itemCode"])),
                              c("itemCode", "itemName")])

process.df = process.df[!is.na(process.df$comGroup), ]

    
## Merge with regional and subregional
process.df = merge(process.df, FAOcountryProfile[, c("FAOST_CODE", "LAB_NAME")],
  all.x = TRUE)
process.df = arrange(merge(process.df,
  FAOregionProfile[, c("FAOST_CODE", "UNSD_SUB_REG_CODE",
  "UNSD_MACRO_REG_CODE")], all.x = TRUE), FAOST_CODE, itemCode, Year)
## This is a hack for the region until Fillipo fixes the countryprofile.
process.df = merge(process.df,
  na.omit(unique(FAOregionProfile[, c("UNSD_MACRO_REG_CODE", "UNSD_MACRO_REG")])),
  all.x = TRUE)
process.df = merge(process.df,
  na.omit(unique(FAOregionProfile[, c("UNSD_SUB_REG_CODE", "UNSD_SUB_REG")])),
  all.x = TRUE)
process.df$UNSD_SUB_REG_CODE = NULL
process.df$UNSD_MACRO_REG_CODE = NULL

## These countries does not have region or subregion available.
FAOcountryProfile[FAOcountryProfile$FAOST_CODE %in%
                  unique(process.df[is.na(process.df$UNSD_MACRO_REG),
                                    "FAOST_CODE"]), "LAB_NAME"]

FAOcountryProfile[FAOcountryProfile$FAOST_CODE %in%
                  unique(process.df[is.na(process.df$UNSD_SUB_REG),
                                    "FAOST_CODE"]), "LAB_NAME"]


process.df = process.df[!is.na(process.df$UNSD_MACRO_REG) &
  !is.na(process.df$UNSD_SUB_REG),]

## Save the original value
process.df$ovalueArea = process.df$valueArea
process.df$ovalueProd = process.df$valueProd

## Replace values with symbol T or duplicated F with NA
process.df[which(process.df$symbArea == "T"), "valueArea"] = NA
process.df[which(process.df$symbProd == "T"), "valueProd"] = NA
process.df[which(duplicated(process.df[, c("FAOST_CODE", "itemCode", "symbArea",
                                 "valueArea")]) & process.df$symbArea == "F"),
         "valueArea"] = NA
process.df[which(duplicated(process.df[, c("FAOST_CODE", "itemCode", "symbProd",
                                 "valueProd")]) & process.df$symbProd == "F"),
         "valueProd"] = NA


## A function to compute yield to account for zeros in area and
## production. To account fo identification problem, we compute yield
## as NA if one of area or production is zero or NA.
computeYield = function(production, area){
  if(length(production) != length(area))
    stop("Length of prodduction is not the same as area")
  yield = ifelse(production != 0 & area != 0, production/area, NA)
}

process.df$valueYield = with(process.df, computeYield(valueProd, valueArea))
process.df$ovalueYield = with(process.df, computeYield(ovalueProd, ovalueArea))

## Removing blocks which contains only NA's or zero. 
process.dt = data.table(process.df)
tmp = process.dt[, sum(valueArea, valueProd, na.rm = TRUE),
  by = c("FAOST_CODE", "itemCode")]
tmp[, allMiss := V1 == 0 | is.na(V1)]
tmp$V1 = NULL
rmNA.dt = merge(process.dt, tmp, by = c("FAOST_CODE", "itemCode"))

## Fix conflicting error, it is impossible for harvested area to be
## zero when production is non-zero and vice versa. The solution is to
## replace the zero for these entries with NA and try to impute them.
rmNA.dt[valueArea == 0 & valueProd != 0, valueArea := as.numeric(NA)]
rmNA.dt[valueArea != 0 & valueProd == 0, valueProd := as.numeric(NA)]


## These item are removed if we specify FALSE for useNAAreaGrp, this
## is because they don't have area data on area.
rmNA.dt = rmNA.dt[(!comGroup %in% c("milk", "poultry_meat", "othr_meat"))]

## Function to check the sparsity of the data
checkSparsity = function(Data){
  image(data.matrix(is.na(Data)))
  text(rep(0.5, NCOL(Data)), seq(0, 1, length = NCOL(Data)),
       labels = paste(colnames(Data), " (", round(sapply(X = Data,
         FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))
}

## TODO (Michael): Set the keys for the table
## final.dt = data.table(rmNA.df[which(rmNA.df$includeGroup & rmNA.df$allMiss), ])
final.dt = rmNA.dt[c(!allMiss & includeGroup), ]
final.dt[, includeGroup := NULL]
final.dt[, allMiss := NULL]


diffv = function(x){
    T = length(x)
    ## Use linear interpolation for weight (this might not be suitable)
    x[x == 0] = NA
    if(sum(!is.na(x)) >= 2)
        ## TODO (Michael): test whether approx or na.approx is faster.
        x = na.approx(x)
    tmp = c(x[2:T]/x[1:(T - 1)])
    tmp
}

final.dt[, dvalueArea := c(NA, diffv(valueArea))]
final.dt[is.nan(dvalueArea), dvalueArea := NA]
final.dt[Year == 2000, dvalueArea := NA]
final.dt[, dvalueProd := c(NA, diffv(valueProd))]
final.dt[is.nan(dvalueProd), dvalueProd := NA]
final.dt[Year == 2000, dvalueProd := NA]
final.dt[, dvalueYield := c(NA, diffv(valueYield))]
final.dt[is.nan(dvalueYield), dvalueYield := NA]
final.dt[Year == 2000, dvalueYield := NA]




## ## Read and process the test data
## ## ---------------------------------------------------------------------
## raw.dt = data.table(read.csv("nicola_ws_request_12yrs.csv", header = TRUE,
##     stringsAsFactors = FALSE))
## setnames(raw.dt, old = c("ItemGroup", "AREA", "ITEM", "ELE"),
##          new = c("itemGroup", "FAOST_CODE", "itemCode", "elementCode"))

## ## Remove test area and excluded countries 
## raw.dt = subset(raw.dt, !(FAOST_CODE %in% c(351, 296, 357, 76, 245, 246, 246, 43,
##     298)))
## raw.dt$itemGroup = NULL

## mraw.df = melt(raw.dt, c("FAOST_CODE", "itemCode", "elementCode"))
## mraw.df$type = ifelse(grepl("F", mraw.df$variable), "symb", "value")


## mraw.df$variable = as.numeric(gsub("[^0-9]", "", mraw.df$variable))
## cmraw.df = dcast(mraw.df, FAOST_CODE + itemCode + variable ~ type + elementCode)
## colnames(cmraw.df) = c("FAOST_CODE", "itemCode", "year",
##         "symbArea", "symbProd", "valueArea", "valueProd")
## cmraw.df$valueProd = as.numeric(cmraw.df$valueProd)
## cmraw.df$valueArea = as.numeric(cmraw.df$valueArea)
## cmraw.df$symbArea = gsub("[[:space:]]", "", cmraw.df$symbArea)
## cmraw.df$symbProd = gsub("[[:space:]]", "", cmraw.df$symbProd)
## cmraw.df$valueYield = with(cmraw.df, valueProd/valueArea)
## cmraw.df$symbYield = with(cmraw.df, ifelse(symbArea == "" & symbProd == "",
##     "Y", "N"))
## cmraw.df[is.na(cmraw.df$symbYield), "symbYield"] = "N"

## final.df = merge(cmraw.df, FAOcountryProfile[, c("FAOST_CODE", "LAB_NAME")],
##   all.x = TRUE)
## final.df = arrange(merge(final.df,
##   FAOregionProfile[, c("FAOST_CODE", "UNSD_SUB_REG",
##   "UNSD_MACRO_REG")], all.x = TRUE), FAOST_CODE, itemCode, year)


## diffv = function(x){
##     T = length(x)
##     ## Use linear interpolation for weight (this might not be suitable)
##     x[x == 0] = NA
##     if(sum(!is.na(x)) >= 2)
##         ## TODO (Michael): test whether approx or na.approx is faster.
##         x = na.approx(x)
##     tmp = c(x[2:T]/x[1:(T - 1)])
##     tmp
## }


## final.dt = data.table(final.df)
## ## final.df$dvalueArea = c(NA, diff(log(final.df$valueArea)))
## ## final.dt[, dvalueArea := c(NA, diff(log(valueArea)))]
## final.dt[, dvalueArea := c(NA, diffv(valueArea))]
## final.dt[is.nan(dvalueArea), dvalueArea := NA]
## final.dt[year == 2000, dvalueArea := NA]
## ## final.df$dvalueProd = c(NA, diff(log(final.df$valueProd)))
## ## final.dt[, dvalueProd := c(NA, diff(log(valueProd)))]
## final.dt[, dvalueProd := c(NA, diffv(valueProd))]
## final.dt[is.nan(dvalueProd), dvalueProd := NA]
## final.dt[year == 2000, dvalueProd := NA]
## ## final.df$dvalueYield = c(NA, diff(log(final.df$valueYield)))
## ## final.dt[, dvalueYield := c(NA, diff(log(valueYield)))]
## final.dt[, dvalueYield := c(NA, diffv(valueYield))]
## final.dt[is.nan(dvalueYield), dvalueYield := NA]
## final.dt[year == 2000, dvalueYield := NA]


## final.dt[, varDvalueArea := var(dvalueArea, na.rm = TRUE),
##          by = c("FAOST_CODE", "itemCode")]
## final.dt[, varDvalueProd := var(dvalueProd, na.rm = TRUE),
##          by = c("FAOST_CODE", "itemCode")]
## final.dt[, varDvalueYield := var(dvalueYield, na.rm = TRUE),
##          by = c("FAOST_CODE", "itemCode")]
## final.dt[, meanProd := mean(valueProd, na.rm = TRUE),
##          by = c("FAOST_CODE", "itemCode")]


## test = unique(subset(final.dt, meanProd >= 300,
##   select = c("varDvalueArea", "varDvalueProd",
##                                  "varDvalueYield")))
## with(test, plot(varDvalueArea, varDvalueYield))

## with(final.dt, plot(dvalueArea, dvalueYield, col = rgb(0, 0, 0, alpha = 0.1),
##                     pch = 19))

## The plot suggests that the log-normal distribution may not be a
## good validation distribution since many of the fit does not appear
## satisfactory.
pdf(file = "yieldCheck.pdf")
par(mfrow = c(2, 1))
for(i in unique(final.dt$itemCode)){
  yieldRange = range(final.dt[Year %in% c(2000, 2010) & itemCode == i &
    symbYield == "Y", valueYield], na.rm = TRUE)
  try({hist(final.dt[Year == 2000 & itemCode == i & symbYield == "Y", valueYield],
            breaks = 50, freq = FALSE, xlim = yieldRange)
  param = fitdistr(na.omit(final.dt[Year == 2000 & itemCode == i, valueYield]),
    densfun = "lognormal")
  curve(dlnorm(x, meanlog = param$estimate[1], sdlog = param$estimate[2]),
        add = TRUE, col = "red")})  
  try({hist(final.dt[Year == 2010 & itemCode == i & symbYield == "Y", valueYield],
            breaks = 50, freq = FALSE, xlim = yieldRange)
  param = fitdistr(na.omit(final.dt[Year == 2010 & itemCode == i, valueYield]),
    densfun = "lognormal")
  curve(dlnorm(x, meanlog = param$estimate[1], sdlog = param$estimate[2]),
        add = TRUE, col = "red")})
}
graphics.off()
system("evince yieldCheck.pdf&")

final.dt[, yieldMin := min(final.dt[symbYield == "Y", valueYield], na.rm = TRUE),
         by = c("year","itemCode")]

final.dt[, yieldMax := min(final.dt[symbYield == "Y", valueYield], na.rm = TRUE),
         by = c("year","itemCode")]



final.dt[, lvalueArea := log(valueArea)]
final.dt[, lvalueProd := log(valueProd)]
final.dt[, lvalueYield := log(valueYield)]


plot.df = melt(final.dt[symbYield == "Y",
  list(FAOST_CODE, itemCode, year, lvalueArea, lvalueProd, lvalueYield)],
  id.var = c("FAOST_CODE", "itemCode", "year"))
colnames(plot.df) = c("FAOST_CODE", "itemCode", "Year", "type", "value")
plot.df$type = gsub("lvalue", "", plot.df$type)

pdf(file = "crossCountryCompare.pdf")
for(i in unique(plot.df$itemCode)){
try(print(ggplot(data = plot.df[plot.df$itemCode == i, ],
             aes(x = factor(Year), y = value)) +
      geom_boxplot() +
          facet_wrap(~type) + 
      labs(x = NULL,
           y = paste(unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode
             == i, "itemName"]), "(",
             unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == i,
                                           "itemCode"]), ")", sep = ""))))
}
graphics.off()
system("evince crossCountryCompare.pdf&")


pdf(file = "withinCountrycompare.pdf")
for(i in unique(plot.df$itemCode)){
  for(j in unique(plot.df$FAOST_CODE)){
    try(print(ggplot(data = plot.df[plot.df$itemCode == i &
                       plot.df$FAOST_CODE == j, ],
                     aes(x = Year, y = value)) +
              geom_line(aes(col = type)) + 
              labs(x = NULL,
           y = paste(unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode
             == i, "itemName"]), "(",
             unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == i,
                                                   "itemCode"]), ")", sep = ""),
              title = FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE == j),
                     "LAB_NAME"])))
  }
}
graphics.off()
system("evince withinCountrycompare.pdf&")

pdf(file = "changeDist.pdf")
for(i in unique(final.dt$itemCode)){
par(mfrow = c(3, 1))  
try({
  hist(final.dt[!is.na(dvalueArea) & (abs(dvalueArea) != Inf &
                     itemCode == i & symbArea == ""), dvalueArea],
            breaks = 500, xlim = c(-10, 20), freq = FALSE,
            xlab = "Distribution of change in area",
       main = paste(unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode
              == i, "itemName"]), "(",
         unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == i,
                                       "itemCode"]), ")", sep = ""));
       areaDist = fitdistr(final.dt[!is.na(dvalueArea) &
         (abs(dvalueArea) != Inf & itemCode == i & symbArea == ""), dvalueArea],
         densfun = "cauchy");
       curve(dcauchy(x, location = areaDist$estimate[1],
                     scale = areaDist$estimate[2]),
             add = TRUE, col = "red")
       abline(v = qcauchy(c(0.025, 0.975), location = areaDist$estimate[1],
                     scale = areaDist$estimate[2]))
  pos = quantile(final.dt[!is.na(dvalueArea) & (abs(dvalueArea) != Inf &
    itemCode == i & symbArea == ""), dvalueArea], probs = c(0.025, 0.5, 0.975),
    na.rm = TRUE)
  abline(v = c(pos[1] - (pos[2] - pos[1]), pos[3] + (pos[3] - pos[2])),
         lty = 2)
  ## m = mean(final.dt[!is.na(dvalueArea) & (abs(dvalueArea) != Inf &
  ##    itemCode == i & symbArea == ""), dvalueArea], na.rm = TRUE)
  ## sd = sqrt(var(final.dt[!is.na(dvalueArea) & (abs(dvalueArea) != Inf &
  ##    itemCode == i & symbArea == ""), dvalueArea], na.rm = TRUE))
  ## abline(v = c(m - 3 * sd, m + 3 * sd), lty = 3)
  ## legend("topleft", legend = c("Cauchy", "Quantile", "Std"), lty = 1:3,
  ##        bty = "n")
  abline(v = c(0.6, 1.4), lty = 3)
  legend("topleft", legend = c("Cauchy", "Quantile", "Old"), lty = 1:3,
         bty = "n")
  hist(final.dt[!is.na(dvalueProd) & (abs(dvalueProd) != Inf &
                     itemCode == i & symbProd == ""), dvalueProd],
            breaks = 500, xlim = c(-10, 20), freq = FALSE,
            xlab = "Distribution of change in Production", main = NULL);
       areaDist = fitdistr(final.dt[!is.na(dvalueProd) &
         (abs(dvalueProd) != Inf & itemCode == i & symbProd == ""), dvalueProd],
         densfun = "cauchy");
       curve(dcauchy(x, location = areaDist$estimate[1],
                     scale = areaDist$estimate[2]),
             add = TRUE, col = "red")
       abline(v = qcauchy(c(0.025, 0.975), location = areaDist$estimate[1],
                     scale = areaDist$estimate[2]))
  pos = quantile(final.dt[!is.na(dvalueProd) & (abs(dvalueProd) != Inf &
    itemCode == i & symbProd == ""), dvalueProd], probs = c(0.025, 0.5, 0.975),
    na.rm = TRUE)
  abline(v = c(pos[1] - (pos[2] - pos[1]), pos[3] + (pos[3] - pos[2])),
         lty = 2)
  ## m = mean(final.dt[!is.na(dvalueProd) & (abs(dvalueProd) != Inf &
  ##    itemCode == i & symbProd == ""), dvalueProd], na.rm = TRUE)
  ## sd = sqrt(var(final.dt[!is.na(dvalueProd) & (abs(dvalueProd) != Inf &
  ##    itemCode == i & symbProd == ""), dvalueProd], na.rm = TRUE))
  ## abline(v = c(m - 3 * sd, m + 3 * sd), lty = 3)
  abline(v = c(0.6, 1.4), lty = 3)  
    hist(final.dt[!is.na(dvalueYield) & (abs(dvalueYield) != Inf &
                     itemCode == i & symbYield == "Y"), dvalueYield],
            breaks = 500, xlim = c(-10, 20), freq = FALSE,
            xlab = "Distribution of change in yield", main = NULL);
       yieldDist = fitdistr(final.dt[!is.na(dvalueYield) &
         (abs(dvalueYield) != Inf & itemCode == i & symbYield == "Y"),
         dvalueYield], densfun = "cauchy");
       curve(dcauchy(x, location = yieldDist$estimate[1],
                     scale = yieldDist$estimate[2]),
             add = TRUE, col = "red")
       abline(v = qcauchy(c(0.025, 0.975), location = yieldDist$estimate[1],
                     scale = yieldDist$estimate[2]))
  pos = quantile(final.dt[!is.na(dvalueArea) & (abs(dvalueArea) != Inf &
    itemCode == i & symbArea == ""), dvalueArea], probs = c(0.025, 0.5, 0.975),
    na.rm = TRUE)
  abline(v = c(pos[1] - (pos[2] - pos[1]), pos[3] + (pos[3] - pos[2])),
         lty = 2)
  ## m = mean(final.dt[!is.na(dvalueYield) & (abs(dvalueYield) != Inf &
  ##    itemCode == i & symbYield == "Y"), dvalueYield], na.rm = TRUE)
  ## sd = sqrt(var(final.dt[!is.na(dvalueYield) & (abs(dvalueYield) != Inf &
  ##    itemCode == i & symbYield == "Y"), dvalueYield], na.rm = TRUE))
  ## abline(v = c(m - 3 * sd, m + 3 * sd), lty = 3)
  abline(v = c(0.6, 1.4), lty = 3)
     })
}
graphics.off()
system("evince changeDist.pdf&") 



## official.dt = data.table(subset(final.df, symbYield == "Y"))

## official.dt$valueLogArea = log(official.dt$valueArea)
## official.dt$valueLogProd = log(official.dt$valueProd)
## official.dt$valueLogYield = log(official.dt$valueYield)
## official.dt[, varlArea := var(valueLogArea, na.rm = TRUE),
##          by = c("FAOST_CODE", "itemCode")]
## official.dt[, varlProd := var(valueLogProd, na.rm = TRUE),
##          by = c("FAOST_CODE", "itemCode")]
## official.dt[, varlYield := var(valueLogYield, na.rm = TRUE),
##          by = c("FAOST_CODE", "itemCode")]




pdf(file = "yieldTimeCheck.pdf", width = 31, height = 21)
for(i in unique(final.dt$itemCode)){
  try({tsPanel(Data = final.dt[final.dt$itemCode == i &
                 final.dt$symbYield == "Y", ],
          country = "LAB_NAME", year = "year", var = "valueYield",
          facetScales = "fixed",
          ylab = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
            i , "itemName"]), title = "Yield by country over time", ncol = 15)})
}
graphics.off()
system("evince yieldTimeCheck.pdf &")

pdf(file = "yieldTimeRegCheck.pdf", width = 15, height = 10)
for(i in unique(final.dt$itemCode)){
  try({print(ggplot(data = final.dt[final.dt$itemCode == i &
                      final.dt$symbYield == "Y", ],
                    aes(x = year, y = valueYield)) +
             geom_line(aes(col = factor(FAOST_CODE))) +
             facet_wrap(~UNSD_MACRO_REG, ncol = 3) +
             theme(legend.position = "none") +
             labs(x = "year",
             y = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
                    i , "itemName"])) +
             labs(title = "Yield by region over time") + geom_smooth(lwd = 5))})
}
graphics.off()
system("evince yieldTimeRegCheck.pdf &")


pdf(file = "yieldAreaTimeRegCheck.pdf", width = 15, height = 10)
for(i in unique(final.dt$itemCode)){
  try({print(ggplot(data = final.dt[final.dt$itemCode == i &
                      final.dt$symbYield == "Y", ],
                    aes(x = year, y = valueYield)) +
             geom_line(aes(col = factor(FAOST_CODE))) +
             facet_wrap(~UNSD_MACRO_REG, ncol = 3) +
             theme(legend.position = "none") +
             labs(x = "year",
             y = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
                    i , "itemName"])) +
             labs(title = "Yield by region over time") +
             geom_smooth(lwd = 2, linetype = "dotted"));
     print(ggplot(data = final.dt[final.dt$itemCode == i &
                      final.dt$symbYield == "Y", ],
                    aes(x = year, y = valueArea)) +
             geom_line(aes(col = factor(FAOST_CODE))) +
             facet_wrap(~UNSD_MACRO_REG, ncol = 3) +
             theme(legend.position = "none") +
             labs(x = "year",
             y = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
                    i , "itemName"])) +
             labs(title = "Area by region over time") +
           geom_smooth(lwd = 2, linetype = "dotted"))})
}
graphics.off()
system("evince yieldAreaTimeRegCheck.pdf &")



pdf(file = "yieldAreaLogTimeRegCheck.pdf", width = 15, height = 10)
for(i in unique(final.dt$itemCode)){
  try({print(ggplot(data = final.dt[final.dt$itemCode == i &
                      final.dt$symbYield == "Y", ],
                    aes(x = year, y = valueLogYield)) +
             geom_line(aes(col = factor(FAOST_CODE))) +
             facet_wrap(~UNSD_MACRO_REG, ncol = 3) +
             theme(legend.position = "none") +
             labs(x = "year",
             y = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
                    i , "itemName"])) +
             labs(title = "Yield by region over time") +
             geom_smooth(lwd = 2, linetype = "dotted"));
     print(ggplot(data = final.dt[final.dt$itemCode == i &
                      final.dt$symbYield == "Y", ],
                    aes(x = year, y = valueLogArea)) +
             geom_line(aes(col = factor(FAOST_CODE))) +
             facet_wrap(~UNSD_MACRO_REG, ncol = 3) +
             theme(legend.position = "none") +
             labs(x = "year",
             y = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
                    i , "itemName"])) +
             labs(title = "Area by region over time") +
           geom_smooth(lwd = 2, linetype = "dotted"))})
}
graphics.off()
system("evince yieldAreaLogTimeRegCheck.pdf &")


## Seems like the variance of the production can be driven by
## different processes, need to elaborate on this.
with(official.dt,
     plot(varlArea, varlYield, col = rgb(official.dt$varlProd, 0, 0,
       maxColorValue = max(official.dt$varlProd, na.rm = TRUE)), pch = 19))


with(official.dt, plot(varlArea, varlYield, xlim = c(0, 15), ylim = c(0, 15)))

## Prob need to calculate the variance of the diff(log(x))


covNA = function(x, y){
  if(all(is.na(x + y))){
    tmp = NA
  } else {
    tmp = cov(x, y, use = "complete.obs")
  }
  as.numeric(tmp)
}

final.dt[, covlAreaYield := covNA(valueLogArea, valueLogYield),
         by = c("FAOST_CODE", "itemCode")]
final.dt[, estVarlProd := varlArea + varlYield - covlAreaYield]

with(final.dt, plot(varlProd, estVarlProd, xlim = c(0, 20), ylim = c(0, 20)))
abline(a = 0, b = 1, col = "red")

## Looks like the estimated variance of production is smaller than the
## estimate based on the variance of production.




## final.df = arrange(final.df, FAOST_CODE, itemCode, year)

## final.df$dvalueArea = c(NA, diff(final.df$valueArea))
## final.df$dvalueProd = c(NA, diff(final.df$valueProd))
## final.df$dvalueYield = c(NA, diff(final.df$valueYield))
## final.df[final.df$year == 2000, c("dvalueArea", "dvalueProd", "dvalueYield")] = NA






## Test codes
## ---------------------------------------------------------------------

## par(mfrow = c(2, 1))
## hist(subset(cmimp.df, itemCode == 15 & year == 2000 & symbArea == " " & symbProd == " ", select = valueYield, drop = TRUE), breaks = 100)
## hist(subset(cmimp.df, itemCode == 15 & year == 2000 & (symbArea != " " | symbProd != " "), select = valueYield, drop = TRUE), breaks = 100)


## pdf(file = "~/Desktop/yield.pdf")
## for(i in unique(cmimp.df$itemCode)){
##     tmp = subset(cmimp.df, itemCode == i)
##     try({print(ggplot(data = tmp, aes(x = factor(year), y = valueYield)) +
##                geom_violin() +
##                facet_wrap(~official, ncol = 1) +
##                labs(x = NULL, y = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == as.character(i), "itemName"]), title = "Distribution of official vs non-official computed yield over time")
##                )})
## }
## graphics.off()
## system("open ~/Desktop/yield.pdf")


