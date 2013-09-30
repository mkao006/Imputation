########################################################################
## Title: Script to run out-of-sample simulation for wheat
## Date: 2013-06-12
########################################################################

source("wheatDataManipulation.R")


## Simulation
## ---------------------------------------------------------------------

n.sim = 100
sim.df = data.frame(propSim = runif(n.sim, 0.05, 1 - 1e-05),
    propReal = rep(NA, n.sim), MAPE = rep(NA, n.sim),
    method = rep(NA, n.sim))

## pdf(file = "checkImputation.pdf")
for(i in 1:n.sim){
  print(paste0("Simulation Number: ", i))
  tmp.dt = wheatPrep.dt
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
  impSim.dt = try(FAOProductionImpute(Data = tmp.dt, area = "simArea",
    prod = "simProd", yield = "simYield", country = "FAOST_CODE",
    region = "UNSD_SUB_REG", year = "Year"))
  if(!inherits(impSim.dt, "try-error")){
    sim.df[i, "method"] = unique(impSim.dt[, yieldMethodology])
    ## Check the MAPE of the imputation
    sim.df[i, "MAPE"] = 
      impSim.dt[1:nrow(impSim.dt) %in% simMissProd & ovalueProd != 0 &
                !is.na(imputedProd) & !is.na(imputedProd),
                sum(abs((ovalueProd - imputedProd)/(ovalueProd)))/
                length(imputedProd)]
  } else {
    sim.df[i, c("MAPE", "method")] = NA
  }
}

summary(sim.df)
subSim.df = sim.df[sim.df$MAPE <= 1e100, ]

with(subSim.df[subSim.df$method == "lmeMeanImp", ],
     plot(propReal, MAPE, xlim = c(0, 1), ylim = c(range(subSim.df$MAPE,
                                            na.rm = TRUE))))
with(subSim.df[subSim.df$method != "lmeMeanImp", ],
     points(propReal, MAPE, col = "red"))

pdf(file = "wheatSimulationResult.pdf", width = 10)
with(subSim.df[subSim.df$MAPE < 1.0, ], plot(propReal, MAPE, xlim = c(0, 1),
          ylim = c(range(MAPE, na.rm = TRUE)),
     xlab = "Missing proportion in yield",
     ylab = "Mean absolute percentage error"))
with(subSim.df[subSim.df$MAPE < 1.0 & !is.na(subSim.df$MAPE), ],
     lines(lowess(propReal, MAPE, f = 0.2), col = "red", lwd = 2), xlim = c(0, 1),
           ylim = c(range(MAPE, na.rm = TRUE)))
abline(h = seq(0, 1, by = 0.05), lty = 2, col = "grey50")
graphics.off()
system("evince wheatSimulationResult.pdf&")


pdf(file = "checkWheatSim.pdf", width = 10)
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
system("evince checkWheatSim.pdf&")
