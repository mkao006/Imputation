##' A function to plot the individual imputation result by country and
##' save the plot as pdf.
##'
##' @param object The Fitted object from \code{swsImputation}
##' @param productionObsVar The column name of the observed production.
##' @param areaObsVar The column name of the observed area.
##' @param yieldObsVar The column name of the observed yield.
##' @param itemCode The column name of the item code.
##' @param file Optional name for the pdf.
##'
##' @seealso \code{impDiag}
##' @export


impFit = function(object, production, area, yield, country, item,
    file){
    ## Check all individual imputation
    imputed.dt = object$imputed
    if(missing(file))
        file = "imputatoinFit.pdf"
    pdf(file = file, width = 10)
    by(data = imputed.dt,
       INDICES = imputed.dt[, country, with = FALSE],
       FUN = function(x){
           myCountryCode = unlist(unique(x[, country, with = FALSE]))
           myItemCode = unlist(unique(x[, item, with = FALSE]))
           myCountry =
               FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE == myCountryCode), "FAO_TABLE_NAME"]
           myItem =
               with(FAOmetaTable, unique(itemTable[itemTable$itemCode ==
                                                   myItemCode, "itemName"]))
           par(mfrow = c(4, 1), mar = c(2.1, 4.1, 3.1, 2.1))
           try({
               ymax = max(x[, c(production, "imputedProd"), with = FALSE],
                   na.rm = TRUE) * 1.2
               with(x, plot(Year, eval(parse(text = production)),
                            ylim = c(0, ymax), type = "b",
                            col = "black", xlab = "", ylab = "Production",
                            main = paste0(myCountry, " (", myCountryCode, ") - ",
                                myItem, " (", myItemCode, ")"),
                            cex = 2))
               with(x[is.na(eval(parse(text = production))), ],
                    points(Year, imputedProd, col = "blue", pch = 19))
           }, silent = TRUE)
           
           try({
               ymax = max(x[, c(area, "imputedArea"), with = FALSE],
                   na.rm = TRUE) * 1.2  
               with(x, plot(Year, eval(parse(text = area)),
                            ylim = c(0, ymax), type = "b",
                            col = "black", xlab = "", ylab = "Area",
                            cex = 2))
               with(x[is.na(eval(parse(text = area))),],
                    points(Year, imputedArea, col = "blue", pch = 19))
           }, silent = TRUE)
           
           
           try({
               ymax = max(x[, c(yield, "imputedYield", "groupedMean"),
                   with = FALSE], na.rm = TRUE) * 1.2
               with(x, plot(Year, eval(parse(text = yield)),
                            ylim = c(0, ymax), type = "b",
                            col = "black", xlab = "", ylab = "Yield",
                            cex = 2))
               with(x[!is.na(eval(parse(text = yield))), ],
                    points(Year, fittedYield, col = "red", pch = 19))
               with(x[is.na(eval(parse(text = yield))), ],
                    points(Year, imputedYield, col = "blue", pch = 19))
           }, silent = TRUE)
           try({
               with(x, plot(Year, groupedMean, ylab = "Average Yield",
                            ylim = c(0, ymax), type = "b"))
           }, silent = TRUE)
           
       }
       )
    graphics.off()
    cat("plots saved as '", file, "'\n")
}
