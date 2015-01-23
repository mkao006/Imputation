##' Plot Ensemble and Model Fits
##' 
##' This function plots each of the individual models in the ensemble as well
##' as the initial data and the final ensemble.  This function is not meant to
##' be called directly by the user but instead is a helper function called by
##' ensembleImpute (if the argument plot = TRUE).
##' 
##' @param data The data.table containing the data being imputed.
##' @param modelFits A list of length equal to the number of models.  Each
##' element of the list should be a numeric vector of length nrow(data). This
##' object is usually created by computeEnsembleFit.
##' @param modelWeights A matrix of dimension nrow(data) x length(modelFits).
##' Each element corresponds to the weight for model/column j and for 
##' observation/row i.  Note: modelFits and modelWeights need not have the same
##' ordering, but they should have the same names as this is how they are
##' matched.
##' @param ensemble A numeric vector containing either the original data (if it
##' was not missing) or the imputed value (if the original data was missing).
##' @param returnFormat In what format should the plots be returned?  If "faceted", a
##' single plot showing with all countries is returned.  If "individual", a
##' list with ggplot objects is returned.  If "prompt", the first country's
##' plot is displayed and then the user must press ENTER to cycle through the
##' plots.
##' 
##' @return If returnType = "faceted" or "prompt", then no value is returned
##' (but a plot is generated).  However, if returnType = prompt, then a list of
##' ggplot objects is returned.
##' 

plotEnsemble = function(data, modelFits, modelWeights, ensemble,
                        returnFormat = "faceted"){
    
    ### Data Quality Checks
    stopifnot(is(data, "data.table"))
    stopifnot(is(modelFits, "list"))
    stopifnot(is(modelWeights, "data.table"))
    stopifnot(length(modelFits) == ncol(modelWeights))
    stopifnot(all(nrow(data) == sapply(modelFits, length)))
    stopifnot(nrow(data) == nrow(modelWeights))
    stopifnot(names(modelWeights) %in% names(modelFits))
    stopifnot(names(modelFits) %in% names(modelWeights))
    stopifnot(nrow(data) == length(ensemble))
    stopifnot(returnFormat %in% c("faceted", "individual", "prompt"))
    
    ### Set up toPlot data.table (holds data for ggplot call)
    plotKeys = data[, any(is.na(get(imputationValueColumn))), by = byKey]
    plotKeys = plotKeys[(V1), get(byKey)]
    filter = data[, get(byKey) %in% plotKeys]
    toPlot = data[filter, ]
    toPlot$ensemble = ensemble[filter]
    setnames(toPlot, old = c(yearValue, byKey, imputationValueColumn),
             new = c("year", "byKey", "imputationValueColumn"))
    
    ### Set up toPlotModels data.table
    toPlotModels = lapply(modelFits, function(x) x[filter])
    toPlotModels = data.table(do.call("cbind", toPlotModels))
    modelNames = colnames(toPlotModels)
    nModels = length(modelNames)
    toPlotModels$year = toPlot$year
    toPlotModels$byKey = toPlot$byKey
    toPlotModels = data.table:::melt.data.table(data = toPlotModels,
                    id.vars = c("year", "byKey"))
    setnames(toPlotModels, "value", "modelFit")
    
    ### Set up toPlotWeights data.table and append to toPlotModels
    toPlotWeights = modelWeights[filter, ]
    # If all weights are 0 (i.e. no imputation) set all weights to
    # a really small value so the plot shows only a line
    toPlotWeights[apply(toPlotWeights, 1, sum, na.rm = T) == 0,
                  colnames(toPlotWeights) := 1e-8]
    toPlotWeights$year = toPlot$year
    toPlotWeights$byKey = toPlot$byKey
    toPlotWeights = data.table:::melt.data.table(data = toPlotWeights,
                    id.vars = c("year", "byKey"))
    setnames(toPlotWeights, "value", "modelWeight")
    toPlotModels = merge(toPlotModels, toPlotWeights,
                         by = c("year", "byKey", "variable"))
    
    ### Set plot colors
    gg_color_hue = function(n){
        hues = seq(15, 375, length=n+1)
        hcl(h=hues, l=65, c=100)[1:n]
    }
    if(nModels <= 9){
        plotColors = brewer.pal(nModels, "Set1")
    } else if(nModels <= 12){
        plotColors = brewer.pal(nModels, "Paired")
    } else {
        plotColors = gg_color_hue(nModels)
    }
    
    ### Plotting call
    toPlotModels[, maxY := max(modelFit, na.rm = TRUE), by = "byKey"]
    toPlotModels[, ribbonWidth := maxY * modelWeight * .03]
    if(returnFormat == "faceted"){
        print(ggplot2::ggplot(toPlotModels,
                        # year - .5 to center the lines on each year
                        ggplot2::aes(x = year, color = variable,
                                     shape = variable)) +
            ggplot2::geom_ribbon(ggplot2::aes(ymax = modelFit + ribbonWidth,
                                              ymin = modelFit - ribbonWidth,
                                              shape = "none",
                                              fill = variable)) +
            ggplot2::geom_point(data = toPlot,
                ggplot2::aes(x = year, y = ensemble,
                             color = ifelse(is.na(imputationValueColumn),
                                            "Ensemble", "Data"),
                             shape = ifelse(is.na(imputationValueColumn),
                                            "Ensemble", "Data"),
                             fill = ifelse(is.na(imputationValueColumn),
                                            "Ensemble", "Data"))) +
            ggplot2::facet_wrap( ~ byKey, scale = "free") +
            ggplot2::scale_size_continuous(range = c(.5, 2)) +
            ggplot2::scale_color_manual( 
                values = c("black", "black", plotColors),
                limits = c("Data", "Ensemble", modelNames)) +
            ggplot2::scale_fill_manual(
                values = c(NA, NA, plotColors),
                limits = c("Data", "Ensemble", modelNames)) +
            ggplot2::labs(x = "Year", y = imputationValueColumn,
                          size = "Model Weight", color = "", shape = "",
                          fill = "") +
            ggplot2::scale_shape_manual(values = c(16, 4, rep(NA, nModels)),
                                        limits=c("Data", "Ensemble",
                                                 modelNames)) +
            ggplot2::expand_limits(y = 0)
        )
        return()
    } else {
        plotList = lapply(unique(toPlot$byKey), function(i){
            ggplot2::ggplot(toPlotModels[byKey == i,],
                            # year - .5 to center the lines on each year
                            ggplot2::aes(x = year, color = variable,
                                         shape = variable)) +
                ggplot2::geom_ribbon(ggplot2::aes(ymax = modelFit + ribbonWidth,
                                                  ymin = modelFit - ribbonWidth,
                                                  shape = "none",
                                                  fill = variable)) +
                ggplot2::geom_point(data = toPlot[byKey == i,],
                    ggplot2::aes(x = year, y = ensemble,
                                 color = ifelse(is.na(imputationValueColumn),
                                                "Ensemble", "Data"),
                                 shape = ifelse(is.na(imputationValueColumn),
                                                "Ensemble", "Data"),
                                 fill = ifelse(is.na(imputationValueColumn),
                                                "Ensemble", "Data"))) +
                ggplot2::facet_wrap( ~ byKey, scale = "free") +
                ggplot2::scale_size_continuous(range = c(.5, 2)) +
                ggplot2::scale_color_manual( 
                    values = c("black", "black", plotColors),
                    limits = c("Data", "Ensemble", modelNames)) +
                ggplot2::scale_fill_manual(
                    values = c(NA, NA, plotColors),
                    limits = c("Data", "Ensemble", modelNames)) +
                ggplot2::labs(x = "Year", y = imputationValueColumn,
                              size = "Model Weight", color = "", shape = "",
                              fill = "") +
                ggplot2::scale_shape_manual(values = c(16, 4, rep(NA, nModels)),
                                            limits=c("Data", "Ensemble",
                                                     modelNames)) +
                ggplot2::expand_limits(y = 0)
        })
    }
    if(returnFormat == "individual")
        return(plotList)
    if(returnFormat == "prompt")
        for(i in 1:length(plotList)){
            print(plotList[[i]])
            readline("Next?")
        }
}