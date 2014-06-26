computeYield = function(production, areaHarvested){
    ifelse(production == 0 | areaHarvested == 0, NA, production/areaHarvested)
}
