makeLimits <- function(tmp.df2, df2, row.days, rowCondition, checkElementsCondition, varName){
  if (exists("tmp.df2") & !is.null(tmp.df2)){
    minValue <- tmp.df2$minValue
    maxValue <- tmp.df2$maxValue
    scaleFactor <- tmp.df2$scale_factor
  }else{
    maxValue <- df2$maxValue
    minValue <- df2$minValue
    scaleFactor <- df22$scale_factor
  }
  checkLimitsCondition <<- checkLimits(varName, minValue, maxValue, row.days)
  
  if (length(checkElementsCondition) == 0){
    renderer <- paste0(
      "function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.TextCell.renderer.apply(this, arguments);
      if (", rowCondition,") {
      cellProperties.readOnly = true;
      }else if (", checkLimitsCondition,") {
      td.style.background = 'red';
      }
  }")
    }else{
      renderer <- paste0(
        "function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.TextCell.renderer.apply(this, arguments);
        if (", rowCondition,") {
        cellProperties.readOnly = true;
        }else if (", checkLimitsCondition,") {
        td.style.background = 'red';
        }else if (", checkElementsCondition,"){
        td.style.background = '#ff9999';
        }
}")
    }
  return(renderer)
  }
