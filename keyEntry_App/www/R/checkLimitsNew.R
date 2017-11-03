checkLimitsNew <- function(input, output, session, uiCheckLimits, uiMessage, tableIds, df){
  observeEvent(input[[uiCheckLimits]],{
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
  
  output[[tableIds[1]]] <- renderRHandsontable({
    rhandsontable(df, useTypes = TRUE, 
                  row_highlight = row.highlight) %>% 
      hot_cols(colWidths = 55) %>%
      hot_cols(halign = "htCenter") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_col(
        id, readOnly = TRUE,
        renderer = paste0(
          "function(instance, td, row, col, prop, value, cellProperties) {",
          "Handsontable.TextCell.renderer.apply(this, arguments);
  }")
      ) %>%
      hot_cols(
        renderer = renderer
      )
  })
  })
  }