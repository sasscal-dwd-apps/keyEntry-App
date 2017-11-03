makeRules <- function(formElements, wb, temp, formSheets, row.days){
  id <- which(!is.na(formElements$rule))
  #print(head(id))
  if (length(id)>0){
    for (i0 in 1:length(id)){
      #local({
      my_i0 <- i0
      ruleElement <- formElements$element_abbr[id[my_i0]]
      # Read worksheet for the rule
      formRule <- readWorksheet(wb, sheet = grep(paste0("rule", my_i0), 
                                                 formSheets))
      ruleFormula <- formRule$formula[!is.na(formRule$rule)]
      if (ruleFormula == "mean"){
        temp[row.days, ruleElement] <- ruleMean(temp, formRule, 
                                                row.days)
      }
      if (ruleFormula == "diff"){
        temp[row.days, ruleElement] <- ruleDiff(temp, formRule, 
                                                row.days)
      }
      if (ruleFormula == "between"){
        temp[row.days, ruleElement] <- ruleBtwn(temp, formRule, 
                                                row.days)
      }
      #})
    }
  }
  
  #####################################################################
  # CALCULATE MAX, MIN, MEAN & SUM
  row.pos <- which(row.names(temp) %in% row.days == T)
  temp <- maxValues(temp, row.pos)
  temp <- minValues(temp, row.pos)
  temp <- meanValues(temp, row.pos)
  temp <- sumValues(temp, row.pos)

  return(temp)
}
