#'
#' Make a table 1
#'


table1_maker <- function(.dat, .dict = data_dict, ...){
  
  #Build the formula for table1
  .table_1_form <- paste(.dict[["table1"]][["entries"]], collapse = " + ")
  .table_1_form <- as.formula(paste("~", .table_1_form, "|", .dict[["table1"]][["split"]]))
  
  #Relevel case control status for aesthetics
  .dat[[eval_dict$case_var]] <- relevel(.dat[[eval_dict$case_var]], eval_dict$case)
   
  #Evaluate the table using table1 package
  .table_out <- table1::table1(.table_1_form, data = .dat, render.categorical="FREQ (PCTnoNA%)", ...)
  
  return(.table_out)
  
}




