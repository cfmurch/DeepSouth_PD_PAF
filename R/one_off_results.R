#'
#' Script to handle the small handful of results in the second paragraph of Subject Characteristics
#'



one_off_maker <- function(.dat_orig, .dict = eval_dict){
  
  #Call data by reference
  .dat <- data.table::copy(.dat_orig)
  
  #Just step through the covariates
  .one_off_out <- lapply(.dict[["one_off"]][["vars"]], function(.var){
    
   #Build the dataset to be used by pairwiseCI, need to coerce to success and failures
   .dat <- .dat[!is.na(.dat[[.dict[["case_var"]]]]) & !is.na(.dat[[.var]]),]
   .table <- table(.dat[[.dict[["case_var"]]]], .dat[[.var]])
    
   #Get marginal proportions
   .margin_prop <- prop.table(.table, margin = 1)[,colnames(.table)==.dict[["one_off"]][["or_succ"]][which(.dict[["one_off"]][["vars"]] == .var)]]
    
   #Run Prop.or and a chisq.test
   .or_curr <- pairwiseCI::Prop.or(x = .table[1,], y = .table[2,], alternative = "two.sided", CImethod = "Woolf")
   .chisq_curr <- chisq.test(.table)
    
   #Validate on two proportion prop.test
   .prop_curr <- prop.test(x = .table[,colnames(.table)==.dict[["one_off"]][["or_succ"]][which(.dict[["one_off"]][["vars"]] == .var)]],
                            n = table(.dat[[.dict[["case_var"]]]]))
    
   #Building the output string
   .out <- c(.var,
             paste0(sprintf("%.2f", .margin_prop*100), "%"), 
             paste0(sprintf("%.3f", .or_curr$estimate), " [", sprintf("%.3f", .or_curr$conf.int[1]), " - ",  sprintf("%.3f", .or_curr$conf.int[2]), "]"),
             paste0("P=", formatC(.chisq_curr$p.value, digits = 2, format = "e")))
   
   names(.out) <- c("Variable", "Prop Ctrl", "Prop PD", "OR [95%CI]", "Pval")
   
   return(.out)
  })
  
  .one_off_out <- do.call(rbind, .one_off_out)
  return(.one_off_out)
  
  
}






