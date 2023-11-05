#'
#' Wrapper for the unadjusted OR data in talbe 3
#'
#'




unadj_eval <- function(.dat_orig, .dict = eval_dict){
  
  #Pull table by reference
  .dat <- data.table::copy(.dat_orig)
  
  #Get the case, no need to iterate
  .case <- .dict[["case_var"]]
  .case_succ <- .dict[["case"]]
  
  #Step through the strata indices
  eval_list <- lapply(seq_along(.dict[["strata"]]), function(.idx_strata){
    
    #Strata variable name
    .strata_var <- names(.dict[["strata"]])[[.idx_strata]]
    
    #Iterate through strata levels
    eval_strata <- lapply(.dict[["strata"]][[.strata_var]], function(.stratum){
      
      #Subset data if needed
      if(.stratum != "All") .dat <- .dat[.dat[[.strata_var]] == .stratum,]
      
      #Work through each variables
      eval_var <- lapply(seq_along(.dict[["vars"]]), function(.idx){
        
        #Pull variable name
        .var <- .dict[["vars"]][[.idx]]
        
        #Removing missing rows for convenience
        .dat <- .dat[!is.na(.dat[[.case]]) & !is.na(.dat[[.var]]), ]
        
        #Get exposure counts
        .tab_count <- table(.dat[[.case]])
        
        #GLM based OR
        .formula <- paste0("`", .case, "` ~ `", .var, "`")
        .glm_curr <- tryCatch(glm(.formula, data = .dat, family = binomial(link = "logit")), error = function(e) NULL)
         
        
        #For a valid GLM
        if(!is.null(.glm_curr)){
        
          #1 - coerce the summary output and identify covariate row
          .glm_out <- as.data.frame(summary(.glm_curr)[["coefficients"]])
          .row_idx <- grep(.var, rownames(.glm_out))
          
          #1b - Check if a Firth adjustment should be used instead
          if(max(.glm_out$`Std. Error`) > 100){
            #Set alpha to 0.1 to get the single-sided 95% confidence interval for the penalized profile ML
            .glm_curr <- logistf::logistf(.formula, data = .dat, alpha = 0.1)
            #We need to build out the dataframe
            .glm_out <- data.frame(Estimate = .glm_curr$coefficients, 
                                   `Std. Error`= sqrt(diag(.glm_curr$var)),
                                   `z value` = sqrt(qchisq(1-.glm_curr$prob, df=1)),
                                   `Pr(>|z|)` = .glm_curr$prob*2,
                                   check.names = FALSE)
            #We also extract the CI directly from the model object which has already been set to a single-sided 95% LB
            .ci_lb <- .glm_curr$ci.lower
            .use_firth <- TRUE
          }
        
          #2 - Calculate one-sided p-value
          .test_stat <- .glm_out$`z value`[.row_idx]
          .pval <- pnorm(.test_stat, lower.tail = FALSE)
          .pval <- formatC(.pval, digits = 2, format = "e")#signif(.pval, 2)
          
          #3 - Extract OR and SE
          .or <- .glm_out$Estimate[.row_idx]
          .se <- .glm_out$`Std. Error`[.row_idx]
        
          #4 - Get the LB CI shift
          .lb_shift <- .se*qnorm(0.95)
        
          #5 - Build the OR [CI] string; this is dependent on whether a Firth model was used
          if(exists(".use_firth")){
            .or_out <- paste0(sprintf("%.3f", exp(.or)), " [", sprintf("%.3f", exp(.ci_lb[.row_idx])), "] *")
            rm(.use_firth)
          } else 
          .or_out <- paste0(sprintf("%.3f", exp(.or)), " [", sprintf("%.3f", exp(.or - .lb_shift)), "]")
          
          
          #Branch depending on whether it's categorical or not
          if(!is.na(.dict[["or_succ"]][[.idx]])){ 
            #6a - Count based summary statistic
            .summ_stat <- .dat[, count_prop(.SD[[.var]]), by = .case]
            
          #Otherwise the covariate is continuous, no OR
          } else{
            #6b - Continuous based summary statistic
            .summ_stat <- .dat[, mean_sd(.SD[[.var]]), by = .case]
          }
          
        #For non-valid GLM
        } else{
          #Appropriate summary statistic
          if(!is.na(.dict[["or_succ"]][[.idx]])){ .summ_stat <- .dat[, count_prop(.SD[[.var]]), by = .case]
          } else .summ_stat <- .dat[, mean_sd(.SD[[.var]]), by = .case]
          
          #Filler GLM string
          .or_out <- .pval <- "-"
        }
        
          
        #Modelling is done, build the the vector for printing
        .out <- c(rev(.tab_count), .summ_stat$V1, .or_out, .pval)
        names(.out) <- c(names(rev(.tab_count)), as.character(.summ_stat[[.case]]), "OR [LB CI]", "Pval")
          
        return(.out)
      })
      
      
      #Rbind the list
      eval_out <- do.call(rbind, eval_var)
      eval_out <- data.frame(Var = .dict[["vars"]], eval_out, check.names = FALSE)
      
      return(eval_out)
      
    })
    
    #Name the individual strata and return
    names(eval_strata) <- paste0(.strata_var, " - ", .dict[["strata"]][[.strata_var]])
    return(eval_strata)
  })
  
  #Peel off the outer list layer
  eval_list <- unlist(eval_list, recursive = FALSE)
  return(eval_list)
}









