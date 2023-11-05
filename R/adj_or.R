#'
#' Same general wrapper as unadj_or
#' Slight adjustments clean up the code and make it slightly more efficient so it's worth duplicating
#'
#'




adj_eval <- function(.dat_orig, .dict = eval_dict){
  
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
      
      #Subset data if needed by strata
      if(.stratum != "All") .dat <- .dat[.dat[[.strata_var]] == .stratum,]
      
      #But always subset by the formula variables
      .dat <- .dat[complete.cases(.dat[,colnames(.dat) %in% c(.case, .dict[["formula_vars"]][[.stratum]]), with = FALSE]),]
      
      #This is essentially the same flow as in unadj_eval() but we can compress it significantly since there's only one lapply
      
      #One set of exposure counts
      .tab_count <- table(.dat[[.case]])
      
      #Only need a single formula call
      .formula <- paste0("`", .case, "` ~ ", paste(paste0("`", .dict[["formula_vars"]][[.stratum]], "`"), collapse = " + "))
      .glm_curr <- tryCatch(glm(.formula, data = .dat, family = binomial(link = "logit")), error = function(e) NULL)
      
      #Coerce the summary output and identify covariate row
      .glm_out <- as.data.frame(summary(.glm_curr)[["coefficients"]])
      
      #Check if an ultra large SE indicates a Firth should be used intead
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
      
      #Subset according to the covariates in the formula
      .row_idx <- grepl(paste(.dict[["formula_vars"]][[.stratum]], collapse = "|"), rownames(.glm_out))
      
      #Pull the pvalues
      .test_stat <- .glm_out$`z value`[.row_idx]
      .pval <- pnorm(.test_stat, lower.tail = FALSE)
      .pval <- formatC(.pval, digits = 2, format = "e")#signif(.pval, 2)
      
      #Pull the OR
      .or <- .glm_out$Estimate[.row_idx]
      .se <- .glm_out$`Std. Error`[.row_idx]
      
      #Same lower bound shift
      .lb_shift <- .se*qnorm(0.95)
      
      #Build the OR [CI] string, include logic to annotate as a Firth if needed
      if(exists(".use_firth")){
        .or_out <- paste0(sprintf("%.3f", exp(.or)), " [", sprintf("%.3f", exp(.ci_lb[.row_idx])), "] *")
        rm(.use_firth)
      } else .or_out <- paste0(sprintf("%.3f", exp(.or)), " [", sprintf("%.3f", exp(.or - .lb_shift)), "]")
      
      #Finally, the vector for printing; we duplicate .tab_count since I'm getting tired
      .out <- data.frame(.tab_count[names(.tab_count) == .dict$case], .tab_count[names(.tab_count) == .dict$ctrl], rownames(.glm_out)[.row_idx], .or_out, .pval, row.names = NULL)
      names(.out) <- c(.dict$case, .dict$ctrl, "Risk Factor", "Adj OR [LB CI]", "Pval")
      
      
      
      
      #While we're here, might as well make PAF
      paf_list <- lapply(.dict[["paf_vars"]][[.stratum]], function(.var){
        
        #Calculate prevalence for the current variable
        .preval <- prop.table(table(.dat[[.var]][.dat[[.case]] == .dict[["case"]]]))[names(table(.dat[[.var]])) == .dict[["paf_prev"]][[.var]]]
        
        #Redo the model index
        .row_idx <- grep(.var, rownames(.glm_out))
        
        #Pull the OR
        .or <- exp(.glm_out$Estimate[.row_idx])
        
        #Confidence interval for two-sided 95% - profile methods
        #.ci <- exp(confint(.glm_curr))
        #.ci <- .ci[grep(.var, rownames(.ci)),]
        
        #CI redo using Wald approximation
        .lb_shift <- .glm_out$`Std. Error`[.row_idx]*qnorm(0.975)
        .ci <- exp(c(.glm_out$Estimate[.row_idx] - .lb_shift, .glm_out$Estimate[.row_idx] + .lb_shift))
        
        #Get our OR and CI and pass to paf_maker()
        .paf_vec <- c(.or, .ci)
        .paf_vec <- paf_maker(.paf_vec, .preval)
        
        # #Boot strap version
        # set.seed(12345)
        # .boot <- sapply(c(1:1000), function(.ii){
        #   .idx_boot <- sample(1:nrow(.dat), replace = TRUE)
        #   .dat_boot <- .dat[.idx_boot,]
        #   .glm_boot <- update(.glm_curr, data = .dat_boot)
        #   .coef_boot <- as.data.frame(summary(.glm_boot)[["coefficients"]])
        #   .or_boot <- .coef_boot$Estimate[.row_idx]
        #   .paf_boot <- paf_maker(exp(.or_boot), .preval)
        #   return(.paf_boot)
        # })
        # .boot <- sort(.boot)
        # .ci <- .boot[c(25, 975)]
        # .paf_vec <- c(paf_maker(.or, .preval), .ci)
        
        
        
        #AFGlm version
        #We have to take a very roundabout approach in order to build this out unfortunately
        .dat_af <- data.table::copy(.dat)
        #Coerce all factors to numeric binary and make a data.frame
        .dat_af[,(c(.dict[["case_var"]], .dict[["formula_vars"]][[.stratum]])) := lapply(.SD, function(xx){as.numeric(xx)-1}), .SDcols = c(.dict[["case_var"]], .dict[["formula_vars"]][[.stratum]])]
        .dat_af <- as.data.frame(.dat_af)
        
        #Build the equivalent GLM; make sure using as.formula to be able to extract terms
        .glm_af <- glm(as.formula(.formula), data = .dat_af, family = binomial(link = "logit"))
        #Coerce to AFglm
        .glm_af <- AF::AFglm(.glm_af, data = .dat_af, exposure = .var, case.control = TRUE)
        #Extract CI and build output vector
        .ci <- summary(.glm_af)$confidence.interval
        .paf_vec <- c(paf_maker(.or, .preval), .ci)
        
        
        
        #Build a temporary vector
        .paf_out <- c(.paf_vec, .or, .preval)
        names(.paf_out) <- c("PAF", "LB", "UB", "OR", "Prev")
        
        return(.paf_out)
      })
      
      #Bind the list
      paf_dat <- as.data.frame(do.call(rbind, paf_list))
      #Coerce the variables for annotation
      .var_names <- .dict[["paf_vars"]][[.stratum]]
      
      #Calculate the joint PAF if needed
      if(nrow(paf_dat) > 1){
        .joint_paf <- lapply(paf_dat[, colnames(paf_dat) %in% c("PAF", "LB", "UB")], function(.paf){1 - prod(1 - .paf)})
        .joint_paf <- do.call(c, .joint_paf)
        
        #For completion, get the combined adjusted OR from the model which we coerce to a filler since it's an OR conditional, not an AND
        .row_idx <- grepl(paste(.dict[["paf_vars"]][[.stratum]], collapse="|"), rownames(.glm_out))
        #.joint_or <- exp(sum(.glm_out$Estimate[.row_idx]))
        .joint_or <- NA
        
        #And joint prevalence I guess, this is horribly non-robust...
        .dat[, All_vars := rowSums(do.call(cbind, lapply(.SD, as.numeric))) - length(.dict[["paf_vars"]][[.stratum]]), .SDcols = .dict[["paf_vars"]][[.stratum]]]
        #.joint_preval <- prop.table(table(.dat[["All_vars"]][.dat[[.case]] == .dict[["case"]]]))[names(table(.dat[["All_vars"]])) == max(.dat[["All_vars"]])]
        .dat$All_vars[.dat$All_vars > 0] <- 1
        .joint_preval <- prop.table(table(.dat[["All_vars"]][.dat[[.case]] == .dict[["case"]]]))[names(table(.dat[["All_vars"]])) > 0]
        
        #Build the same kind of output string and add it to the data frame
        .joint_out <- c(.joint_paf, .joint_or, .joint_preval)
        names(.joint_out) <- names(paf_dat)
        paf_dat <- rbind(paf_dat, .joint_out)
        
        #Reannotate .var_names
        .var_names <- c(.var_names, "Joint")
      }
      
      #Build an appropriate string for PAF
      .paf_string <- paste0(sprintf("%.2f", paf_dat$PAF*100), "% [", sprintf("%.2f", paf_dat$LB*100), "% - ", sprintf("%.2f", paf_dat$UB*100), "%]")
      
      #The vector printed to match the manuscripts table
      paf_dat <- data.frame(.var_names, rep(.stratum, length(.var_names)), .paf_string, sprintf("%.3f", paf_dat$OR), paste0(sprintf("%.2f", paf_dat$Prev*100), "%"))
      names(paf_dat) <- c("Risk Factor", "Stratum", "PAF [95%CI]", "Adj OR", "Prevalence PwP")
      
      
      #We return a list with both the model output and the PAF data
      return(list(OR = .out, PAF = paf_dat))
    })

    #Name the individual strata and return
    names(eval_strata) <- paste0(.strata_var, " - ", .dict[["strata"]][[.strata_var]])
    return(eval_strata)
  })
  
  #Peel off the outer list layer
  eval_list <- unlist(eval_list, recursive = FALSE)
  
  #Split out the OR output and the PAF output
  eval_or <- lapply(eval_list, function(xx){xx$OR})
  eval_paf <- lapply(eval_list, function(xx){xx$PAF})
  
  return(list(OR = eval_or, PAF = eval_paf))
}





