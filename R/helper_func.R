#'
#' Helper functions and dictionaries
#'


`%not_in%` <- Negate(`%in%`)


#Helper functions for summary statistics
mean_sd <- function(.var){
  
  #Mean / SD
  .mean <- mean(na.omit(.var))
  .sd <- sd(na.omit(.var))
  
  #Build string
  .out <- paste(sprintf("%.3f", .mean), "±", sprintf("%.3f", .sd))
  
  return(.out)
}


count_prop <- function(.var, .var_lvl = "Y"){
  
  #Count / prop
  .count <- length(na.omit(.var[.var == .var_lvl]))
  .prop <- .count / length(na.omit(.var))
  
  #Build string
  .out <- paste0(.count, " (", sprintf("%.3f", .prop * 100), "%)")
  
  return(.out)
}




#Calculating either single PAF or joint PAF
paf_maker <- function(.rr, .p, .joint = FALSE){
  
  #Calculate PAF, works whether a vector or single value
  .paf <- .p*((.rr-1)/.rr)
  
  #If more than one value, calculate the joint paf
  if(length(.paf) > 1 && isTRUE(.joint)) .paf <- 1 - prod(1 - .paf)
  
  return(.paf)
}









#Data processing and table 1 dictionary
data_dict <- list(#Dictionary for read-in
                  data_proc = list(#Character columns coerced to factors
                                      col = c("Case__status", "Sex", "RBD",
                                              "MTBI/_concussion", "Repeated__blows__to_head","Agent_orange__chemical__warfare__exposure", "Pesticide/_herbicide__exposure",
                                              "Hispanic__or_Latino", "Race", "Jewish__ancestry", "Family__history__PD", 
                                              "Lost__10lbs__last_year", "Gained__10lbs__last_year", "Constipation"),
                                      
                                      #Factors needing to be releveled
                                      relevel = c(`Race` = "White"),
                                      
                                      #Levels to recast
                                      recast = list(recast_var = c("MTBI/_concussion", "MTBI_10yr_prior", "Pesticide/_herbicide__exposure"),
                                                    orig = c("Y but after onset", "Y but around or after onset"),
                                                    new = c(NA)),
                                      
                                      #Cull spouse
                                      cull = list(cull_var = c("Spouse"),
                                                  compare_var = c("Sample_ID")),
                                      
                                      #New var quotes
                                      new_var = c(Race_bin = quote(factor(.dat$Race, 
                                                                          levels = c("White", as.character(unique(na.omit(.dat[["Race"]][.dat[["Race"]] %not_in% c("White")])))), 
                                                                          labels = c("White", rep("Other", length(unique(na.omit(.dat[["Race"]][.dat[["Race"]] %not_in% c("White")]))))))),
                                                  
                                                  Race_black = quote(factor(.dat$Race, 
                                                                             levels = c("Black or African American", as.character(unique(na.omit(.dat[["Race"]][.dat[["Race"]] %not_in% c("Black or African American")])))), 
                                                                             labels = c("Black or African American", rep("Other", length(unique(na.omit(.dat[["Race"]][.dat[["Race"]] %not_in% c("Black or African American")]))))))),
                                                  
                                                  Deep_south = quote(factor(.dat$State__current__residence, 
                                                                            levels = c(unique(grep("al|ms|la|ga|sc", .dat$State__current__residence, ignore.case = TRUE, value = TRUE)), 
                                                                                       as.character(unique(na.omit(.dat[["State__current__residence"]][.dat[["State__current__residence"]] %not_in% unique(grep("al|ms|la|ga|sc", .dat$State__current__residence, ignore.case = TRUE, value = TRUE))])))), 
                                                                            labels = c(rep("DS", length(unique(grep("al|ms|la|ga|sc", .dat$State__current__residence, ignore.case = TRUE, value = TRUE)))), rep("Other", length(unique(na.omit(.dat[["State__current__residence"]][.dat[["State__current__residence"]] %not_in% unique(grep("al|ms|la|ga|sc", .dat$State__current__residence, ignore.case = TRUE, value = TRUE))]))))))),
                                                  
                                                  Periphery = quote(factor(.dat$State__current__residence, 
                                                                            levels = c(unique(grep("nc|tx|fl|ar|tn", .dat$State__current__residence, ignore.case = TRUE, value = TRUE)), 
                                                                                       as.character(unique(na.omit(.dat[["State__current__residence"]][.dat[["State__current__residence"]] %not_in% unique(grep("nc|tx|fl|ar|tn", .dat$State__current__residence, ignore.case = TRUE, value = TRUE))])))), 
                                                                            labels = c(rep("Periph", length(unique(grep("nc|tx|fl|ar|tn", .dat$State__current__residence, ignore.case = TRUE, value = TRUE)))), rep("Other", length(unique(na.omit(.dat[["State__current__residence"]][.dat[["State__current__residence"]] %not_in% unique(grep("nc|tx|fl|ar|tn", .dat$State__current__residence, ignore.case = TRUE, value = TRUE))]))))))),
                                                  
                                                  
                                                  #2-step for MTBI_10yr_prior
                                                  MTBI_10yr_prior = quote(replace(.dat[["MTBI/_concussion"]], which(.dat[["MTBI/_concussion"]] == "Y" & (.dat[["PD_age__at_onset"]] - .dat[["MTBI/_concussion__age"]])<10), "N")),
                                                                                
                                                  #2-step for positive family history
                                                  Family_PD_posit = quote(replace(.dat[["Family__history__PD"]], 
                                                                                  list = c(which(.dat[["Family_history__PD_degree"]] %in% c("third or higher")), which(is.na(.dat[["Family_history__PD_degree"]]) & .dat[["Family__history__PD"]] == "Y")), 
                                                                                  values = c(rep("N", length(which(.dat[["Family_history__PD_degree"]] %in% c("third or higher")))), 
                                                                                             rep(NA, length(which(is.na(.dat[["Family_history__PD_degree"]]) & .dat[["Family__history__PD"]] == "Y")))))),
                                                  
                                                  #2-step for Pesticide
                                                  Pesticide_duration_filt = quote(replace(.dat[["Pesticide/_herbicide__exposure__duration"]], 
                                                                                          list = which(.dat[["Pesticide/_herbicide__exposure"]] == "Y but around or after onset"), values = NA))
                                                 
                                                  )),
                                                  
                     
                  table1 = list(split = "Case__status",
                                   entries = c("Age", "PD_age__at_onset", "Sex", "Jewish__ancestry", "Race_black", "Hispanic__or_Latino", 
                                               "Race_bin", "Deep_south", "Periphery", "Constipation", "RBD", "Lost__10lbs__last_year"))
)





eval_dict <- list(case_var = c("Case__status"), case = c("PD"), ctrl = c("Control"),
                      
                  #Stratification splits
                  strata = list(`All` = c("All"), `Sex` = c("M", "F")),
                      
                  #Variables to evaluate, also used for formula building
                  vars = c("Age", "Family_PD_posit", "MTBI/_concussion", "MTBI_10yr_prior", "Repeated__blows__to_head",
                           "Pesticide/_herbicide__exposure", "Pesticide_duration_filt", "Agent_orange__chemical__warfare__exposure"),
                  
                  #Toggle for count variables, can also be leveraged for prop.test if needed
                  or_succ = c(NA, rep("Y", 5), NA, "Y"),
                  
                  #Variables used for adjusted model building
                  formula_vars = list(All = c("Age", "Family_PD_posit", "MTBI/_concussion", "Repeated__blows__to_head",
                                              "Pesticide/_herbicide__exposure", "Agent_orange__chemical__warfare__exposure"),
                                      M = c("Age", "Family_PD_posit", "MTBI/_concussion", "Repeated__blows__to_head",
                                            "Pesticide/_herbicide__exposure", "Agent_orange__chemical__warfare__exposure"),
                                      F = c("Age", "Family_PD_posit", "MTBI/_concussion", "Pesticide/_herbicide__exposure")),
                  
                  #Variables specifically used for PAF calculation
                  paf_vars = list(All = c("Pesticide/_herbicide__exposure"),
                                  F = c("Pesticide/_herbicide__exposure"),
                                  M = c("Pesticide/_herbicide__exposure", "Agent_orange__chemical__warfare__exposure", "Repeated__blows__to_head")),
                  
                  #Entry to help calculate prevalence, needs to be dynamic because of data frame subsetting
                  paf_prev = c(`Pesticide/_herbicide__exposure` = "Y",
                               `Agent_orange__chemical__warfare__exposure` = "Y",
                               `Repeated__blows__to_head` = "Y"),
                  
                  #Variable for one off calculations
                  one_off = list(vars = c("Sex", "Constipation", "Lost__10lbs__last_year", "RBD"),
                                 binom_var = c("Sex"),
                                 or_succ = c("M", rep("Y", 3)))
                  )






















