#'
#'  Data read-in, relevel factors
#'




read_in_supp <- function(.path, .sheet = 1, .start = 3, .sep = "_", .dict = data_dict[["data_proc"]]){
  
  #Drop temp file if necessary
  .temp_path <- grep("~", .path)
  if(length(.temp_path) > 0) .path <- .path[-.temp_path]
  
  #Read in the file using openxlsx
  .dat <- openxlsx::read.xlsx(.path, startRow = .start, sep.names = .sep, sheet = .sheet)
  
  #Coerce to data.table
  .dat <- data.table::as.data.table(.dat)
  
  #Convert character to factor
  .dat[,(.dict[["col"]]) := lapply(.SD, as.factor), .SDcols = .dict[["col"]]]
  
  #Cull as needed - This will get dropped in Supplemental
  for(ii in seq_along(.dict[["cull"]][["cull_var"]])){
    .dat[[.dict[["cull"]][["cull_var"]][ii]]][is.na(.dat[[.dict[["cull"]][["cull_var"]][ii]]]) | 
                                        !(.dat[[.dict[["cull"]][["cull_var"]][ii]]] %in% .dat[[.dict[["cull"]][["compare_var"]][ii]]])] <- NA
  }; rm(ii)
  
  #Make any new vars from quotes in dictionary
  .dat[, (names(.dict[["new_var"]])) := lapply(.dict[["new_var"]], eval, envir = environment())]
  
  #Relevel as needed
  invisible(sapply(seq_along(.dict[["relevel"]]), function(.idx){
    .dat[[names(.dict[["relevel"]][.idx])]] <<- relevel(.dat[[names(.dict[["relevel"]][.idx])]], ref = .dict[["relevel"]][.idx])
    return(NULL)
  }))
   
  #Recast levels; used for MTBI and Agent Orange
  invisible(sapply(.dict[["recast"]][["recast_var"]], function(.var){
    .out <- replace(.dat[[.var]], list = which(.dat[[.var]] %in% .dict[["recast"]][["orig"]]), values = .dict[["recast"]][["new"]])
    .dat[[.var]] <<- droplevels(.out)
    return(NULL)
  }))

  
 
  #Return
  return(.dat)
  
}



#Specialty function to prep spouse
filter_prep <- function(.dat){
  
  #Initialize the variable
  .dat$Spouse_pair <- 1
  
  #Get rid of join PD-PD or NHC-NHC pairs - this will get dropped since it's done a prior in supplemental data
  .dat$Spouse_pair[c(intersect(grep("P", .dat$Sample_ID), grep("P", .dat$Spouse)), intersect(grep("C", .dat$Sample_ID), grep("C", .dat$Spouse)))] <- 0
  
  #Filter on the main race / ethnicity criteria
  .dat$Spouse_pair[which((is.na(.dat$Race_bin) & is.na(.dat$Hispanic__or_Latino)) | .dat$Race_bin!="White" | .dat$Hispanic__or_Latino!="N")] <- NA
  .dat <- .dat[!is.na(.dat$Spouse_pair)]
  
  #Update levels with missing
  levels(.dat$Hispanic__or_Latino) <- unique(c(levels(.dat$Hispanic__or_Latino), "Unknown"))
  .dat$Hispanic__or_Latino[is.na(.dat$Hispanic__or_Latino)] <- "Unknown"
  levels(.dat$Race_bin) <- unique(c(levels(.dat$Race_bin), "Unknown"))
  .dat$Race_bin[is.na(.dat$Race_bin)] <- "Unknown"
  
  
  
  #Get rid of orphan spouse IDs
  .dat$Spouse_pair[.dat$Spouse %in% na.omit(setdiff(.dat$Spouse, .dat$Sample_ID))] <- 0
  
  #Get rid of all other non-spouses
  .dat$Spouse_pair[is.na(.dat$Spouse)] <- 0
  
  return(.dat)
  
  
}
