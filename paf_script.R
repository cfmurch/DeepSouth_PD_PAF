#See individual .R files in /R for details on modular function calls


suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(table1)
  library(pairwiseCI)
  library(AF)})


#Source /R
R.utils::sourceDirectory("./R/", modifiedOnly = FALSE)

#Set file input directory
input_dir <- paste0("./input/", list.files("./input/"))
  
#Pre-processing - build out the list of read-in data
data_curr <- read_in_supp(input_dir)

#Build primary demographics table
table_1 <- table1_maker(data_curr)
  
#Additional validation of demographics
one_off_results <- one_off_maker(data_curr)

#Unadjusted model development	
unadj_glm <- unadj_eval(data_curr)

#Adjusted models with PAF
adj_or_paf <- adj_eval(data_curr)

#Render a markdown
rmarkdown::render("output/output_report_all_data.Rmd")

#Supplemental Material with spouse and singleton specific designs


#Prep the spouse_pair variable
data_supp <- filter_prep(data_curr)
  
#Build the spouse and singleton datasets
data_curr_spouse <- data_supp[data_supp$Spouse_pair == 1,]
data_curr_nonspouse <- data_supp[data_supp$Spouse_pair == 0,]


#Singletons

#Build primary demographics table
table_1_nonspouse <- table1_maker(data_curr_nonspouse)
#Additional validation of demographics
one_off_results_nonspouse <- one_off_maker(data_curr_nonspouse)
#Unadjusted model development	
unadj_glm_nonspouse <- unadj_eval(data_curr_nonspouse)
#Adjusted models with PAF
adj_or_paf_nonspouse <- adj_eval(data_curr_nonspouse)
#Render a markdown
rmarkdown::render("output/output_report_singleton.Rmd")


#Spouses

#Build primary demographics table
table_1_spouse <- table1_maker(data_curr_spouse)
#Additional validation of demographics
one_off_results_spouse <- one_off_maker(data_curr_spouse)
#Unadjusted model development	
unadj_glm_spouse <- unadj_eval(data_curr_spouse)
#Adjusted models with PAF
adj_or_paf_spouse <- adj_eval(data_curr_spouse)
#Render a markdown
rmarkdown::render("output/output_report_spouse.Rmd")
