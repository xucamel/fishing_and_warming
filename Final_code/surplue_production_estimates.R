setwd("D:/OneDrive/Postdoctor/Project")
library(ggplot2)
library(tidyverse)

# read the parameter estiamtes (rda)
rda_files <- list.files(path = "D:/OneDrive/Postdoctor/Project/Output/Final_output", pattern = "\\.rda$", full.names = TRUE)
df_list <- lapply(rda_files, readRDS)
par_est <- abind::abind(df_list , along = 1)

# read the surplus production
rda_files <- list.files(path = "D:/OneDrive/Postdoctor/Project/Output/Final_output", pattern = "Final_surplus_production_.*\\.csv", full.names = TRUE)
df_list <- lapply(rda_files, read.csv)

df_list <- lapply(df_list, function(df) {
  df$site_id <- as.character(df$site_id)  # Convert site_id to character
  # Add similar conversions here for other columns if needed
  return(df)
})

sp_df <- bind_rows(df_list)

# reading input
input_df = read.csv("Data/Final_data/all_processed_spm_input_data.csv")


# check the convergency MN
split_r = data.frame(split_r = rep(0,dim(par_est)[1]))

for (i in 1:dim(par_est)[1]){
  split_r$split_r[i] = max(par_est[i,,6])
}
sum(split_r$split_r<1.05)

# warming impact figure df
figure_2_df = data.frame(warming_impact = par_est[,8,1], high = par_est[,8,5], low = par_est[,8,4] ,split_r = split_r$split_r, stock_id = unique(input_df$stock_id))
figure_2_df = left_join(figure_2_df, input_df[,c("stock_id","Common","site_id")], by=c("stock_id"))

qualified_stock = figure_2_df[figure_2_df$split_r<1.05,]

# only include converged outputs
sp_df_converge = sp_df[sp_df$stock_id%in%qualified_stock$stock_id,]

too_large_sp_stock = unique(sp_df_converge[sp_df_converge$sp>100000,]$stock_id)

sp_df_converge = sp_df_converge[!sp_df_converge$stock_id%in%too_large_sp_stock,]

sp_df_converge = sp_df_converge[,c("stock_id","year","site_id","species","sp","sp_2.5","sp_97.5")]

names(sp_df_converge)[3] = "nhdhr_site_id"

names(sp_df_converge)[5:7] = c("estimated surplud production (kg)","2.5% quantile (kg)","97.5% quantile (kg)")

sum(is.na(sp_df_converge$`estimated surplud production`))*3

sp_df_converge = sp_df_converge[complete.cases(sp_df_converge),]

write.csv(sp_df_converge,"Output/Final_output/Estimated_surplus_production.csv",row.names = FALSE)
