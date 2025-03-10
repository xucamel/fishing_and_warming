setwd("D:/OneDrive/Postdoctor/Project")
library(rstan)
library(dplyr)
SPM_stan = stan_model(file="Code/Final_code/SPM_catch_calculation_include_sp.stan")

# reading input
input_df = read.csv("Data/Final_data/all_processed_spm_input_data.csv")

# Replace NA in 'estimated_weight_kg' and 'mean_cpue_from_sum' with -999
input_df$estimated_weight_kg[is.na(input_df$estimated_weight_kg)] <- -999
input_df$mean_cpue_from_sum[is.na(input_df$mean_cpue_from_sum)] <- -999

# Replace NA in 'tri_harvest' and 'stocking_weight' with 0
input_df$tri_harvest[is.na(input_df$tri_harvest)] <- 0
input_df$stocking_weight[is.na(input_df$stocking_weight)] <- 0

# rename Year and Common
names(input_df)[c(2,4)] = c("year","species")

# number of lakes
length(unique(input_df$site_id))

# number of stocks
Nstocks = length(unique(input_df$stock_id))

# output container 
output = array(dim=c(Nstocks,8,7))
output_quantities = data.frame(stock_id=rep(NA,Nstocks), P_terminal=rep(NA,Nstocks),P_no_fishing=rep(NA,Nstocks),P_no_fishing_adjusted=rep(NA,Nstocks),P_no_env=rep(NA,Nstocks),P_one_env=rep(NA,Nstocks),N_divergent=rep(NA,Nstocks),max_f=rep(NA,Nstocks),min_f=rep(NA,Nstocks))
output_exploited_rate = input_df
output_exploited_rate$exploited_rate = NA
output_exploited_rate$sp = NA
output_exploited_rate$sp_sd = NA
output_exploited_rate$sp_2.5 = NA
output_exploited_rate$sp_97.5 = NA

# run the model
for (i in 1:Nstocks){
  ## model input 
  stock_i = unique(input_df$stock_id)[i]
  data_i = input_df[input_df$stock_id==stock_i,]
  min_year_i = min(data_i$year[which(data_i$mean_cpue_from_sum>0)],data_i$year[which(data_i$estimated_weight_kg>0)])
  max_year_i = max(data_i$year[which(data_i$mean_cpue_from_sum>0)],data_i$year[which(data_i$estimated_weight_kg>0)])
  catch_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$estimated_weight_kg
  cpue_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$mean_cpue_from_sum
  env_i = scale(data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$gdd_wtr_5c, scale = T)[,]
  stocking_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$stocking_weight
  tri_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$tri_harvest
  
  stan_data <- list(
    N_1 = max_year_i-min_year_i+1,
    Catch_1 = catch_i,
    CPUE_1 = cpue_i,
    Environment_1 = env_i,
    Stocking_1 = stocking_i,
    Tribal_1 = tri_i,
    k_1_prior = c(max(catch_i),max(catch_i*10000)),
    r_1_prior = c(0.05,0.5)
  )
  
  chain=3; iter=2000; warmup=1000; thin=1;
  
  fit_SPM_stan <- sampling(SPM_stan,
                           data=stan_data,
                           chain=chain,
                           iter=iter,
                           warmup=warmup,
                           cores = chain,
                           thin=thin,
                           control = list(adapt_delta = 0.999,max_treedepth = 12)
  ) 
  
  output[i,,1] = summary(fit_SPM_stan)$summary[1:8,"mean"]
  output[i,,2] = summary(fit_SPM_stan)$summary[1:8,"se_mean"]
  output[i,,3] = summary(fit_SPM_stan)$summary[1:8,"sd"]
  output[i,,4] = summary(fit_SPM_stan)$summary[1:8,"2.5%"]
  output[i,,5] = summary(fit_SPM_stan)$summary[1:8,"97.5%"]
  output[i,,6] = summary(fit_SPM_stan)$summary[1:8,"Rhat"]
  output[i,,7] = summary(fit_SPM_stan)$summary[1:8,"n_eff"]
  output_quantities$P_terminal[i] = summary(fit_SPM_stan)$summary["P_terminal","mean"]
  output_quantities$P_no_fishing[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal","mean"]
  output_quantities$P_no_fishing_adjusted[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal_adjusted","mean"]
  output_quantities$P_no_env[i] = summary(fit_SPM_stan)$summary["P_no_env_terminal","mean"]
  output_quantities$P_one_env[i] = summary(fit_SPM_stan)$summary["P_one_env_terminal","mean"]
  output_quantities$N_divergent[i] = get_num_divergent(fit_SPM_stan)
  output_quantities$max_f[i] = max(colMeans(as.data.frame(rstan::extract(fit_SPM_stan,"F_1"))))
  output_quantities$min_f[i] = min(colMeans(as.data.frame(rstan::extract(fit_SPM_stan,"F_1"))))
  output_quantities$stock_id[i] = stock_i[i]
  
  output_exploited_rate[output_exploited_rate$stock_id==stock_i&output_exploited_rate$year%in%c(min_year_i:max_year_i),]$sp = colMeans(as.data.frame(rstan::extract(fit_SPM_stan,"surplus_production")))
  output_exploited_rate[output_exploited_rate$stock_id==stock_i&output_exploited_rate$year%in%c(min_year_i:max_year_i),]$sp_sd = apply(as.data.frame(rstan::extract(fit_SPM_stan,"surplus_production")),2,sd)
  output_exploited_rate[output_exploited_rate$stock_id==stock_i&output_exploited_rate$year%in%c(min_year_i:max_year_i),]$sp_2.5 = apply(as.data.frame(rstan::extract(fit_SPM_stan,"surplus_production")),2,quantile, probs = c(0.025), na.rm = TRUE)
  output_exploited_rate[output_exploited_rate$stock_id==stock_i&output_exploited_rate$year%in%c(min_year_i:max_year_i),]$sp_97.5 = apply(as.data.frame(rstan::extract(fit_SPM_stan,"surplus_production")),2,quantile, probs = c(0.975), na.rm = TRUE)
  
  print(i)
}

saveRDS(output,"Output/Final_output.rda")
write.csv(output_quantities,"Output/Final_quantities.csv",row.names = FALSE)
write.csv(output_exploited_rate,"Output/Final_surplus_production.csv",row.names = FALSE)


