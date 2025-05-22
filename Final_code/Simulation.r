#setwd("C:/Users/user/Desktop/OneDrive/Postdoctor/Project_2_Method")
library(rstan)

# parameter values of the fishery 
k = 1000;r = 0.4;q = 0.02; m = 1.23
Nsim = 20; Nyear = 30
sigmaP = 0.1
cpue_error = 0.1  
catch_error = 0.2 

# functions for population dynamics
SP_PT = function(p,r,m,k,impact_e,E){
  return (r/(m-1)*p*k*(1-p^(m-1))*exp(impact_e*E))
}

catch_PT = function(p,k,f){
  return(p*k*(1-exp(-f)))
}

biomass_PT = function(p,k,sp,c){
  return(p*k+sp-c)
}

P_PT = function(p,r,k,m,f,e,impact_e){
  return (p+r/(m-1)*p*(1-p^(m-1))*exp(impact_e*e)-p*(1-exp(-f)))
}

# set seed 
set.seed(01171) # sim1_20

# random sample a time series of GDD
#GDD_pool = read.csv("C:/Users/user/Desktop/OneDrive/Postdoctor/Project/Data/MN/MN_BG_input.csv")

# random sample a time series of GDD in cluster
input_BG_MN = read.csv("MN_BG_input.csv")

r_dow = sample(input_BG_MN$dow,1)
E = scale(input_BG_MN[input_BG_MN$dow==r_dow,]$gdd_wtr_5c)[1:Nyear]

# warming impact true value 
impact_e = rnorm(Nsim,0,1)
write.csv(impact_e,file = "Warming_impact_true_value_0_0.5.csv",row.names = FALSE)

#creat the data base containing the true biomass, catch and CPUE
data_base = matrix(NA, Nsim, Nyear*4)
temp = numeric()
b_true = numeric()
p_true = numeric()
c_true = numeric()
cpue_true = numeric()
impact_e_true = numeric()
SP = numeric()

###### F3 high contrasts in fishing pressure #####
f = sample(100:900,Nyear)/1000
F = -log(1-f)
write.csv(f,file = "Fishing_mortality_true_value.csv",row.names = FALSE)

repeat{
  for (simTime in 1:Nsim)
  {
    temp[1] = sample(10:100,1)/100*k # b_initial
    b_true[1] = temp[1]* rlnorm(1,-0.5*sigmaP^2,sigmaP)
    p_true[1] = b_true[1]/k
    c_true[1] = catch_PT(p_true[1],k,F[1])
    cpue_true[1] = b_true[1]*q
    SP[1] = SP_PT(p_true[1],r,m,k,impact_e[simTime],E[1])
    for (i in 2:Nyear)
    {
      temp[i] = biomass_PT(p_true[i-1],k,SP[i-1],c_true[i-1])
      b_true[i] = temp[i]*rlnorm(1,-0.5*sigmaP^2,sigmaP)
      p_true[i] = b_true[i]/k
      SP[i] = SP_PT(p_true[i],r,m,k,impact_e[simTime],E[i])
      c_true[i] = catch_PT(p_true[i],k,F[i])
      cpue_true[i] = b_true[i]*q
    }
    data_base[simTime,]<-c(b_true,c_true,cpue_true,p_true)
  }
  if (sum(is.na(data_base))==0&sum(data_base<0)==0){
    break
  }
}
write.csv(data_base,file = "data_base_F_high_contrast.csv",row.names = FALSE)

# creat the observed data (c and cpue)
obs_data_base = matrix(NA,Nsim,Nyear*2)
c_obs = numeric()
cpue_obs = numeric()
sigmaO_catch <- catch_error
sigmaO_cpue = cpue_error
for (simTime in 1:Nsim)
{
  for (i in 1:Nyear)
  {
    c_obs[i] = data_base[simTime,Nyear+i]*rlnorm(1,-0.5*sigmaO_catch^2,sigmaO_catch)
    cpue_obs[i] = data_base[simTime,2*Nyear+i]*rlnorm(1,-0.5*sigmaO_cpue^2,sigmaO_cpue)
  }
  obs_data_base[simTime,]<-c(c_obs,cpue_obs)
}
write.csv(obs_data_base,file = "data_obs_F_high_contrast.csv",row.names = FALSE)

## run the SP model 
num_d_scenario = 5
output = array(dim=c(Nsim,8,4,num_d_scenario))
output_f = array(dim=c(Nsim,Nyear,4,num_d_scenario))
output_p = array(dim=c(Nsim,Nyear,3,num_d_scenario))
diagnostic = array(dim=c(Nsim,5,num_d_scenario))

one_lake_stan = stan_model(file="simulation_PNAS.stan")
#one_lake_stan = stan_model(file="Code/Stan_code/simulation_PNAS.stan")
chain=4; iter=200; warmup=100; thin=1;

for (i in 1:Nsim){
  for (o in 1:num_d_scenario){
    catch_year = sample(1:Nyear,o,replace = FALSE)
    cpue_year = sample(1:Nyear,2*o,replace = FALSE)
    Catch_1 = obs_data_base[i,(1:Nyear)]
    CPUE_1 = obs_data_base[i,(Nyear+1):(2*Nyear)]
    CPUE_1[-cpue_year] = Catch_1[-catch_year] = 0
    min_year_i = min(which(CPUE_1>0),which(Catch_1>0))
    max_year_i = max(which(CPUE_1>0),which(Catch_1>0))
    stan_data = list(
      N_1=max_year_i-min_year_i+1,
      Catch_1=Catch_1[min_year_i:max_year_i],
      CPUE_1=CPUE_1[min_year_i:max_year_i],
      Environment_1=E[min_year_i:max_year_i],
      k_1_prior=c(max(Catch_1),10000*(max(Catch_1))),
      r_1_prior=c(0.2,0.8)
    )
    fit_stan <- sampling(
      one_lake_stan,
      data=stan_data,
      chain=chain,
      iter=iter,
      warmup=warmup,
      cores = chain,
      thin=thin,
      control = list(max_treedepth = 20L,adapt_delta = 0.999))
    output[i,,1,o] = summary(fit_stan)$summary[1:8,"mean"]
    output[i,,2,o] = summary(fit_stan)$summary[1:8,"sd"]
    output[i,,3,o] = summary(fit_stan)$summary[1:8,"2.5%"]
    output[i,,4,o] = summary(fit_stan)$summary[1:8,"97.5%"]
### estimates of f
    ## mean
    output_f[i,min_year_i:max_year_i,1,o] = colMeans(extract(fit_stan, "F_1")$F_1)
    ## SD
    output_f[i,min_year_i:max_year_i,2,o] = apply(extract(fit_stan, "F_1")$F_1, 2, sd)
    ## 95% quantile
    output_f[i,min_year_i:max_year_i,3,o] =    apply(extract(fit_stan, "F_1")$F_1, 2, quantile, prob = 0.95)
    ## 5% quantile
    output_f[i,min_year_i:max_year_i,4,o] = apply(extract(fit_stan, "F_1")$F_1, 2, quantile, prob = 0.05)
    ### estimates of p, p_no_fishing, p_no_warming
    ## p
    output_p[i,min_year_i:max_year_i,1,o] = colMeans(extract(fit_stan, "P_med_1")$P_med_1)
    ## p_no_fishing
    output_p[i,min_year_i:max_year_i,2,o] = colMeans(extract(fit_stan, "P_no_fishing")$P_no_fishing)
    ## p_no_warming
    output_p[i,min_year_i:max_year_i,3,o] =    colMeans(extract(fit_stan, "P_no_env")$P_no_env)
    
    ### diagnostic
    diagnostic[i,1,o] = sum(is.na(summary(fit_stan)$summary[,"Rhat"]))
    diagnostic[i,2,o] = max(summary(fit_stan)$summary[,"Rhat"],na.rm = T)
    diagnostic[i,3,o] = sum(is.na(summary(fit_stan)$summary[,"n_eff"]))
    diagnostic[i,4,o] = min(summary(fit_stan)$summary[,"n_eff"],na.rm = T)
    diagnostic[i,5,o] = get_num_divergent(fit_stan)
    print(c(i,o))
  }
}

saveRDS(output,"output_F3_sim1_20.rda")
saveRDS(output_f,"output_f_F3_sim1_20.rda")
saveRDS(output_p,"output_p_F3_sim1_20.rda")
saveRDS(diagnostic,"diagnostic_F3_sim1_20.rda")
