
functions {

real SP_PT(real P, real r, real k, real m, real E, real impact_E) {
return r/(m-1)*P*k*(1-P^(m-1))*exp(impact_E*E);
}

real catch_PT(real P, real r, real k, real m, real F, real E, real impact_E) {
return P*k*(1+r/(m-1)*(1-P^(m-1))*exp(impact_E*E))*exp(-F);
}

real P_PT(real P, real r, real k, real m, real F, real E, real impact_E) {
  return (P+r/(m-1)*P*(1-P^(m-1))*exp(impact_E*E))*(1-exp(-F));
}

real P_PT_stocking(real P, real r, real k, real m, real F, real E, real impact_E, real S) {
  return P+r/(m-1)*P*(1-P^(m-1))*exp(impact_E*E)-P*(1-exp(-F))+S/k;
}

real bmsy_PT(real k, real m) {
real PMSY;
PMSY = m ^ (-1 / (m - 1));
return k * PMSY;
}

real fmsy_PT(real r, real m) {
return r/(m-1) *(1-1/m);
}

real msy_PT(real fmsy, real bmsy, real E, real impact_E) {
return fmsy * bmsy * exp(impact_E*E);
}
}

data {
int N_1;
vector[N_1] Catch_1;
vector[N_1] CPUE_1;
vector[N_1] Environment_1;
vector[N_1] Stocking_1;
vector[N_1] Tribal_1;
vector[2] k_1_prior;
vector[2] r_1_prior;
}

parameters{

real <lower=0> r_1;
real <lower=0> k_1;
real <lower=0> q_1;
real <lower=0, upper=1> P_initial_1;
real <lower=0> sigma_sq_1;
real <lower=0> tau_sq_1;
real <lower=0> m_1;
real impact_E_1;
vector <lower=0> [N_1] F_1;

}
transformed parameters{

real<lower=0> sigma_1;
real<lower=0> tau_1;
vector<lower=0>[N_1]P_med_1;
vector<lower=0>[N_1]C_pred_1;
sigma_1=sqrt(sigma_sq_1);
tau_1=sqrt(tau_sq_1);
P_med_1[1]=P_initial_1;
C_pred_1[1]=catch_PT(P_med_1[1],r_1,k_1,m_1,F_1[1],Environment_1[1],impact_E_1);
for (t in 2:N_1){
P_med_1[t]=P_PT(P_med_1[t-1],r_1,k_1,m_1,F_1[t-1],Environment_1[t-1],impact_E_1);
C_pred_1[t]=catch_PT(P_med_1[t],r_1,k_1,m_1,F_1[t],Environment_1[t],impact_E_1);
  }

}
model{
// priors
r_1~lognormal(log((r_1_prior[1]+r_1_prior[2])/2),(log((r_1_prior[1]+r_1_prior[2])/2)-log(r_1_prior[1]))/2);
k_1~lognormal(log((k_1_prior[1]+k_1_prior[2])/2),(log((k_1_prior[1]+k_1_prior[2])/2)-log(k_1_prior[1]))/2);
sigma_sq_1~inv_gamma(4, 0.01);
tau_sq_1~inv_gamma(4,0.01);
F_1~exponential(1);
P_initial_1~beta(1,1);
impact_E_1~normal(0,2);
target += -log(q_1);
log(m_1) ~ skew_normal(-0.5,1,10);
target += -log(m_1);

// observation of recreational harvest and CPUE
for (t in 1:N_1){
if (CPUE_1[t]>(-1)){
CPUE_1[t]~lognormal(log(q_1*k_1*P_med_1[t]),sigma_1);
  }
if (Catch_1[t]>(-1)){
  if ((Catch_1[t]+Tribal_1[t]-Stocking_1[t])>0){
    (Catch_1[t]+Tribal_1[t]-Stocking_1[t])~lognormal(log(C_pred_1[t]),tau_1);
  }
}
 }

}

generated quantities {
  real BMSY;
  real FMSY;
  real P_no_fishing_terminal;
  real P_no_env_terminal;
  real P_one_env_terminal;
  real P_terminal;
  real P_no_fishing_terminal_adjusted;
  vector [N_1] MSY;
  vector [N_1] P_no_fishing;
  vector [N_1] P_no_env;
  vector [N_1] P_one_env;
  vector [N_1] P_no_fishing_adjusted;
  vector [N_1] surplus_production;
  BMSY = bmsy_PT(k_1, 2);
  FMSY = fmsy_PT(r_1, 2);

  // simulate two scenarios (no fishing and no impact of temperature)
  for (t in 1:N_1){
    MSY[t]  = msy_PT(BMSY,FMSY,Environment_1[t],impact_E_1);
  }
  P_no_fishing[1] = P_initial_1;
  P_no_env[1] = P_initial_1;
  P_one_env[1] = P_initial_1;
  P_no_fishing_adjusted[1] = P_initial_1;
  surplus_production[1] = SP_PT(P_initial_1,r_1,k_1,m_1,Environment_1[1],impact_E_1);
  for (t in 2:N_1){
    //P_no_fishing[t] = P_PT(P_med_1[t-1],r_1,k_1,m_1,0, Environment_1[t-1],impact_E_1);
    //P_no_fishing_adjusted[t] = P_PT(P_med_1[t-1],r_1,k_1,m_1,0, Environment_1[t-1],impact_E_1*1.98);
    P_no_fishing[t] = P_PT_stocking(P_med_1[t-1],r_1,k_1,m_1,0, Environment_1[t-1],impact_E_1,Stocking_1[t-1]);
    P_no_fishing_adjusted[t] = P_PT_stocking(P_med_1[t-1],r_1,k_1,m_1,0, Environment_1[t-1],impact_E_1*1.98,Stocking_1[t-1]);
    P_no_env[t] = P_PT(P_med_1[t-1],r_1,k_1,m_1,F_1[t-1], Environment_1[t-1],0);
    P_one_env[t] = P_PT(P_med_1[t-1],r_1,k_1,m_1,F_1[t-1], Environment_1[1],impact_E_1);
    surplus_production[t] = SP_PT(P_initial_1,r_1,k_1,m_1,Environment_1[t],impact_E_1);
  }
  P_no_fishing_terminal = P_no_fishing[N_1];
  P_no_fishing_terminal_adjusted = P_no_fishing_adjusted[N_1];
  P_no_env_terminal = P_no_env[N_1];
  P_one_env_terminal = P_one_env[N_1];
  P_terminal = P_med_1[N_1];
}
