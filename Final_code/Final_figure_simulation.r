setwd("C:/Users/user/Desktop/OneDrive/Postdoctor/Project_2_Method")
setwd("D:/OneDrive/Postdoctor/Project_2_Method")
library(ggplot2)
library(dplyr)
library(abind) 

# simulation times
Nsim <- 10

# Parameter values of the fishery 
k <- 1000
r <- 0.5
q <- 0.02
m <- 1.23
Nyear <- 30
sigmaP <- 0.1
cpue_error <- 0.1  
catch_error <- 0.2 

###### read all data ######
### read diagnostic data 1
# all the diagnostic file
file_list <- list.files("Output/df", pattern = "diagnostic_sim.*\\.rda",full.names = TRUE)

# Initialize an empty array to store the data
combined_array <- array(dim = c(Nsim, 5, 5))

# Loop through the file list and read in each Rda file using readRDS, and combine with combined_array
for (file in file_list) {
  array <- readRDS(file)
  combined_array <- abind(combined_array, array, along = 1)
}

# Remove the first empty array in combined_array
dia_ar <- combined_array[-(1:Nsim), , ]

### read fishing mortality estimates files 2
# all the fishing mortality file 
file_list <- list.files("Output/df", pattern = "output_f_sim.*\\.rda",full.names = TRUE)

# Initialize an empty array to store the data
combined_array <- array(dim = c(Nsim, Nyear, 4, 5))

# Loop through the file list and read in each Rda file using readRDS, and combine with combined_array
for (file in file_list) {
  array <- readRDS(file)
  combined_array <- abind(combined_array, array, along = 1)
}

# Remove the first empty array in combined_array
f_ar <- combined_array[-(1:Nsim), , , ]

### read all the data base file 3
# all the data base file 
file_list <- list.files("Output/df",pattern = "true_obs_warm_dur_cy_cpuey_sim.*\\.csv",full.names = TRUE)

# Create an empty data frame to store the combined data
combined_data <- data.frame()

# Loop through each file, read it, and combine it with the existing data
for (file_name in file_list) {
  file_data <- read.csv(file_name, header = TRUE)  # or read.csv2() depending on the file format
  combined_data <- rbind(combined_data, file_data)
}

base_df = combined_data

### read all the parameter estimates file 4
# all parameter estiamtes file
file_list <- list.files("Output/df", pattern = "output_sim.*\\.rda",full.names = TRUE)

# Initialize an empty array to store the data
combined_array <- array(dim = c(Nsim, 8, 4, 5))

# Loop through the file list and read in each Rda file using readRDS, and combine with combined_array
for (file in file_list) {
  array <- readRDS(file)
  combined_array <- abind(combined_array, array, along = 1)
}

# Remove the first empty array in combined_array
par_ar <- combined_array[-(1:Nsim), , , ]

### read all the depletion rate file 5
# all depletion rate file
file_list <- list.files("Output/df", pattern = "output_p_sim.*\\.rda",full.names = TRUE)

# Initialize an empty array to store the data
combined_array <- array(dim = c(Nsim, Nyear, 3, 5))

# Loop through the file list and read in each Rda file using readRDS, and combine with combined_array
for (file in file_list) {
  array <- readRDS(file)
  combined_array <- abind(combined_array, array, along = 1)
}

# Remove the first empty array in combined_array
p_ar <- combined_array[-(1:Nsim), , , ]

### read true gdd and f  6
f_GDD_ture_df = read.csv("Output/df/GDD_FM_F3.csv")
######

# compare estiamtes of f with true f
# creat data frame for figure
f_fig_df = data.frame(F_his = "F_3", year =rep(1:Nyear,dim(f_ar)[1]*5), f_true = rep(f_GDD_ture_df$f,dim(f_ar)[1]*5)) # Nsim*N of data quantity scenario
f_fig_df$D_quan = rep(1:5,each=dim(f_ar)[1]*Nyear) #  Nsim*Nyear
f_fig_df$f_est = c(aperm(f_ar[,,1,], c(2, 1, 3)))
f_fig_df$sim_N = c(rep(rep(1:dim(f_ar)[1],each=Nyear),5))

# data availability identification
f_fig_df$D_cpue = 0
f_fig_df$D_c = 0
for (i in 1: Nsim){
  for (o in 1:5){
    f_fig_df[f_fig_df$sim_N==i&f_fig_df$D_quan==o&f_fig_df$year%in%base_df[i,(152+1):(152+o)],]$D_c = 1 # potential E base_df
    f_fig_df[f_fig_df$sim_N==i&f_fig_df$D_quan==o&f_fig_df$year%in%base_df[i,(152+6):(152+5+2*o)],]$D_cpue = 1
  }
}

# one column representing data vailablity 
f_fig_df$D_ava = f_fig_df$D_cpue+f_fig_df$D_c
f_fig_df$D_ava[f_fig_df$D_ava==2] = 1 

# one column representing which data is available
f_fig_df <- f_fig_df %>%
  dplyr::mutate(D_type = dplyr::case_when(
    D_cpue == 1 & D_c == 0 ~ "CPUE available",
    D_cpue == 0 & D_c == 1 ~ "Harvest available",
    D_cpue == 1 & D_c == 1 ~ "Both data available",
    D_cpue == 0 & D_c == 0 ~ "No data available"
  ))


# mutate D_quan
f_fig_df <- f_fig_df %>%
  mutate(D_quan = ifelse(D_quan == 1, "1 harvest, 2 CPUE",
                         ifelse(D_quan == 2, "2 harvest, 4 CPUE",
                                ifelse(D_quan == 3, "3 harvest, 6 CPUE",
                                       ifelse(D_quan == 4, "4 harvest, 8 CPUE","5 harvest, 10 CPUE")))))

# one simulation run
f_fig_df_filtered <- filter(f_fig_df, sim_N==2)

ggplot(data = f_fig_df_filtered) +
  geom_point(aes(x = year, y = f_est, color = factor(D_type))) +
  geom_line(aes(x = year, y = f_est, linetype = 'Estimated')) +  # Place linetype inside aes
  geom_line(aes(x = year, y = -log(1 - f_true), linetype = 'True'), color = "black") +  # Place linetype inside aes
  facet_wrap(~ D_quan, ncol = 1, nrow = 5) +
  scale_x_continuous(limits = c(0.5, 16.5), breaks = seq(5, 25, by = 5)) +
  scale_y_continuous() +  # Added for completeness if needed
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Year") +
  ylab("Fishing mortality") +
  scale_linetype_manual(values = c('True' = 1, 'Estimated' = 2), # Define linetypes
                        labels = c('True' = "True", 'Estimated' = "Estimated")) +
  labs(linetype = 'Parameter', color = 'Data type availability')
ggsave("Output/Figure/one_simulation_FM_CF.png", width = 6, height = 6, dpi = 600)


### compare estiamtes with true values
# creat data frame for figure
par_fig_df = data.frame(F_his = "F_3", par =rep(c("Warming impact","r","k","q","Initial depletion"),each = dim(f_ar)[1]*5),sim_N = rep(1:dim(f_ar)[1],each = 5), D_quan = rep(1:5,dim(f_ar)[1])) # Nsim*data quantity scenarios 

par_fig_df$value_true = c(rep(base_df$impact_e,each=5), rep(r,dim(f_ar)[1]*5), rep(k,dim(f_ar)[1]*5),rep(q,dim(f_ar)[1]*5),rep(base_df$X,each=5)) 

par_fig_df$value_est = c(aperm(par_ar[,8,1,], c(2,1)),aperm(par_ar[,1,1,], c(2,1)),aperm(par_ar[,2,1,], c(2,1)),aperm(par_ar[,3,1,], c(2,1)),aperm(par_ar[,4,1,], c(2,1)))

# mutate D_quan
par_fig_df <- par_fig_df %>%
  mutate(D_quan = ifelse(D_quan == 1, "1 harvest, 2 CPUE",
                         ifelse(D_quan == 2, "2 harvest, 4 CPUE",
                                ifelse(D_quan == 3, "3 harvest, 6 CPUE",
                                       ifelse(D_quan == 4, "4 harvest, 8 CPUE","5 harvest, 10 CPUE")))))

# fig.1 warming impact
par_fig_warming = par_fig_df[par_fig_df$par=="Warming impact",]
par_fig_warming = par_fig_warming[-42,]
ggplot(data=par_fig_warming) +
  geom_point(aes(x=value_true, y=value_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ D_quan, ncol = 5, nrow = 1) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black")) +
  xlab("True impact") +
  ylab("Estimated impact") +
  guides(color = FALSE, shape = FALSE) +
  xlim(c(-1.5, 1.5)) +
  ylim(c(-1.5, 1.5))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray") 
ggsave("Output/Figure/warming_pairs_final_CF.png", width = 7, height = 2, dpi = 600)

# Calculate R-squared values for each group
r_squared <- par_fig_warming %>%
  group_by(D_quan) %>%
  summarize(label_pos = coef(lm(value_true ~ value_est))[2])  # Adjust label position if necessary

R_squared <- par_fig_warming %>%
  group_by(D_quan) %>%
  summarize(label_pos = as.numeric(cor.test(value_true ,value_est)[4]$estimate))

# Join the R-squared values back to the original data
par_fig_warming <- left_join(par_fig_warming, R_squared, by = "D_quan")

# Plot
g <- ggplot(data=par_fig_warming) +
  geom_point(aes(x=value_true, y=value_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ D_quan, ncol = 5, nrow = 1) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black")) +
  xlab("True impact") +
  ylab("Estimated impact") +
  guides(color = FALSE, shape = FALSE) +
  xlim(c(-1.5, 1.5)) +
  ylim(c(-1.5, 1.5))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray") +
  geom_text(aes(label=sprintf("Cor = %.2f", label_pos), x = -0.1, y = 1.3), hjust=1, vjust=1, color="black", size=3)  # Adjust x, y for label position
g
ggsave("Output/Figure/warming_pairs_final_CF_cor.png", plot = g, width = 7, height = 2, dpi = 600)
# fig.2 P_initial
par_fig_filter = filter(par_fig_df, par=="Initial depletion")
ggplot(data=par_fig_filter) +
  geom_point(aes(x=value_true, y=value_est*1.25)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ D_quan, ncol = 5, nrow = 1) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black")) +
  xlab("True values") +
  ylab("Estimates") +
  guides(color = FALSE, shape = FALSE)+
  xlim(c(0, 1)) +
  ylim(c(0, 1))
ggsave("Output/Figure/P_initial_pairs.png", width = 7, height = 2, dpi = 600)

# fig.3 box of parameters' relative error
par_fig_filter = filter(par_fig_df, par%in%c("r"))
#par_fig_df$re = (par_fig_df$value_est-par_fig_df$value_true)/par_fig_df$value_true

# fig P_terminal, P_termianl_no_fishing and P_terminal_no_warming
dim(p_ar)
dim(base_df)
# creat data frame for figure
p_fig_df = data.frame(F_his = "F_3", par =rep(c("Initial depletion","Terminal depletion","Terminal depletion (no fishing)","Terminal depletion (no warming)"),each = dim(f_ar)[1]*5),sim_N = rep(1:dim(f_ar)[1],each = 5), D_quan = rep(1:5,dim(f_ar)[1])) # Nsim*data quantity scenarios 

# get the true terminal depletion
td_true = vector()
for (i in 1:dim(f_ar)[1]){
  td_true[i] = base_df[i,base_df$dur[i]]
}

# get the true terminal depletion without warming and fishing 
# function
P_PT = function(p,r,k,m,f,e,impact_e){
  return (p+r/(m-1)*p*(1-p^(m-1))*exp(impact_e*e)-p*(1-exp(-f)))
}

# matrix to contain the results
no_fishing_p = matrix(NA,dim(f_ar)[1],Nyear)
no_warming_p = matrix(NA,dim(f_ar)[1],Nyear)
no_fishing_p_vector = vector()
no_warming_p_vector = vector()
for (i in 1:dim(f_ar)[1]){
  no_fishing_p[i,1] = no_warming_p[i,1] = base_df[i,1]
  for (o in 2:base_df$dur[i]){
    gdd_i = scale(f_GDD_ture_df$GDD_one[1:base_df$dur[i]])[1:base_df$dur[i]]
    f_i = f_GDD_ture_df$f
    no_fishing_p[i,o] = P_PT(base_df[i,o-1],r,k,m,0,gdd_i[o-1],base_df$impact_e[i])
    no_warming_p[i,o] = P_PT(base_df[i,o-1],r,k,m,f_i[o-1],gdd_i[o-1],0)
  }
  no_fishing_p_vector[i] = no_fishing_p[i,base_df$dur[i]]
  no_warming_p_vector[i] = no_warming_p[i,base_df$dur[i]]
}

p_fig_df$value_true = c(rep(base_df$X,each=5), rep(td_true,each=5), rep(no_fishing_p_vector,each=5),rep(no_warming_p_vector,each=5) ) 

# get the estiamted terminal depletions with different scenarios
td_est = matrix(NA, dim(p_ar)[1],5)
td_est_no_fishing = matrix(NA, dim(p_ar)[1],5)
td_est_no_warming = matrix(NA, dim(p_ar)[1],5)

for (i in 1:dim(f_ar)[1]){
  td_est[i,] = p_ar[i,base_df$dur[i],1,]
  td_est_no_fishing[i,] = p_ar[i,base_df$dur[i],2,]
  td_est_no_warming[i,] = p_ar[i,base_df$dur[i],3,]
}
p_fig_df$value_est = c(aperm(par_ar[,4,1,], c(2,1)),aperm(td_est,c(2,1)),aperm(td_est_no_fishing,c(2,1)),aperm(td_est_no_warming,c(2,1)))

# mutate D_quan
p_fig_df <- p_fig_df %>%
  mutate(D_quan = ifelse(D_quan == 1, "1 harvest, 2 CPUE",
                         ifelse(D_quan == 2, "2 harvest, 4 CPUE",
                                ifelse(D_quan == 3, "3 harvest, 6 CPUE",
                                       ifelse(D_quan == 4, "4 harvest, 8 CPUE","5 harvest, 10 CPUE")))))

# fig one on one terminal depletion
p_fig_filter = filter(p_fig_df, par=="Terminal depletion")
#p_fig_filter = filter(p_fig_filter, value_true<0.77)

R_squared <- p_fig_filter %>%
  group_by(D_quan) %>%
  summarize(label_pos = as.numeric(cor.test(value_true ,value_est)[4]$estimate))

p_fig_filter <- left_join(p_fig_filter, R_squared, by = "D_quan")

g1 = ggplot(data=p_fig_filter) +
  geom_point(aes(x=value_true, y=value_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ D_quan,ncol=5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black")) +
  xlab("True stock status") +
  ylab("Estimated stock status") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  guides(color = FALSE, shape = FALSE)+
  geom_text(aes(label=sprintf("Cor = %.2f", label_pos), x = 0.5, y = 0.95), hjust=1, vjust=1, color="black", size=3)  # Adjust x, y for label position

g1
ggsave("Output/Figure/P_terminal_pairs_final_cor.png", width = 7, height = 2, dpi = 600)

# true terminal depletion without fishing or warming
p_fig_filter = filter(p_fig_df, par=="Terminal depletion (no fishing)")

R_squared <- p_fig_filter %>%
  group_by(D_quan) %>%
  summarize(label_pos = as.numeric(cor.test(value_true ,value_est)[4]$estimate))

p_fig_filter <- left_join(p_fig_filter, R_squared, by = "D_quan")
g2 = ggplot(data=p_fig_filter) +
  geom_point(aes(x=value_true, y=value_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ D_quan,ncol=5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black")) +
  xlab("True values") +
  ylab("Estimates") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  guides(color = FALSE, shape = FALSE) +
  geom_text(aes(label=sprintf("Cor = %.2f", label_pos), x = 0.5, y = 0.95), hjust=1, vjust=1, color="black", size=3)  # Adjust x, y for label position

g2
ggsave("Output/Figure/Terminal depletion (no fishing)_pairs.png", width = 7, height = 2, dpi = 600)

p_fig_filter = filter(p_fig_df, par=="Terminal depletion (no warming)")

R_squared <- p_fig_filter %>%
  group_by(D_quan) %>%
  summarize(label_pos = as.numeric(cor.test(value_true ,value_est)[4]$estimate))

p_fig_filter <- left_join(p_fig_filter, R_squared, by = "D_quan")
g3 = ggplot(data=p_fig_filter) +
  geom_point(aes(x=value_true, y=value_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ D_quan,ncol=5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black")) +
  xlab("True values") +
  ylab("Estimates") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  guides(color = FALSE, shape = FALSE) +
  geom_text(aes(label=sprintf("Cor = %.2f", label_pos), x = 0.5, y = 0.95), hjust=1, vjust=1, color="black", size=3)  # Adjust x, y for label position
g3
ggsave("Output/Figure/Terminal depletion (no warming)_pairs.png", width = 7, height = 2, dpi = 600)

g = gridExtra::grid.arrange(g2, g3, ncol = 1)
ggsave("Output/Figure/Terminal depletion (two scenarios)_pairs.png", g,width = 7, height = 5, dpi = 600)

### ratio 
p_benchmark = filter(p_fig_df, par=="Terminal depletion")
p_no_fishing = filter(p_fig_df, par=="Terminal depletion (no fishing)")
p_no_warming = filter(p_fig_df, par=="Terminal depletion (no warming)")

ratio_fig = data.frame(sim_N=p_benchmark$sim_N,D_quan=p_benchmark$D_quan)
ratio_fig$value_true= abs(p_no_fishing$value_true-p_benchmark$value_true)/abs(p_no_warming$value_true-p_benchmark$value_true)
ratio_fig$value_est= abs(p_no_fishing$value_est-p_benchmark$value_est)/abs(p_no_warming$value_est-p_benchmark$value_est)

ratio_fig_4 = ratio_fig[ratio_fig$D_quan=="5 harvest, 10 CPUE",] 
sum(ratio_fig_4$value_true>1&ratio_fig_4$value_est>1,na.rm = TRUE)
sum(ratio_fig_4$value_true<1&ratio_fig_4$value_est<1,na.rm = TRUE)
sum(ratio_fig_4$value_true<1&ratio_fig_4$value_est>1,na.rm = TRUE)
sum(ratio_fig_4$value_true>1&ratio_fig_4$value_est<1,na.rm = TRUE)

ggplot(data=ratio_fig) +
  geom_point(aes(x=value_true, y=value_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ D_quan,ncol=5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black")) +
  xlab("True values") +
  ylab("Estimates") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  guides(color = FALSE, shape = FALSE)

# showing the correct judgement percentage
RI_fig_df = data.frame(value_type = rep(c("true","est"),each=dim(f_ar)[1]*5))

# df of terminal depletions of three scenarios
p_fig_filter_ter = filter(p_fig_df, par=="Terminal depletion")
p_fig_filter_nw_ter = filter(p_fig_df, par=="Terminal depletion (no warming)")
p_fig_filter_nf_ter = filter(p_fig_df, par=="Terminal depletion (no fishing)")

# fishing effect
RI_fig_df$fishing_effect = c(p_fig_filter_nf_ter$value_true- p_fig_filter_ter$value_true, p_fig_filter_nf_ter$value_est- p_fig_filter_ter$value_est)

# warming effect
RI_fig_df$warming_effect = c(p_fig_filter_nw_ter$value_true- p_fig_filter_ter$value_true, p_fig_filter_nw_ter$value_est- p_fig_filter_ter$value_est)

# repeat sim_N and D_quan twice
RI_fig_df$sim_N = rep(p_fig_filter_nw_ter$sim_N,2)
RI_fig_df$D_quan = rep(p_fig_filter_nw_ter$D_quan,2)

# different catogory mark
# NA or not
RI_fig_df = RI_fig_df %>%
  mutate(category = ifelse(is.na(warming_effect) | is.na(fishing_effect), 0, 1))

RI_fig_df <- replace(RI_fig_df, is.na(RI_fig_df), 0)
RI_fig_df[RI_fig_df$warming_effect>0&RI_fig_df$fishing_effect>RI_fig_df$warming_effect,]$category = 2

RI_fig_df[RI_fig_df$warming_effect<0&RI_fig_df$fishing_effect>abs(RI_fig_df$warming_effect),]$category = 3

RI_fig_df[RI_fig_df$fishing_effect>0&RI_fig_df$warming_effect<0&RI_fig_df$fishing_effect<abs(RI_fig_df$warming_effect),]$category = 4

# category plot for true value
RI_fig_filter = filter(RI_fig_df,value_type=="true")
ggplot(RI_fig_filter, aes(x=fishing_effect,y=warming_effect)) +
  geom_point()+
  # Reference lines
  geom_hline(yintercept=0, color="black", linetype = "dashed") +
  geom_abline(slope=-1, color="black", linetype = "dashed") +
  geom_abline(slope=1, color="black", linetype = "dashed") +
  # Label quadrants
  # annotate(geom="text", x=0.1, y=0.4, 
  #          label="Temperature variation \ndepletes the population \nmore than fishing\n1%", color="red") +
  # annotate(geom="text", x=0.6, y=0.4, 
  #          label="Fishing depletes the\npopulation more\n than temperature variation\n47%", color="coral3") +
  # annotate(geom="text", x=0.1, y=-0.4, 
  #          label="Temperature variation \nfully offsets fishing\n6%", color="blue") +
  # annotate(geom="text", x=0.6, y=-0.4, 
  #          label="Temperature variation \npartially offsets fishing\n46%", color="darkgreen") +
  # # Labels
  # labs(x="Contribution of fishing\nto stock depletion", 
#      y="Contribution of temperature variation\nto stock depletion") +
# Theme
theme_bw() +
  xlim(0, 0.7) +
  ylim(-0.5, 0.5) +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_color_manual(values=c("black","darkgreen", "coral3","blue", "red"))

# category plot for est value
# 
RI_fig_true = filter(RI_fig_df,value_type=="true")
RI_fig_est = filter(RI_fig_df,value_type=="est")

for (i in 1:length(RI_fig_est$value_type)){
  if (RI_fig_true$category[i] == RI_fig_est$category[i]){
    RI_fig_est$judge[i] = 1
  } else {
    RI_fig_est$judge[i] = 2
  }
}

# true judgement with different D_quan
judge_count <- aggregate(judge ~ D_quan, data = RI_fig_est, function(x) sum(x == 1))

ggplot(RI_fig_est, aes(x=fishing_effect,y=warming_effect,col=factor(judge))) +
  geom_point()+
  # Reference lines
  geom_hline(yintercept=0, color="black", linetype = "dashed") +
  geom_abline(slope=-1, color="black", linetype = "dashed") +
  geom_abline(slope=1, color="black", linetype = "dashed") +
  # Label quadrants
  # annotate(geom="text", x=0.1, y=0.4, 
  #          label="Temperature variation \ndepletes the population \nmore than fishing\n1%", color="red") +
  # annotate(geom="text", x=0.6, y=0.4, 
  #          label="Fishing depletes the\npopulation more\n than temperature variation\n47%", color="coral3") +
  # annotate(geom="text", x=0.1, y=-0.4, 
  #          label="Temperature variation \nfully offsets fishing\n6%", color="blue") +
  # annotate(geom="text", x=0.6, y=-0.4, 
  #          label="Temperature variation \npartially offsets fishing\n46%", color="darkgreen") +
  # # Labels
  # labs(x="Contribution of fishing\nto stock depletion", 
#      y="Contribution of temperature variation\nto stock depletion") +
# Theme
theme_bw() +
  xlim(0, 0.7) +
  ylim(-0.5, 0.5) +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_color_manual(values=c("black", "red"))+
  facet_grid(~D_quan)


# one simulation showing the true p and estimated p over time
# compare estiamtes of f with true f
# creat data frame for figure
p_time_fig_df = data.frame(F_his = "F_3", year =rep(1:Nyear,dim(p_ar)[1]*5)) # Nsim*N of data quantity scenario
p_time_fig_df$D_quan = rep(1:5,each=dim(p_ar)[1]*Nyear) #  Nsim*Nyear
p_time_fig_df$p_est = c(aperm(p_ar[,,1,], c(2, 1, 3)))
p_true_time = as.matrix(base_df[,1:30])
p_time_fig_df$p_true = c(rep(c(t(p_true_time)),5))
p_time_fig_df$sim_N = c(rep(rep(1:dim(p_ar)[1],each=Nyear),5))

# data availability identification
p_time_fig_df$D_cpue = 0
p_time_fig_df$D_c = 0
for (i in 1: Nsim){
  for (o in 1:5){
    p_time_fig_df[p_time_fig_df$sim_N==i&p_time_fig_df$D_quan==o&p_time_fig_df$year%in%base_df[i,(152+1):(152+o)],]$D_c = 1 # potential E base_df
    p_time_fig_df[p_time_fig_df$sim_N==i&p_time_fig_df$D_quan==o&p_time_fig_df$year%in%base_df[i,(152+6):(152+5+2*o)],]$D_cpue = 1
  }
}

# one column representing data vailablity 
p_time_fig_df$D_ava = p_time_fig_df$D_cpue+p_time_fig_df$D_c
p_time_fig_df$D_ava[p_time_fig_df$D_ava==2] = 1 

# one column representing which data is available
p_time_fig_df <- p_time_fig_df %>%
  dplyr::mutate(D_type = dplyr::case_when(
    D_cpue == 1 & D_c == 0 ~ "CPUE available",
    D_cpue == 0 & D_c == 1 ~ "Harvest available",
    D_cpue == 1 & D_c == 1 ~ "Both data available",
    D_cpue == 0 & D_c == 0 ~ "No data available"
  ))

# mutate D_quan
p_time_fig_df <- p_time_fig_df %>%
  mutate(D_quan = ifelse(D_quan == 1, "1 harvest, 2 CPUE",
                         ifelse(D_quan == 2, "2 harvest, 4 CPUE",
                                ifelse(D_quan == 3, "3 harvest, 6 CPUE",
                                       ifelse(D_quan == 4, "4 harvest, 8 CPUE","5 harvest, 10 CPUE")))))

# one simulation run
p_time_fig_df_filtered <- filter(p_time_fig_df, sim_N==2)

ggplot(data = p_time_fig_df_filtered) +
  geom_point(aes(x = year, y = p_est, color = factor(D_type))) +
  geom_line(aes(x = year, y = p_est, linetype = 'Estimated')) +  # Place linetype inside aes
  geom_line(aes(x = year, y = p_true, linetype = 'True'), color = "black") +  # Place linetype inside aes
  facet_wrap(~ D_quan, ncol = 1, nrow = 5) +
  scale_x_continuous(limits = c(0.5, 16.5), breaks = seq(5, 25, by = 5)) +
  scale_y_continuous(limits = c(0, 1)) +  # Added for completeness if needed
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Year") +
  ylab("Stock status \n (ratio of biomass to carrying capacity)") +
  scale_linetype_manual(values = c('True' = 1, 'Estimated' = 2), # Define linetypes
                        labels = c('True' = "True", 'Estimated' = "Estimated")) +
  labs(linetype = 'Parameter', color = 'Data type availability')

ggsave("Output/Figure/one_simulation_P_time_series_CF.png", width = 6, height = 6, dpi = 600)


