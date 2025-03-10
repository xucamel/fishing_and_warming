setwd("D:/OneDrive/Postdoctor/Project")
library(ggplot2)
library(tidyverse)
# figure 2 
# read the parameter estiamtes (rda)
rda_files <- list.files(path = "D:/OneDrive/Postdoctor/Project/Output/Final_output", pattern = "\\.rda$", full.names = TRUE)
df_list <- lapply(rda_files, readRDS)
par_est <- abind::abind(df_list , along = 1)

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


# plot
# warming significant
# reorder 
figure_temp_influence = figure_2_df[figure_2_df$split_r<1.05,]
figure_temp_influence = figure_temp_influence[order(figure_temp_influence$warming_impact),]


length(unique((figure_temp_influence$stock_id)))
length(unique((figure_temp_influence[figure_temp_influence$low>0,]$stock_id)))
length(unique((figure_temp_influence[figure_temp_influence$high<0,]$stock_id)))
# col
figure_temp_influence$col_line <- ifelse(figure_temp_influence$low > 0, "blue", 
                                         ifelse(figure_temp_influence$high < 0, "red", "black"))
g1 = ggplot(figure_temp_influence, aes(x = warming_impact, y = rev(row.names(figure_temp_influence)), color = col_line)) +
  geom_segment(aes(x = low, xend = high, y = rev(row.names(figure_temp_influence)), yend = rev(row.names(figure_temp_influence)), color = col_line), size = 0.6) +
  geom_hline(yintercept = 0, linetype = 3, color = "black", size = 0.8) +
  geom_vline(xintercept = 0, color = "black", size = 0.8, linetype = "dashed") +
  geom_text(aes(x = 3, y = 160, label = "102 populations \n(20%)"), size = 3.5, hjust = 0, color = "blue") +
  geom_text(aes(x = -4, y = 48, label = "69 populations\n (13%)"), size = 3.5, hjust = 1, color = "red") +
  scale_x_continuous(limits = c(-4, 4), expand = c(0, 0), breaks = seq(-4, 4, by = 1), name = "Warming impact") +
  scale_y_discrete(limits = factor(row.names(figure_temp_influence), levels = rev(row.names(figure_temp_influence))), name = "") +
  scale_color_manual(values = c("gray15", "blue", "red")) +
  guides(color = FALSE) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(),
        axis.ticks = element_line(color = "black", size = 0.6),
        axis.title = element_text(),
        plot.title = element_text(),
        panel.grid = element_blank())+
  labs(tag="a")  + 
  geom_point(size = 2, shape = 19)

# warming vs species
impact_explain = figure_2_df[figure_2_df$split_r<1.05,]
g2 = ggplot(impact_explain, aes(x=factor(Common,levels = c("Cisco","Yellow Perch","Walleye","Northern Pike","Smallmouth Bass","Bluegill","Black Crappie","Largemouth Bass")), y=warming_impact)) + 
  geom_boxplot(notch=T)+
  theme_bw()+
  ylab("Warming impact")+
  xlab("Species")+
  geom_hline(yintercept = 0, linetype="dashed",color = "red", size=1)+
  theme(axis.text.x = element_text(angle=90,face="bold"),axis.text.y = element_text(face="bold"))+
  ylim(-2,2)

cor.test(impact_explain$initial_depletion,abs(impact_explain$warming_impact))
g3 = ggplot(impact_explain, aes(x=initial_depletion, y=abs(warming_impact))) + 
  geom_point( color="black") +
  geom_smooth(method=lm , color="red", fill="gray", se=TRUE)+
  theme_bw()+
  ylab("Absolute value of warming impact")+
  xlab("Depletion in initial year \n (ratio of biomass to carrying capacity \nin the initial year)")+
  geom_text(label="Cor= -0.10; P<0.05",x=0.7, y=3) +
  scale_y_continuous(limits = c(0, 3))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))+
  labs(tag="C")

g3


