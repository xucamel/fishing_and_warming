setwd("C:/Scratch/LLX/Project")
setwd("C:/Users/user/Desktop/OneDrive/Postdoctor/Project/submission/SA")
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tools)
library(ggmap)
library(RColorBrewer)
library(scales)
library(grid)
library(gridExtra)
library(broom)
library(patchwork)
library(cowplot)
library(stringdist)
library(maps)
library(shape)
library(tidyverse)
library(gridExtra)
library(grid)
library(gridtext)

# Function to change the second letter of species names to lowercase
to_lowercase_second_letter <- function(name) {
  paste0(substr(name, 1, 1), tolower(substr(name, 2, nchar(name))))
}

# figure 2 
# read the parameter estiamtes (rda)
rda_files <- list.files(path = "C:/Users/user/Desktop/OneDrive/Postdoctor/Project/submission/SA/Data", pattern = "\\.rda$", full.names = TRUE)
df_list <- lapply(rda_files, readRDS)
par_est <- abind::abind(df_list , along = 1)

# reading input
input_df = read.csv("Data/all_processed_spm_input_data.csv")


# check the convergency 
split_r = data.frame(split_r = rep(0,dim(par_est)[1]))

for (i in 1:dim(par_est)[1]){
  split_r$split_r[i] = max(par_est[i,,6])
}
sum(split_r$split_r<1.05)


# warming impact figure df
figure_2_df = data.frame(warming_impact = par_est[,8,1], high = par_est[,8,5], low = par_est[,8,4] ,split_r = split_r$split_r, stock_id = unique(input_df$stock_id),initial_depletion = par_est[,4,1])
figure_2_df = left_join(figure_2_df, input_df[,c("stock_id","Common","site_id")], by=c("stock_id"))
figure_2_df = unique(figure_2_df)

figure_2_df$Common <- sapply(figure_2_df$Common, to_lowercase_second_letter)

figure_temp_influence = figure_2_df[figure_2_df$split_r<1.05,]

# get the input data for all 521 stocks that converge
head(input_df)
head(figure_temp_influence)
nrow(figure_temp_influence)

con_input_df = input_df[input_df$stock_id%in%figure_temp_influence$stock_id,]
length(unique(con_input_df$stock_id))
head(con_input_df)

con_input_df <- con_input_df %>%
  rename(
    number_of_recreational_catch_record = num_of_year,
    recreational_catch_kg = estimated_weight_kg,
    number_of_cpue_record= years_with_data_count,
    cpue = mean_cpue_from_sum
  )

con_input_df <- con_input_df %>%
  mutate(
    tri_harvest = replace(tri_harvest, is.na(tri_harvest), 0),
    stocking_weight = replace(stocking_weight, is.na(stocking_weight), 0)
  )

write.csv(con_input_df,"Data/input_data.csv",row.names = FALSE)
# plot
# warming significant
# reorder 
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
  geom_text(aes(x = 2.8, y = 60, label = "102\n(20%)"), size = 3.5, hjust = 0, color = "blue") +
  geom_text(aes(x = -2.8, y = 500, label = "69 \n(13%)"), size = 3.5, hjust = 1, color = "red") +
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
        panel.grid = element_blank())
  geom_point(size = 2, shape = 19)
  g1 <- g1 + 
    labs(tag = "A") +
    theme(plot.tag = element_text(size = 9, face = "bold"))
# jpeg("Output/Figure/Fig_g1.jpeg",width=4,height=9,units = "in",res=600)
# g1
# dev.off()
# warming vs species
impact_explain = figure_2_df[figure_2_df$split_r<1.05,]

g2 = ggplot(impact_explain, aes(x=factor(Common,levels = c("Cisco","Yellow perch","Walleye","Northern pike","Smallmouth bass","Bluegill","Black crappie","Largemouth bass")), y=warming_impact)) + 
  geom_boxplot(notch=F) +
  theme_classic() +
  ylab("Warming impact") +
  xlab("Species") +
  geom_hline(yintercept = 0, linetype="dashed", color = "red", size=1) +
  theme(axis.text.x = element_text(angle=90, face="bold", hjust=1),
        axis.text.y = element_text(face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  ylim(-2,2) +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 1.5, xmax = 4.5, ymin = -Inf, ymax = Inf, fill = "springgreen1", alpha = 0.1) +
  annotate("rect", xmin = 4.5, xmax = 8.5, ymin = -Inf, ymax = Inf, fill = "coral", alpha = 0.1)

g2 <- g2 + 
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 9, face = "bold"))

cor.test(impact_explain$initial_depletion,abs(impact_explain$warming_impact))

g3 = ggplot(impact_explain, aes(x=initial_depletion, y=abs(warming_impact))) + 
  geom_point( color="black") +
  geom_smooth(method=lm , color="red", fill="gray", se=TRUE)+
  theme_bw()+
  ylab("Absolute value of warming impact")+
  xlab("Depletion in initial year \n (ratio of biomass to carrying capacity \nin the initial year)")+
  geom_text(label="Cor= -0.14; P<0.01",x=0.6, y=3) +
  scale_y_continuous(limits = c(0, 3))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))

g3 <- g3 + 
  labs(tag = "C") +
  theme(plot.tag = element_text(size = 9, face = "bold"))

# Separate positive and negative values of warming_impact
impact_explain$warming_category <- ifelse(impact_explain$warming_impact >= 0, "Positive", "Negative")

# Calculate linear model summaries for positive and negative warming impacts
positive_model <- lm(warming_impact ~ initial_depletion, data = subset(impact_explain, warming_impact >= 0))
negative_model <- lm(warming_impact ~ initial_depletion, data = subset(impact_explain, warming_impact < 0))

# Extract coefficients and p-values
positive_slope <- round(coef(positive_model)[2], 2)
positive_p_value <- round(summary(positive_model)$coefficients[2, 4], 3)

negative_slope <- round(coef(negative_model)[2], 2)
negative_p_value <- round(summary(negative_model)$coefficients[2, 4], 3)

# Plot with separate smoothing lines for positive and negative y values
# g3_a <- ggplot(impact_explain, aes(x = initial_depletion, y = warming_impact)) + 
#   geom_point(color = "black") +
#   geom_smooth(data = subset(impact_explain, warming_impact >= 0), 
#               aes(x = initial_depletion, y = warming_impact), 
#               method = "lm", color = "blue", fill = "lightblue", se = TRUE) +
#   geom_smooth(data = subset(impact_explain, warming_impact < 0), 
#               aes(x = initial_depletion, y = warming_impact), 
#               method = "lm", color = "red", fill = "lightpink", se = TRUE) +
#   theme_bw() +
#   ylab("Warming impact") +
#   xlab("Stock status in initial year \n (ratio of biomass to carrying capacity \n in the initial year)") +
#   # Add text for coefficients and p-values
#   geom_text(label = paste0("Slope = ", positive_slope, "\nP_value = ", positive_p_value),
#             x = 0.5, y = 2, color = "blue", hjust = 0) +
#   geom_text(label = paste0("Slope = ", negative_slope, "\nP_value = ", negative_p_value),
#             x = 0.5, y = -2, color = "red", hjust = 0) +
#   theme(axis.text.x = element_text(face = "bold"),
#         axis.text.y = element_text(face = "bold"),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.border = element_rect(color = "black", fill = NA, size = 0.5))+
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black")
# 
# g3_a
# 
# jpeg("Output/Figure/Fig1_initial_depletion_with_impact_warming_two_directions.jpeg",width=6,height=6,units = "in",res=600)
# g3_a
# dev.off()

combined_plot_right <- plot_grid(g2, g3, ncol = 1,rel_heights = c(1, 1), rel_widths = c(1, 1))
combined_plot = plot_grid(g1, combined_plot_right , ncol = 2,rel_heights = c(1, 1), rel_widths = c(1, 1))

jpeg("Figure/Figure2.jpeg",width=183,height=183,units = "mm",res=600)
combined_plot 
dev.off()

pdf("Figure/Figure2.pdf", width = 7.2, height = 7.2)  # Adjust size as needed
plot(combined_plot)  # or ggplot(...)
dev.off()

#### warming impact spatial distribution ####
lake_meta_df = read.csv("Data/lake_metadata.csv")
lake_meta_df$site_id <- gsub("nhdhr_", "", lake_meta_df$site_id)

# Identify site_ids in impact_explain that are not in lake_meta_df
missing_site_ids <- setdiff(impact_explain$site_id, lake_meta_df$site_id)

# Print the missing site_ids
missing_site_ids

impact_explain = left_join(impact_explain,lake_meta_df,"site_id")

# map 
wi <- map_data("state")

# Custom labeller function
impact_explain$species_classification <- ifelse(impact_explain$Common == "Cisco", 
                                                "Cold-water", 
                                                ifelse(impact_explain$Common %in% c("Bluegill", "Black Crappie", "Largemouth Bass", "Smallmouth Bass"), 
                                                       "Warm-water", 
                                                       "Cool-water"))


sig_warm = impact_explain[impact_explain$high<0|impact_explain$low>0,]
impact_explain_1 = impact_explain
impact_explain_1[impact_explain_1$warming_impact>2,]$warming_impact = 2
impact_explain_1[impact_explain_1$warming_impact<(-2),]$warming_impact = -2
g1 <- ggplot() +
  # US states background
  geom_polygon(data = wi, aes(long, lat, group = group), fill = "gray", color = "black") +
  
  # Points for warming impact
  geom_point(data = impact_explain_1, 
             aes(x = centroid_lon, y = centroid_lat, color = warming_impact), 
             size = 1) +
  
  # Coordinate limits
  coord_fixed(ratio = 1, xlim = c(-98, -89), ylim = c(42.5, 50)) +
  
  # Facet wrap for species
  facet_wrap(~factor(Common, levels = c("Cisco", 
                                        "Yellow perch", 
                                        "Walleye", 
                                        "Northern pike", 
                                        "Smallmouth bass", 
                                        "Bluegill", 
                                        "Black crappie", 
                                        "Largemouth bass")), 
             nrow = 2) +
  
  # Axis labels
  xlab("Longitude (°W)") +
  ylab("Latitude (°N)") +
  
  # Viridis color scale for better scientific visualization
  scale_color_viridis_c(
    option = "C",   # Use a balanced, perceptually uniform palette
    name = "Warming impact",
    limits = c(-2, 2),
    breaks = c(-2, -1, 0, 1, 2),
    labels = c("<-2", "-1", "0", "1", ">2") # Modify endpoints to "<-2" and ">2"
  )+
  
  # Clean theme for scientific papers
  theme_minimal(base_size = 10) +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = "right",
    strip.text = element_text(size = 10)
  )

jpeg("Figure/Figure3.jpeg",width=183,height=100,units = "mm",res=600)
g1
dev.off()

pdf("Figure/Figure3.pdf", width = 18.3/2.54, height = 10.0/2.54)  # Adjust size as needed
plot(g1)  # or ggplot(...)
dev.off()
#####

# Calculate the number of positive warming impacts and the percentage
impact_summary <- impact_explain %>%
  group_by(species_classification) %>%
  summarize(
    total_species = n(),
    positive_warming_impact = sum(warming_impact < 0),
    percentage_positive = (positive_warming_impact / total_species) * 100
  )
impact_summary 

#### the arrow figure ####
rda_files <- list.files(path = "C:/Users/user/Desktop/OneDrive/Postdoctor/Project/submission/SA/Data", pattern = "Final_quantities.*\\.csv$", full.names = TRUE)
df_list <- lapply(rda_files, read.csv)
quan_est <- abind::abind(df_list , along = 1)
quan_est <- as.data.frame(quan_est)
quan_est$stock_id = figure_2_df$stock_id

arrow_df = left_join(impact_explain,quan_est,"stock_id")

arrow_df$P_terminal <- as.numeric(as.character(arrow_df$P_terminal))
arrow_df$P_no_fishing <- as.numeric(as.character(arrow_df$P_no_fishing))
arrow_df$P_no_env <- as.numeric(as.character(arrow_df$P_no_env))

# the mean depletion rate
mean_depletion = arrow_df %>%
  group_by(Common) %>%
  summarize(mean_dep = median(P_terminal, na.rm = TRUE))

mean_depletion 

# number of stocks for each species
arrow_df %>%
  group_by(Common) %>%
  summarise(stock_count = n_distinct(stock_id))
# plot and figure arrangement
p_we <- 
  arrow_df %>%
  filter(Common=="Walleye") %>%
  # Reorder stock_id based on P_terminal for each species
  group_by(Common) %>%
  mutate(stock_id = factor(stock_id, levels = stock_id[order(P_terminal)])) %>%
  ggplot( aes(x=P_terminal, y=stock_id)) +
  geom_point()+
  facet_grid(rows=vars(Common), scale="free_y",space = "free_y",switch = "both") +
  geom_segment(aes(x=P_terminal,y=stock_id,xend=P_no_fishing,yend=stock_id),colour="blue",arrow=arrow(type = "closed",length = unit(0.035, "inches")))+
  geom_segment(aes(x=P_terminal,y=stock_id,xend=P_no_env,yend=stock_id),colour="red",arrow=arrow(type = "closed",length = unit(0.035, "inches")))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank())+
  xlab(label="")+
  ylab(label="")+xlim(0,1)+
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.25,0.5,0.75, 1), labels = c("0", "0.25","0.5","0.75", "1")) #+geom_vline(xintercept = 0.264, linetype = "dashed") 

p_np <- 
  arrow_df %>%
  filter(Common=="Northern pike") %>%
  # Reorder stock_id based on P_terminal for each species
  group_by(Common) %>%
  mutate(stock_id = factor(stock_id, levels = stock_id[order(P_terminal)])) %>%
  ggplot( aes(x=P_terminal, y=stock_id)) +
  geom_point()+
  facet_grid(rows=vars(Common), scale="free_y",space = "free_y",switch = "both")+
  geom_segment(aes(x=P_terminal,y=stock_id,xend=P_no_fishing,yend=stock_id),colour="blue",arrow=arrow(type = "closed",length = unit(0.035, "inches")))+
  geom_segment(aes(x=P_terminal,y=stock_id,xend=P_no_env,yend=stock_id),colour="red",arrow=arrow(type = "closed",length = unit(0.035, "inches")))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank())+
  xlab(label="")+
  ylab(label="")+xlim(0,1)+
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.25,0.5,0.75, 1), labels = c("0", "0.25","0.5","0.75", "1"))

p_yp_cc <- 
  arrow_df %>%
  filter(Common %in% c("Yellow perch", "Cisco")) %>%
  # Reorder stock_id based on P_terminal for each species
  group_by(Common) %>%
  mutate(stock_id = factor(stock_id, levels = stock_id[order(P_terminal)])) %>%
  ggplot(aes(x = P_terminal, y = stock_id)) +
  geom_point() +
  facet_grid(rows = vars(Common), scale = "free_y", space = "free_y", switch = "both") +
  geom_segment(aes(x = P_terminal, y = stock_id, xend = P_no_fishing, yend = stock_id), colour = "blue", arrow = arrow(type = "closed", length = unit(0.035, "inches"))) +
  geom_segment(aes(x = P_terminal, y = stock_id, xend = P_no_env, yend = stock_id), colour = "red", arrow = arrow(type = "closed", length = unit(0.035, "inches"))) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(),  # remove y axis labels
        axis.ticks.y = element_blank()) +
  xlab(label = "") +
  ylab(label = "") + xlim(0, 1) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.5", "0.75", "1")) 
#+geom_vline(data = . %>% mutate(vline_x = ifelse(Common == "Black crappie", 0.231, ifelse(Common == "Cisco", 0.16, NA))), aes(xintercept = vline_x), linetype = "dashed")

p_bc_bg <- 
  arrow_df %>%
  filter(Common %in% c("Black crappie", "Bluegill")) %>%
  # Reorder stock_id based on P_terminal for each species
  group_by(Common) %>%
  mutate(stock_id = factor(stock_id, levels = stock_id[order(P_terminal)])) %>%
  ggplot(aes(x = P_terminal, y = stock_id)) +
  geom_point() +
  facet_grid(rows = vars(Common), scale = "free_y", space = "free_y", switch = "both") +
  geom_segment(aes(x = P_terminal, y = stock_id, xend = P_no_fishing, yend = stock_id), colour = "blue", arrow = arrow(type = "closed", length = unit(0.035, "inches"))) +
  geom_segment(aes(x = P_terminal, y = stock_id, xend = P_no_env, yend = stock_id), colour = "red", arrow = arrow(type = "closed", length = unit(0.035, "inches"))) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(),  # remove y axis labels
        axis.ticks.y = element_blank()) +
  xlab(label = "") +
  ylab(label = "") + xlim(0, 1) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.5", "0.75", "1"))


p_lb_sb <- 
  arrow_df %>%
  filter(Common %in% c("Largemouth bass", "Smallmouth bass")) %>%
  # Reorder stock_id based on P_terminal for each species
  group_by(Common) %>%
  mutate(stock_id = factor(stock_id, levels = stock_id[order(P_terminal)])) %>%
  ggplot(aes(x = P_terminal, y = stock_id)) +
  geom_point() +
  facet_grid(rows = vars(Common), scale = "free_y", space = "free_y", switch = "both") +
  geom_segment(aes(x = P_terminal, y = stock_id, xend = P_no_fishing, yend = stock_id), colour = "blue", arrow = arrow(type = "closed", length = unit(0.035, "inches"))) +
  geom_segment(aes(x = P_terminal, y = stock_id, xend = P_no_env, yend = stock_id), colour = "red", arrow = arrow(type = "closed", length = unit(0.035, "inches"))) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(),  # remove y axis labels
        axis.ticks.y = element_blank(),
        strip.text.x = element_text(margin = margin(b = 0, t = 0))) +
  xlab(label = "") +
  ylab(label = "") + xlim(0, 1) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.5", "0.75", "1")) 


## arrange plot arrow
# Create the custom title with arrows
top_with_arrows <- gTree(children = gList(
  textGrob("No warming", x = 0.35, y = 0.2, just = "center"),
  textGrob("No fishing", x = 0.65, y = 0.2, just = "center"),
  segmentsGrob(x0 = 0.4, x1 = 0.45, y0 = 0.6, y1 = 0.6, 
               arrow = arrow(type = "closed", length = unit(0.035, "inches")), 
               gp = gpar(col = "red")),
  segmentsGrob(x0 = 0.45, x1 = 0.4, y0 = 0.0, y1 = 0.0, 
               arrow = arrow(type = "closed", length = unit(0.035, "inches")), 
               gp = gpar(col = "red")),
  segmentsGrob(x0 = 0.695, x1 = 0.74, y0 = 0.25, y1 = 0.25, 
               arrow = arrow(type = "closed", length = unit(0.035, "inches")), 
               gp = gpar(col = "blue"))
))

# Now arrange your plots with the custom title
g_arrow <- grid.arrange(
  p_we, p_np, p_yp_cc, p_bc_bg, p_lb_sb,
  #layout_matrix = layout_matrix, 
  widths = c(0.3, 0.3, 0.3, 0.3, 0.3),
  bottom = "Stock status in terminal year\n(ratio of biomass to carrying capacity in the terminal year)", 
  top = top_with_arrows  # Add the custom top legend with arrows
)

plotdir <- "Figure"
ggsave(g_arrow, filename=file.path(plotdir, "Fig_S13.png"), 
       width=10, height=10, units="in", dpi=600)
#####

#### the four category ####
# Format data
fig_4_data <- arrow_df %>% 
  # Calculate fishing effect
  mutate(effect_fishing=(P_terminal-P_no_fishing)*-1,
         effect_warming=(P_terminal-P_no_env)*-1)

fig_4_data = fig_4_data %>%
  mutate(ccol="red")

fig_4_data[fig_4_data$effect_warming>0&fig_4_data$effect_fishing>fig_4_data$effect_warming,]$ccol = "coral"

fig_4_data[fig_4_data$effect_warming<0&fig_4_data$effect_fishing>abs(fig_4_data$effect_warming),]$ccol = "blue"

fig_4_data[fig_4_data$effect_fishing>0&fig_4_data$effect_warming<0&fig_4_data$effect_fishing<abs(fig_4_data$effect_warming),]$ccol = "darkgreen"

# Calculate the count and percentage for each category
category_summary <- fig_4_data %>%
  group_by(ccol) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Display the result
category_summary

figure4=ggplot(fig_4_data, aes(x=effect_fishing*(-1),y=effect_warming*(-1),col=ccol)) +
  geom_point(aes(col=ccol)) +
  # Reference lines
  geom_hline(yintercept=0, color="black", linetype = "dashed") +
  geom_abline(slope=-1, color="black", linetype = "dashed") +
  geom_abline(slope=1, color="black", linetype = "dashed") +
  # Label quadrants
  annotate(geom="text", x=-0.1, y=-0.4, 
           label="Warming \ndepletes the population \nmore than fishing\n1%", color="red") +
  annotate(geom="text", x=-0.6, y=-0.4, 
           label="Fishing depletes the\npopulation more\n than warming\n48%", color="coral3") +
  annotate(geom="text", x=-0.1, y=0.4, 
           label="Warming \nfully offsets fishing\n7%", color="blue") +
  annotate(geom="text", x=-0.6, y=0.4, 
           label="Warming \npartially offsets fishing\n44%", color="darkgreen") +
  # Labels
  labs(x="Contribution of fishing\nto biomass change", 
       y="Contribution of warming\nto biomass change") +
  # Theme
  theme_bw() +
  xlim(-0.7, 0) +
  ylim(-0.5, 0.5) +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_color_manual(values=c("darkgreen", "coral3","blue", "red"))
# Plot data
jpeg("Figure/Figure4.jpeg", width=6, height=6, res=600, units = "in")
figure4
dev.off()

pdf("Figure/Figure4.pdf", width = 6, height = 6)  # Adjust size as needed
plot(figure4)  # or ggplot(...)
dev.off()
#####

#### how many population is warming and how many is cooling ####
trend_gdd_df = input_df[input_df$stock_id%in%impact_explain$stock_id,]

# number of stocks
Nstocks = length(unique(trend_gdd_df$stock_id))

# output container 
output_trend_gdd = data.frame(stock_id = unique(trend_gdd_df$stock_id), gdd_trend=NA )

for (i in 1:Nstocks){
  ## model input 
  stock_i = unique(trend_gdd_df$stock_id)[i]
  data_i = trend_gdd_df[trend_gdd_df$stock_id==stock_i,]
  min_year_i = min(data_i$year[which(data_i$mean_cpue_from_sum>0)],data_i$year[which(data_i$estimated_weight_kg>0)])
  max_year_i = max(data_i$year[which(data_i$mean_cpue_from_sum>0)],data_i$year[which(data_i$estimated_weight_kg>0)])
  env_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]
  model <- glm(env_i$gdd_wtr_5c ~ env_i$Year)
  
  # Extract the coefficient for 'Year'
  year_coef <- coef(model)["env_i$Year"]
  output_trend_gdd$gdd_trend[i] = year_coef 
  
  print(i)
}
sum(output_trend_gdd$gdd_trend<0)
#####

#### figure 1 map and the gdd trend ####
site_id_map_df = lake_meta_df[lake_meta_df$site_id%in%impact_explain$site_id,]
wi <- map_data("state")
ca <- map_data("world", region = c("Canada", "Great Lakes"))
base_theme <-  theme(axis.text=element_text(size=11),
                     axis.text.y = element_text(angle = 90, hjust = 0.5),
                     axis.title=element_text(size=11),
                     strip.text = element_text(size=11),
                     plot.tag = element_text(size=11),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)))
g1 = ggplot() +
  geom_polygon(data = ca, aes(long, lat, group = group, fill = region), color = "black") +
  geom_polygon(data = wi, aes(long, lat, group = group), fill = "gray90", color = "black", xlim = c(-100, -85), ylim = c(40, 50)) +
  geom_point(data = site_id_map_df, aes(centroid_lon, centroid_lat), color = 'black', shape = 2, size = 1) +
  coord_fixed(1.3, xlim = c(-97, -88), ylim = c(43, 50)) +
  xlab("Longitude (°W)") +
  ylab("Latitude (°N)") +
  labs(tag = "A") +
  theme_bw() +
  scale_fill_manual(values = c("Canada" = "gray90", "Great Lakes" = "white")) +
  base_theme+
  theme(legend.position="none")+
  geom_text(aes(x = -96, y = 48.5, label = "MN"), size = 3, hjust = 0.5, vjust = 0.5)+
  geom_text(aes(x = -90, y = 43, label = "WI"), size = 3, hjust = 0.5, vjust = 0.5)+
  #geom_text(aes(x = -97.5, y = 43, label = "SD"), size = 3, hjust = 0.5, vjust = 0.5)+
  geom_text(aes(x = -95.5, y = 49.5, label = "Canada"), size = 3, hjust = 0.5, vjust = 0.5)

## g 2

gdd_trend_df = input_df[,c("Year","gdd_wtr_5c","site_id")]

gdd_year = unique(gdd_trend_df)

# Fit the linear model
lm_model <- lm(gdd_wtr_5c ~ Year, data = gdd_year)
cor.test(gdd_year$gdd_wtr_5c,gdd_year$Year)

# Extract the linear trend coefficient and p-value
coef_value <- coef(lm_model)["Year"]
p_value <- summary(lm_model)$coefficients["Year", "Pr(>|t|)"]

# Create the plot and add annotations for the coefficient and p-value
g2 <- ggplot(gdd_year, aes(x = Year, y = gdd_wtr_5c, group = Year)) +
  geom_boxplot(outlier.size = 0.5) +
  labs(x = "Year", y = "Growing degree days (°C)", tag = "B") +
  geom_smooth(method = lm, color = "coral", fill = "black", se = TRUE, aes(group = 1), size = 0.5) +
  annotate("text", x = 2005, y = 3500, 
           label = paste0("Cor = ", 0.14, "; P < 0.01 "), 
           hjust = 0, color = "black") +
  theme_bw() + base_theme +
  scale_x_continuous(limits = c(1989.5, 2021.5), breaks = seq(1990, 2020, by = 5))

# Display the plot
g2

# Export plot
# ggsave(g1, filename=file.path(plotdir, "Fig1_site_map.png"), 
#        width=80, height=80, units="mm", dpi=600)
# 
# ggsave(g2, filename=file.path(plotdir, "Fig1_gdd_trend.png"), 
#        width=80, height=80, units="mm", dpi=600)



combined_plot = plot_grid(g1, g2 , ncol = 2,rel_heights = c(1, 1), rel_widths = c(1, 1))
ggsave(combined_plot, filename=file.path(plotdir, "Figure1.png"), 
       width=183, height=90, units="mm", dpi=600)

pdf("Figure/Figure1.pdf", width = 18.3/2.54, height = 9.0/2.54)  # Adjust size as needed
plot(combined_plot)  # or ggplot(...)
dev.off()

#####
