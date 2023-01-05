# ================== Straifying by ancestry ##### 

rm(list=ls())

library(ggplot2)
library(maps)
library(cowplot)
library(patternplot)
library(hrbrthemes)
library(viridis)
library(ggrepel)
library(likert)
library(dplyr)

# ================== Combine datasets all NDDs ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_family_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_family_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_family_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2, datac2, datae2)
dataALL$sex <- "combined"

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Family h2",
                                       "Family c2",
                                       "Family e2"))

dataNDDs <- dataALL
dataNDDs$disorder <- "NDDs combined"

# ================== Combine datasets LD ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/LD/stratified_LD_family_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/LD/stratified_LD_family_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/LD/stratified_LD_family_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2, datac2, datae2)
dataALL$sex <- "combined"

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Family h2",
                                       "Family c2",
                                       "Family e2"))

dataLD <- dataALL
dataLD$disorder <- "Intellectual \nDisabilities"

# ================== Combine datasets CMD ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_family_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_family_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_family_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2, datac2, datae2)
dataALL$sex <- "combined"

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Family h2",
                                       "Family c2",
                                       "Family e2"))

dataCMD <- dataALL
dataCMD$disorder <- "Communication \nDisorders"

# ================== Combine datasets ASD ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_family_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_family_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_family_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2, datac2, datae2)
dataALL$sex <- "combined"

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Family h2",
                                       "Family c2",
                                       "Family e2"))

dataASD <- dataALL
dataASD$disorder <- "ASD"

# ================== Combine datasets ADHD ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_family_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_family_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_family_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2, datac2, datae2)
dataALL$sex <- "combined"

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Family h2",
                                       "Family c2",
                                       "Family e2"))

dataADHD <- dataALL
dataADHD$disorder <- "ADHD"

# ================== Combine datasets SLD ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_family_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_family_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_family_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2, datac2, datae2)
dataALL$sex <- "combined"

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Family h2",
                                       "Family c2",
                                       "Family e2"))

dataSLD <- dataALL
dataSLD$disorder <- "Specific \nLearning \nDisorders"

# ================== Combine datasets MD ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/MD/stratified_MD_family_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/MD/stratified_MD_family_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/MD/stratified_MD_family_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2, datac2, datae2)
dataALL$sex <- "combined"

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Family h2","Family c2","Family e2"))

dataMD <- dataALL

dataMD$disorder <- "Motor \nDisorders"

# ================== Bind All ================== 
datah2 <- rbind(dataNDDs,
                dataLD,
                dataCMD,
                dataASD,
                dataADHD,
                dataSLD,
                dataMD)

# ================== h2 ==================  #####
# ============= Data =============
data <- datah2
data <- data[data$type == "countries" |
               data$type == "ethnicities",]
data <- data[data$sex == "combined",]
data$name <- factor(data$X, levels = c("REM0.5_Australia_h2", "REM0.5_Aus_USA_Nor_Swe_h2", "REM0.5_Canada_h2", 
                                       "REM0.5_China_h2", "REM0.5_Netherlands_h2", "REM0.5_Norway_h2", 
                                       "REM0.5_Sweden_h2", "REM0.5_UK_h2", "REM0.5_USA_h2", "REM0.5_Australia_c2", 
                                       "REM0.5_Aus_USA_Nor_Swe_c2", "REM0.5_Canada_c2", "REM0.5_China_c2", 
                                       "REM0.5_Netherlands_c2", "REM0.5_Norway_c2", "REM0.5_Sweden_c2", 
                                       "REM0.5_UK_c2", "REM0.5_USA_c2", "REM0.5_Australia_e2", "REM0.5_Aus_USA_Nor_Swe_e2", 
                                       "REM0.5_Canada_e2", "REM0.5_China_e2", "REM0.5_Netherlands_e2", 
                                       "REM0.5_Norway_e2", "REM0.5_Sweden_e2", "REM0.5_UK_e2", "REM0.5_USA_e2",
                                       "REM0.5_50_h2", "REM0.5_50_75_h2", "REM0.5_75_100_h2", "REM0.5_100_h2", 
                                       "REM0.5_50_c2", "REM0.5_50_75_c2", "REM0.5_75_100_c2", "REM0.5_100_c2", 
                                       "REM0.5_50_e2", "REM0.5_50_75_e2", "REM0.5_75_100_e2", "REM0.5_100_e2"),
                    labels = c("Australia", "Aus_USA_Nor_Swe", "Canada", 
                               "China", "Netherlands", "Norway", 
                               "Sweden", "UK", "USA", "Australia", 
                               "Aus_USA_Nor_Swe", "Canada", "China", 
                               "Netherlands", "Norway", "Sweden", 
                               "UK", "USA", "Australia", "Aus_USA_Nor_Swe", 
                               "Canada", "China", "Netherlands", 
                               "Norway", "Sweden", "UK", "USA",
                               "Less than 50%","50-74%","75-99%","100%",
                               "Less than 50%","50-74%","75-99%","100%",
                               "Less than 50%","50-74%","75-99%","100%"))

data <- data[data$name != "Aus_USA_Nor_Swe",]

# ============= Plots =============
data <- data[data$type == "ethnicities",]
data$pos <- 0.02
data$col <- "white"
#data <- data[data$component == "Family h2",]
data <- data[complete.cases(data$est),]
data$disorder <- factor(data$disorder, levels = 
                          c("NDDs combined", 
                            "Communication \nDisorders",
                            "ASD", "ADHD",
                            "Specific \nLearning \nDisorders", 
                            "Motor \nDisorders"),
                        labels = c("NDDs combined", 
                                   "Communication disorders",
                                   "ASD", "ADHD",
                                   "Specific learning disorders", 
                                   "Motor disorders"))
data$disorder <- reverse.levels(data$disorder)
data[,2:3] <- data[,2:3] %>% 
  mutate_if(is.numeric, round, digits = 2)

data$estimate <- paste(data$est, data$se, sep = " (")
data$br <- ")"
data$estimate <- paste(data$estimate, data$br, sep = "")

bubble1 <- ggplot(data = data, aes(x=name, y=disorder, size = est, color = est, group = disorder, text=est)) +
  geom_point(alpha=1) +
  scale_size(range = c(2, 8), name="Grand estimate") +
  theme(legend.position="none") +
  #geom_text_repel(aes(label=estimate), size=4, color="black" ) +
  scale_color_manual(values = c("dodgerblue3","firebrick1","orange"),
                     breaks = c("Family h2","Family c2","Family e2"))+
  facet_grid(component~.)+
  scale_color_gradient2(low="#d9ed92", mid = "#76c893", high="#184e77", 
                        midpoint=0.5, limits=c(0,1))+
  theme(legend.direction = "horizontal",
        legend.position="bottom",
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        panel.border = element_rect(linetype = 'solid', colour = 'gray',fill = NA),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid'),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.text.y = element_text(size=10, colour="black", family = "sans", angle = 0), 
        axis.text.x = element_text(size=10, colour="black", family = "sans", angle = 45, hjust = 1),
        #axis.text.x = element_blank(),
        axis.title= element_text(size=10),
        strip.text.x = element_text(size=10, angle = 0),
        strip.text.y = element_text(size=10, angle = 0),
        plot.title = element_text(size=10, angle = 0),
        strip.background = element_rect(colour="black", fill="white"))+
  xlab("")+
  ylab("")
#ggtitle("Heritability and environmental influences on Neurodevelopment Disorders")
