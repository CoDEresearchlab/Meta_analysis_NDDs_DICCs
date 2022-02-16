##### Ages family #####
# ================== Straifying all NDDs by covariates ##### 
library(ggplot2)
library(maps)
library(cowplot)
library(patternplot)

# ================== Combine datasets ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_family_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_family_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_family_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2, datac2, datae2)
dataALL$sex <- "combined"

datah2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_family_males_h2.csv")
datac2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_family_males_c2.csv")
datae2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_family_males_e2.csv")

datah2M$component <- "h2"
datac2M$component <- "c2"
datae2M$component <- "e2"

dataM <- rbind(datah2M,datac2M,datae2M)

datah2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_family_females_h2.csv")
datac2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_family_females_c2.csv")
datae2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_family_females_e2.csv")

datah2F$component <- "h2"
datac2F$component <- "c2"
datae2F$component <- "e2"

dataF <- rbind(datah2F,datac2F,datae2F)

dataMF <- rbind(dataM,dataF)

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Heritability",
                                       "Shared environment",
                                       "Nonshared environment"))

dataMF$sig <- factor(dataMF$sig, levels = c("sig","nosig"),
                     labels = c("Significant moderation",
                                "Non-significant moderation"))
dataMF$star <- with(dataMF, ifelse(dataMF$sig == "Significant moderation", "*",NA))
dataMF$component <- factor(dataMF$component, levels = c("h2","c2","e2"),
                           labels = c("Heritability",
                                      "Shared environment",
                                      "Nonshared environment"))

# ================== Age combined ==================  #####
# ============= Data =============

data <- dataALL[dataALL$type == "ages",]
data <- data[data$sex == "combined",]
data$name <- factor(data$X, levels = c("REM0.5_childhood_h2", "REM0.5_middlechildhood_h2", "REM0.5_adolescence_h2", 
                                       "REM0.5_childhood_middlechildhood_h2", "REM0.5_childhood_adolescence_h2", 
                                       "REM0.5_middlechildhood_adolescence_h2", "REM0.5_childhood_c2", 
                                       "REM0.5_middlechildhood_c2", "REM0.5_adolescence_c2", "REM0.5_childhood_middlechildhood_c2", 
                                       "REM0.5_childhood_adolescence_c2", "REM0.5_middlechildhood_adolescence_c2", 
                                       "REM0.5_childhood_e2", "REM0.5_middlechildhood_e2", "REM0.5_adolescence_e2", 
                                       "REM0.5_childhood_middlechildhood_e2", "REM0.5_childhood_adolescence_e2", 
                                       "REM0.5_middlechildhood_adolescence_e2"),
                    labels = c("Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence"))

data$disorder <- "ALL"

# ============= Plots =============

dataALLNDDs <- data[data$name == "Childhood" | 
                      data$name == "Middle childhood" | 
                      data$name == "Adolescence",]

dataALLNDDs$disorder <- "ALL"

ageALLNDDs = ggplot(data=dataALLNDDs,
                    aes(x = name, y = est, group = component, color = component))+
  geom_line(color="black") +
  geom_point(size=7, stroke=1) +
  scale_shape_manual(values = c(16,0))+
  #geom_pointrange(aes(ymin = est-se, ymax = est+se, color = component))+
  #geom_hline(aes(fill=ref),yintercept = c(0,0.29), linetype=2)+
  xlab('')+ ylab("Grand estimate")+
  ggtitle("NDDs combined")+
  geom_errorbar(aes(ymin = est-se, ymax = est+se, linetype="solid"),width=0.1, size=1)+ 
  #facet_grid(grand~., scales= "free", space="free", rows = vars(grand))+
  #facet_grid(rows = vars(grand), space = "free", scales = "free")+
  theme(legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        strip.text.y = element_text(size=15, angle = 0),
        plot.title=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=15),
        panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"))+
  #expand_limits(y=c(-3,5))+
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2))+
  guides(color=guide_legend(nrow=3,byrow=TRUE))+
  guides(shape=guide_legend(nrow=2,byrow=TRUE))+
  guides(linetype=F)+
  #scale_color_brewer(palette = 4)+
  #scale_fill_brewer(palette = 4)
  scale_color_manual(values = c("#FF5666","black","#759fbc"),labels = c("Heritability",
                                                                        "Shared environment",
                                                                        "Nonshared environment"))

ageALLNDDs

# ================== Straifying CMD by covariates ##### 



library(ggplot2)
library(maps)
library(cowplot)
library(patternplot)

# ================== Combine datasets ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_family_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_family_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_family_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2, datac2, datae2)
dataALL$sex <- "combined"

datah2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_family_males_h2.csv")
datac2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_family_males_c2.csv")
datae2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_family_males_e2.csv")

datah2M$component <- "h2"
datac2M$component <- "c2"
datae2M$component <- "e2"

dataM <- rbind(datah2M,datac2M,datae2M)

datah2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_family_females_h2.csv")
datac2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_family_females_c2.csv")
datae2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_family_females_e2.csv")

datah2F$component <- "h2"
datac2F$component <- "c2"
datae2F$component <- "e2"

dataF <- rbind(datah2F,datac2F,datae2F)

dataMF <- rbind(dataM,dataF)

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Heritability",
                                       "Shared environment",
                                       "Nonshared environment"))

dataMF$sig <- factor(dataMF$sig, levels = c("sig","nosig"),
                     labels = c("Significant moderation",
                                "Non-significant moderation"))
dataMF$star <- with(dataMF, ifelse(dataMF$sig == "Significant moderation", "*",NA))
dataMF$component <- factor(dataMF$component, levels = c("h2","c2","e2"),
                           labels = c("Heritability",
                                      "Shared environment",
                                      "Nonshared environment"))

# ================== Age combined ==================  #####
# ============= Data =============

data <- dataALL[dataALL$type == "ages",]
data <- data[data$sex == "combined",]
data$name <- factor(data$X, levels = c("REM0.5_childhood_h2", "REM0.5_middlechildhood_h2", "REM0.5_adolescence_h2", 
                                       "REM0.5_childhood_middlechildhood_h2", "REM0.5_childhood_adolescence_h2", 
                                       "REM0.5_middlechildhood_adolescence_h2", "REM0.5_childhood_c2", 
                                       "REM0.5_middlechildhood_c2", "REM0.5_adolescence_c2", "REM0.5_childhood_middlechildhood_c2", 
                                       "REM0.5_childhood_adolescence_c2", "REM0.5_middlechildhood_adolescence_c2", 
                                       "REM0.5_childhood_e2", "REM0.5_middlechildhood_e2", "REM0.5_adolescence_e2", 
                                       "REM0.5_childhood_middlechildhood_e2", "REM0.5_childhood_adolescence_e2", 
                                       "REM0.5_middlechildhood_adolescence_e2"),
                    labels = c("Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence"))

data$disorder <- "CMD"

# ============= Plots =============

dataCMD <- data[data$name == "Childhood" |
                  data$name == "Adolescence",]

ageCMD = ggplot(data=dataCMD,
                aes(x = name, y = est, group = component, color = component, na.omit = T))+
  geom_line(color="black") +
  geom_point(size=7, stroke=1) +
  scale_shape_manual(values = c(16,0))+
  #geom_pointrange(aes(ymin = est-se, ymax = est+se, color = component))+
  #geom_hline(aes(fill=ref),yintercept = c(0,0.29), linetype=2)+
  xlab('')+ ylab("Grand estimate")+
  ggtitle("Communication Disorders")+
  geom_errorbar(aes(ymin = est-se, ymax = est+se, linetype="solid"),width=0.1, size=1)+ 
  #facet_grid(grand~., scales= "free", space="free", rows = vars(grand))+
  #facet_grid(rows = vars(grand), space = "free", scales = "free")+
  theme(legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        strip.text.y = element_text(size=15, angle = 0),
        plot.title=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=15),
        panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"))+
  #expand_limits(y=c(-3,5))+
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2))+
  guides(color=guide_legend(nrow=3,byrow=TRUE))+
  guides(shape=guide_legend(nrow=2,byrow=TRUE))+
  guides(linetype=F)+
  #scale_color_brewer(palette = 4)+
  #scale_fill_brewer(palette = 4)
  scale_color_manual(values = c("#FF5666","black","#759fbc"),labels = c("Heritability",
                                                                        "Shared environment",
                                                                        "Nonshared environment"))

ageCMD

# ================== Straifying ASD by covariates ##### 



library(ggplot2)
library(maps)
library(cowplot)
library(patternplot)

# ================== Combine datasets ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_family_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_family_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_family_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2, datac2, datae2)
dataALL$sex <- "combined"

datah2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_family_males_h2.csv")
datac2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_family_males_c2.csv")
datae2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_family_males_e2.csv")

datah2M$component <- "h2"
datac2M$component <- "c2"
datae2M$component <- "e2"

dataM <- rbind(datah2M,datac2M,datae2M)

datah2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_family_females_h2.csv")
datac2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_family_females_c2.csv")
datae2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_family_females_e2.csv")

datah2F$component <- "h2"
datac2F$component <- "c2"
datae2F$component <- "e2"

dataF <- rbind(datah2F,datac2F,datae2F)

dataMF <- rbind(dataM,dataF)

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Heritability",
                                       "Shared environment",
                                       "Nonshared environment"))

dataMF$sig <- factor(dataMF$sig, levels = c("sig","nosig"),
                     labels = c("Significant moderation",
                                "Non-significant moderation"))
dataMF$star <- with(dataMF, ifelse(dataMF$sig == "Significant moderation", "*",NA))
dataMF$component <- factor(dataMF$component, levels = c("h2","c2","e2"),
                           labels = c("Heritability",
                                      "Shared environment",
                                      "Nonshared environment"))

# ================== Age combined ==================  #####
# ============= Data =============

data <- dataALL[dataALL$type == "ages",]
data <- data[data$sex == "combined",]
data$name <- factor(data$X, levels = c("REM0.5_childhood_h2", "REM0.5_middlechildhood_h2", "REM0.5_adolescence_h2", 
                                       "REM0.5_childhood_middlechildhood_h2", "REM0.5_childhood_adolescence_h2", 
                                       "REM0.5_middlechildhood_adolescence_h2", "REM0.5_childhood_c2", 
                                       "REM0.5_middlechildhood_c2", "REM0.5_adolescence_c2", "REM0.5_childhood_middlechildhood_c2", 
                                       "REM0.5_childhood_adolescence_c2", "REM0.5_middlechildhood_adolescence_c2", 
                                       "REM0.5_childhood_e2", "REM0.5_middlechildhood_e2", "REM0.5_adolescence_e2", 
                                       "REM0.5_childhood_middlechildhood_e2", "REM0.5_childhood_adolescence_e2", 
                                       "REM0.5_middlechildhood_adolescence_e2"),
                    labels = c("Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence"))

data$disorder <- "ASD"

# ============= Plots =============

dataASD <- data[data$name == "Childhood" | 
                  data$name == "Middle childhood" | 
                  data$name == "Adolescence",]

ageASD = ggplot(data=dataASD,
                aes(x = name, y = est, group = component, color = component))+
  geom_line(color="black") +
  geom_point(size=7, stroke=1) +
  scale_shape_manual(values = c(16,0))+
  #geom_pointrange(aes(ymin = est-se, ymax = est+se, color = component))+
  #geom_hline(aes(fill=ref),yintercept = c(0,0.29), linetype=2)+
  xlab('')+ ylab("Grand estimate")+
  ggtitle("ASD")+
  geom_errorbar(aes(ymin = est-se, ymax = est+se, linetype="solid"),width=0.1, size=1)+ 
  #facet_grid(grand~., scales= "free", space="free", rows = vars(grand))+
  #facet_grid(rows = vars(grand), space = "free", scales = "free")+
  theme(legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        strip.text.y = element_text(size=15, angle = 0),
        plot.title=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=15),
        panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"))+
  #expand_limits(y=c(-3,5))+
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2))+
  guides(color=guide_legend(nrow=3,byrow=TRUE))+
  guides(shape=guide_legend(nrow=2,byrow=TRUE))+
  guides(linetype=F)+
  #scale_color_brewer(palette = 4)+
  #scale_fill_brewer(palette = 4)
  scale_color_manual(values = c("#FF5666","black","#759fbc"),labels = c("Heritability",
                                                                        "Shared environment",
                                                                        "Nonshared environment"))

ageASD

# ================== Straifying ADHD by covariates ##### 



library(ggplot2)
library(maps)
library(cowplot)
library(patternplot)

# ================== Combine datasets ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_family_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_family_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_family_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2, datac2, datae2)
dataALL$sex <- "combined"

datah2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_family_males_h2.csv")
datac2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_family_males_c2.csv")
datae2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_family_males_e2.csv")

datah2M$component <- "h2"
datac2M$component <- "c2"
datae2M$component <- "e2"

dataM <- rbind(datah2M,datac2M,datae2M)

datah2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_family_females_h2.csv")
datac2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_family_females_c2.csv")
datae2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_family_females_e2.csv")

datah2F$component <- "h2"
datac2F$component <- "c2"
datae2F$component <- "e2"

dataF <- rbind(datah2F,datac2F,datae2F)

dataMF <- rbind(dataM,dataF)

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Heritability",
                                       "Shared environment",
                                       "Nonshared environment"))

dataMF$sig <- factor(dataMF$sig, levels = c("sig","nosig"),
                     labels = c("Significant moderation",
                                "Non-significant moderation"))
dataMF$star <- with(dataMF, ifelse(dataMF$sig == "Significant moderation", "*",NA))
dataMF$component <- factor(dataMF$component, levels = c("h2","c2","e2"),
                           labels = c("Heritability",
                                      "Shared environment",
                                      "Nonshared environment"))

# ================== Age combined ==================  #####
# ============= Data =============

data <- dataALL[dataALL$type == "ages",]
data <- data[data$sex == "combined",]
data$name <- factor(data$X, levels = c("REM0.5_childhood_h2", "REM0.5_middlechildhood_h2", "REM0.5_adolescence_h2", 
                                       "REM0.5_childhood_middlechildhood_h2", "REM0.5_childhood_adolescence_h2", 
                                       "REM0.5_middlechildhood_adolescence_h2", "REM0.5_childhood_c2", 
                                       "REM0.5_middlechildhood_c2", "REM0.5_adolescence_c2", "REM0.5_childhood_middlechildhood_c2", 
                                       "REM0.5_childhood_adolescence_c2", "REM0.5_middlechildhood_adolescence_c2", 
                                       "REM0.5_childhood_e2", "REM0.5_middlechildhood_e2", "REM0.5_adolescence_e2", 
                                       "REM0.5_childhood_middlechildhood_e2", "REM0.5_childhood_adolescence_e2", 
                                       "REM0.5_middlechildhood_adolescence_e2"),
                    labels = c("Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence"))

data$disorder <- "ADHD"

# ============= Plots =============

dataADHD <- data[data$name == "Childhood" | 
                   data$name == "Middle childhood" | 
                   data$name == "Adolescence",]

ageADHD = ggplot(data=dataADHD,
                 aes(x = name, y = est, group = component, color = component))+
  geom_line(color="black") +
  geom_point(size=7, stroke=1) +
  scale_shape_manual(values = c(16,0))+
  #geom_pointrange(aes(ymin = est-se, ymax = est+se, color = component))+
  #geom_hline(aes(fill=ref),yintercept = c(0,0.29), linetype=2)+
  xlab('')+ ylab("Grand estimate")+
  ggtitle("ADHD")+
  geom_errorbar(aes(ymin = est-se, ymax = est+se, linetype="solid"),width=0.1, size=1)+ 
  #facet_grid(grand~., scales= "free", space="free", rows = vars(grand))+
  #facet_grid(rows = vars(grand), space = "free", scales = "free")+
  theme(legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        strip.text.y = element_text(size=15, angle = 0),
        plot.title=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=15),
        panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"))+
  #expand_limits(y=c(-3,5))+
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2))+
  guides(color=guide_legend(nrow=3,byrow=TRUE))+
  guides(shape=guide_legend(nrow=2,byrow=TRUE))+
  guides(linetype=F)+
  #scale_color_brewer(palette = 4)+
  #scale_fill_brewer(palette = 4)
  scale_color_manual(values = c("#FF5666","black","#759fbc"),labels = c("Heritability",
                                                                        "Shared environment",
                                                                        "Nonshared environment"))

ageADHD


# ================== Straifying SLD by covariates ##### 



library(ggplot2)
library(maps)
library(cowplot)
library(patternplot)

# ================== Combine datasets ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_family_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_family_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_family_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2, datac2, datae2)
dataALL$sex <- "combined"

datah2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_family_males_h2.csv")
datac2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_family_males_c2.csv")
datae2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_family_males_e2.csv")

datah2M$component <- "h2"
datac2M$component <- "c2"
datae2M$component <- "e2"

dataM <- rbind(datah2M,datac2M,datae2M)

datah2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_family_females_h2.csv")
datac2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_family_females_c2.csv")
datae2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_family_females_e2.csv")

datah2F$component <- "h2"
datac2F$component <- "c2"
datae2F$component <- "e2"

dataF <- rbind(datah2F,datac2F,datae2F)

dataMF <- rbind(dataM,dataF)

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Heritability",
                                       "Shared environment",
                                       "Nonshared environment"))

dataMF$sig <- factor(dataMF$sig, levels = c("sig","nosig"),
                     labels = c("Significant moderation",
                                "Non-significant moderation"))
dataMF$star <- with(dataMF, ifelse(dataMF$sig == "Significant moderation", "*",NA))
dataMF$component <- factor(dataMF$component, levels = c("h2","c2","e2"),
                           labels = c("Heritability",
                                      "Shared environment",
                                      "Nonshared environment"))

# ================== Age combined ==================  #####
# ============= Data =============

data <- dataALL[dataALL$type == "ages",]
data <- data[data$sex == "combined",]
data$name <- factor(data$X, levels = c("REM0.5_childhood_h2", "REM0.5_middlechildhood_h2", "REM0.5_adolescence_h2", 
                                       "REM0.5_childhood_middlechildhood_h2", "REM0.5_childhood_adolescence_h2", 
                                       "REM0.5_middlechildhood_adolescence_h2", "REM0.5_childhood_c2", 
                                       "REM0.5_middlechildhood_c2", "REM0.5_adolescence_c2", "REM0.5_childhood_middlechildhood_c2", 
                                       "REM0.5_childhood_adolescence_c2", "REM0.5_middlechildhood_adolescence_c2", 
                                       "REM0.5_childhood_e2", "REM0.5_middlechildhood_e2", "REM0.5_adolescence_e2", 
                                       "REM0.5_childhood_middlechildhood_e2", "REM0.5_childhood_adolescence_e2", 
                                       "REM0.5_middlechildhood_adolescence_e2"),
                    labels = c("Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence"))

data$disorder <- "SLD"

# ============= Plots =============

dataSLD <- data[data$name == "Childhood" | 
                  data$name == "Middle childhood" | 
                  data$name == "Adolescence",]

ageSLD = ggplot(data=dataSLD,
                aes(x = name, y = est, group = component, color = component))+
  geom_line(color="black") +
  geom_point(size=7, stroke=1) +
  scale_shape_manual(values = c(16,0))+
  #geom_pointrange(aes(ymin = est-se, ymax = est+se, color = component))+
  #geom_hline(aes(fill=ref),yintercept = c(0,0.29), linetype=2)+
  xlab('')+ ylab("Grand estimate")+
  ggtitle("Specific Learning Disorders")+
  geom_errorbar(aes(ymin = est-se, ymax = est+se, linetype="solid"),width=0.1, size=1)+ 
  #facet_grid(grand~., scales= "free", space="free", rows = vars(grand))+
  #facet_grid(rows = vars(grand), space = "free", scales = "free")+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        strip.text.y = element_text(size=15, angle = 0),
        plot.title=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=15),
        panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"))+
  #expand_limits(y=c(-3,5))+
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2))+
  guides(color=guide_legend(nrow=3,byrow=TRUE))+
  guides(shape=guide_legend(nrow=2,byrow=TRUE))+
  guides(linetype=F)+
  #scale_color_brewer(palette = 4)+
  #scale_fill_brewer(palette = 4)
  scale_color_manual(values = c("#FF5666","black","#759fbc"),labels = c("Heritability",
                                                                        "Shared environment",
                                                                        "Nonshared environment"))

ageSLD
# ================== Grid ==================  #####

data <- rbind(dataALLNDDs,
              dataCMD,
              dataASD,
              dataADHD,
              dataSLD)

data$disorder <- factor(data$disorder, levels = c("ALL",
                                                  "CMD",
                                                  "ASD",
                                                  "ADHD",
                                                  "SLD"),
                        labels = c("NDDs \n combined",
                                   "Communication \n disorders",
                                   "ASD",
                                   "ADHD",
                                   "Specific \n learning \n disorders"))

data$component <- factor(data$component, levels = c("Heritability", 
                                                    "Shared environment", "Nonshared environment"),
                         labels = c("Family h2","Family c2","Family e2"))

age = ggplot(data=data,
             aes(x = name, y = est, group = component, color = component))+
  geom_line(color="black") +
  geom_point(size=4, stroke=1) +
  scale_shape_manual(values = c(16,0))+
  #geom_pointrange(aes(ymin = est-se, ymax = est+se, color = component))+
  #geom_hline(aes(fill=ref),yintercept = c(0,0.29), linetype=2)+
  xlab('')+ ylab("Grand estimate")+
  geom_errorbar(aes(ymin = est-se, ymax = est+se, linetype="solid"),width=0.1, size=.5)+ 
  #facet_grid(grand~., scales= "free", space="free", rows = vars(grand))+
  #facet_grid(rows = vars(grand), space = "free", scales = "free")+
  theme(legend.direction = "horizontal",
        legend.position="bottom",
        legend.text = element_text(size=15),
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
        axis.text.y = element_text(size=15, colour="black", family = "sans", angle = 0), 
        axis.text.x = element_text(size=15, colour="black", family = "sans", angle = 45, hjust = 1),
        #axis.text.x = element_blank(),
        axis.title= element_text(size=15),
        strip.text.x = element_text(size=15, angle = 0),
        strip.text.y = element_text(size=15, angle = 0),
        plot.title = element_text(size=15, angle = 0),
        strip.background = element_rect(colour="black", fill="white"))+
  expand_limits(y=c(0,1))+
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2))+
  facet_wrap(disorder~., scales = "fixed", nrow = 1)+
  #scale_color_brewer(palette = 4)+
  guides(linetype=F)+
  #scale_fill_brewer(palette = 4)
  scale_color_manual(values = c("dodgerblue3","orange","lightskyblue"))

##### Ages snp #####
# ================== Straifying all NDDs by covariates ##### 



library(ggplot2)
library(maps)
library(cowplot)
library(patternplot)

# ================== Combine datasets ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_allNDDs_snp_combined_h2.csv")

datah2$component <- "h2"

dataALL <- rbind(datah2)
dataALL$sex <- "combined"

datah2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_allNDDs_snp_males_h2.csv")
datac2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_allNDDs_snp_males_c2.csv")
datae2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_allNDDs_snp_males_e2.csv")

datah2M$component <- "h2"
datac2M$component <- "c2"
datae2M$component <- "e2"

dataM <- rbind(datah2M,datac2M,datae2M)

datah2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_snp_females_h2.csv")
datac2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_snp_females_c2.csv")
datae2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/all NDDs/stratified_all NDDs_snp_females_e2.csv")

datah2F$component <- "h2"
datac2F$component <- "c2"
datae2F$component <- "e2"

dataF <- rbind(datah2F,datac2F,datae2F)

dataMF <- rbind(dataM,dataF)

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Heritability",
                                       "Shared environment",
                                       "Nonshared environment"))

dataMF$sig <- factor(dataMF$sig, levels = c("sig","nosig"),
                     labels = c("Significant moderation",
                                "Non-significant moderation"))
dataMF$star <- with(dataMF, ifelse(dataMF$sig == "Significant moderation", "*",NA))
dataMF$component <- factor(dataMF$component, levels = c("h2","c2","e2"),
                           labels = c("Heritability",
                                      "Shared environment",
                                      "Nonshared environment"))

# ================== Age combined ==================  #####
# ============= Data =============

data <- dataALL[dataALL$type == "ages",]
data <- data[data$sex == "combined",]
data$name <- factor(data$X, levels = c("REM0.5_childhood_h2", "REM0.5_middlechildhood_h2", "REM0.5_adolescence_h2", 
                                       "REM0.5_childhood_middlechildhood_h2", "REM0.5_childhood_adolescence_h2", 
                                       "REM0.5_middlechildhood_adolescence_h2", "REM0.5_childhood_c2", 
                                       "REM0.5_middlechildhood_c2", "REM0.5_adolescence_c2", "REM0.5_childhood_middlechildhood_c2", 
                                       "REM0.5_childhood_adolescence_c2", "REM0.5_middlechildhood_adolescence_c2", 
                                       "REM0.5_childhood_e2", "REM0.5_middlechildhood_e2", "REM0.5_adolescence_e2", 
                                       "REM0.5_childhood_middlechildhood_e2", "REM0.5_childhood_adolescence_e2", 
                                       "REM0.5_middlechildhood_adolescence_e2"),
                    labels = c("Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence"))

data$disorder <- "ALL"

# ============= Plots =============

dataALLNDDs <- data[data$name == "Childhood" | 
                      data$name == "Middle childhood" | 
                      data$name == "Adolescence",]

dataALLNDDs$disorder <- "ALL"

ageALLNDDs = ggplot(data=dataALLNDDs,
                    aes(x = name, y = est, group = component, color = component))+
  geom_line(color="black") +
  geom_point(size=7, stroke=1) +
  scale_shape_manual(values = c(16,0))+
  #geom_pointrange(aes(ymin = est-se, ymax = est+se, color = component))+
  #geom_hline(aes(fill=ref),yintercept = c(0,0.29), linetype=2)+
  xlab('')+ ylab("Grand estimate")+
  ggtitle("NDDs combined")+
  geom_errorbar(aes(ymin = est-se, ymax = est+se, linetype="solid"),width=0.1, size=1)+ 
  #facet_grid(grand~., scales= "free", space="free", rows = vars(grand))+
  #facet_grid(rows = vars(grand), space = "free", scales = "free")+
  theme(legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        strip.text.y = element_text(size=15, angle = 0),
        plot.title=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=15),
        panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"))+
  #expand_limits(y=c(-3,5))+
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2))+
  guides(color=guide_legend(nrow=3,byrow=TRUE))+
  guides(shape=guide_legend(nrow=2,byrow=TRUE))+
  guides(linetype=F)+
  #scale_color_brewer(palette = 4)+
  #scale_fill_brewer(palette = 4)
  scale_color_manual(values = c("firebrick1","black","firebrick1"),labels = c("Heritability",
                                                                              "Shared environment",
                                                                              "Nonshared environment"))

ageALLNDDs

# ================== Straifying CMD by covariates ##### 



library(ggplot2)
library(maps)
library(cowplot)
library(patternplot)

# ================== Combine datasets ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_snp_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_snp_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_snp_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2)
dataALL$sex <- "combined"

datah2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_snp_males_h2.csv")
datac2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_snp_males_c2.csv")
datae2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_snp_males_e2.csv")

datah2M$component <- "h2"
datac2M$component <- "c2"
datae2M$component <- "e2"

dataM <- rbind(datah2M,datac2M,datae2M)

datah2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_snp_females_h2.csv")
datac2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_snp_females_c2.csv")
datae2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/CMD/stratified_CMD_snp_females_e2.csv")

datah2F$component <- "h2"
datac2F$component <- "c2"
datae2F$component <- "e2"

dataF <- rbind(datah2F,datac2F,datae2F)

dataMF <- rbind(dataM,dataF)

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Heritability",
                                       "Shared environment",
                                       "Nonshared environment"))

dataMF$sig <- factor(dataMF$sig, levels = c("sig","nosig"),
                     labels = c("Significant moderation",
                                "Non-significant moderation"))
dataMF$star <- with(dataMF, ifelse(dataMF$sig == "Significant moderation", "*",NA))
dataMF$component <- factor(dataMF$component, levels = c("h2","c2","e2"),
                           labels = c("Heritability",
                                      "Shared environment",
                                      "Nonshared environment"))

# ================== Age combined ==================  #####
# ============= Data =============

data <- dataALL[dataALL$type == "ages",]
data <- data[data$sex == "combined",]
data$name <- factor(data$X, levels = c("REM0.5_childhood_h2", "REM0.5_middlechildhood_h2", "REM0.5_adolescence_h2", 
                                       "REM0.5_childhood_middlechildhood_h2", "REM0.5_childhood_adolescence_h2", 
                                       "REM0.5_middlechildhood_adolescence_h2", "REM0.5_childhood_c2", 
                                       "REM0.5_middlechildhood_c2", "REM0.5_adolescence_c2", "REM0.5_childhood_middlechildhood_c2", 
                                       "REM0.5_childhood_adolescence_c2", "REM0.5_middlechildhood_adolescence_c2", 
                                       "REM0.5_childhood_e2", "REM0.5_middlechildhood_e2", "REM0.5_adolescence_e2", 
                                       "REM0.5_childhood_middlechildhood_e2", "REM0.5_childhood_adolescence_e2", 
                                       "REM0.5_middlechildhood_adolescence_e2"),
                    labels = c("Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence"))

data$disorder <- "CMD"

# ============= Plots =============

dataCMD <- data[data$name == "Childhood" |
                  data$name == "Adolescence",]

ageCMD = ggplot(data=dataCMD,
                aes(x = name, y = est, group = component, color = component, na.omit = T))+
  geom_line(color="black") +
  geom_point(size=7, stroke=1) +
  scale_shape_manual(values = c(16,0))+
  #geom_pointrange(aes(ymin = est-se, ymax = est+se, color = component))+
  #geom_hline(aes(fill=ref),yintercept = c(0,0.29), linetype=2)+
  xlab('')+ ylab("Grand estimate")+
  ggtitle("Communication Disorders")+
  geom_errorbar(aes(ymin = est-se, ymax = est+se, linetype="solid"),width=0.1, size=1)+ 
  #facet_grid(grand~., scales= "free", space="free", rows = vars(grand))+
  #facet_grid(rows = vars(grand), space = "free", scales = "free")+
  theme(legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        strip.text.y = element_text(size=15, angle = 0),
        plot.title=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=15),
        panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"))+
  #expand_limits(y=c(-3,5))+
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2))+
  guides(color=guide_legend(nrow=3,byrow=TRUE))+
  guides(shape=guide_legend(nrow=2,byrow=TRUE))+
  guides(linetype=F)+
  #scale_color_brewer(palette = 4)+
  #scale_fill_brewer(palette = 4)
  scale_color_manual(values = c("firebrick1","black","firebrick1"),labels = c("Heritability",
                                                                              "Shared environment",
                                                                              "Nonshared environment"))

ageCMD

# ================== Straifying ASD by covariates ##### 



library(ggplot2)
library(maps)
library(cowplot)
library(patternplot)

# ================== Combine datasets ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_snp_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_snp_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_snp_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2)
dataALL$sex <- "combined"

datah2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_snp_males_h2.csv")
datac2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_snp_males_c2.csv")
datae2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_snp_males_e2.csv")

datah2M$component <- "h2"
datac2M$component <- "c2"
datae2M$component <- "e2"

dataM <- rbind(datah2M,datac2M,datae2M)

datah2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_snp_females_h2.csv")
datac2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_snp_females_c2.csv")
datae2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ASD/stratified_ASD_snp_females_e2.csv")

datah2F$component <- "h2"
datac2F$component <- "c2"
datae2F$component <- "e2"

dataF <- rbind(datah2F,datac2F,datae2F)

dataMF <- rbind(dataM,dataF)

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Heritability",
                                       "Shared environment",
                                       "Nonshared environment"))

dataMF$sig <- factor(dataMF$sig, levels = c("sig","nosig"),
                     labels = c("Significant moderation",
                                "Non-significant moderation"))
dataMF$star <- with(dataMF, ifelse(dataMF$sig == "Significant moderation", "*",NA))
dataMF$component <- factor(dataMF$component, levels = c("h2","c2","e2"),
                           labels = c("Heritability",
                                      "Shared environment",
                                      "Nonshared environment"))

# ================== Age combined ==================  #####
# ============= Data =============

data <- dataALL[dataALL$type == "ages",]
data <- data[data$sex == "combined",]
data$name <- factor(data$X, levels = c("REM0.5_childhood_h2", "REM0.5_middlechildhood_h2", "REM0.5_adolescence_h2", 
                                       "REM0.5_childhood_middlechildhood_h2", "REM0.5_childhood_adolescence_h2", 
                                       "REM0.5_middlechildhood_adolescence_h2", "REM0.5_childhood_c2", 
                                       "REM0.5_middlechildhood_c2", "REM0.5_adolescence_c2", "REM0.5_childhood_middlechildhood_c2", 
                                       "REM0.5_childhood_adolescence_c2", "REM0.5_middlechildhood_adolescence_c2", 
                                       "REM0.5_childhood_e2", "REM0.5_middlechildhood_e2", "REM0.5_adolescence_e2", 
                                       "REM0.5_childhood_middlechildhood_e2", "REM0.5_childhood_adolescence_e2", 
                                       "REM0.5_middlechildhood_adolescence_e2"),
                    labels = c("Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence"))

data$disorder <- "ASD"

# ============= Plots =============

dataASD <- data[data$name == "Childhood" | 
                  data$name == "Middle childhood" | 
                  data$name == "Adolescence",]

ageASD = ggplot(data=dataASD,
                aes(x = name, y = est, group = component, color = component))+
  geom_line(color="black") +
  geom_point(size=7, stroke=1) +
  scale_shape_manual(values = c(16,0))+
  #geom_pointrange(aes(ymin = est-se, ymax = est+se, color = component))+
  #geom_hline(aes(fill=ref),yintercept = c(0,0.29), linetype=2)+
  xlab('')+ ylab("Grand estimate")+
  ggtitle("ASD")+
  geom_errorbar(aes(ymin = est-se, ymax = est+se, linetype="solid"),width=0.1, size=1)+ 
  #facet_grid(grand~., scales= "free", space="free", rows = vars(grand))+
  #facet_grid(rows = vars(grand), space = "free", scales = "free")+
  theme(legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        strip.text.y = element_text(size=15, angle = 0),
        plot.title=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=15),
        panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"))+
  #expand_limits(y=c(-3,5))+
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2))+
  guides(color=guide_legend(nrow=3,byrow=TRUE))+
  guides(shape=guide_legend(nrow=2,byrow=TRUE))+
  guides(linetype=F)+
  #scale_color_brewer(palette = 4)+
  #scale_fill_brewer(palette = 4)
  scale_color_manual(values = c("firebrick1","black","firebrick1"),labels = c("Heritability",
                                                                              "Shared environment",
                                                                              "Nonshared environment"))

ageASD

# ================== Straifying ADHD by covariates ##### 



library(ggplot2)
library(maps)
library(cowplot)
library(patternplot)

# ================== Combine datasets ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_snp_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_snp_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_snp_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2)
dataALL$sex <- "combined"

datah2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_snp_males_h2.csv")
datac2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_snp_males_c2.csv")
datae2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_snp_males_e2.csv")

datah2M$component <- "h2"
datac2M$component <- "c2"
datae2M$component <- "e2"

dataM <- rbind(datah2M,datac2M,datae2M)

datah2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_snp_females_h2.csv")
datac2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_snp_females_c2.csv")
datae2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/ADHD/stratified_ADHD_snp_females_e2.csv")

datah2F$component <- "h2"
datac2F$component <- "c2"
datae2F$component <- "e2"

dataF <- rbind(datah2F,datac2F,datae2F)

dataMF <- rbind(dataM,dataF)

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Heritability",
                                       "Shared environment",
                                       "Nonshared environment"))

dataMF$sig <- factor(dataMF$sig, levels = c("sig","nosig"),
                     labels = c("Significant moderation",
                                "Non-significant moderation"))
dataMF$star <- with(dataMF, ifelse(dataMF$sig == "Significant moderation", "*",NA))
dataMF$component <- factor(dataMF$component, levels = c("h2","c2","e2"),
                           labels = c("Heritability",
                                      "Shared environment",
                                      "Nonshared environment"))

# ================== Age combined ==================  #####
# ============= Data =============

data <- dataALL[dataALL$type == "ages",]
data <- data[data$sex == "combined",]
data$name <- factor(data$X, levels = c("REM0.5_childhood_h2", "REM0.5_middlechildhood_h2", "REM0.5_adolescence_h2", 
                                       "REM0.5_childhood_middlechildhood_h2", "REM0.5_childhood_adolescence_h2", 
                                       "REM0.5_middlechildhood_adolescence_h2", "REM0.5_childhood_c2", 
                                       "REM0.5_middlechildhood_c2", "REM0.5_adolescence_c2", "REM0.5_childhood_middlechildhood_c2", 
                                       "REM0.5_childhood_adolescence_c2", "REM0.5_middlechildhood_adolescence_c2", 
                                       "REM0.5_childhood_e2", "REM0.5_middlechildhood_e2", "REM0.5_adolescence_e2", 
                                       "REM0.5_childhood_middlechildhood_e2", "REM0.5_childhood_adolescence_e2", 
                                       "REM0.5_middlechildhood_adolescence_e2"),
                    labels = c("Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence"))

data$disorder <- "ADHD"

# ============= Plots =============

dataADHD <- data[data$name == "Childhood" | 
                   data$name == "Middle childhood" | 
                   data$name == "Adolescence",]

ageADHD = ggplot(data=dataADHD,
                 aes(x = name, y = est, group = component, color = component))+
  geom_line(color="black") +
  geom_point(size=7, stroke=1) +
  scale_shape_manual(values = c(16,0))+
  #geom_pointrange(aes(ymin = est-se, ymax = est+se, color = component))+
  #geom_hline(aes(fill=ref),yintercept = c(0,0.29), linetype=2)+
  xlab('')+ ylab("Grand estimate")+
  ggtitle("ADHD")+
  geom_errorbar(aes(ymin = est-se, ymax = est+se, linetype="solid"),width=0.1, size=1)+ 
  #facet_grid(grand~., scales= "free", space="free", rows = vars(grand))+
  #facet_grid(rows = vars(grand), space = "free", scales = "free")+
  theme(legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        strip.text.y = element_text(size=15, angle = 0),
        plot.title=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=15),
        panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"))+
  #expand_limits(y=c(-3,5))+
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2))+
  guides(color=guide_legend(nrow=3,byrow=TRUE))+
  guides(shape=guide_legend(nrow=2,byrow=TRUE))+
  guides(linetype=F)+
  #scale_color_brewer(palette = 4)+
  #scale_fill_brewer(palette = 4)
  scale_color_manual(values = c("firebrick1","black","firebrick1"),labels = c("Heritability",
                                                                              "Shared environment",
                                                                              "Nonshared environment"))

ageADHD


# ================== Straifying SLD by covariates ##### 



library(ggplot2)
library(maps)
library(cowplot)
library(patternplot)

# ================== Combine datasets ================== 

datah2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_snp_combined_h2.csv")
datac2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_snp_combined_c2.csv")
datae2 <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_snp_combined_e2.csv")

datah2$component <- "h2"
datac2$component <- "c2"
datae2$component <- "e2"

dataALL <- rbind(datah2)
dataALL$sex <- "combined"

datah2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_snp_males_h2.csv")
datac2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_snp_males_c2.csv")
datae2M <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_snp_males_e2.csv")

datah2M$component <- "h2"
datac2M$component <- "c2"
datae2M$component <- "e2"

dataM <- rbind(datah2M,datac2M,datae2M)

datah2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_snp_females_h2.csv")
datac2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_snp_females_c2.csv")
datae2F <- read.csv("~/Desktop/Meta NDD DICC/Models/h2/SLD/stratified_SLD_snp_females_e2.csv")

datah2F$component <- "h2"
datac2F$component <- "c2"
datae2F$component <- "e2"

dataF <- rbind(datah2F,datac2F,datae2F)

dataMF <- rbind(dataM,dataF)

dataALL$sig <- factor(dataALL$sig, levels = c("sig","nosig"),
                      labels = c("Significant moderation",
                                 "Non-significant moderation"))
dataALL$star <- with(dataALL, ifelse(dataALL$sig == "Significant moderation", "*",NA))
dataALL$component <- factor(dataALL$component, levels = c("h2","c2","e2"),
                            labels = c("Heritability",
                                       "Shared environment",
                                       "Nonshared environment"))

dataMF$sig <- factor(dataMF$sig, levels = c("sig","nosig"),
                     labels = c("Significant moderation",
                                "Non-significant moderation"))
dataMF$star <- with(dataMF, ifelse(dataMF$sig == "Significant moderation", "*",NA))
dataMF$component <- factor(dataMF$component, levels = c("h2","c2","e2"),
                           labels = c("Heritability",
                                      "Shared environment",
                                      "Nonshared environment"))

# ================== Age combined ==================  #####
# ============= Data =============

data <- dataALL[dataALL$type == "ages",]
data <- data[data$sex == "combined",]
data$name <- factor(data$X, levels = c("REM0.5_childhood_h2", "REM0.5_middlechildhood_h2", "REM0.5_adolescence_h2", 
                                       "REM0.5_childhood_middlechildhood_h2", "REM0.5_childhood_adolescence_h2", 
                                       "REM0.5_middlechildhood_adolescence_h2", "REM0.5_childhood_c2", 
                                       "REM0.5_middlechildhood_c2", "REM0.5_adolescence_c2", "REM0.5_childhood_middlechildhood_c2", 
                                       "REM0.5_childhood_adolescence_c2", "REM0.5_middlechildhood_adolescence_c2", 
                                       "REM0.5_childhood_e2", "REM0.5_middlechildhood_e2", "REM0.5_adolescence_e2", 
                                       "REM0.5_childhood_middlechildhood_e2", "REM0.5_childhood_adolescence_e2", 
                                       "REM0.5_middlechildhood_adolescence_e2"),
                    labels = c("Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence",
                               "Childhood", "Middle childhood", "Adolescence", "Childhood & Middle childhood", 
                               "Childhood & Adolescence", "Middle childhood & Adolescence"))

data$disorder <- "SLD"

# ============= Plots =============

dataSLD <- data[data$name == "Childhood" |
                  data$name == "Adolescence",]

ageSLD = ggplot(data=dataSLD,
                aes(x = name, y = est, group = component, color = component))+
  geom_line(color="black") +
  geom_point(size=7, stroke=1) +
  scale_shape_manual(values = c(16,0))+
  #geom_pointrange(aes(ymin = est-se, ymax = est+se, color = component))+
  #geom_hline(aes(fill=ref),yintercept = c(0,0.29), linetype=2)+
  xlab('')+ ylab("Grand estimate")+
  ggtitle("Specific Learning Disorders")+
  geom_errorbar(aes(ymin = est-se, ymax = est+se, linetype="solid"),width=0.1, size=1)+ 
  #facet_grid(grand~., scales= "free", space="free", rows = vars(grand))+
  #facet_grid(rows = vars(grand), space = "free", scales = "free")+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        strip.text.y = element_text(size=15, angle = 0),
        plot.title=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=15),
        panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"))+
  #expand_limits(y=c(-3,5))+
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2))+
  guides(color=guide_legend(nrow=3,byrow=TRUE))+
  guides(shape=guide_legend(nrow=2,byrow=TRUE))+
  guides(linetype=F)+
  #scale_color_brewer(palette = 4)+
  #scale_fill_brewer(palette = 4)
  scale_color_manual(values = c("firebrick1","black","firebrick1"),labels = c("Heritability",
                                                                              "Shared environment",
                                                                              "Nonshared environment"))

ageSLD
# ================== Grid ==================  #####

data <- rbind(dataALLNDDs,
              dataCMD,
              dataASD,
              dataADHD,
              dataSLD)

data$disorder <- factor(data$disorder, levels = c("ALL",
                                                  "CMD",
                                                  "ASD",
                                                  "ADHD",
                                                  "SLD"),
                        labels = c("NDDs \n combined",
                                   "Communication \n disorders",
                                   "ASD",
                                   "ADHD",
                                   "Specific \n learning \n disorders"))

data$component <- factor(data$component, levels = c("Heritability"),
                         labels = c("SNP h2"))

age = ggplot(data=data,
             aes(x = name, y = est, group = component, color = component))+
  geom_line(color="black") +
  geom_point(size=4, stroke=1) +
  scale_shape_manual(values = c(16,0))+
  #geom_pointrange(aes(ymin = est-se, ymax = est+se, color = component))+
  #geom_hline(aes(fill=ref),yintercept = c(0,0.29), linetype=2)+
  xlab('')+ ylab("Grand estimate")+
  geom_errorbar(aes(ymin = est-se, ymax = est+se, linetype="solid"),width=0.1, size=.5)+ 
  #facet_grid(grand~., scales= "free", space="free", rows = vars(grand))+
  #facet_grid(rows = vars(grand), space = "free", scales = "free")+
  theme(legend.direction = "horizontal",
        legend.position="bottom",
        legend.text = element_text(size=15),
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
        axis.text.y = element_text(size=15, colour="black", family = "sans", angle = 0), 
        axis.text.x = element_text(size=15, colour="black", family = "sans", angle = 45, hjust = 1),
        #axis.text.x = element_blank(),
        axis.title= element_text(size=15),
        strip.text.x = element_text(size=15, angle = 0),
        strip.text.y = element_text(size=15, angle = 0),
        plot.title = element_text(size=15, angle = 0),
        strip.background = element_rect(colour="black", fill="white"))+
  expand_limits(y=c(0,1))+
  #scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2))+
  facet_wrap(disorder~., scales = "fixed", nrow = 1)+
  #scale_color_brewer(palette = 4)+
  guides(linetype=F)+
  #scale_fill_brewer(palette = 4)
  scale_color_manual(values = c("firebrick1","orange","lightskyblue"))
