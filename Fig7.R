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


# ================== Country combined ==================  #####
# ============= Data =============

data <- dataALL[dataALL$type == "countries",]
data <- data[data$sex == "combined",]
data$name <- factor(data$X, levels = c("REM0.5_Australia_h2", "REM0.5_Canada_h2", 
                                       "REM0.5_China_h2", "REM0.5_Netherlands_h2", "REM0.5_Norway_h2", 
                                       "REM0.5_Sweden_h2", "REM0.5_UK_h2", "REM0.5_USA_h2", 
                                       "REM0.5_Australia_c2", "REM0.5_Canada_c2", "REM0.5_China_c2", 
                                       "REM0.5_Netherlands_c2", "REM0.5_Norway_c2", "REM0.5_Sweden_c2", 
                                       "REM0.5_UK_c2", "REM0.5_USA_c2", "REM0.5_Australia_e2", 
                                       "REM0.5_Canada_e2", "REM0.5_China_e2", "REM0.5_Netherlands_e2", 
                                       "REM0.5_Norway_e2", "REM0.5_Sweden_e2", "REM0.5_UK_e2", "REM0.5_USA_e2"),
                    labels = c("Australia", "Canada", 
                               "China", "Netherlands", "Norway", 
                               "Sweden", "UK", "USA", "Australia", 
                               "Canada", "China", 
                               "Netherlands", "Norway", "Sweden", 
                               "UK", "USA", "Australia", 
                               "Canada", "China", "Netherlands", 
                               "Norway", "Sweden", "UK", "USA"))

# ============= Plots =============
library(dplyr)

myworld <- data
myworld$region <- myworld$name
world <- map_data("world")

worldSubset <- inner_join(world, myworld, by = "region")
head(worldSubset)

worldSubset$component <- factor(worldSubset$component, levels = c("Heritability", "Shared environment", "Nonshared environment"),
                                labels = c("Family h2","Family c2","Family e2"))

# Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5),
  legend.key.size = unit(.3, "in"),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
  strip.text.x = element_text(size=15, angle = 0),
  legend.position = "bottom",
  strip.background = element_rect(colour="black", fill="white"))

world_map<-map_data("world")

country1 <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group, fill = es)) + 
  coord_fixed(1.3) +
  geom_map(dat=world_map, map = world_map, 
           aes(map_id=region), fill="grey", color="white")+
  geom_polygon(aes(fill = est)) +
  #geom_polygon(data = worldSubset, aes(x = long, y = lat, group = group), colour = 'black', fill = h2) +
  #scale_fill_distiller(palette ="GnBu", direction = -1) + # or direction=1
  ggtitle("") +
  facet_grid(rows = vars(component), space = "free")+
  #scale_fill_viridis_c()+
  #scale_colour_discrete(guide = FALSE) +
  labs(fill='Grand estimate')  +
  scale_fill_gradient2(low="#caf0f8", mid = "#00b4d8", high="#03045e", 
                       midpoint=0.5, limits=c(0,1))+
  #guides(fill=guide_legend(title="Grand h2 estimate"))+
  plain

