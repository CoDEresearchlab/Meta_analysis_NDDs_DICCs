# ============= Distribution of estimates (Aga, 22/07/21) =============
rm(list=ls())

# Load libraries
#install.packages("compute.es")
#install.packages("metafor")
#install.packages("gmodels")
#install.packages("MAd")
#install.packages("DescTools")

library("compute.es")
library("metafor")
library("gmodels")
library("MAd")
library("DescTools")
library("psych")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("cowplot")
library("ggridges")
library("hexbin")

##### 
##### ALL aim 1 #####
# ============= h2 =============
# ============= Load and subset data =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_heritability.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design == "Family")
datMeta <- subset(datMeta, !is.na(datMeta$h2))

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort 
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_agg0.5_study[!duplicated(datMeta_agg0.5_study$id),]

# Now label cohorts and refs and disorders
datMeta_agg0.5_study$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study
datMeta_agg0.5_study$ref <- agg_by_study_cohorts$Ref

datMeta_agg0.5_study$agg <- "Study"
datMeta_agg0.5_study$Comp <- "Family h2"
datMeta_agg0.5_study_A <- datMeta_agg0.5_study

# ============= c2 =============
# ============= Load and subset data =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_heritability.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design == "Family")
datMeta <- subset(datMeta, !is.na(datMeta$c2))
datMeta$c2 <- as.numeric(datMeta$c2)
datMeta$c2_SE <- as.numeric(datMeta$c2_SE)

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort 
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_agg0.5_study[!duplicated(datMeta_agg0.5_study$id),]

# Now label cohorts and refs and disorders
datMeta_agg0.5_study$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study
datMeta_agg0.5_study$ref <- agg_by_study_cohorts$Ref

datMeta_agg0.5_study$agg <- "Study"
datMeta_agg0.5_study$Comp <- "Family c2"
datMeta_agg0.5_study_C <- datMeta_agg0.5_study



# ============= e2 =============
# ============= Load and subset data =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_heritability.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design == "Family")
datMeta <- subset(datMeta, !is.na(datMeta$e2))
datMeta$e2 <- as.numeric(datMeta$e2)
datMeta$e2_SE <- as.numeric(datMeta$e2_SE)

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort 
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_agg0.5_study[!duplicated(datMeta_agg0.5_study$id),]

# Now label cohorts and refs and disorders
datMeta_agg0.5_study$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study
datMeta_agg0.5_study$ref <- agg_by_study_cohorts$Ref

datMeta_agg0.5_study$agg <- "Study"
datMeta_agg0.5_study$Comp <- "Family e2"
datMeta_agg0.5_study_E <- datMeta_agg0.5_study



# ============= snp h2 =============
# ============= Load and subset data =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_heritability.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design == "SNP")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta$h2 <- as.numeric(datMeta$h2)
datMeta$h2_SE <- as.numeric(datMeta$h2_SE)

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort 
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_agg0.5_study[!duplicated(datMeta_agg0.5_study$id),]

# Now label cohorts and refs and disorders
datMeta_agg0.5_study$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study
datMeta_agg0.5_study$ref <- agg_by_study_cohorts$Ref

datMeta_agg0.5_study$agg <- "Study"
datMeta_agg0.5_study$Comp <- "SNP h2"
datMeta_agg0.5_study_snph2 <- datMeta_agg0.5_study

# ============= Plot as density ============= 
dat <- rbind(datMeta_agg0.5_study_A, datMeta_agg0.5_study_C, datMeta_agg0.5_study_E,
             datMeta_agg0.5_study_snph2)

p1 <- ggplot(dat, aes(x = es, y=1, fill = Comp, alpha=0.1)) +
  geom_density_ridges(aes(fill = Comp, alpha=0.1)) +
  #geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  #scale_fill_manual(values = c("lightcoral","skyblue1")) +
  theme(#legend.direction = "vertical",
    legend.position="none",
    legend.text = element_text(size=10),
    legend.title = element_blank(),
    panel.border = element_rect(linetype = 'solid', colour = 'gray',fill = NA),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid'),
    axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
    #axis.text.y = element_text(size=15, colour="black", family = "sans", angle = 0), 
    axis.text.x = element_text(size=15, colour="black", family = "sans", angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    strip.text.x = element_text(size=15, angle = 0),
    strip.text.y = element_text(size=15, angle = 0),
    plot.title = element_text(size=15, angle = 0),
    strip.background = element_rect(colour="black", fill="white")) + 
  #facet_grid(disorder~., scales = "free", space = "free")+
  #facet_wrap(disorder~., scales = "free")+
  xlab("Estimates")+
  ggtitle("Genetic and environmental influences across neurodevelopmental disorders")+
  scale_fill_manual(values = c("dodgerblue3","firebrick1","lightskyblue","orange"),
                    breaks = c("Family h2","Family c2","Family e2","SNP h2"))+
  #scale_y_discrete(expand = expansion(mult = c(-2, 0)))+
  #scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1.5, by = 0.1))+
  guides(alpha=FALSE)

p1

# ============= Plot as bar chart ============= 
A <- dat[dat$Comp == "Family h2",]
A <- A$es

C <- dat[dat$Comp == "Family c2",]
C <- C$es

E <- dat[dat$Comp == "Family e2",]
E <- E$es

H <- dat[dat$Comp == "SNP h2",]
H <- H$es

col2rgb("dodgerblue2")
col2rgb("firebrick1")
col2rgb("lightskyblue")
col2rgb("orange")

myblue <- rgb(28,134,238, max = 238, alpha = 100, names = "blue")
myred <- rgb(255,48,48, max = 255, alpha = 100, names = "red")
mylightsky <- rgb(135,206,250, max = 250, alpha = 100, names = "lightsky")
myorange <- rgb(255,165,0, max = 255, alpha = 100, names = "orange")

# As density
hist(A, col=myblue, xlim=c(0,1), probability = T)  #first histogram
lines(density(A))
hist(C, col=myred, xlim=c(0,1), add=T, probability = T)  #second histogram
lines(density(C))
hist(E, col=mylightsky, xlim=c(0,1), add=T, probability = T)  #third histogram
lines(density(E))
hist(H, col=myorange, xlim=c(0,1), add=T, probability = T)  #fourth histogram
lines(density(E))
box()

# As bars
hist(A, col=myblue, xlim=c(0,1), xlab = "Estimate", ylab = "Number of studies", 
     main = "Genetic and environmental influences 
     across NDDs")  #first histogram
hist(C, col=myred, xlim=c(0,1), add=T)  #second histogram
hist(E, col=mylightsky, xlim=c(0,1), add=T)  #third histogram
hist(H, col=myorange, xlim=c(0,1), add=T)  #fourth histogram
box()

##### ALL aim 2 #####
# ============= rA =============
# ============= Load and subset data =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_comorbidity.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design == "Family")
datMeta <- subset(datMeta, !is.na(datMeta$rA))
datMeta <- datMeta[datMeta$NDD.DICC == "NDD",]

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study <- agg(id=Full.Ref, es=rA, var=rA.rG_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort 
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_agg0.5_study[!duplicated(datMeta_agg0.5_study$id),]

# Now label cohorts and refs and disorders
datMeta_agg0.5_study$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study
datMeta_agg0.5_study$ref <- agg_by_study_cohorts$Ref

datMeta_agg0.5_study$agg <- "Study"
datMeta_agg0.5_study$Comp <- "Family rA"
datMeta_agg0.5_study_rA <- datMeta_agg0.5_study

# ============= rC =============
# ============= Load and subset data =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_comorbidity.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta_all$rC <- as.numeric(datMeta_all$rC)
datMeta_all$rC_SE <- as.numeric(datMeta_all$rC_SE)
datMeta <- subset(datMeta_all, Design == "Family")
datMeta <- subset(datMeta, !is.na(datMeta$rC))
datMeta <- datMeta[datMeta$NDD.DICC == "NDD",]

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study <- agg(id=Full.Ref, es=rC, var=rC_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort 
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_agg0.5_study[!duplicated(datMeta_agg0.5_study$id),]

# Now label cohorts and refs and disorders
datMeta_agg0.5_study$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study
datMeta_agg0.5_study$ref <- agg_by_study_cohorts$Ref

datMeta_agg0.5_study$agg <- "Study"
datMeta_agg0.5_study$Comp <- "Family rC"
datMeta_agg0.5_study_rC <- datMeta_agg0.5_study



# ============= rE =============
# ============= Load and subset data =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_comorbidity.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design == "Family")
datMeta <- subset(datMeta, !is.na(datMeta$rE))
datMeta$rE <- as.numeric(datMeta$rE)
datMeta$rE_SE <- as.numeric(datMeta$rE_SE)
datMeta <- datMeta[datMeta$NDD.DICC == "NDD",]

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study <- agg(id=Full.Ref, es=rE, var=rE_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort 
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_agg0.5_study[!duplicated(datMeta_agg0.5_study$id),]

# Now label cohorts and refs and disorders
datMeta_agg0.5_study$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study
datMeta_agg0.5_study$ref <- agg_by_study_cohorts$Ref

datMeta_agg0.5_study$agg <- "Study"
datMeta_agg0.5_study$Comp <- "Family rE"
datMeta_agg0.5_study_rE <- datMeta_agg0.5_study



# ============= SNP rG =============
# ============= Load and subset data =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_comorbidity.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design == "SNP")
datMeta <- subset(datMeta, !is.na(datMeta$rA))

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study <- agg(id=Full.Ref, es=rA, var=rA.rG_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort 
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_agg0.5_study[!duplicated(datMeta_agg0.5_study$id),]

# Now label cohorts and refs and disorders
datMeta_agg0.5_study$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study
datMeta_agg0.5_study$ref <- agg_by_study_cohorts$Ref

datMeta_agg0.5_study$agg <- "Study"
datMeta_agg0.5_study$Comp <- "SNP rG"
datMeta_agg0.5_study_SNPrA <- datMeta_agg0.5_study


# ============= Plot ============= 
dat <- rbind(datMeta_agg0.5_study_rA, datMeta_agg0.5_study_rC, datMeta_agg0.5_study_rE,
             datMeta_agg0.5_study_SNPrA)

p2 <- ggplot(dat, aes(x = es, y=1, fill = Comp, alpha=0.1)) +
  geom_density_ridges(aes(fill = Comp, alpha=0.1)) +
  #geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  #scale_fill_manual(values = c("lightcoral","skyblue1")) +
  scale_fill_manual(values = c("#184e77",
                               "#168aad",
                               "#52b69a","#d9ed92"),
                    breaks = c("Family rA","Family rC","Family rE","SNP rG"))+
  theme(#legend.direction = "vertical",
    legend.position="none",
    legend.text = element_text(size=10),
    legend.title = element_blank(),
    panel.border = element_rect(linetype = 'solid', colour = 'gray',fill = NA),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid'),
    axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
    #axis.text.y = element_text(size=15, colour="black", family = "sans", angle = 0), 
    axis.text.x = element_text(size=15, colour="black", family = "sans", angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    strip.text.x = element_text(size=15, angle = 0),
    strip.text.y = element_text(size=15, angle = 0),
    plot.title = element_text(size=15, angle = 0),
    strip.background = element_rect(colour="black", fill="white")) + 
  #facet_grid(disorder~., scales = "free", space = "free")+
  #facet_wrap(disorder~., scales = "free")+
  xlab("Estimates")+
  #scale_y_discrete(expand = expansion(mult = c(-2, 0)))+
  #scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1.5, by = 0.1))+
  guides(alpha=FALSE)+
  ggtitle("Genetic and environmental correlations 
across comorbidites between neurodevelopmental disorders")

p2




# ============= Plot as bar chart ============= 
A <- dat[dat$Comp == "Family rA",]
A <- A$es

C <- dat[dat$Comp == "Family rC",]
C <- C$es

E <- dat[dat$Comp == "Family rE",]
E <- E$es

H <- dat[dat$Comp == "SNP rG",]
H <- H$es

col2rgb("#184e77")
col2rgb("#168aad")
col2rgb("#52b69a")
col2rgb("#d9ed92")

myblue <- rgb(24,78,119, max = 119, alpha = 100, names = "blue")
myred <- rgb(22,138,173, max = 173, alpha = 100, names = "red")
mylightsky <- rgb(82,182,154, max = 182, alpha = 100, names = "lightsky")
myorange <- rgb(217,237,146, max = 237, alpha = 100, names = "orange")

# As density
hist(A, col=myblue, xlim=c(0,1), probability = T)  #first histogram
lines(density(A))
hist(C, col=myred, xlim=c(0,1), add=T, probability = T)  #second histogram
lines(density(C))
hist(E, col=mylightsky, xlim=c(0,1), add=T, probability = T)  #third histogram
lines(density(E))
hist(H, col=myorange, xlim=c(0,1), add=T, probability = T)  #fourth histogram
lines(density(E))
box()

# As bars
hist(A, col=myblue, xlim=c(-1,1), xlab = "Estimate", ylab = "Number of studies", 
     main = "Genetic and environmental correlations 
across comorbidites between NDDs")  #first histogram
hist(C, col=myred, xlim=c(-1,1), add=T)  #second histogram
hist(E, col=mylightsky, xlim=c(-1,1), add=T)  #third histogram
hist(H, col=myorange, xlim=c(-1,1), add=T)  #fourth histogram
box()


##### ALL aim 3 #####
# ============= rA =============
# ============= Load and subset data =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_comorbidity.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design == "Family")
datMeta <- subset(datMeta, !is.na(datMeta$rA))
datMeta <- datMeta[datMeta$NDD.DICC == "DICC",]

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study <- agg(id=Full.Ref, es=rA, var=rA.rG_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort 
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_agg0.5_study[!duplicated(datMeta_agg0.5_study$id),]

# Now label cohorts and refs and disorders
datMeta_agg0.5_study$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study
datMeta_agg0.5_study$ref <- agg_by_study_cohorts$Ref

datMeta_agg0.5_study$agg <- "Study"
datMeta_agg0.5_study$Comp <- "Family rA"
datMeta_agg0.5_study_rA <- datMeta_agg0.5_study

# ============= rC =============
# ============= Load and subset data =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_comorbidity.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta_all$rC <- as.numeric(datMeta_all$rC)
datMeta_all$rC_SE <- as.numeric(datMeta_all$rC_SE)
datMeta <- subset(datMeta_all, Design == "Family")
datMeta <- subset(datMeta, !is.na(datMeta$rC))
datMeta <- datMeta[datMeta$NDD.DICC == "DICC",]

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study <- agg(id=Full.Ref, es=rC, var=rC_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort 
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_agg0.5_study[!duplicated(datMeta_agg0.5_study$id),]

# Now label cohorts and refs and disorders
datMeta_agg0.5_study$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study
datMeta_agg0.5_study$ref <- agg_by_study_cohorts$Ref

datMeta_agg0.5_study$agg <- "Study"
datMeta_agg0.5_study$Comp <- "Family rC"
datMeta_agg0.5_study_rC <- datMeta_agg0.5_study



# ============= rE =============
# ============= Load and subset data =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_comorbidity.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design == "Family")
datMeta <- subset(datMeta, !is.na(datMeta$rE))
datMeta$rE <- as.numeric(datMeta$rE)
datMeta$rE_SE <- as.numeric(datMeta$rE_SE)

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study <- agg(id=Full.Ref, es=rE, var=rE_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort 
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_agg0.5_study[!duplicated(datMeta_agg0.5_study$id),]

# Now label cohorts and refs and disorders
datMeta_agg0.5_study$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study
datMeta_agg0.5_study$ref <- agg_by_study_cohorts$Ref

datMeta_agg0.5_study$agg <- "Study"
datMeta_agg0.5_study$Comp <- "Family rE"
datMeta_agg0.5_study_rE <- datMeta_agg0.5_study



# ============= Plot ============= 
dat <- rbind(datMeta_agg0.5_study_rA, datMeta_agg0.5_study_rC, datMeta_agg0.5_study_rE)

p3 <- ggplot(dat, aes(x = es, y=1, fill = Comp, alpha=0.1)) +
  geom_density_ridges(aes(fill = Comp, alpha=0.1)) +
  #geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  #scale_fill_manual(values = c("lightcoral","skyblue1")) +
  scale_fill_manual(values = c("#184e77",
                               "#168aad",
                               "#52b69a"),
                    breaks = c("Family rA","Family rC","Family rE"))+
  theme(legend.direction = "vertical",
        legend.position="none",
        legend.text = element_text(size=15),
        legend.title = element_blank(),
        panel.border = element_rect(linetype = 'solid', colour = 'gray',fill = NA),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid'),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        #axis.text.y = element_text(size=15, colour="black", family = "sans", angle = 0), 
        axis.text.x = element_text(size=15, colour="black", family = "sans", angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        strip.text.x = element_text(size=15, angle = 0),
        strip.text.y = element_text(size=15, angle = 0),
        plot.title = element_text(size=15, angle = 0),
        strip.background = element_rect(colour="black", fill="white")) + 
  #facet_grid(disorder~., scales = "free", space = "free")+
  #facet_wrap(disorder~., scales = "free")+
  ylab("Estimates")+
  #scale_y_discrete(expand = expansion(mult = c(-2, 0)))+
  #scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1.5, by = 0.1))+
  guides(alpha=FALSE)+
  ggtitle("Genetic and environmental correlations 
across comorbidites between neurodevelopmental and DICC disorders")


p3


# ============= Plot as bar chart ============= 
A <- dat[dat$Comp == "Family rA",]
A <- A$es

C <- dat[dat$Comp == "Family rC",]
C <- C$es

E <- dat[dat$Comp == "Family rE",]
E <- E$es

col2rgb("#184e77")
col2rgb("#168aad")
col2rgb("#52b69a")

myblue <- rgb(24,78,119, max = 119, alpha = 100, names = "blue")
myred <- rgb(22,138,173, max = 173, alpha = 100, names = "red")
mylightsky <- rgb(82,182,154, max = 182, alpha = 100, names = "lightsky")

# As density
hist(A, col=myblue, xlim=c(0,1), probability = T)  #first histogram
lines(density(A))
hist(C, col=myred, xlim=c(0,1), add=T, probability = T)  #second histogram
lines(density(C))
hist(E, col=mylightsky, xlim=c(0,1), add=T, probability = T)  #third histogram
lines(density(E))
box()

# As bars
hist(A, col=myblue, xlim=c(-1,1), xlab = "Estimate", ylab = "Number of studies", 
     main = "Genetic and environmental correlations 
across comorbidites between NDDs and DICCs")  #first histogram
hist(C, col=myred, xlim=c(-1,1), add=T)  #second histogram
hist(E, col=mylightsky, xlim=c(-1,1), add=T)  #third histogram
box()



##### Grid #####

library(cowplot)

p <- ggdraw() +
  draw_plot(p1, x = 0, y = .66, height = .33, width = 1) +
  draw_plot(p2, x = 0, y = .33, height = .33, width = 1) +
  draw_plot(p3, x = 0, y = .01, height = .33, width = 1) 
#draw_plot_label(c("A", "B", "C","D"), c(0, .5, 0, .5), c(1, 1, .5, .5), size = 12)
