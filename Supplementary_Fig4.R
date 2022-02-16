##### Heterogeneity #####
library(ggplot2)

##### data #####
h2 <- read.table(text = 
                   "Disorder i2 type
                   NDDs 74.52841 total
                   NDDs 53.16553 between
                   NDDs 21.36289 within
                   LD 84.33076 total
                   LD 42.16538 between
                   LD 42.16538 within
                   CMD 82.45394 total
                   CMD 73.749728 between
                   CMD 8.704213 within
                   ASD 85.8619 total
                   ASD 78.430554 between
                   ASD 7.431351 within
                   ADHD 78.42783 total
                   ADHD 54.34128 between
                   ADHD 24.08654 within
                   SLD 47.24296 total
                   SLD 33.10159 between
                   SLD 14.14137 within
                   MD 35.82319 total
                   MD 17.91159 between
                   MD 17.91159 within", header=T)

h2$comp <- "h2"

c2 <- read.table(text = 
                   "Disorder i2 type
                   NDDs 17.60619 total
                   NDDs 3.123741e-08 between
                   NDDs 1.760619e+01 within
                   CMD 21.21307 total
                   CMD 9.700873e-09 between
                   CMD 2.121307e+01 within
                   ASD 40.74211 total
                   ASD 1.440879e-08 between
                   ASD 4.074211e+01 within
                   ADHD 3.037311 total
                   ADHD 3.037311e+00 between
                   ADHD 7.140456e-08 within
                   SLD 3.968422e-08 total
                   SLD 3.91636e-08 between
                   SLD 5.20619e-10 within
                   MD 9.942946e-09 total
                   MD 4.971473e-09 between
                   MD 4.971473e-09 within", header=T)
c2$comp <- "c2"

e2 <- read.table(text = 
                   "Disorder i2 type
                   NDDs 38.11439 total
                   NDDs 21.02548 between
                   NDDs 17.08891 within
                   LD 4.895555e-09 total
                   LD 2.450484e-09 between
                   LD 2.445071e-09 within
                   CMD 8.866212 total
                   CMD 3.637999e-08 between
                   CMD 8.866212e+00 within
                   ASD 10.71958 total
                   ASD 3.775824e-08 between
                   ASD 1.071958e+01 within
                   ADHD 42.79256 total
                   ADHD 4.022046e-08 between
                   ADHD 4.279256e+01 within
                   SLD 5.193929 total
                   SLD 5.193929e+00 between
                   SLD 8.808452e-08 within
                   MD 36.59322 total
                   MD 18.29661 between
                   MD 18.29661 within", header=T)

e2$comp <- "e2"

snph2 <- read.table(text = 
                      "Disorder i2 type
                   NDDs 6.13261e-08 total
                   NDDs 6.109550e-08 between
                   NDDs 2.306019e-10 within
                   CMD 9.595173e-09 total
                   CMD 9.595173e-09 between
                   CMD 0.000000e+00 within
                   ASD 4.993347e-08 total
                   ASD 4.990192e-08 between
                   ASD 3.154920e-11 within
                   ADHD 3.390728e-08 total
                   ADHD 3.390728e-08 between
                   ADHD 0.000000e+00 within
                   SLD 1.450782e-08 total
                   SLD 1.450782e-08 between
                   SLD 0.000000e+00 within", header=T)

snph2$comp <- "snph2"

rA <- read.table(text = 
                   "Disorder i2 type
                   NDDsNDDs 89.43994 total
                   NDDsNDDs 34.25190 between
                   NDDsNDDs 55.18804 within
                   ASDADHD 94.40236 total
                   ASDADHD 65.17986 between
                   ASDADHD 29.22250 within
                   ADHDMD 98.56708 total
                   ADHDMD 49.28354 between
                   ADHDMD 49.28354 within
                   ADHDSLD 78.84489 total
                   ADHDSLD 17.26974 between
                   ADHDSLD 61.57515 within
                   CMDMD 9.901568e-11 total
                   CMDMD 4.950784e-11 between
                   CMDMD 4.950784e-11 within
                   CMDSLD 7.341903e-11 total
                   CMDSLD 3.670951e-11 between
                   CMDSLD 3.670951e-11 within", header=T)

rA$comp <- "rA"

rC <- read.table(text = 
                   "Disorder i2 type
                   NDDsNDDs 94.88139 total
                   NDDsNDDs 36.32232 between
                   NDDsNDDs 58.55907 within
                   ADHDSLD 52.58489 total
                   ADHDSLD 5.545663 between
                   ADHDSLD 47.039228 within", header=T)

rC$comp <- "rC"

rE <- read.table(text = 
                   "Disorder i2 type
                   NDDsNDDs 24.32483 total
                   NDDsNDDs 2.432483e+01 between
                   NDDsNDDs 1.571362e-08 within
                   ASDADHD 62.00787 total
                   ASDADHD 6.200787e+01 between
                   ASDADHD 8.530810e-09 within
                   ADHDSLD 5.126436e-08 total
                   ADHDSLD 5.126436e-08 between
                   ADHDSLD 0.000000e+00 within", header=T)

rE$comp <- "rE"

snprA <- read.table(text = 
                      "Disorder i2 type
                   NDDsNDDs 48.93374 total
                   NDDsNDDs 33.06375 between
                   NDDsNDDs 15.86999 within
                   ASDADHD 24.32396 total
                   ASDADHD 8.810865e-07 between
                   ASDADHD 2.432396e+01 within", header=T)

snprA$comp <- "snprA"

rA1 <- read.table(text = 
                    "Disorder i2 type
                   NDDsDICC 92.67907 total
                   NDDsDICC 54.81205 between
                   NDDsDICC 37.86702 within
                   ADHDCD 92.78364 total
                   ADHDCD 46.39182 between
                   ADHDCD 46.39182 within
                   ADHDODD 83.36221 total
                   ADHDODD 41.68111 between
                   ADHDODD 41.68111 within
                   ASDCD 1.439397e-08 total
                   ASDCD 0.000000e+00 between
                   ASDCD 1.439397e-08 within", header=T)

rA1$comp <- "rA1"

rC1 <- read.table(text = 
                    "Disorder i2 type
                   NDDsDICC 94.57296 total
                   NDDsDICC 1.894490e-07 between
                   NDDsDICC 9.457296e+01 within
                   ADHDCD 95.77612 total
                   ADHDCD 47.88806 between
                   ADHDCD 47.88806 within
                   ADHDODD 94.31511 total
                   ADHDODD 47.15755 between
                   ADHDODD 47.15755 within
                   ASDCD 81.76907 total
                   ASDCD 0.000000e+00 between
                   ASDCD 81.76907 within", header=T)

rC1$comp <- "rC1"

rE1 <- read.table(text = 
                    "Disorder i2 type
                   NDDsDICC 91.36302 total
                   NDDsDICC 4.361213e-08 between
                   NDDsDICC 9.136302e+01 within
                   ADHDCD 2.877967e-08 total
                   ADHDCD 1.438983e-08 between
                   ADHDCD 1.438983e-08 within
                   ADHDODD 92.67242 total
                   ADHDODD 46.33621 between
                   ADHDODD 46.33621 within
                   ASDCD 2.057571e-08 total
                   ASDCD 0.000000e+00 between
                   ASDCD 2.057571e-08 within", header=T)

rE1$comp <- "rE1"

#### combine #####
data1 <- rbind(h2, c2, e2, snph2)
data2 <- rbind(rA, rC, rE, snprA)
data3 <- rbind(rA1, rC1, rE1)

options(scipen = 0)

##### Plot1 #####
#data1 <- h2
#data1$id <- 1:21

data1$Disorder <- factor(data1$Disorder, levels = c("NDDs", "LD", "CMD", 
                                                    "ASD", "ADHD", "SLD", "MD"),
                         labels = c("NDDs combined", 
                                    "Intellectual disabilities", "Communication disorders", "ASD", 
                                    "ADHD", "Specific learning disorders", "Motor disorders"))

data1$comp <- factor(data1$comp, levels = c("h2", "c2", "e2", "snph2"),
                     labels = c("Family h2", "Family c2", "Family e2", "SNP h2"))

data1$type <- factor(data1$type, levels = c("total", "between", "within"),
                     labels = c("Total heterogeneity", "Between-cluster heterogeneity",
                                "Within-cluster heterogeneity"))

data1$Disorder <- fct_rev(data1$Disorder)

p1 = ggplot(data1, aes(x = Disorder,
                       y = i2, fill = type))+
  coord_flip()+
  #geom_segment( aes(x=Disorder, xend=Disorder, y=0, yend=i2), size = 1, position = position_dodge(width = .9))+
  geom_segment( aes(x=Disorder, xend=Disorder, y=0, yend=i2), size = 1)+
  geom_point(size=4, alpha=1, shape = 21)+
  #facet_wrap(facets = data1$comp, nrow = 1, scales = "fixed")+
  facet_grid(.~comp, space = "fixed", scales = "free")+
  #geom_hline(yintercept=0, linetype = "dashed", size = 0.4)+
  theme(legend.position='none',
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(angle = 0, size = 15),
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        #axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing=unit(1, "lines"))+
  #guides(fill = guide_legend(reverse = TRUE))+
  #geom_text(aes(y= -10, label = value), size = 5)+
  #geom_text(aes(Label = Sig), hjust = -15, vjust = .7, colour="black", size = 15) +
  scale_fill_manual(values = c("dodgerblue3","lightskyblue","orange"))+
  #scale_y_continuous(limits = c(-10, 150), breaks = seq(-10, 150, 20))+
  #scale_x_discrete(Labels = c("Females","Males"))+
  xlab("")+ ylab("I2 statistic") +
  #scale_x_discrete(expand = c(-1.1,-1.1))+
  ggtitle("Sources of variance in NDDs")

##### Plot2 #####
#data1 <- h2
#data1$id <- 1:21

data2$Disorder <- factor(data2$Disorder, levels = c("NDDsNDDs", "ASDADHD", "ADHDMD", 
                                                    "ADHDSLD", "CMDMD", "CMDSLD"),
                         labels = c("NDDs combined", 
                                    "ASD & ADHD", "ADHD & motor disorders", "ADHD & specific learning disorders", 
                                    "Communication disorders & motor disorders", "Communication disorders & specific learning disorders"))

data2$comp <- factor(data2$comp, levels = c("rA", "rC", "rE", "snprA"),
                     labels = c("Family rA", "Family rC", "Family rE", "SNP rG"))

data2$type <- factor(data2$type, levels = c("total", "between", "within"),
                     labels = c("Total heterogeneity", "Between-cluster heterogeneity",
                                "Within-cluster heterogeneity"))

data2$Disorder <- fct_rev(data2$Disorder)

p2 = ggplot(data2, aes(x = Disorder,
                       y = i2, fill = type))+
  coord_flip()+
  #geom_segment( aes(x=Disorder, xend=Disorder, y=0, yend=i2), size = 1, position = position_dodge(width = .9))+
  geom_segment( aes(x=Disorder, xend=Disorder, y=0, yend=i2), size = 1)+
  geom_point(size=4, alpha=1, shape = 21)+
  #facet_wrap(facets = data1$comp, nrow = 1, scales = "fixed")+
  facet_grid(.~comp, space = "fixed", scales = "free")+
  #geom_hline(yintercept=0, linetype = "dashed", size = 0.4)+
  theme(legend.position='none',
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(angle = 0, size = 15),
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        #axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing=unit(1, "lines"))+
  #guides(fill = guide_legend(reverse = TRUE))+
  #geom_text(aes(y= -10, label = value), size = 5)+
  #geom_text(aes(Label = Sig), hjust = -15, vjust = .7, colour="black", size = 15) +
  scale_fill_manual(values = c("dodgerblue3","lightskyblue","orange"))+
  #scale_y_continuous(limits = c(-10, 150), breaks = seq(-10, 150, 20))+
  #scale_x_discrete(Labels = c("Females","Males"))+
  xlab("")+ ylab("I2 statistic") +
  #scale_x_discrete(expand = c(-1.1,-1.1))+
  ggtitle("Co-occurence between NDDs")

##### Plot3 #####
#data1 <- h2
#data1$id <- 1:21

data3$Disorder <- factor(data3$Disorder, levels = c("NDDsDICC", "ADHDCD", "ADHDODD", 
                                                    "ASDCD"),
                         labels = c("NDDs and DICCs combined", 
                                    "ADHD & conduct disorder", "ADHD & oppositional defiant disorder", 
                                    "ASD & conduct disorder"))

data3$comp <- factor(data3$comp, levels = c("rA1", "rC1", "rE1"),
                     labels = c("Family rA", "Family rC", "Family rE"))

data3$type <- factor(data3$type, levels = c("total", "between", "within"),
                     labels = c("Total heterogeneity", "Between-cluster heterogeneity",
                                "Within-cluster heterogeneity"))

data3$Disorder <- fct_rev(data3$Disorder)

p3 = ggplot(data3, aes(x = Disorder,
                       y = i2, fill = type))+
  coord_flip()+
  #geom_segment( aes(x=Disorder, xend=Disorder, y=0, yend=i2), size = 1, position = position_dodge(width = .9))+
  geom_segment( aes(x=Disorder, xend=Disorder, y=0, yend=i2), size = 1)+
  geom_point(size=4, alpha=1, shape = 21)+
  #facet_wrap(facets = data1$comp, nrow = 1, scales = "fixed")+
  facet_grid(.~comp, space = "fixed", scales = "free")+
  #geom_hline(yintercept=0, linetype = "dashed", size = 0.4)+
  theme(legend.position='bottom',
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        strip.text.x = element_text(angle = 0, size = 15),
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        #axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing=unit(1, "lines"))+
  #guides(fill = guide_legend(reverse = TRUE))+
  #geom_text(aes(y= -10, label = value), size = 5)+
  #geom_text(aes(Label = Sig), hjust = -15, vjust = .7, colour="black", size = 15) +
  scale_fill_manual(values = c("dodgerblue3","lightskyblue","orange")) +
  #scale_y_continuous(limits = c(-10, 150), breaks = seq(-10, 150, 20))+
  #scale_x_discrete(Labels = c("Females","Males"))+
  xlab("")+ ylab("I2 statistic") +
  #scale_x_discrete(expand = c(-1.1,-1.1))+
  ggtitle("Co-occurrence between NDDs and DICCs")

#### grid #####
library(cowplot)

p = ggdraw() +
  draw_plot(p1, x = 0, y = .67, height = .33, width = 1) +
  draw_plot(p2, x = 0, y = .34, height = .33, width = 1) +
  draw_plot(p3, x = 0, y = 0, height = .34, width = 1)
#draw_plot_label(c("Whole sample", "Males vs females"), c(0.3, 0.2), c(1, 1), size = 20)
