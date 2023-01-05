# ============= Comparing aggregation at different correlations (Aga, 20/05/21) =============
rm(list=ls())
setwd("~/Desktop/Meta NDD DICC/Figures/figs/")

#####data1#####
agg <- read.table(text =
                    "Agg Grand SE Component Corr k
Study 0.66 0.03 A 0.5 236
Study 0.66 0.04 A 0.3 236
Study 0.66 0.03 A 0.9 236
Cohort 0.66 0.04 A 0.5 77
Cohort 0.66 0.04 A 0.3 77
Cohort 0.66 0.04 A 0.9 77
Country 0.65 0.06 A 0.5 15
Country 0.64 0.06 A 0.3 15
Country 0.65 0.06 A 0.9 15
Study 0.17 0.02 C 0.5 127
Study 0.17 0.02 C 0.3 127
Study 0.17 0.02 C 0.9 127
Cohort 0.14 0.03 C 0.5 45
Cohort 0.15 0.02 C 0.3 45
Cohort 0.13 0.03 C 0.9 45
Country 0.12 0.05 C 0.5 12
Country 0.14 0.04 C 0.3 12
Country 0.20 0.05 C 0.9 12
Study 0.29 0.02 E 0.5 195
Study 0.30 0.02 E 0.3 195
Study 0.28 0.01 E 0.9 195
Cohort 0.30 0.02 E 0.5 66
Cohort 0.30 0.02 E 0.3 66
Cohort 0.30 0.02 E 0.9 66
Country 0.31 0.04 E 0.5 15
Country 0.31 0.04 E 0.3 15
Country 0.31 0.05 E 0.9 15
Study 0.1898304 0.02931259 SNPh2 0.5 29
Study 0.187567 0.02672678 SNPh2 0.3 29
Study 0.1933579 0.03310565 SNPh2 0.9 29 
Cohort 0.1966916 0.04767242 SNPh2 0.5 11
Cohort 0.194267 0.04192625 SNPh2 0.3 11
Cohort 0.1995337 0.05446977 SNPh2 0.9 11
Country 0.2056268 0.07784221 SNPh2 0.5 5
Country 0.2041165 0.06406603 SNPh2 0.3 5
Country 0.2068252 0.09747809 SNPh2 0.9 5", header = T)

agg$Corr <- factor(agg$Corr)
agg$Agg <- factor(agg$Agg, levels = c("Study", "Cohort", "Country"))
agg$Component <- factor(agg$Component)
agg$label <- -0.1

#####1#####
#####A#####

aggA <- agg[agg$Component == "A",]

A <- ggplot(aggA, aes(fill=Corr, y=Grand, x=fct_rev(Corr), facets=Agg))+
  #geom_segment( aes(x=ph, xend=ph, y=0, yend=est), size = 2, position = position_dodge(width = .9))+
  #geom_point(size=25, alpha=1, shape = 21)+
  geom_bar(stat = "identity", color = "black", position = "dodge", width = .9, size = 0) +
  geom_errorbar(aes(ymin = Grand-SE, ymax  = Grand+SE),width = 0.2,position = position_dodge(.9), size  = 1)+
  theme(legend.direction = "vertical",
        legend.position="right",
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
  ggtitle("Family h2")+
  xlab("Correlation")+
  ylab("Grand estimates and standard errors")+
  scale_fill_brewer(palette = 4)+
  coord_flip()+
  #geom_text(aes(label = k), vjust = 0.5, hjust = 5, size = 15)+
  #geom_text(aes(y=label, label = k), size = 15)+
  #scale_x_discrete(labels=c("Parent","Teacher","Child"))+
  scale_y_continuous(limits = c(-0.1,1.5))+
  #scale_fill_manual(values = c("dodgerblue3","firebrick1","lightskyblue"))+
  facet_grid(Agg~., scales = "free", space = "free")+
  geom_text(aes(y=label, label = k), size = 3.5)
#facet_wrap(facets~., scales = "free")

A

#####C#####

aggC <- agg[agg$Component == "C",]

C <- ggplot(aggC, aes(fill=Corr, y=Grand, x=fct_rev(Corr), facets=Agg))+
  #geom_segment( aes(x=ph, xend=ph, y=0, yend=est), size = 2, position = position_dodge(width = .9))+
  #geom_point(size=25, alpha=1, shape = 21)+
  geom_bar(stat = "identity", color = "black", position = "dodge", width = .9, size = 0) +
  geom_errorbar(aes(ymin = Grand-SE, ymax  = Grand+SE),width = 0.2,position = position_dodge(.9), size  = 1)+
  theme(legend.direction = "vertical",
        legend.position="none",
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
  ggtitle("Family c2")+
  xlab("Correlation")+
  ylab("Grand estimates and standard errors")+
  scale_fill_brewer(palette = 4)+
  coord_flip()+
  #geom_text(aes(label = k), vjust = 0.5, hjust = 5, size = 15)+
  #geom_text(aes(y=label, label = k), size = 15)+
  #scale_x_discrete(labels=c("Parent","Teacher","Child"))+
  scale_y_continuous(limits = c(-0.1,1.5))+
  #scale_fill_manual(values = c("dodgerblue3","firebrick1","lightskyblue"))+
  facet_grid(Agg~., scales = "free", space = "free")+
  geom_text(aes(y=label, label = k), size = 3.5)
#facet_wrap(facets~., scales = "free")

C

#####E#####

aggE <- agg[agg$Component == "E",]

E <- ggplot(aggE, aes(fill=Corr, y=Grand, x=fct_rev(Corr), facets=Agg))+
  #geom_segment( aes(x=ph, xend=ph, y=0, yend=est), size = 2, position = position_dodge(width = .9))+
  #geom_point(size=25, alpha=1, shape = 21)+
  geom_bar(stat = "identity", color = "black", position = "dodge", width = .9, size = 0) +
  geom_errorbar(aes(ymin = Grand-SE, ymax  = Grand+SE),width = 0.2,position = position_dodge(.9), size  = 1)+
  theme(legend.direction = "vertical",
        legend.position="none",
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
  ggtitle("Family e2")+
  xlab("Correlation")+
  ylab("Grand estimates and standard errors")+
  scale_fill_brewer(palette = 4)+
  coord_flip()+
  #geom_text(aes(label = k), vjust = 0.5, hjust = 5, size = 15)+
  #geom_text(aes(y=label, label = k), size = 15)+
  #scale_x_discrete(labels=c("Parent","Teacher","Child"))+
  scale_y_continuous(limits = c(-0.1,1.5))+
  #scale_fill_manual(values = c("dodgerblue3","firebrick1","lightskyblue"))+
  facet_grid(Agg~., scales = "free", space = "free")+
  geom_text(aes(y=label, label = k), size = 3.5)
#facet_wrap(facets~., scales = "free")

E

#####A#####

aggA <- agg[agg$Component == "SNPh2",]

SNPh2 <- ggplot(aggA, aes(fill=Corr, y=Grand, x=fct_rev(Corr), facets=Agg))+
  #geom_segment( aes(x=ph, xend=ph, y=0, yend=est), size = 2, position = position_dodge(width = .9))+
  #geom_point(size=25, alpha=1, shape = 21)+
  geom_bar(stat = "identity", color = "black", position = "dodge", width = .9, size = 0) +
  geom_errorbar(aes(ymin = Grand-SE, ymax  = Grand+SE),width = 0.2,position = position_dodge(.9), size  = 1)+
  theme(legend.direction = "vertical",
        legend.position="none",
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
  ggtitle("SNP h2")+
  xlab("Correlation")+
  ylab("Grand estimates and standard errors")+
  scale_fill_brewer(palette = 4)+
  coord_flip()+
  #geom_text(aes(label = k), vjust = 0.5, hjust = 5, size = 15)+
  #geom_text(aes(y=label, label = k), size = 15)+
  #scale_x_discrete(labels=c("Parent","Teacher","Child"))+
  scale_y_continuous(limits = c(-0.1,1.5))+
  #scale_fill_manual(values = c("dodgerblue3","firebrick1","lightskyblue"))+
  facet_grid(Agg~., scales = "free", space = "free")+
  geom_text(aes(y=label, label = k), size = 3.5)
#facet_wrap(facets~., scales = "free")

SNPh2

# ============= grid =============
library(cowplot)

ace <- ggdraw() +
  draw_plot(A, x = 0, y = .75, height = .25, width = 1) +
  draw_plot(C, x = 0, y = .5, height = .25, width = 1) +
  draw_plot(E, x = 0, y = .25, height = .25, width = 1) +
  draw_plot(SNPh2, x = 0, y = 0, height = .25, width = 1)
#draw_plot_label(c("A", "B", "C","D"), c(0, .5, 0, .5), c(1, 1, .5, .5), size = 12)

ggsave(ace, filename = "~/Desktop/Meta NDD DICC/MS/Agg_corrs_A_C_E.png", height = 12, width = 5)



#####data2#####
agg <- read.table(text =
                    "Agg Grand SE Component Corr k
Study 0.3563786 0.1187356 rA 0.5 37
Study 0.353081 0.1207922 rA 0.3 37
Study 0.362881 0.1142745 rA 0.9 37
Cohort 0.3626197 0.137058 rA 0.5 14
Cohort 0.3591558 0.1379893 rA 0.3 14
Cohort 0.3694243 0.1355196 rA 0.9 14
Country 0.2567396 0.2009632 rA 0.5 7
Country 0.2579016 0.2037175 rA 0.3 7
Country 0.254279 0.1949143 rA 0.9 7
Study 0.6327429 0.327847 rC 0.5 16
Study 0.6325167 0.3267602 rC 0.3 16
Study 0.6332004 0.3299277 rC 0.9 16
Cohort 0.5581717 0.2654803 rC 0.5 6
Cohort 0.5576647 0.2627856 rC 0.3 6
Cohort 0.5593759 0.2706713 rC 0.9 6
Country 0.4846299 0.287135 rC 0.5 4
Country 0.4865425 0.2857964 rC 0.3 4
Country 0.4798656 0.2895888 rC 0.9 4
Study 0.1662566 0.05031854 rE 0.5 22
Study 0.1575259 0.04932483 rE 0.3 22
Study 0.1802529 0.05079407 rE 0.9 22
Cohort 0.1803841 0.0605361 rE 0.5 7
Cohort 0.17101 0.05788545 rE 0.3 7
Cohort 0.1966865 0.06627542 rE 0.9 7
Country 0.1657465 0.06621719 rE 0.5 6
Country 0.1586699 0.06214838 rE 0.3 6
Country 0.1782971 0.07285498 rE 0.9 6
Study 0.3861622 0.1881093 SNPrG 0.5 6
Study 0.4146083 0.2003782 SNPrG 0.3 6
Study 0.3059656 0.1434468 SNPrG 0.9 6
Cohort 0.4150269 0.2121289 SNPrG 0.5 4
Cohort 0.4300239 0.2169408 SNPrG 0.3 4
Cohort 0.3937422 0.2058996 SNPrG 0.9 4
Country 0.4523502 0.372221 SNPrG 0.5 2
Country 0.4671235 0.3743597 SNPrG 0.3 2
Country 0.4220504 0.365521 SNPrG 0.9 2", header = T)

agg$Corr <- factor(agg$Corr)
agg$Agg <- factor(agg$Agg, levels = c("Study", "Cohort", "Country"))
agg$Component <- factor(agg$Component)
agg$label <- -0.1

#####1#####
#####A#####

aggA <- agg[agg$Component == "rA",]

A <- ggplot(aggA, aes(fill=Corr, y=Grand, x=fct_rev(Corr), facets=Agg))+
  #geom_segment( aes(x=ph, xend=ph, y=0, yend=est), size = 2, position = position_dodge(width = .9))+
  #geom_point(size=25, alpha=1, shape = 21)+
  geom_bar(stat = "identity", color = "black", position = "dodge", width = .9, size = 0) +
  geom_errorbar(aes(ymin = Grand-SE, ymax  = Grand+SE),width = 0.2,position = position_dodge(.9), size  = 1)+
  theme(legend.direction = "vertical",
        legend.position="none",
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
  ggtitle("Family rA")+
  xlab("Correlation")+
  ylab("Grand estimates and standard errors")+
  scale_fill_brewer(palette = 4)+
  coord_flip()+
  #geom_text(aes(label = k), vjust = 0.5, hjust = 5, size = 15)+
  #geom_text(aes(y=label, label = k), size = 15)+
  #scale_x_discrete(labels=c("Parent","Teacher","Child"))+
  scale_y_continuous(limits = c(-0.1,1.5))+
  #scale_fill_manual(values = c("dodgerblue3","firebrick1","lightskyblue"))+
  facet_grid(Agg~., scales = "free", space = "free")+
  geom_text(aes(y=label, label = k), size = 3.5)
#facet_wrap(facets~., scales = "free")

A

#####C#####

aggC <- agg[agg$Component == "rC",]

C <- ggplot(aggC, aes(fill=Corr, y=Grand, x=fct_rev(Corr), facets=Agg))+
  #geom_segment( aes(x=ph, xend=ph, y=0, yend=est), size = 2, position = position_dodge(width = .9))+
  #geom_point(size=25, alpha=1, shape = 21)+
  geom_bar(stat = "identity", color = "black", position = "dodge", width = .9, size = 0) +
  geom_errorbar(aes(ymin = Grand-SE, ymax  = Grand+SE),width = 0.2,position = position_dodge(.9), size  = 1)+
  theme(legend.direction = "vertical",
        legend.position="none",
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
  ggtitle("Family rC")+
  xlab("Correlation")+
  ylab("Grand estimates and standard errors")+
  scale_fill_brewer(palette = 4)+
  coord_flip()+
  #geom_text(aes(label = k), vjust = 0.5, hjust = 5, size = 15)+
  #geom_text(aes(y=label, label = k), size = 15)+
  #scale_x_discrete(labels=c("Parent","Teacher","Child"))+
  scale_y_continuous(limits = c(-0.1,1.5))+
  #scale_fill_manual(values = c("dodgerblue3","firebrick1","lightskyblue"))+
  facet_grid(Agg~., scales = "free", space = "free")+
  geom_text(aes(y=label, label = k), size = 3.5)
#facet_wrap(facets~., scales = "free")

C

#####E#####

aggE <- agg[agg$Component == "rE",]

E <- ggplot(aggE, aes(fill=Corr, y=Grand, x=fct_rev(Corr), facets=Agg))+
  #geom_segment( aes(x=ph, xend=ph, y=0, yend=est), size = 2, position = position_dodge(width = .9))+
  #geom_point(size=25, alpha=1, shape = 21)+
  geom_bar(stat = "identity", color = "black", position = "dodge", width = .9, size = 0) +
  geom_errorbar(aes(ymin = Grand-SE, ymax  = Grand+SE),width = 0.2,position = position_dodge(.9), size  = 1)+
  theme(legend.direction = "vertical",
        legend.position="none",
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
  ggtitle("Family rE")+
  xlab("Correlation")+
  ylab("Grand estimates and standard errors")+
  scale_fill_brewer(palette = 4)+
  coord_flip()+
  #geom_text(aes(label = k), vjust = 0.5, hjust = 5, size = 15)+
  #geom_text(aes(y=label, label = k), size = 15)+
  #scale_x_discrete(labels=c("Parent","Teacher","Child"))+
  scale_y_continuous(limits = c(-0.1,1.5))+
  #scale_fill_manual(values = c("dodgerblue3","firebrick1","lightskyblue"))+
  facet_grid(Agg~., scales = "free", space = "free")+
  geom_text(aes(y=label, label = k), size = 3.5)
#facet_wrap(facets~., scales = "free")

E

#####A#####

aggA <- agg[agg$Component == "SNPrG",]

SNPh2 <- ggplot(aggA, aes(fill=Corr, y=Grand, x=fct_rev(Corr), facets=Agg))+
  #geom_segment( aes(x=ph, xend=ph, y=0, yend=est), size = 2, position = position_dodge(width = .9))+
  #geom_point(size=25, alpha=1, shape = 21)+
  geom_bar(stat = "identity", color = "black", position = "dodge", width = .9, size = 0) +
  geom_errorbar(aes(ymin = Grand-SE, ymax  = Grand+SE),width = 0.2,position = position_dodge(.9), size  = 1)+
  theme(legend.direction = "vertical",
        legend.position="none",
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
  ggtitle("SNP rG")+
  xlab("Correlation")+
  ylab("Grand estimates and standard errors")+
  scale_fill_brewer(palette = 4)+
  coord_flip()+
  #geom_text(aes(label = k), vjust = 0.5, hjust = 5, size = 15)+
  #geom_text(aes(y=label, label = k), size = 15)+
  #scale_x_discrete(labels=c("Parent","Teacher","Child"))+
  scale_y_continuous(limits = c(-0.1,1.5))+
  #scale_fill_manual(values = c("dodgerblue3","firebrick1","lightskyblue"))+
  facet_grid(Agg~., scales = "free", space = "free")+
  geom_text(aes(y=label, label = k), size = 3.5)
#facet_wrap(facets~., scales = "free")

SNPh2

# ============= grid =============
library(cowplot)

rG1 <- ggdraw() +
  draw_plot(A, x = 0, y = .75, height = .25, width = 1) +
  draw_plot(C, x = 0, y = .5, height = .25, width = 1) +
  draw_plot(E, x = 0, y = .25, height = .25, width = 1) +
  draw_plot(SNPh2, x = 0, y = 0, height = .25, width = 1)
#draw_plot_label(c("A", "B", "C","D"), c(0, .5, 0, .5), c(1, 1, .5, .5), size = 12)

ggsave(rG1, filename = "~/Desktop/Meta NDD DICC/MS/Agg_corrs_A_C_E.png", height = 12, width = 5)




#####data3#####
agg <- read.table(text =
                    "Agg Grand SE Component Corr k
Study 0.6216004 0.1949068 rA 0.5 15
Study 0.6206113 0.1931971 rA 0.3 15
Study 0.6234075 0.1981127 rA 0.9 15
Cohort 0.6306867 0.2101471 rA 0.5 11
Cohort 0.6296504 0.2080758 rA 0.3 11
Cohort 0.6326043 0.2140904 rA 0.9 11
Country 0.7486956 0.3241453 rA 0.5 6
Country 0.7470569 0.321445 rA 0.3 6
Country 0.7518206 0.3293354 rA 0.9 6
Study 0.8793885 0.3408401 rC 0.5 11
Study 0.8844781 0.3426042 rC 0.3 11
Study 0.8678838 0.3376945 rC 0.9 11
Cohort 0.8674792 0.3421767 rC 0.5 8
Cohort 0.875575 0.3439192 rC 0.3 8
Cohort 0.8466747 0.3370168 rC 0.9 8
Country 0.8930936 0.4091056 rC 0.5 4
Country 0.8955912 0.4005967 rC 0.3 4
Country 0.8882388 0.4463876 rC 0.9 4
Study 0.3847766 0.1434504 rE 0.5 13
Study 0.3877643 0.1447856 rE 0.3 13
Study 0.3793539 0.1413333 rE 0.9 13
Cohort 0.3863897 0.1238413 rE 0.5 10
Cohort 0.3934631 0.1308481 rE 0.3 10
Cohort 0.371018 0.1082712 rE 0.9 10
Country 0.4453136 0.1382693 rE 0.5 6
Country 0.4495355 0.1423472 rE 0.3 6
Country 0.4371067 0.1300883 rE 0.9 6", header = T)

agg$Corr <- factor(agg$Corr)
agg$Agg <- factor(agg$Agg, levels = c("Study", "Cohort", "Country"))
agg$Component <- factor(agg$Component)
agg$label <- -0.1

#####3#####
#####A#####

aggA <- agg[agg$Component == "rA",]

A <- ggplot(aggA, aes(fill=Corr, y=Grand, x=fct_rev(Corr), facets=Agg))+
  #geom_segment( aes(x=ph, xend=ph, y=0, yend=est), size = 2, position = position_dodge(width = .9))+
  #geom_point(size=25, alpha=1, shape = 21)+
  geom_bar(stat = "identity", color = "black", position = "dodge", width = .9, size = 0) +
  geom_errorbar(aes(ymin = Grand-SE, ymax  = Grand+SE),width = 0.2,position = position_dodge(.9), size  = 1)+
  theme(legend.direction = "vertical",
        legend.position="none",
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
  ggtitle("Family rA")+
  xlab("Correlation")+
  ylab("Grand estimates and standard errors")+
  scale_fill_brewer(palette = 4)+
  coord_flip()+
  #geom_text(aes(label = k), vjust = 0.5, hjust = 5, size = 15)+
  #geom_text(aes(y=label, label = k), size = 15)+
  #scale_x_discrete(labels=c("Parent","Teacher","Child"))+
  scale_y_continuous(limits = c(-0.1,1.5))+
  #scale_fill_manual(values = c("dodgerblue3","firebrick1","lightskyblue"))+
  facet_grid(Agg~., scales = "free", space = "free")+
  geom_text(aes(y=label, label = k), size = 3.5)
#facet_wrap(facets~., scales = "free")

A

ggsave(A, filename = "./Agg_corrs_A.png", height = 15, width = 15)

#####C#####

aggC <- agg[agg$Component == "rC",]

C <- ggplot(aggC, aes(fill=Corr, y=Grand, x=fct_rev(Corr), facets=Agg))+
  #geom_segment( aes(x=ph, xend=ph, y=0, yend=est), size = 2, position = position_dodge(width = .9))+
  #geom_point(size=25, alpha=1, shape = 21)+
  geom_bar(stat = "identity", color = "black", position = "dodge", width = .9, size = 0) +
  geom_errorbar(aes(ymin = Grand-SE, ymax  = Grand+SE),width = 0.2,position = position_dodge(.9), size  = 1)+
  theme(legend.direction = "vertical",
        legend.position="none",
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
  ggtitle("Family rC")+
  xlab("Correlation")+
  ylab("Grand estimates and standard errors")+
  scale_fill_brewer(palette = 4)+
  coord_flip()+
  #geom_text(aes(label = k), vjust = 0.5, hjust = 5, size = 15)+
  #geom_text(aes(y=label, label = k), size = 15)+
  #scale_x_discrete(labels=c("Parent","Teacher","Child"))+
  scale_y_continuous(limits = c(-0.1,1.5))+
  #scale_fill_manual(values = c("dodgerblue3","firebrick1","lightskyblue"))+
  facet_grid(Agg~., scales = "free", space = "free")+
  geom_text(aes(y=label, label = k), size = 3.5)
#facet_wrap(facets~., scales = "free")

C

ggsave(C, filename = "./Agg_corrs_C.png", height = 15, width = 15)

#####E#####

aggE <- agg[agg$Component == "rE",]

E <- ggplot(aggE, aes(fill=Corr, y=Grand, x=fct_rev(Corr), facets=Agg))+
  #geom_segment( aes(x=ph, xend=ph, y=0, yend=est), size = 2, position = position_dodge(width = .9))+
  #geom_point(size=25, alpha=1, shape = 21)+
  geom_bar(stat = "identity", color = "black", position = "dodge", width = .9, size = 0) +
  geom_errorbar(aes(ymin = Grand-SE, ymax  = Grand+SE),width = 0.2,position = position_dodge(.9), size  = 1)+
  theme(legend.direction = "vertical",
        legend.position="none",
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
  ggtitle("Family rE")+
  xlab("Correlation")+
  ylab("Grand estimates and standard errors")+
  scale_fill_brewer(palette = 4)+
  coord_flip()+
  #geom_text(aes(label = k), vjust = 0.5, hjust = 5, size = 15)+
  #geom_text(aes(y=label, label = k), size = 15)+
  #scale_x_discrete(labels=c("Parent","Teacher","Child"))+
  scale_y_continuous(limits = c(-0.1,1.5))+
  #scale_fill_manual(values = c("dodgerblue3","firebrick1","lightskyblue"))+
  facet_grid(Agg~., scales = "free", space = "free")+
  geom_text(aes(y=label, label = k), size = 3.5)
#facet_wrap(facets~., scales = "free")

E

ggsave(E, filename = "./Agg_corrs_E.png", height = 15, width = 15)

# ============= grid =============
library(cowplot)

rG2 <- ggdraw() +
  draw_plot(A, x = 0, y = .75, height = .25, width = 1) +
  draw_plot(C, x = 0, y = .5, height = .25, width = 1) +
  draw_plot(E, x = 0, y = .25, height = .25, width = 1)
#draw_plot_label(c("A", "B", "C","D"), c(0, .5, 0, .5), c(1, 1, .5, .5), size = 12)


#####grid total#####
library(cowplot)

all <- ggdraw() +
  draw_plot(ace, x = 0, y = 0, height = 1, width = .33) +
  draw_plot(rG1, x = .33, y = 0, height = 1, width = .33) +
  draw_plot(rG2, x = .66, y = 0, height = 1, width = .33)
#draw_plot_label(c("A", "B", "C","D"), c(0, .5, 0, .5), c(1, 1, .5, .5), size = 12)





