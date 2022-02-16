library(ggplot2)
library(cowplot)
library(reshape2)
library(likert)

h2disorders <- read.table(text = 
                            "disorder est se k sig type sex comp
SLD_h2  0.6186232 0.04323819  89  NA disorders Combined h2
ADHD_h2 0.6707750 0.03834482 121  NA disorders Combined h2
ASD_h2  0.7579345 0.11252989  36  NA disorders Combined h2
CMD_h2  0.6442894 0.19130985  23  NA disorders Combined h2
MD_h2   0.7435540 0.07721119   7  NA disorders Combined h2
LD_h2   0.8637047 0.43878049   2  NA disorders Combined h2
SLD_c2 0.1884125 0.02219481 65 NA disorders Combined c2
ADHD_c2 0.1050908 0.02394414 48 NA disorders Combined c2
ASD_c2 0.1308141 0.05257128 14 NA disorders Combined c2
CMD_c2 0.3546309 0.06133978 15 NA disorders Combined c2
MD_c2 0.1329307 0.1143858 3 NA disorders Combined c2
SLD_e2 0.240122 0.01920605 67 NA disorders Combined e2
ADHD_e2 0.3048609 0.01718971 107 NA disorders Combined e2
ASD_e2 0.26611 0.02601681 28 NA disorders Combined e2
CMD_e2 0.2136093 0.03821487 18 NA disorders Combined e2
MD_e2 0.3806472 0.1126349 6 NA disorders Combined e2
LD_e2 0.09501981 0.1614803 2 NA disorders Combined e2
SLD_snph2  0.2972107 0.07954612  9  NA disorders Combined h2
ADHD_snph2 0.2033597 0.03993313 14  NA disorders Combined h2
ASD_snph2  0.1446589 0.04015108 15  NA disorders Combined h2
CMD_snph2  0.3212003 0.13709231  4  NA disorders Combined h2", header=T)

facets <- c("Family h2","Family h2","Family h2","Family h2","Family h2","Family h2",
            "Family c2","Family c2","Family c2","Family c2","Family c2",
            "Family e2","Family e2","Family e2","Family e2","Family e2","Family e2",
            "SNP h2","SNP h2","SNP h2","SNP h2"
)

h2disorders$disorder <- factor(h2disorders$disorder, levels = unique(h2disorders$disorder))
h2disorders$disorder <- factor(h2disorders$disorder, levels = c( "LD_h2","CMD_h2","ASD_h2","ADHD_h2","SLD_h2","MD_h2",
                                                                 "CMD_c2","ASD_c2","ADHD_c2","SLD_c2","MD_c2",
                                                                 "LD_e2","CMD_e2","ASD_e2","ADHD_e2","SLD_e2","MD_e2",
                                                                 "CMD_snph2","ASD_snph2","ADHD_snph2","SLD_snph2"),
                               labels = c("Intellectual disabilities","Communication disorders","ASD","ADHD","Specific learning disorders","Motor disorders",   
                                          "Communication disorders","ASD","ADHD","Specific learning disorders","Motor disorders",   
                                          "Intellectual disabilities","Communication disorders","ASD","ADHD","Specific learning disorders","Motor disorders",   
                                          "Communication disorders","ASD","ADHD","Specific learning disorders"
                               ))

h2disorders$label <- -0.1
h2disorders$facets <- facets
h2disorders$facets <- factor(h2disorders$facets, levels = unique(h2disorders$facets))

h2disorders$disorder <- reverse.levels(h2disorders$disorder)
h2disorders$facets <- reverse.levels(h2disorders$facets)

p <- ggplot(h2disorders, aes(fill=facets, x=facets, y= est))+
  #geom_segment( aes(x=ph, xend=ph, y=0, yend=est), size = 2, position = position_dodge(width = .9))+
  #geom_point(size=25, alpha=1, shape = 21)+
  geom_bar(stat = "identity", color = "black", position = "dodge", width = .9, size = 0) +
  geom_errorbar(aes(ymin = est-se, ymax  = est+se),width = 0.2,position = position_dodge(.9), size  = .7)+
  theme(legend.direction = "vertical",
        legend.position="right",
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
  ggtitle("")+
  xlab("")+
  ylab("Grand estimate")+
  #scale_fill_brewer(palette = "BuPu")+
  #scale_fill_brewer(palette = 4)+
  coord_flip()+
  #geom_text(aes(label = k), vjust = 0.5, hjust = 5, size = 10)+
  geom_text(aes(y=label, label = k), size = 3)+
  #scale_fill_gradient2(low = "#faa300", high = "orange")+
  #scale_x_discrete(labels=c("Parent","Teacher","Child"))+
  #scale_y_continuous(limits = c(0,.06))+
  scale_fill_manual(values = c("dodgerblue3","firebrick1","lightskyblue","orange"),
                    breaks = c("Family h2","Family c2","Family e2","SNP h2"))+
  facet_grid(disorder~., scales = "free", space = "free")+
  scale_y_continuous(limits = c(-.1,1.4), breaks = seq(0,1.4,0.2))
#facet_wrap(facets~., scales = "free")
