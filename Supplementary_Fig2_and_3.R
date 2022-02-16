library(ggplot2)
library(cowplot)
library(reshape2)
library(likert)

#####h2#####
data <- read.table(text = 
                     "group pheno est se
LD LD	0.863704731	0.438780491

CMD Language_ability	0.650307691	0.204802666
CMD SLI	0.868654013	0.60312253
CMD Speech	0.799211264	0.166681317
CMD Stuttering	0.580573257	0.167848365
CMD Syntax	0.648968623	0.369238501

ADHD ADHD	0.702311266	0.047861471
ADHD Hyperactivity	0.655242261	0.164539562
ADHD Impulsivity	0.756101177	0.070593062
ADHD Hyperactivity/Impulsivity 0.685044195	0.059941698
ADHD Inattention	0.649577562	0.052140407

ASD ASD	0.789674338	0.135712621
ASD CIs	0.763044905	0.089512119
ASD RRBIs	0.83431613	0.494442729
ASD SIs	0.665106911	0.049124892
ASD Strict_autism	0.509867824	0.284682343

SLD Dyslexia	0.621781428	0.036464516
SLD Dysgraphia	0.556386839	0.175326086
SLD Dyscalculia	0.547499986	0.038644244

Dyslexia Dyslexia1	0.553235204	0.157618797
Dyslexia Decoding	0.690921415	0.136150697
Dyslexia Grammar	0.549705708	0.098807577
Dyslexia Nonword_reading	0.666618146	0.12817131
Dyslexia Ortho_skills	0.488633274	0.146348353
Dyslexia Phonolo_skills	0.588593211	0.086471252
Dyslexia Rapid_Naming	0.603752068	0.122152397
Dyslexia Reading	0.624025277	0.04053953
Dyslexia Reading_Compreh	0.562060762	0.067134144
Dyslexia Reading_Fluency	0.643458219	0.133680452
Dyslexia Spelling	0.620961132	0.114532884
Dyslexia Vocabulary	0.254450763	0.138343065
Dyslexia Word_Reading	0.647955583	0.075741045

Dysgraphia Writing	0.556386839	0.175326086

Dyscalculia Calculations	0.389034649	0.134982248
Dyscalculia Maths	0.571242372	0.035231628
Dyscalculia Maths_Fluency	0.524742422	0.142544388
Dyscalculia Maths_Problems	0.358918771	0.185380683

MD Coordination	0.818674821	0.074692106
MD DCD	0.691445059	0.130187654
MD Motor_Control	0.682691092	0.120784221
MD TIC	0.563161369	0.170662873
", header=T)

data$pheno <- factor(data$pheno, levels = c("LD",  "Language_ability", "Speech", "Stuttering", 
                                            "Syntax",  "SLI",  "ADHD", "Hyperactivity", "Impulsivity", "Hyperactivity/Impulsivity", 
                                            "Inattention",  "ASD", "CIs", "RRBIs", "SIs", "Strict_autism", 
                                            "Dyslexia", "Dysgraphia", "Dyscalculia",  "Dyslexia1", 
                                            "Decoding", "Grammar", "Nonword_reading", "Ortho_skills", "Phonolo_skills", 
                                            "Rapid_Naming", "Reading", "Reading_Compreh", "Reading_Fluency", 
                                            "Spelling", "Vocabulary", "Word_Reading",  "Writing",  
                                            "Calculations", "Maths", "Maths_Fluency", "Maths_Problems",  
                                            "Coordination", "DCD", "Motor_Control", "TIC"),
                     labels = c("LD",   "Language ability", "Speech", "Stuttering", 
                                "Syntax",   "SLI", "ADHD", "Hyperactivity", "Impulsivity", "Hyperactivity/Impulsivity", 
                                "Inattention",   "ASD", "CIs", "RRBIs", "SIs", "Strict autism", 
                                "Dyslexia", "Dysgraphia", "Dyscalculia",   "Dyslexia", 
                                "Decoding", "Grammar", "Nonword reading", "Orthographic skills", "Phonological skills", 
                                "Rapid naming", "Reading ability", "Reading comprehension", "Reading fluency", 
                                "Spelling", "Vocabulary", "Word reading",   "Writing",   
                                "Calculations", "Maths ability", "Maths fluency", "Problem solving",   
                                "Coordination", "DCD", "Motor control", "Tics"))

data$group <- factor(data$group, levels = c("LD", "CMD", "CMD", "CMD", "CMD", "CMD",
                                            "ASD", "ASD", "ASD", "ASD", "ASD",
                                            "ADHD", "ADHD", "ADHD", "ADHD", "ADHD", 
                                            "SLD", "SLD", "SLD", 
                                            "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", 
                                            "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", 
                                            "Dysgraphia",
                                            "Dyscalculia", "Dyscalculia", "Dyscalculia", "Dyscalculia",  
                                            "MD", "MD", "MD", "MD"),
                     labels = c("Intellectual disabilities",
                                "Communication disorders", "Communication disorders", "Communication disorders", "Communication disorders", "Communication disorders",
                                "ASD", "ASD", "ASD", "ASD", "ASD",
                                "ADHD", "ADHD", "ADHD", "ADHD", "ADHD", 
                                "Specific learning disorders", "Specific learning disorders", "Specific learning disorders",
                                "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes",
                                "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes",
                                "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes",
                                "Dyslexia-related phenotypes",
                                "Dysgraphia-related phenotypes",
                                "Dyscalculia-related phenotypes", "Dyscalculia-related phenotypes", "Dyscalculia-related phenotypes", "Dyscalculia-related phenotypes",
                                "Motor disorders", "Motor disorders", "Motor disorders", "Motor disorders"))

data$pheno <- factor(data$pheno, levels = unique(data$pheno))
data$group <- factor(data$group, levels = unique(data$group))
data$group <- with(data, reorder(group, pheno))

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=est, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=est, fill=group), stat="identity", alpha=1) +
  geom_errorbar(aes(ymin = est-se, ymax  = est+se),width = 0.2,position = position_dodge(.9), size  = .5, alpha=1)+
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.8, xend = start, yend = 0.8), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.6, xend = start, yend = 0.6), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.4, xend = start, yend = 0.4), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.2, xend = start, yend = 0.2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),6), y = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0", "0.2", "0.4", "0.6", "0.8", "1") , color="black", size=4 , angle=0, hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=est, fill=group), stat="identity", alpha=0.5) +
  ylim(-1,3) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank() 
  ) +
  coord_polar() + 
  scale_fill_manual(breaks = c("Intellectual disabilities",
                               "Communication disorders", 
                               "ASD",
                               "ADHD", 
                               "Specific learning disorders",
                               "Dyslexia-related phenotypes",
                               "Dysgraphia-related phenotypes",
                               "Dyscalculia-related phenotypes",
                               "Motor disorders"),
                    values = c("#184e77","#1e6091","#1a759f",
                               "#168aad","#34a0a4","#52b69a",
                               "#76c893","#99d98c","#b5e48c"))+
  ggtitle("Heritability of sub-categories of Neurodevelopmental Disorders")+
  geom_text(data=label_data, aes(x=id, y= est+se+0.2, label=pheno, hjust=hjust), color="black", alpha=1, size=4, angle= label_data$angle, inherit.aes = FALSE ) 

#####rA#####
data <- read.table(text = 
                     "group pheno est se
ADHD_ASD ADHD_ASD	0.707655554	0.269200848
ADHD_ASD Hyp_SIs	0.221457633	0.187047535
ADHD_ASD Inatt_RRBIs	0.160136385	0.106785137
ADHD_ASD Inatt_SIs	0.27093204	0.238245871
ADHD_MD ADHD_DCD	0.90953082	0.803324371
ADHD_SLD ADHD_Dyslexia	0.066819	0.119934685
ADHD_SLD ADHD_Dyscalculia	-0.285848682	0.10661913
ADHD_SLD ADHD_Reading	0.189628588	0.219497722
ADHD_SLD Hyp_Reading	0.11236683	0.084846985
ADHD_SLD Inatt_Reading	0.066371708	0.161128423
ADHD_SLD Inatt_Maths	-0.323198043	0.114065791
CMD_SLD SLI_Dyslexia	0.664886859	0.154351398
SLD_SLD Dyslexia_Dyscalculia	0.557132452	0.064594214
SLD_SLD Reading_Maths	0.553292084	0.07632603
ADHD_ODD ADHD_ODD	0.583380723	0.197199564
ADHD_ODD Hyp_ODD	0.800304253	0.56839256
ADHD_ODD Inatt_ODD	0.517984687	0.099667995
", header=T)

data$pheno <- factor(data$pheno, levels = c("ADHD_ASD", "Hyp_SIs", "Inatt_RRBIs", "Inatt_SIs", "ADHD_DCD", 
                                            "ADHD_Dyslexia", "ADHD_Dyscalculia", "ADHD_Reading", "Hyp_Reading", 
                                            "Inatt_Reading", "Inatt_Maths", "SLI_Dyslexia", "Dyslexia_Dyscalculia", 
                                            "Reading_Maths", "ADHD_ODD", "Hyp_ODD", "Inatt_ODD"),
                     labels = c("ADHD & ASD", "Hyperactivity & SIs", "Inattention & RRBIs", "Inattention & SIs", 
                                "ADHD & DCD", 
                                "ADHD & dyslexia", "ADHD & dyscalculia", "ADHD & reading ability", "Hyperactivity & reading ability", 
                                "Inattention & reading ability", "Inattention & maths ability", "SLI & dyslexia", "Dyslexia & dyscalculia", 
                                "Reading ability & maths ability",
                                "ADHD & ODD", "Hyperactivity & ODD", "Inattention & ODD"))

data$group <- factor(data$group, levels = c("ADHD_ASD", "ADHD_ASD", "ADHD_ASD", "ADHD_ASD", "ADHD_MD", 
                                            "ADHD_SLD", "ADHD_SLD", "ADHD_SLD", "ADHD_SLD", "ADHD_SLD", "ADHD_SLD", 
                                            "CMD_SLD", "SLD_SLD", "SLD_SLD",
                                            "ADHD_ODD", "ADHD_ODD", "ADHD_ODD"),
                     labels = c("ADHD & ASD", "ADHD & ASD", "ADHD & ASD", "ADHD & ASD", "ADHD & motor disorders", 
                                "ADHD & specific learning disorders", "ADHD & specific learning disorders", 
                                "ADHD & specific learning disorders", "ADHD & specific learning disorders", 
                                "ADHD & specific learning disorders", "ADHD & specific learning disorders", 
                                "Communication disorders & specific learning disorders", 
                                "Specific learning disorders subtypes", 
                                "Specific learning disorders subtypes",
                                "ADHD & ODD", "ADHD & ODD", "ADHD & ODD"))

data$pheno <- factor(data$pheno, levels = unique(data$pheno))
data$group <- factor(data$group, levels = unique(data$group))
#data$group <- with(data, reorder(group, pheno))

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p1 <- ggplot(data, aes(x=as.factor(id), y=est, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=est, fill=group), stat="identity", alpha=1) +
  geom_errorbar(aes(ymin = est-se, ymax  = est+se),width = 0.2,position = position_dodge(.9), size  = .5, alpha=1)+
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.8, xend = start, yend = 0.8), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.6, xend = start, yend = 0.6), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.4, xend = start, yend = 0.4), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.2, xend = start, yend = 0.2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),6), y = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0", "0.2", "0.4", "0.6", "0.8", "1") , color="black", size=4 , angle=0, hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=est, fill=group), stat="identity", alpha=0.5) +
  ylim(-1,3) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_polar() + 
  scale_fill_manual(breaks = c("ADHD & ASD", "ADHD & motor disorders", 
                               "ADHD & specific learning disorders", 
                               "Communication disorders & specific learning disorders", 
                               "Specific learning disorders subtypes",
                               "ADHD & ODD"),
                    values = c("#d90429","#f94144",
                               "#f3722c",
                               "#f8961e","#f9844a","#f9c74f"))+
  ggtitle("Genetic correlations \nbetween sub-categories of Neurodevelopmental Disorders \nand between sub-categories of Neurodevelopmental and DICC disorders")+
  geom_text(data=label_data, aes(x=id, y= est+se+0.3, label=pheno, hjust=hjust), color="black", alpha=1, size=4, angle= label_data$angle, inherit.aes = FALSE ) 

#####grid#####
library(cowplot)

Fig = ggdraw() +
  draw_plot(p, x = 0, y = .5, height = .5, width = 1) +
  draw_plot(p1, x = 0, y = 0, height = .5, width = 1) 
#draw_plot_label(c("Whole sample", "Males vs females"), c(0.3, 0.2), c(1, 1), size = 20)

#####snph2#####
data <- read.table(text = 
                     "group pheno est se
                     
CMD Language_ability 0.321200332	0.137092314

ADHD ADHD	0.205287298	0.042106751
ADHD Hyperactivity/Impulsivity 0.126954862	0.105617551
ADHD Inattention	0.269857517	0.165832913

ASD ASD	0.129995414	0.044328886
ASD SIs	0.203665406	0.091930458

SLD Dyslexia 0.270922992	0.094659778
SLD Dyscalculia	0.319893547	0.111375797

Dyslexia Reading	0.279823129	0.109095429

Dyscalculia Maths	0.319893547	0.111375797
", header=T)

data$pheno <- factor(data$pheno, levels = c("Language_ability", "ADHD", "Hyperactivity/Impulsivity", 
                                            "Inattention", "ASD", "SIs", "Dyslexia", "Dyscalculia", "Reading", 
                                            "Maths"),
                     labels = c("Language ability", "ADHD", "Hyperactivity/Impulsivity", 
                                "Inattention", "ASD", "SIs",
                                "Dyslexia", "Dyscalculia", "Reading ability", "Maths ability"))

data$group <- factor(data$group, levels = c("CMD", "ADHD", "ADHD", "ADHD", "ASD", "ASD", "SLD", "SLD", 
                                            "Dyslexia", "Dyscalculia"),
                     labels = c("Communication Disorders", 
                                "ADHD", "ADHD", "ADHD", 
                                "ASD", "ASD",
                                "Specific Learning Disorders", "Specific Learning Disorders", 
                                "Dyslexia-related phenotypes",
                                "Dyscalculia-specific phenotypes"))

data$pheno <- factor(data$pheno, levels = unique(data$pheno))
data$group <- factor(data$group, levels = unique(data$group))
data$group <- with(data, reorder(group, pheno))

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=est, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=est, fill=group), stat="identity", alpha=1) +
  geom_errorbar(aes(ymin = est-se, ymax  = est+se),width = 0.2,position = position_dodge(.9), size  = .5, alpha=1)+
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.8, xend = start, yend = 0.8), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.6, xend = start, yend = 0.6), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.4, xend = start, yend = 0.4), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.2, xend = start, yend = 0.2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),6), y = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0", "0.2", "0.4", "0.6", "0.8", "1") , color="black", size=4 , angle=0, hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=est, fill=group), stat="identity", alpha=0.5) +
  ylim(-1,3) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank() 
  ) +
  coord_polar() + 
  scale_fill_manual(breaks = c("Communication Disorders", 
                               "ASD",
                               "ADHD", 
                               "Specific Learning Disorders",
                               "Dyslexia-related phenotypes",
                               "Dyscalculia-specific phenotypes"),
                    values = c("#184e77","#1a759f",
                               "#168aad","#52b69a",
                               "#76c893","#b5e48c"))+
  ggtitle("SNP heritability of sub-categories of Neurodevelopmental Disorders")+
  geom_text(data=label_data, aes(x=id, y= est+se+0.2, label=pheno, hjust=hjust), color="black", alpha=1, size=4, angle= label_data$angle, inherit.aes = FALSE ) 

#####c2#####
data <- read.table(text = 
                     "group pheno est se

CMD Language_ability	0.359246212	0.067305582

ADHD ADHD	0.115254133	0.034396516
ADHD Hyperactivity/Impulsivity 0.162862556	0.057119792
ADHD Inattention	0.081508462	0.031493524

ASD ASD	0.061922763	0.035847938
ASD RRBIs	0.239616924	0.244489673
ASD SIs	0.313604698	0.215058498

SLD Dyslexia	0.190036963	0.024854202
SLD Dysgraphia	0.078796975	0.080857536
SLD Dyscalculia	0.19331812	0.037188086

Dyslexia Dyslexia1	0.236540796	0.174576739
Dyslexia Decoding	0.173221611	0.100042469
Dyslexia Grammar	0.296174993	0.243246254
Dyslexia Ortho_skills	0.458285495	0.182768642
Dyslexia Phonolo_skills 0.203784105	0.084574346
Dyslexia Rapid_Naming	0.173630791	0.127883721
Dyslexia Reading	0.188031705	0.030688726
Dyslexia Reading_Compreh	0.193664456	0.066593482
Dyslexia Reading_Fluency	0.157275637	0.085496367
Dyslexia Spelling	0.136989291	0.076631576
Dyslexia Vocabulary	0.574675398	0.154190595
Dyslexia Word_Reading	0.215865196	0.064881569

Dysgraphia Writing	0.078796975	0.080857536

Dyscalculia Maths	0.189279621	0.037966144
Dyscalculia Maths_Fluency	0.209878532	0.135344009
Dyscalculia Maths_Problems	0.275004454	0.188659923

MD DCD	0.124982726	0.153691763
", header=T)

data$pheno <- factor(data$pheno, levels = c("Language_ability", "ADHD", "Hyperactivity/Impulsivity", "Inattention", 
                                            "ASD", "RRBIs", "SIs", "Dyslexia", "Dysgraphia", "Dyscalculia", 
                                            "Dyslexia1", "Decoding", "Grammar", "Ortho_skills", "Phonolo_skills", 
                                            "Rapid_Naming", "Reading", "Reading_Compreh", "Reading_Fluency", 
                                            "Spelling", "Vocabulary", "Word_Reading", "Writing", "Maths", 
                                            "Maths_Fluency", "Maths_Problems", "DCD"),
                     labels = c("Language ability", "ADHD", "Hyperactivity/Impulsivity", 
                                "Inattention", "ASD", "RRBIs", "SIs", 
                                "Dyslexia", "Dysgraphia", "Dyscalculia",  
                                "Dyslexia", 
                                "Decoding", "Grammar", "Ortho skills", "Phonolo skills", 
                                "Rapid naming", "Reading ability", "Reading comprehension", "Reading fluency", 
                                "Spelling", "Vocabulary", "Word reading",   "Writing",   
                                "Maths ability", "Maths fluency", "Problem solving",   
                                "DCD"))

data$group <- factor(data$group, levels = c("CMD", "ADHD", "ADHD", "ADHD", "ASD", "ASD", "ASD", "SLD", 
                                            "SLD", "SLD", "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", 
                                            "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", 
                                            "Dyslexia", "Dyslexia", "Dysgraphia", "Dyscalculia", "Dyscalculia", 
                                            "Dyscalculia", "MD"),
                     labels = c("Communication Disorders", 
                                "ADHD", "ADHD", "ADHD", "ASD", "ASD", "ASD",
                                "Specific Learning Disorders", "Specific Learning Disorders", "Specific Learning Disorders",
                                "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes",
                                "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes",
                                "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes",
                                "Dyslexia-related phenotypes",
                                "Dysgraphia-specific phenotypes",
                                "Dyscalculia-specific phenotypes", "Dyscalculia-specific phenotypes", "Dyscalculia-specific phenotypes",
                                "Motor Disorders"))

data$pheno <- factor(data$pheno, levels = unique(data$pheno))
data$group <- factor(data$group, levels = unique(data$group))

data$group <- with(data, reorder(group, pheno))

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=est, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=est, fill=group), stat="identity", alpha=1) +
  geom_errorbar(aes(ymin = est-se, ymax  = est+se),width = 0.2,position = position_dodge(.9), size  = .5, alpha=1)+
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.8, xend = start, yend = 0.8), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.6, xend = start, yend = 0.6), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.4, xend = start, yend = 0.4), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.2, xend = start, yend = 0.2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),6), y = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0", "0.2", "0.4", "0.6", "0.8", "1") , color="black", size=4 , angle=0, hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=est, fill=group), stat="identity", alpha=0.5) +
  ylim(-1,3) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank() 
  ) +
  coord_polar() + 
  scale_fill_manual(breaks = c("Communication Disorders", 
                               "ASD",
                               "ADHD", 
                               "Specific Learning Disorders",
                               "Dyslexia-related phenotypes",
                               "Dysgraphia-related phenotypes",
                               "Dyscalculia-related phenotypes",
                               "Motor Disorders"),
                    values = c("#1e6091","#1a759f",
                               "#168aad","#34a0a4","#52b69a",
                               "#76c893","#99d98c","#b5e48c"))+
  ggtitle("Shared environmental influences on sub-categories of Neurodevelopmental Disorders")+
  geom_text(data=label_data, aes(x=id, y= est+se+0.2, label=pheno, hjust=hjust), color="black", alpha=1, size=4, angle= label_data$angle, inherit.aes = FALSE ) 

#####e2#####
data <- read.table(text = 
                     "group pheno est se
                     
LD LD	0.095019811	0.161480338

CMD Language_ability	0.214378087	0.04477453
CMD Speech 0.199762797	0.154901468
CMD Stuttering 0.205510956	0.117046038
CMD Syntax 0.488533654	0.23585355

ADHD ADHD	0.304856998	0.029683387
ADHD Hyperactivity 0.378474607	0.106470537
ADHD Impulsivity 0.236810133	0.081468697
ADHD Hyperactivity/Impulsivity 0.27237892	0.026314018
ADHD Inattention	0.276854887	0.018819164

ASD ASD	0.258011958	0.033195492
ASD CIs 0.271786482	0.056165825
ASD RRBIs	0.351419203	0.088725573
ASD SIs	0.296843975	0.052144668

SLD Dyslexia	0.230985876	0.020654715
SLD Dysgraphia	0.379681842	0.119700224
SLD Dyscalculia	0.266055625	0.023849798

Dyslexia Dyslexia1	0.256913883	0.147976695
Dyslexia Decoding	0.1485955	0.056859453
Dyslexia Grammar	0.263219057	0.097114471
Dyslexia Phonolo_skills 0.225239168	0.063612944
Dyslexia Rapid_Naming	0.249805603	0.083406313
Dyslexia Reading	0.227482152	0.028299033
Dyslexia Reading_Compreh	0.258108751	0.049048443
Dyslexia Reading_Fluency	0.245086655	0.060622791
Dyslexia Spelling	0.232221783	0.064489263
Dyslexia Vocabulary	0.177510764	0.07191791
Dyslexia Word_Reading	0.123231225	0.035443411

Dysgraphia Writing	0.379681842	0.119700224

Dyscalculia Calculations 0.55046478	0.225182773
Dyscalculia Maths	0.24753129	0.02461272
Dyscalculia Maths_Fluency	0.272779312	0.090185138
Dyscalculia Maths_Problems	0.36325314	0.125677589

MD Coordination 0.384139767	0.257184827
MD DCD	0.425644429	0.197127351
MD Motor_control 0.405489827	0.331115981
MD TIC 0.437132649	0.158220446
", header=T)

data$pheno <- factor(data$pheno, levels = c("LD", "Language_ability", "Speech", "Stuttering", "Syntax", 
                                            "ADHD", "Hyperactivity", "Impulsivity", "Hyperactivity/Impulsivity", 
                                            "Inattention", "ASD", "CIs", "RRBIs", "SIs", "Dyslexia", "Dysgraphia", 
                                            "Dyscalculia", "Dyslexia1", "Decoding", "Grammar", "Phonolo_skills", 
                                            "Rapid_Naming", "Reading", "Reading_Compreh", "Reading_Fluency", 
                                            "Spelling", "Vocabulary", "Word_Reading", "Writing", "Calculations", 
                                            "Maths", "Maths_Fluency", "Maths_Problems", "Coordination", "DCD", 
                                            "Motor_control", "TIC"),
                     labels = c("Learning disability", "Language ability", "Speech", "Stuttering", "Syntax", 
                                "ADHD", "Hyperactivity", "Impulsivity", "Hyperactivity/Impulsivity", 
                                "Inattention", "ASD", "CIs", "RRBIs", "SIs", "Dyslexia", "Dysgraphia", 
                                "Dyscalculia", "Dyslexia", "Decoding", "Grammar", "Phonological skills", 
                                "Rapid naming", "Reading ability", "Reading comprehension", "Reading fluency", 
                                "Spelling", "Vocabulary", "Word reading", "Writing ability", "Calculations", 
                                "Maths ability", "Maths fluency", "Maths problems", "Coordination", "DCD", 
                                "Motor control", "Tics"))

data$group <- factor(data$group, levels = c("LD", "CMD", "CMD", "CMD", "CMD", "ADHD", "ADHD", "ADHD", "ADHD", 
                                            "ADHD", "ASD", "ASD", "ASD", "ASD", "SLD", "SLD", "SLD", "Dyslexia", 
                                            "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", 
                                            "Dyslexia", "Dyslexia", "Dyslexia", "Dyslexia", "Dysgraphia", 
                                            "Dyscalculia", "Dyscalculia", "Dyscalculia", "Dyscalculia", "MD", 
                                            "MD", "MD", "MD"),
                     labels = c("Intellectual disabilities", "Communication disorders", "Communication disorders", "Communication disorders", "Communication disorders", "ADHD", "ADHD", "ADHD", "ADHD", 
                                "ADHD", "ASD", "ASD", "ASD", "ASD", "Specific learning disorders", "Specific learning disorders", "Specific learning disorders", "Dyslexia-related phenotypes", 
                                "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", 
                                "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dyslexia-related phenotypes", "Dysgraphia-related phenotypes", 
                                "Dyscalculia-related phenotypes", "Dyscalculia-related phenotypes", "Dyscalculia-related phenotypes", "Dyscalculia-related phenotypes", 
                                "Motor disorders", 
                                "Motor disorders", "Motor disorders", "Motor disorders"))

data$pheno <- factor(data$pheno, levels = unique(data$pheno))
data$group <- factor(data$group, levels = unique(data$group))
data$group <- with(data, reorder(group, pheno))

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=est, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=est, fill=group), stat="identity", alpha=1) +
  geom_errorbar(aes(ymin = est-se, ymax  = est+se),width = 0.2,position = position_dodge(.9), size  = .5, alpha=1)+
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.8, xend = start, yend = 0.8), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.6, xend = start, yend = 0.6), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.4, xend = start, yend = 0.4), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.2, xend = start, yend = 0.2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),6), y = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0", "0.2", "0.4", "0.6", "0.8", "1") , color="black", size=4 , angle=0, hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=est, fill=group), stat="identity", alpha=0.5) +
  ylim(-1,3) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank() 
  ) +
  coord_polar() + 
  scale_fill_manual(breaks = c("Intellectual disabilities",
                               "Communication disorders", 
                               "ASD",
                               "ADHD", 
                               "Specific learning disorders",
                               "Dyslexia-related phenotypes",
                               "Dysgraphia-related phenotypes",
                               "Dyscalculia-related phenotypes",
                               "Motor disorders"),
                    values = c("#184e77","#1e6091","#1a759f",
                               "#168aad","#34a0a4","#52b69a",
                               "#76c893","#99d98c","#b5e48c"))+
  ggtitle("Heritability and environmental influences on sub-categories of Neurodevelopmental Disorders")+
  geom_text(data=label_data, aes(x=id, y= est+se+0.2, label=pheno, hjust=hjust), color="black", alpha=1, size=4, angle= label_data$angle, inherit.aes = FALSE ) 

#####rC#####
data <- read.table(text = 
                     "group pheno est se
ADHD_SLD ADHD_Dyslexia	0.32316492	0.152919731
ADHD_SLD ADHD_Reading	0.119817597	0.112002993
ADHD_SLD Hyp_Reading	0.664253614	0.190652647
ADHD_SLD Inatt_Reading	0.434009615	0.257039672
SLD_SLD Dyslexia_Dyscalculia	0.790874438	0.455043763
SLD_SLD Reading_Maths	0.790874438	0.455043763
ADHD_ODD ADHD_ODD	0.950829274	0.6776079
ADHD_ODD Hyp_ODD	0.874844562	0.859985852
", header=T)

data$pheno <- factor(data$pheno, levels = c("ADHD_Dyslexia", "ADHD_Reading", "Hyp_Reading", "Inatt_Reading", 
                                            "Dyslexia_Dyscalculia", "Reading_Maths", "ADHD_ODD", "Hyp_ODD"),
                     labels = c("ADHD & dyslexia", "ADHD & reading ability", "Hyperactivity & reading ability", "Inattention & reading ability", 
                                "Dyslexia & dyscalculia", "Reading ability & maths ability", "ADHD & ODD", "Hyperactivity & ODD"))

data$group <- factor(data$group, levels = c("ADHD_SLD", "ADHD_SLD", "ADHD_SLD", "ADHD_SLD", "SLD_SLD", 
                                            "SLD_SLD", "ADHD_ODD", "ADHD_ODD"),
                     labels = c("ADHD & specific learning disorders", "ADHD & specific learning disorders", "ADHD & specific learning disorders", "ADHD & specific learning disorders", 
                                "Specific learning disorders subtypes","Specific learning disorders subtypes", 
                                "ADHD & ODD", "ADHD & ODD"))

data$pheno <- factor(data$pheno, levels = unique(data$pheno))
data$group <- factor(data$group, levels = unique(data$group))
#data$group <- with(data, reorder(group, pheno))

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p1 <- ggplot(data, aes(x=as.factor(id), y=est, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=est, fill=group), stat="identity", alpha=1) +
  geom_errorbar(aes(ymin = est-se, ymax  = est+se),width = 0.2,position = position_dodge(.9), size  = .5, alpha=1)+
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.8, xend = start, yend = 0.8), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.6, xend = start, yend = 0.6), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.4, xend = start, yend = 0.4), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.2, xend = start, yend = 0.2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),6), y = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0", "0.2", "0.4", "0.6", "0.8", "1") , color="black", size=4 , angle=0, hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=est, fill=group), stat="identity", alpha=0.5) +
  ylim(-1,3) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_polar() + 
  scale_fill_manual(breaks = c("ADHD & specific learning disorders", 
                               "Specific learning disorders subtypes",
                               "ADHD & ODD"),
                    values = c("#f3722c",
                               "#f9844a","#f9c74f"))+
  ggtitle("Genetic correlations \nbetween sub-categories of Neurodevelopmental Disorders \nand between sub-categories of Neurodevelopmental and DICC disorders")+
  geom_text(data=label_data, aes(x=id, y= est+se+0.3, label=pheno, hjust=hjust), color="black", alpha=1, size=4, angle= label_data$angle, inherit.aes = FALSE ) 

#####rE#####
data <- read.table(text = 
                     "group pheno est se
ADHD_ASD ADHD_ASD	0.26726862	0.11338512
ADHD_ASD Hyp_SIs	0.023333877	0.081468697
ADHD_ASD Inatt_RRBIs	0.09247932	0.108633919
ADHD_ASD Inatt_SIs	0.03	0.081468697

ADHD_SLD ADHD_Dyslexia	0.111539077	0.044663884
ADHD_SLD ADHD_Dyscalculia	0.093812826	0.095263552
ADHD_SLD ADHD_Reading	0.098202296	0.077305119
ADHD_SLD Hyp_Reading	0.033716605	0.048181432
ADHD_SLD Inatt_Reading	0.159163405	0.059537284
ADHD_SLD Inatt_Maths	0.145956376	0.103403977

SLD_SLD Dyslexia_Dyscalculia	0.253485663	0.071543331
SLD_SLD Reading_Maths		0.253485663	0.071543331

ADHD_ODD ADHD_ODD	0.289843407	0.096128828
ADHD_ODD Hyp_ODD	0.874951071	0.739210694
ADHD_ODD Inatt_ODD	0.494173237	0.105561074", header=T)

data$pheno <- factor(data$pheno, levels = c("ADHD_ASD", "Hyp_SIs", "Inatt_RRBIs", "Inatt_SIs", "ADHD_Dyslexia", 
                                            "ADHD_Dyscalculia", "ADHD_Reading", "Hyp_Reading", "Inatt_Reading", 
                                            "Inatt_Maths", "Dyslexia_Dyscalculia", "Reading_Maths", "ADHD_ODD", 
                                            "Hyp_ODD", "Inatt_ODD"),
                     labels = c("ADHD & ASD", "Hyperactivity & SIs", "Inattention & RRBIs", "Inattention & SIs", 
                                "ADHD & dyslexia", "ADHD & dyscalculia", "ADHD & reading ability", "Hyperactivity & reading ability", 
                                "Inattention & reading ability", "Inattention & maths ability", "Dyslexia & dyscalculia", 
                                "Reading ability & maths ability",
                                "ADHD & ODD", "Hyperactivity & ODD", "Inattention & ODD"))

data$group <- factor(data$group, levels = c("ADHD_ASD", "ADHD_ASD", "ADHD_ASD", "ADHD_ASD", "ADHD_SLD", 
                                            "ADHD_SLD", "ADHD_SLD", "ADHD_SLD", "ADHD_SLD", "ADHD_SLD", "SLD_SLD", 
                                            "SLD_SLD", "ADHD_ODD", "ADHD_ODD", "ADHD_ODD"),
                     labels = c("ADHD & ASD", "ADHD & ASD", "ADHD & ASD", "ADHD & ASD",
                                "ADHD & specific learning disorders", "ADHD & specific learning disorders", 
                                "ADHD & specific learning disorders", "ADHD & specific learning disorders", 
                                "ADHD & specific learning disorders", "ADHD & specific learning disorders", 
                                "Specific learning disorders subtypes", 
                                "Specific learning disorders subtypes",
                                "ADHD & ODD", "ADHD & ODD", "ADHD & ODD"))

data$pheno <- factor(data$pheno, levels = unique(data$pheno))
data$group <- factor(data$group, levels = unique(data$group))
#data$group <- with(data, reorder(group, pheno))

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p1 <- ggplot(data, aes(x=as.factor(id), y=est, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=est, fill=group), stat="identity", alpha=1) +
  geom_errorbar(aes(ymin = est-se, ymax  = est+se),width = 0.2,position = position_dodge(.9), size  = .5, alpha=1)+
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.8, xend = start, yend = 0.8), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.6, xend = start, yend = 0.6), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.4, xend = start, yend = 0.4), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.2, xend = start, yend = 0.2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),6), y = c(0, 0.2, 0.4, 0.6, 0.8, 1), label = c("0", "0.2", "0.4", "0.6", "0.8", "1") , color="black", size=4 , angle=0, hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=est, fill=group), stat="identity", alpha=0.5) +
  ylim(-1,3) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_polar() + 
  scale_fill_manual(breaks = c("ADHD & ASD", 
                               "ADHD & specific learning disorders", 
                               "Specific learning disorders subtypes",
                               "ADHD & ODD"),
                    values = c("#d90429",
                               "#f3722c",
                               "#f9844a","#f9c74f"))+
  ggtitle("Genetic correlations \nbetween sub-categories of Neurodevelopmental Disorders \nand between sub-categories of Neurodevelopmental and DICC disorders")+
  geom_text(data=label_data, aes(x=id, y= est+se+0.3, label=pheno, hjust=hjust), color="black", alpha=1, size=4, angle= label_data$angle, inherit.aes = FALSE ) 