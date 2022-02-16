library(circlize)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(reshape2)
library(likert)

rA <- read.table(text = 
"ADHD ASD CMD MD SLD CD ODD
ADHD1 1 0.67 0 0.9 0.07 0.66 0.66 
ASD1 0.67 0 0 0 0 0.35 0 
CMD1 0 0 0 0.33 0.66 0 0
MD1 0.9 0 0.33 0 0 0 0                   
SLD1 0.07 0 0.66 0 0 0 0 
CD1 0.66 0.35 0 0 0 0 0 
ODD1 0.66 0 0 0 0 0 0 ", header=T)

rownames(rA) <- c("ADHD","ASD","COMD",
                  "MD","SLD",
                  "CD","ODD")
colnames(rA) <- c("ADHD","ASD","COMD",
                  "MD","SLD",
                  "CD","ODD")

rA <- as.matrix(rA)

chordDiagramFromMatrix(rA, 
                       grid.col = c("#8ecae6","#219ebc","#023047","#ffb703",
                                    "#fb8500","#c1121f","#cbf3f0"),
                       annotationTrack = c("name", "grid"),
                       #link.visible = rA,
                       symmetric = T,
                       transparency = .3)

rC <- read.table(text = 
                   "ADHD ASD SLD CD ODD
ADHD1 0 0.001 0.32 0.94 0.96 
ASD1 0.001 0 0 0.93 0 
SLD1 0.32 0 0 0 0 
CD1 0.94 0.93 0 0 0 
ODD1 0.96 0 0 0 0 
          ", header=T)

rownames(rC) <- c("ADHD","ASD",
                  "SLD",
                  "CD","ODD")
colnames(rC) <- c("ADHD","ASD",
                  "SLD",
                  "CD","ODD")

rC <- as.matrix(rC)

chordDiagramFromMatrix(rC, 
                       grid.col = c("#8ecae6","#023047","#ffb703",
                                    "#fb8500","#c1121f"),
                       annotationTrack = c("name", "grid"),
                       #link.visible = rC,
                       symmetric = T,
                       transparency = .3,
                       reduce = )

rE <- read.table(text = 
                   "ADHD ASD SLD CD ODD 
ADHD1 0 0.22 0.11 0.11 0.54 
ASD1 .22 0 0 0.07 0 
SLD1 0.11 0 0 0 0 
CD1 0.11 0.07 0 0 0 
ODD1 0.54 0 0 0 0 
         ", header=T)

rownames(rE) <- c("ADHD","ASD",
                  "SLD",
                  "CD","ODD")
colnames(rE) <- c("ADHD","ASD",
                  "SLD",
                  "CD","ODD")

rE <- as.matrix(rE)

chordDiagramFromMatrix(rE, 
                       grid.col = c("#8ecae6","#023047","#ffb703",
                                    "#fb8500","#c1121f"),
                       annotationTrack = c("name", "grid"),
                       #link.visible = rE,
                       symmetric = T,
                       transparency = .3,
                       reduce = )