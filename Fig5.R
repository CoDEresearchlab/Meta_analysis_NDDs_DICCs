# ============= Males and females joy plot (Aga, 20/05/21) =============
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

#####
##### ADHD #####
# ============= Joy plot by study (ggplot) =============
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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "h2", "h2_SE", "c2", "c2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- datMeta[datMeta$Broad.Trait.Disorder == "ADHD",]
datMeta_A_males <- subset(datMeta, Sex == "Males")
datMeta_A_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_A_males <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta_A_males)
datMeta_agg0.5_study_A_females <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta_A_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_A_males <- datMeta_agg0.5_study_A_males[!duplicated(datMeta_agg0.5_study_A_males$id),]
agg_by_study_cohorts_A_females <- datMeta_agg0.5_study_A_females[!duplicated(datMeta_agg0.5_study_A_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_A_males$cohort <- agg_by_study_cohorts_A_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_A_males$ref <- agg_by_study_cohorts_A_males$Ref

datMeta_agg0.5_study_A_females$cohort <- agg_by_study_cohorts_A_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_A_females$ref <- agg_by_study_cohorts_A_females$Ref

# Bind males and females
datMeta_agg0.5_study_A_males$Sex <- "Males"
datMeta_agg0.5_study_A_females$Sex <- "Females"

datMeta_agg0.5_study_A_males_A_females <- rbind(datMeta_agg0.5_study_A_males, datMeta_agg0.5_study_A_females)
datMeta_agg0.5_study_A_males_A_females$agg <- "Study"
datMeta_agg0.5_study_A_males_A_females$Comp <- "Family h2"

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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "c2", "c2_SE", "c2", "c2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$c2))
datMeta <- subset(datMeta, c2 !="")
datMeta <- subset(datMeta, c2 !=" ")
datMeta <- datMeta[datMeta$Broad.Trait.Disorder == "ADHD",]
datMeta_C_males <- subset(datMeta, Sex == "Males")
datMeta_C_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_C_males <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta_C_males)
datMeta_agg0.5_study_C_females <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta_C_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_C_males <- datMeta_agg0.5_study_C_males[!duplicated(datMeta_agg0.5_study_C_males$id),]
agg_by_study_cohorts_C_females <- datMeta_agg0.5_study_C_females[!duplicated(datMeta_agg0.5_study_C_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_C_males$cohort <- agg_by_study_cohorts_C_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_C_males$ref <- agg_by_study_cohorts_C_males$Ref

datMeta_agg0.5_study_C_females$cohort <- agg_by_study_cohorts_C_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_C_females$ref <- agg_by_study_cohorts_C_females$Ref

# Bind males and females
datMeta_agg0.5_study_C_males$Sex <- "Males"
datMeta_agg0.5_study_C_females$Sex <- "Females"

datMeta_agg0.5_study_C_males_C_females <- rbind(datMeta_agg0.5_study_C_males, datMeta_agg0.5_study_C_females)
datMeta_agg0.5_study_C_males_C_females$agg <- "Study"
datMeta_agg0.5_study_C_males_C_females$Comp <- "Family c2"

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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "e2", "e2_SE", "e2", "e2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$e2))
datMeta <- subset(datMeta, e2 !="")
datMeta <- subset(datMeta, e2 !=" ")
datMeta <- datMeta[datMeta$Broad.Trait.Disorder == "ADHD",]
datMeta_E_males <- subset(datMeta, Sex == "Males")
datMeta_E_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_E_males <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta_E_males)
datMeta_agg0.5_study_E_females <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta_E_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_E_males <- datMeta_agg0.5_study_E_males[!duplicated(datMeta_agg0.5_study_E_males$id),]
agg_by_study_cohorts_E_females <- datMeta_agg0.5_study_E_females[!duplicated(datMeta_agg0.5_study_E_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_E_males$cohort <- agg_by_study_cohorts_E_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_E_males$ref <- agg_by_study_cohorts_E_males$Ref

datMeta_agg0.5_study_E_females$cohort <- agg_by_study_cohorts_E_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_E_females$ref <- agg_by_study_cohorts_E_females$Ref

# Bind males and females
datMeta_agg0.5_study_E_males$Sex <- "Males"
datMeta_agg0.5_study_E_females$Sex <- "Females"

datMeta_agg0.5_study_E_males_E_females <- rbind(datMeta_agg0.5_study_E_males, datMeta_agg0.5_study_E_females)
datMeta_agg0.5_study_E_males_E_females$agg <- "Study"
datMeta_agg0.5_study_E_males_E_females$Comp <- "Family e2"

# ============= Bind A, C and E datasets by study ============= 
all_study_agg_data_0.5_adhd <- rbind(datMeta_agg0.5_study_A_males_A_females,
                                     datMeta_agg0.5_study_C_males_C_females,
                                     datMeta_agg0.5_study_E_males_E_females)

all_study_agg_data_0.5_adhd$disorder <- "ADHD"

##### ASD #####
# ============= Joy plot by study (ggplot) =============
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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "h2", "h2_SE", "c2", "c2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- datMeta[datMeta$Broad.Trait.Disorder == "ASD",]
datMeta_A_males <- subset(datMeta, Sex == "Males")
datMeta_A_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_A_males <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta_A_males)
datMeta_agg0.5_study_A_females <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta_A_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_A_males <- datMeta_agg0.5_study_A_males[!duplicated(datMeta_agg0.5_study_A_males$id),]
agg_by_study_cohorts_A_females <- datMeta_agg0.5_study_A_females[!duplicated(datMeta_agg0.5_study_A_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_A_males$cohort <- agg_by_study_cohorts_A_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_A_males$ref <- agg_by_study_cohorts_A_males$Ref

datMeta_agg0.5_study_A_females$cohort <- agg_by_study_cohorts_A_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_A_females$ref <- agg_by_study_cohorts_A_females$Ref

# Bind males and females
datMeta_agg0.5_study_A_males$Sex <- "Males"
datMeta_agg0.5_study_A_females$Sex <- "Females"

datMeta_agg0.5_study_A_males_A_females <- rbind(datMeta_agg0.5_study_A_males, datMeta_agg0.5_study_A_females)
datMeta_agg0.5_study_A_males_A_females$agg <- "Study"
datMeta_agg0.5_study_A_males_A_females$Comp <- "Family h2"

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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "c2", "c2_SE", "c2", "c2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$c2))
datMeta <- subset(datMeta, c2 !="")
datMeta <- subset(datMeta, c2 !=" ")
datMeta <- datMeta[datMeta$Broad.Trait.Disorder == "ASD",]
datMeta_C_males <- subset(datMeta, Sex == "Males")
datMeta_C_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_C_males <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta_C_males)
datMeta_agg0.5_study_C_females <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta_C_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_C_males <- datMeta_agg0.5_study_C_males[!duplicated(datMeta_agg0.5_study_C_males$id),]
agg_by_study_cohorts_C_females <- datMeta_agg0.5_study_C_females[!duplicated(datMeta_agg0.5_study_C_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_C_males$cohort <- agg_by_study_cohorts_C_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_C_males$ref <- agg_by_study_cohorts_C_males$Ref

datMeta_agg0.5_study_C_females$cohort <- agg_by_study_cohorts_C_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_C_females$ref <- agg_by_study_cohorts_C_females$Ref

# Bind males and females
datMeta_agg0.5_study_C_males$Sex <- "Males"
datMeta_agg0.5_study_C_females$Sex <- "Females"

datMeta_agg0.5_study_C_males_C_females <- rbind(datMeta_agg0.5_study_C_males, datMeta_agg0.5_study_C_females)
datMeta_agg0.5_study_C_males_C_females$agg <- "Study"
datMeta_agg0.5_study_C_males_C_females$Comp <- "Family c2"

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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "e2", "e2_SE", "e2", "e2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$e2))
datMeta <- subset(datMeta, e2 !="")
datMeta <- subset(datMeta, e2 !=" ")
datMeta <- datMeta[datMeta$Broad.Trait.Disorder == "ASD",]
datMeta_E_males <- subset(datMeta, Sex == "Males")
datMeta_E_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_E_males <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta_E_males)
datMeta_agg0.5_study_E_females <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta_E_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_E_males <- datMeta_agg0.5_study_E_males[!duplicated(datMeta_agg0.5_study_E_males$id),]
agg_by_study_cohorts_E_females <- datMeta_agg0.5_study_E_females[!duplicated(datMeta_agg0.5_study_E_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_E_males$cohort <- agg_by_study_cohorts_E_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_E_males$ref <- agg_by_study_cohorts_E_males$Ref

datMeta_agg0.5_study_E_females$cohort <- agg_by_study_cohorts_E_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_E_females$ref <- agg_by_study_cohorts_E_females$Ref

# Bind males and females
datMeta_agg0.5_study_E_males$Sex <- "Males"
datMeta_agg0.5_study_E_females$Sex <- "Females"

datMeta_agg0.5_study_E_males_E_females <- rbind(datMeta_agg0.5_study_E_males, datMeta_agg0.5_study_E_females)
datMeta_agg0.5_study_E_males_E_females$agg <- "Study"
datMeta_agg0.5_study_E_males_E_females$Comp <- "Family e2"

# ============= Bind A, C and E datasets by study ============= 
all_study_agg_data_0.5_asd <- rbind(datMeta_agg0.5_study_A_males_A_females,
                                    datMeta_agg0.5_study_C_males_C_females,
                                    datMeta_agg0.5_study_E_males_E_females)

all_study_agg_data_0.5_asd$disorder <- "ASD"

# ============= Plot by study =============
dat <- all_study_agg_data_0.5[all_study_agg_data_0.5$agg == "Study",]

p <- ggplot(dat, aes(x = es, y = agg, fill = Sex)) +
  geom_density_ridges(aes(fill = Sex, alpha=0.3)) +
  scale_fill_manual(values = c("lightcoral","skyblue1")) +
  theme(legend.direction = "vertical",
        legend.position="right",
        legend.text = element_text(size=7),
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
        #axis.text.y = element_text(size=7, colour="black", family = "sans", angle = 0), 
        axis.text.x = element_text(size=7, colour="black", family = "sans", angle = 45, hjust = 1),
        #axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        strip.text.x = element_text(size=10, angle = 0),
        strip.text.y = element_text(size=10, angle = 0),
        axis.text.y = element_blank(),
        plot.title = element_text(size=7, angle = 0),
        strip.background = element_rect(colour="black", fill="white")) + 
  facet_grid("Comp", scales = "free", space = "free")+
  #facet_wrap(Comp~.)+
  ylab("Estimates")+
  scale_y_discrete(expand = expansion(mult = c(-10, 0)))+
  scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1.5, by = 0.1))+
  guides(alpha=FALSE)

##### CMD #####
# ============= Joy plot by study (ggplot) =============
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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "h2", "h2_SE", "c2", "c2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- datMeta[datMeta$Broad.Trait.Disorder == "Communication Disorder",]
datMeta_A_males <- subset(datMeta, Sex == "Males")
datMeta_A_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_A_males <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta_A_males)
datMeta_agg0.5_study_A_females <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta_A_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_A_males <- datMeta_agg0.5_study_A_males[!duplicated(datMeta_agg0.5_study_A_males$id),]
agg_by_study_cohorts_A_females <- datMeta_agg0.5_study_A_females[!duplicated(datMeta_agg0.5_study_A_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_A_males$cohort <- agg_by_study_cohorts_A_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_A_males$ref <- agg_by_study_cohorts_A_males$Ref

datMeta_agg0.5_study_A_females$cohort <- agg_by_study_cohorts_A_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_A_females$ref <- agg_by_study_cohorts_A_females$Ref

# Bind males and females
datMeta_agg0.5_study_A_males$Sex <- "Males"
datMeta_agg0.5_study_A_females$Sex <- "Females"

datMeta_agg0.5_study_A_males_A_females <- rbind(datMeta_agg0.5_study_A_males, datMeta_agg0.5_study_A_females)
datMeta_agg0.5_study_A_males_A_females$agg <- "Study"
datMeta_agg0.5_study_A_males_A_females$Comp <- "Family h2"

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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "c2", "c2_SE", "c2", "c2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$c2))
datMeta <- subset(datMeta, c2 !="")
datMeta <- subset(datMeta, c2 !=" ")
datMeta <- datMeta[datMeta$Broad.Trait.Disorder == "Communication Disorder",]
datMeta_C_males <- subset(datMeta, Sex == "Males")
datMeta_C_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_C_males <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta_C_males)
datMeta_agg0.5_study_C_females <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta_C_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_C_males <- datMeta_agg0.5_study_C_males[!duplicated(datMeta_agg0.5_study_C_males$id),]
agg_by_study_cohorts_C_females <- datMeta_agg0.5_study_C_females[!duplicated(datMeta_agg0.5_study_C_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_C_males$cohort <- agg_by_study_cohorts_C_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_C_males$ref <- agg_by_study_cohorts_C_males$Ref

datMeta_agg0.5_study_C_females$cohort <- agg_by_study_cohorts_C_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_C_females$ref <- agg_by_study_cohorts_C_females$Ref

# Bind males and females
datMeta_agg0.5_study_C_males$Sex <- "Males"
datMeta_agg0.5_study_C_females$Sex <- "Females"

datMeta_agg0.5_study_C_males_C_females <- rbind(datMeta_agg0.5_study_C_males, datMeta_agg0.5_study_C_females)
datMeta_agg0.5_study_C_males_C_females$agg <- "Study"
datMeta_agg0.5_study_C_males_C_females$Comp <- "Family c2"

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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "e2", "e2_SE", "e2", "e2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$e2))
datMeta <- subset(datMeta, e2 !="")
datMeta <- subset(datMeta, e2 !=" ")
datMeta <- datMeta[datMeta$Broad.Trait.Disorder == "Communication Disorder",]
datMeta_E_males <- subset(datMeta, Sex == "Males")
datMeta_E_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_E_males <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta_E_males)
datMeta_agg0.5_study_E_females <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta_E_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_E_males <- datMeta_agg0.5_study_E_males[!duplicated(datMeta_agg0.5_study_E_males$id),]
agg_by_study_cohorts_E_females <- datMeta_agg0.5_study_E_females[!duplicated(datMeta_agg0.5_study_E_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_E_males$cohort <- agg_by_study_cohorts_E_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_E_males$ref <- agg_by_study_cohorts_E_males$Ref

datMeta_agg0.5_study_E_females$cohort <- agg_by_study_cohorts_E_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_E_females$ref <- agg_by_study_cohorts_E_females$Ref

# Bind males and females
datMeta_agg0.5_study_E_males$Sex <- "Males"
datMeta_agg0.5_study_E_females$Sex <- "Females"

datMeta_agg0.5_study_E_males_E_females <- rbind(datMeta_agg0.5_study_E_males, datMeta_agg0.5_study_E_females)
datMeta_agg0.5_study_E_males_E_females$agg <- "Study"
datMeta_agg0.5_study_E_males_E_females$Comp <- "Family e2"

# ============= Bind A, C and E datasets by study ============= 
all_study_agg_data_0.5_cmd <- rbind(datMeta_agg0.5_study_A_males_A_females,
                                    datMeta_agg0.5_study_C_males_C_females,
                                    datMeta_agg0.5_study_E_males_E_females)

all_study_agg_data_0.5_cmd$disorder <- "CMD"

# ============= Plot by study =============
dat <- all_study_agg_data_0.5[all_study_agg_data_0.5$agg == "Study",]

p <- ggplot(dat, aes(x = es, y = agg, fill = Sex)) +
  geom_density_ridges(aes(fill = Sex, alpha=0.3)) +
  scale_fill_manual(values = c("lightcoral","skyblue1")) +
  theme(legend.direction = "vertical",
        legend.position="right",
        legend.text = element_text(size=7),
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
        #axis.text.y = element_text(size=7, colour="black", family = "sans", angle = 0), 
        axis.text.x = element_text(size=7, colour="black", family = "sans", angle = 45, hjust = 1),
        #axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        strip.text.x = element_text(size=10, angle = 0),
        strip.text.y = element_text(size=10, angle = 0),
        axis.text.y = element_blank(),
        plot.title = element_text(size=7, angle = 0),
        strip.background = element_rect(colour="black", fill="white")) + 
  facet_grid("Comp", scales = "free", space = "free")+
  #facet_wrap(Comp~.)+
  ylab("Estimates")+
  scale_y_discrete(expand = expansion(mult = c(-10, 0)))+
  scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1.5, by = 0.1))+
  guides(alpha=FALSE)

##### SLD #####
# ============= Joy plot by study (ggplot) =============
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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "h2", "h2_SE", "c2", "c2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- datMeta[datMeta$Broad.Trait.Disorder == "SLD",]
datMeta_A_males <- subset(datMeta, Sex == "Males")
datMeta_A_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_A_males <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta_A_males)
datMeta_agg0.5_study_A_females <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta_A_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_A_males <- datMeta_agg0.5_study_A_males[!duplicated(datMeta_agg0.5_study_A_males$id),]
agg_by_study_cohorts_A_females <- datMeta_agg0.5_study_A_females[!duplicated(datMeta_agg0.5_study_A_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_A_males$cohort <- agg_by_study_cohorts_A_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_A_males$ref <- agg_by_study_cohorts_A_males$Ref

datMeta_agg0.5_study_A_females$cohort <- agg_by_study_cohorts_A_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_A_females$ref <- agg_by_study_cohorts_A_females$Ref

# Bind males and females
datMeta_agg0.5_study_A_males$Sex <- "Males"
datMeta_agg0.5_study_A_females$Sex <- "Females"

datMeta_agg0.5_study_A_males_A_females <- rbind(datMeta_agg0.5_study_A_males, datMeta_agg0.5_study_A_females)
datMeta_agg0.5_study_A_males_A_females$agg <- "Study"
datMeta_agg0.5_study_A_males_A_females$Comp <- "Family h2"

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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "c2", "c2_SE", "c2", "c2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$c2))
datMeta <- subset(datMeta, c2 !="")
datMeta <- subset(datMeta, c2 !=" ")
datMeta <- datMeta[datMeta$Broad.Trait.Disorder == "SLD",]
datMeta_C_males <- subset(datMeta, Sex == "Males")
datMeta_C_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_C_males <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta_C_males)
datMeta_agg0.5_study_C_females <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta_C_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_C_males <- datMeta_agg0.5_study_C_males[!duplicated(datMeta_agg0.5_study_C_males$id),]
agg_by_study_cohorts_C_females <- datMeta_agg0.5_study_C_females[!duplicated(datMeta_agg0.5_study_C_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_C_males$cohort <- agg_by_study_cohorts_C_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_C_males$ref <- agg_by_study_cohorts_C_males$Ref

datMeta_agg0.5_study_C_females$cohort <- agg_by_study_cohorts_C_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_C_females$ref <- agg_by_study_cohorts_C_females$Ref

# Bind males and females
datMeta_agg0.5_study_C_males$Sex <- "Males"
datMeta_agg0.5_study_C_females$Sex <- "Females"

datMeta_agg0.5_study_C_males_C_females <- rbind(datMeta_agg0.5_study_C_males, datMeta_agg0.5_study_C_females)
datMeta_agg0.5_study_C_males_C_females$agg <- "Study"
datMeta_agg0.5_study_C_males_C_females$Comp <- "Family c2"

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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "e2", "e2_SE", "e2", "e2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$e2))
datMeta <- subset(datMeta, e2 !="")
datMeta <- subset(datMeta, e2 !=" ")
datMeta <- datMeta[datMeta$Broad.Trait.Disorder == "SLD",]
datMeta_E_males <- subset(datMeta, Sex == "Males")
datMeta_E_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_E_males <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta_E_males)
datMeta_agg0.5_study_E_females <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta_E_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_E_males <- datMeta_agg0.5_study_E_males[!duplicated(datMeta_agg0.5_study_E_males$id),]
agg_by_study_cohorts_E_females <- datMeta_agg0.5_study_E_females[!duplicated(datMeta_agg0.5_study_E_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_E_males$cohort <- agg_by_study_cohorts_E_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_E_males$ref <- agg_by_study_cohorts_E_males$Ref

datMeta_agg0.5_study_E_females$cohort <- agg_by_study_cohorts_E_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_E_females$ref <- agg_by_study_cohorts_E_females$Ref

# Bind males and females
datMeta_agg0.5_study_E_males$Sex <- "Males"
datMeta_agg0.5_study_E_females$Sex <- "Females"

datMeta_agg0.5_study_E_males_E_females <- rbind(datMeta_agg0.5_study_E_males, datMeta_agg0.5_study_E_females)
datMeta_agg0.5_study_E_males_E_females$agg <- "Study"
datMeta_agg0.5_study_E_males_E_females$Comp <- "Family e2"

# ============= Bind A, C and E datasets by study ============= 
all_study_agg_data_0.5_sld <- rbind(datMeta_agg0.5_study_A_males_A_females,
                                    datMeta_agg0.5_study_C_males_C_females,
                                    datMeta_agg0.5_study_E_males_E_females)

all_study_agg_data_0.5_sld$disorder <- "SLD"

# ============= Plot by study =============
dat <- all_study_agg_data_0.5[all_study_agg_data_0.5$agg == "Study",]

p <- ggplot(dat, aes(x = es, y = agg, fill = Sex)) +
  geom_density_ridges(aes(fill = Sex, alpha=0.3)) +
  scale_fill_manual(values = c("lightcoral","skyblue1")) +
  theme(legend.direction = "vertical",
        legend.position="right",
        legend.text = element_text(size=7),
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
        #axis.text.y = element_text(size=7, colour="black", family = "sans", angle = 0), 
        axis.text.x = element_text(size=7, colour="black", family = "sans", angle = 45, hjust = 1),
        #axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        strip.text.x = element_text(size=10, angle = 0),
        strip.text.y = element_text(size=10, angle = 0),
        axis.text.y = element_blank(),
        plot.title = element_text(size=7, angle = 0),
        strip.background = element_rect(colour="black", fill="white")) + 
  facet_grid("Comp", scales = "free", space = "free")+
  #facet_wrap(Comp~.)+
  ylab("Estimates")+
  scale_y_discrete(expand = expansion(mult = c(-10, 0)))+
  scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1.5, by = 0.1))+
  guides(alpha=FALSE)

##### ALL #####
# ============= Joy plot by study (ggplot) =============
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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "h2", "h2_SE", "c2", "c2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta_A_males <- subset(datMeta, Sex == "Males")
datMeta_A_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_A_males <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta_A_males)
datMeta_agg0.5_study_A_females <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta_A_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_A_males <- datMeta_agg0.5_study_A_males[!duplicated(datMeta_agg0.5_study_A_males$id),]
agg_by_study_cohorts_A_females <- datMeta_agg0.5_study_A_females[!duplicated(datMeta_agg0.5_study_A_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_A_males$cohort <- agg_by_study_cohorts_A_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_A_males$ref <- agg_by_study_cohorts_A_males$Ref

datMeta_agg0.5_study_A_females$cohort <- agg_by_study_cohorts_A_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_A_females$ref <- agg_by_study_cohorts_A_females$Ref

# Bind males and females
datMeta_agg0.5_study_A_males$Sex <- "Males"
datMeta_agg0.5_study_A_females$Sex <- "Females"

datMeta_agg0.5_study_A_males_A_females <- rbind(datMeta_agg0.5_study_A_males, datMeta_agg0.5_study_A_females)
datMeta_agg0.5_study_A_males_A_females$agg <- "Study"

# ============= Bind all aggregation datasets =============
datMeta_agg0.5_all_agg_A_males_A_females <- datMeta_agg0.5_study_A_males_A_females
datMeta_agg0.5_all_agg_A_males_A_females$Comp <- "Family h2"

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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "c2", "c2_SE", "c2", "c2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$c2))
datMeta <- subset(datMeta, c2 !="")
datMeta <- subset(datMeta, c2 !=" ")
datMeta_C_males <- subset(datMeta, Sex == "Males")
datMeta_C_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_C_males <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta_C_males)
datMeta_agg0.5_study_C_females <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta_C_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_C_males <- datMeta_agg0.5_study_C_males[!duplicated(datMeta_agg0.5_study_C_males$id),]
agg_by_study_cohorts_C_females <- datMeta_agg0.5_study_C_females[!duplicated(datMeta_agg0.5_study_C_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_C_males$cohort <- agg_by_study_cohorts_C_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_C_males$ref <- agg_by_study_cohorts_C_males$Ref

datMeta_agg0.5_study_C_females$cohort <- agg_by_study_cohorts_C_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_C_females$ref <- agg_by_study_cohorts_C_females$Ref

# Bind males and females
datMeta_agg0.5_study_C_males$Sex <- "Males"
datMeta_agg0.5_study_C_females$Sex <- "Females"

datMeta_agg0.5_study_C_males_C_females <- rbind(datMeta_agg0.5_study_C_males, datMeta_agg0.5_study_C_females)
datMeta_agg0.5_study_C_males_C_females$agg <- "Study"

# ============= Bind all aggregation datasets =============

datMeta_agg0.5_all_agg_C_males_C_females <- datMeta_agg0.5_study_C_males_C_females
datMeta_agg0.5_all_agg_C_males_C_females$Comp <- "Family c2"

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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "e2", "e2_SE", "e2", "e2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$e2))
datMeta <- subset(datMeta, e2 !="")
datMeta <- subset(datMeta, e2 !=" ")
datMeta_E_males <- subset(datMeta, Sex == "Males")
datMeta_E_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_E_males <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta_E_males)
datMeta_agg0.5_study_E_females <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta_E_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_E_males <- datMeta_agg0.5_study_E_males[!duplicated(datMeta_agg0.5_study_E_males$id),]
agg_by_study_cohorts_E_females <- datMeta_agg0.5_study_E_females[!duplicated(datMeta_agg0.5_study_E_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_E_males$cohort <- agg_by_study_cohorts_E_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_E_males$ref <- agg_by_study_cohorts_E_males$Ref

datMeta_agg0.5_study_E_females$cohort <- agg_by_study_cohorts_E_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_E_females$ref <- agg_by_study_cohorts_E_females$Ref

# Bind males and females
datMeta_agg0.5_study_E_males$Sex <- "Males"
datMeta_agg0.5_study_E_females$Sex <- "Females"

datMeta_agg0.5_study_E_males_E_females <- rbind(datMeta_agg0.5_study_E_males, datMeta_agg0.5_study_E_females)
datMeta_agg0.5_study_E_males_E_females$agg <- "Study"

# ============= Bind all aggregation datasets =============

datMeta_agg0.5_all_agg_E_males_E_females <- datMeta_agg0.5_study_E_males_E_females
datMeta_agg0.5_all_agg_E_males_E_females$Comp <- "Family e2"

# ============= Bind A, C and E datasets by study ============= 
all_study_agg_data_0.5_all <- rbind(datMeta_agg0.5_all_agg_A_males_A_females,
                                    datMeta_agg0.5_all_agg_C_males_C_females,
                                    datMeta_agg0.5_all_agg_E_males_E_females)

all_study_agg_data_0.5_all$disorder <- "NDDs"

##### ALL SNP #####
# ============= Joy plot by study (ggplot) =============
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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "Measure2", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "Covariate2", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "h2", "h2_SE", "c2", "c2_SE", "e2", 
                              "e2_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: SNP studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="SNP")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta_A_males <- subset(datMeta, Sex == "Males")
datMeta_A_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_A_males <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta_A_males)
datMeta_agg0.5_study_A_females <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta_A_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_A_males <- datMeta_agg0.5_study_A_males[!duplicated(datMeta_agg0.5_study_A_males$id),]
agg_by_study_cohorts_A_females <- datMeta_agg0.5_study_A_females[!duplicated(datMeta_agg0.5_study_A_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_A_males$cohort <- agg_by_study_cohorts_A_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_A_males$ref <- agg_by_study_cohorts_A_males$Ref

datMeta_agg0.5_study_A_females$cohort <- agg_by_study_cohorts_A_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_A_females$ref <- agg_by_study_cohorts_A_females$Ref

# Bind males and females
datMeta_agg0.5_study_A_males$Sex <- "Males"
datMeta_agg0.5_study_A_females$Sex <- "Females"

datMeta_agg0.5_study_A_males_A_females <- rbind(datMeta_agg0.5_study_A_males, datMeta_agg0.5_study_A_females)
datMeta_agg0.5_study_A_males_A_females$agg <- "Study"

# ============= Bind all aggregation datasets =============
datMeta_agg0.5_all_agg_A_males_A_females <- datMeta_agg0.5_study_A_males_A_females
all_study_agg_data_0.5_allsnp <- datMeta_agg0.5_all_agg_A_males_A_females

all_study_agg_data_0.5_allsnp$disorder <- "NDDs"
all_study_agg_data_0.5_allsnp$Comp <- "SNP h2"

##### Plot #####
dat <- rbind(all_study_agg_data_0.5_all,
             #all_study_agg_data_0.5_allsnp,
             all_study_agg_data_0.5_cmd,
             all_study_agg_data_0.5_adhd,
             all_study_agg_data_0.5_asd,
             all_study_agg_data_0.5_sld)

# ============= Plot by study =============

dat$disorder <- factor(dat$disorder, levels = c("NDDs","CMD","ASD","ADHD","SLD"),
                       labels = c("NDDs \n combined",
                                  "Communication disorders",
                                  "ASD",
                                  "ADHD",
                                  "Specific learning disorders"))

p <- ggplot(dat, aes(x = es, y = Comp, fill = Sex)) +
  geom_density_ridges(aes(fill = Sex, alpha=0.3)) +
  scale_fill_manual(values = c("lightcoral","skyblue1")) +
  theme(legend.direction = "vertical",
        legend.position="right",
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
        axis.text.y = element_text(size=10, colour="black", family = "sans", angle = 0), 
        axis.text.x = element_text(size=10, colour="black", family = "sans", angle = 45, hjust = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        strip.text.x = element_text(size=10, angle = 0),
        strip.text.y = element_text(size=10, angle = 0),
        plot.title = element_text(size=7, angle = 0),
        strip.background = element_rect(colour="black", fill="white")) + 
  #facet_grid(disorder~., scales = "free", space = "free")+
  facet_wrap(disorder~., scales = "free")+
  ylab("Estimates")+
  scale_y_discrete(expand = expansion(mult = c(-2, 0)))+
  #scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1.5, by = 0.1))+
  guides(alpha=FALSE)

##### ALL rG NDDs #####
# ============= Joy plot by study (ggplot) =============
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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X.White", "Cohort1", "Cohort2", "Cohort", 
                              "Cohort_for_agg_by_study", "Design1", "Design2", "N", "X..White", 
                              "Mean_age", "Age_range", "Age_group", "Cohort_country1", "Cohort_country2", 
                              "Trait.disorder", "Broad.Trait.disorder", "NDD.DICC", "Binary.Continuous", 
                              "Categorical.Continuous", "Measure1", "Measure2", "Measure3", 
                              "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", "Rater1", 
                              "Rater2", "Covariate1", "Covariate2", "Covariate3", "Design", 
                              "Group", "Sex", "Prevalence", "Wave", "Categorical.Continuous.1", 
                              "Specific_phenotype", "Specific_phenotype_category", "Continuous_phenotype_category", 
                              "Group.1", "rA", "rA.rG_SE", "rC", "rC_SE", "rE", "rE_SE", "Model")]

# Extract subset of data that I want to meta-analyse: SNP studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$rA))
datMeta <- subset(datMeta, NDD.DICC == "NDD")
datMeta_rA_males <- subset(datMeta, Sex == "Males")
datMeta_rA_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_rA_males <- agg(id=Full.Ref, es=rA, var=rA.rG_SE, cor=0.5, data=datMeta_rA_males)
datMeta_agg0.5_study_rA_females <- agg(id=Full.Ref, es=rA, var=rA.rG_SE, cor=0.5, data=datMeta_rA_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_rA_males <- datMeta_agg0.5_study_rA_males[!duplicated(datMeta_agg0.5_study_rA_males$id),]
agg_by_study_cohorts_rA_females <- datMeta_agg0.5_study_rA_females[!duplicated(datMeta_agg0.5_study_rA_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_rA_males$cohort <- agg_by_study_cohorts_rA_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_rA_males$ref <- agg_by_study_cohorts_rA_males$Ref

datMeta_agg0.5_study_rA_females$cohort <- agg_by_study_cohorts_rA_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_rA_females$ref <- agg_by_study_cohorts_rA_females$Ref

# Bind males and females
datMeta_agg0.5_study_rA_males$Sex <- "Males"
datMeta_agg0.5_study_rA_females$Sex <- "Females"

datMeta_agg0.5_study_rA_males_rA_females <- rbind(datMeta_agg0.5_study_rA_males, datMeta_agg0.5_study_rA_females)
datMeta_agg0.5_study_rA_males_rA_females$agg <- "Study"

# ============= Bind all aggregation datasets =============
datMeta_agg0.5_all_agg_rA_males_rA_females <- datMeta_agg0.5_study_rA_males_rA_females

datMeta_agg0.5_all_agg_rA_males_rA_females$disorder <- "NDDs"
datMeta_agg0.5_all_agg_rA_males_rA_females$Comp <- "Family rA"


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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X.White", "Cohort1", "Cohort2", "Cohort", 
                              "Cohort_for_agg_by_study", "Design1", "Design2", "N", "X..White", 
                              "Mean_age", "Age_range", "Age_group", "Cohort_country1", "Cohort_country2", 
                              "Trait.disorder", "Broad.Trait.disorder", "NDD.DICC", "Binary.Continuous", 
                              "Categorical.Continuous", "Measure1", "Measure2", "Measure3", 
                              "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", "Rater1", 
                              "Rater2", "Covariate1", "Covariate2", "Covariate3", "Design", 
                              "Group", "Sex", "Prevalence", "Wave", "Categorical.Continuous.1", 
                              "Specific_phenotype", "Specific_phenotype_category", "Continuous_phenotype_category", 
                              "Group.1", "rC", "rC_SE", "rC", "rC_SE", "rE", "rE_SE", "Model")]

# Extract subset of data that I want to meta-analyse: SNP studies, heritability, sexes combined
datMeta_all$rC <- as.numeric(datMeta_all$rC)
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$rC))
datMeta <- subset(datMeta, NDD.DICC == "NDD")
datMeta_rC_males <- subset(datMeta, Sex == "Males")
datMeta_rC_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_rC_males <- agg(id=Full.Ref, es=rC, var=rC_SE, cor=0.5, data=datMeta_rC_males)
datMeta_agg0.5_study_rC_females <- agg(id=Full.Ref, es=rC, var=rC_SE, cor=0.5, data=datMeta_rC_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_rC_males <- datMeta_agg0.5_study_rC_males[!duplicated(datMeta_agg0.5_study_rC_males$id),]
agg_by_study_cohorts_rC_females <- datMeta_agg0.5_study_rC_females[!duplicated(datMeta_agg0.5_study_rC_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_rC_males$cohort <- agg_by_study_cohorts_rC_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_rC_males$ref <- agg_by_study_cohorts_rC_males$Ref

datMeta_agg0.5_study_rC_females$cohort <- agg_by_study_cohorts_rC_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_rC_females$ref <- agg_by_study_cohorts_rC_females$Ref

# Bind males and females
datMeta_agg0.5_study_rC_males$Sex <- "Males"
datMeta_agg0.5_study_rC_females$Sex <- "Females"

datMeta_agg0.5_study_rC_males_rC_females <- rbind(datMeta_agg0.5_study_rC_males, datMeta_agg0.5_study_rC_females)
datMeta_agg0.5_study_rC_males_rC_females$agg <- "Study"

# ============= Bind all aggregation datasets =============
datMeta_agg0.5_all_agg_rC_males_rC_females <- datMeta_agg0.5_study_rC_males_rC_females

datMeta_agg0.5_all_agg_rC_males_rC_females$disorder <- "NDDs"
datMeta_agg0.5_all_agg_rC_males_rC_females$Comp <- "Family rC"

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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X.White", "Cohort1", "Cohort2", "Cohort", 
                              "Cohort_for_agg_by_study", "Design1", "Design2", "N", "X..White", 
                              "Mean_age", "Age_range", "Age_group", "Cohort_country1", "Cohort_country2", 
                              "Trait.disorder", "Broad.Trait.disorder", "NDD.DICC", "Binary.Continuous", 
                              "Categorical.Continuous", "Measure1", "Measure2", "Measure3", 
                              "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", "Rater1", 
                              "Rater2", "Covariate1", "Covariate2", "Covariate3", "Design", 
                              "Group", "Sex", "Prevalence", "Wave", "Categorical.Continuous.1", 
                              "Specific_phenotype", "Specific_phenotype_category", "Continuous_phenotype_category", 
                              "Group.1", "rE", "rE_SE", "rE", "rE_SE", "rE", "rE_SE", "Model")]

# Extract subset of data that I want to meta-analyse: SNP studies, heritability, sexes combined
datMeta_all$rE <- as.numeric(datMeta_all$rE)
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$rE))
datMeta <- subset(datMeta, NDD.DICC == "NDD")
datMeta_rE_males <- subset(datMeta, Sex == "Males")
datMeta_rE_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_rE_males <- agg(id=Full.Ref, es=rE, var=rE_SE, cor=0.5, data=datMeta_rE_males)
datMeta_agg0.5_study_rE_females <- agg(id=Full.Ref, es=rE, var=rE_SE, cor=0.5, data=datMeta_rE_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_rE_males <- datMeta_agg0.5_study_rE_males[!duplicated(datMeta_agg0.5_study_rE_males$id),]
agg_by_study_cohorts_rE_females <- datMeta_agg0.5_study_rE_females[!duplicated(datMeta_agg0.5_study_rE_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_rE_males$cohort <- agg_by_study_cohorts_rE_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_rE_males$ref <- agg_by_study_cohorts_rE_males$Ref

datMeta_agg0.5_study_rE_females$cohort <- agg_by_study_cohorts_rE_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_rE_females$ref <- agg_by_study_cohorts_rE_females$Ref

# Bind males and females
datMeta_agg0.5_study_rE_males$Sex <- "Males"
datMeta_agg0.5_study_rE_females$Sex <- "Females"

datMeta_agg0.5_study_rE_males_rE_females <- rbind(datMeta_agg0.5_study_rE_males, datMeta_agg0.5_study_rE_females)
datMeta_agg0.5_study_rE_males_rE_females$agg <- "Study"

# ============= Bind all aggregation datasets =============
datMeta_agg0.5_all_agg_rE_males_rE_females <- datMeta_agg0.5_study_rE_males_rE_females

datMeta_agg0.5_all_agg_rE_males_rE_females$disorder <- "NDDs"
datMeta_agg0.5_all_agg_rE_males_rE_females$Comp <- "Family rE"


# ============= Bind A, C and E datasets by study ============= 
all_study_agg_data_0.5_all <- rbind(datMeta_agg0.5_all_agg_rA_males_rA_females,
                                    datMeta_agg0.5_all_agg_rC_males_rC_females,
                                    datMeta_agg0.5_all_agg_rE_males_rE_females)

all_study_agg_data_0.5_all$disorder <- "NDDs"

##### Plot #####
dat <- rbind(all_study_agg_data_0.5_all)

# ============= Plot by study =============

dat$disorder <- factor(dat$disorder, levels = c("NDDs"),
                       labels = c("Comorbidity between \n NDDs"))

p <- ggplot(dat, aes(x = es, y = Comp, fill = Sex)) +
  geom_density_ridges(aes(fill = Sex, alpha=0.3)) +
  scale_fill_manual(values = c("lightcoral","skyblue1")) +
  theme(legend.direction = "vertical",
        legend.position="right",
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
        axis.text.y = element_text(size=10, colour="black", family = "sans", angle = 0), 
        axis.text.x = element_text(size=10, colour="black", family = "sans", angle = 45, hjust = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        strip.text.x = element_text(size=10, angle = 0),
        strip.text.y = element_text(size=10, angle = 0),
        plot.title = element_text(size=7, angle = 0),
        strip.background = element_rect(colour="black", fill="white")) + 
  #facet_grid(disorder~., scales = "free", space = "free")+
  facet_wrap(disorder~., scales = "free")+
  ylab("Estimates")+
  #scale_y_discrete(expand = expansion(mult = c(-2, 0)))+
  #scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1.5, by = 0.1))+
  guides(alpha=FALSE)

##### ALL rG DICC #####
# ============= Joy plot by study (ggplot) =============
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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X.White", "Cohort1", "Cohort2", "Cohort", 
                              "Cohort_for_agg_by_study", "Design1", "Design2", "N", "X..White", 
                              "Mean_age", "Age_range", "Age_group", "Cohort_country1", "Cohort_country2", 
                              "Trait.disorder", "Broad.Trait.disorder", "NDD.DICC", "Binary.Continuous", 
                              "Categorical.Continuous", "Measure1", "Measure2", "Measure3", 
                              "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", "Rater1", 
                              "Rater2", "Covariate1", "Covariate2", "Covariate3", "Design", 
                              "Group", "Sex", "Prevalence", "Wave", "Categorical.Continuous.1", 
                              "Specific_phenotype", "Specific_phenotype_category", "Continuous_phenotype_category", 
                              "Group.1", "rA", "rA.rG_SE", "rC", "rC_SE", "rE", "rE_SE", "Model")]

# Extract subset of data that I want to meta-analyse: SNP studies, heritability, sexes combined
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$rA))
datMeta <- subset(datMeta, NDD.DICC == "DICC")
datMeta_rA_males <- subset(datMeta, Sex == "Males")
datMeta_rA_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_rA_males <- agg(id=Full.Ref, es=rA, var=rA.rG_SE, cor=0.5, data=datMeta_rA_males)
datMeta_agg0.5_study_rA_females <- agg(id=Full.Ref, es=rA, var=rA.rG_SE, cor=0.5, data=datMeta_rA_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_rA_males <- datMeta_agg0.5_study_rA_males[!duplicated(datMeta_agg0.5_study_rA_males$id),]
agg_by_study_cohorts_rA_females <- datMeta_agg0.5_study_rA_females[!duplicated(datMeta_agg0.5_study_rA_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_rA_males$cohort <- agg_by_study_cohorts_rA_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_rA_males$ref <- agg_by_study_cohorts_rA_males$Ref

datMeta_agg0.5_study_rA_females$cohort <- agg_by_study_cohorts_rA_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_rA_females$ref <- agg_by_study_cohorts_rA_females$Ref

# Bind males and females
datMeta_agg0.5_study_rA_males$Sex <- "Males"
datMeta_agg0.5_study_rA_females$Sex <- "Females"

datMeta_agg0.5_study_rA_males_rA_females <- rbind(datMeta_agg0.5_study_rA_males, datMeta_agg0.5_study_rA_females)
datMeta_agg0.5_study_rA_males_rA_females$agg <- "Study"

# ============= Bind all aggregation datasets =============
datMeta_agg0.5_all_agg_rA_males_rA_females <- datMeta_agg0.5_study_rA_males_rA_females

datMeta_agg0.5_all_agg_rA_males_rA_females$disorder <- "DICC"
datMeta_agg0.5_all_agg_rA_males_rA_females$Comp <- "Family rA"


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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X.White", "Cohort1", "Cohort2", "Cohort", 
                              "Cohort_for_agg_by_study", "Design1", "Design2", "N", "X..White", 
                              "Mean_age", "Age_range", "Age_group", "Cohort_country1", "Cohort_country2", 
                              "Trait.disorder", "Broad.Trait.disorder", "NDD.DICC", "Binary.Continuous", 
                              "Categorical.Continuous", "Measure1", "Measure2", "Measure3", 
                              "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", "Rater1", 
                              "Rater2", "Covariate1", "Covariate2", "Covariate3", "Design", 
                              "Group", "Sex", "Prevalence", "Wave", "Categorical.Continuous.1", 
                              "Specific_phenotype", "Specific_phenotype_category", "Continuous_phenotype_category", 
                              "Group.1", "rC", "rC_SE", "rC", "rC_SE", "rE", "rE_SE", "Model")]

# Extract subset of data that I want to meta-analyse: SNP studies, heritability, sexes combined
datMeta_all$rC <- as.numeric(datMeta_all$rC)
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$rC))
datMeta <- subset(datMeta, NDD.DICC == "DICC")
datMeta_rC_males <- subset(datMeta, Sex == "Males")
datMeta_rC_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_rC_males <- agg(id=Full.Ref, es=rC, var=rC_SE, cor=0.5, data=datMeta_rC_males)
datMeta_agg0.5_study_rC_females <- agg(id=Full.Ref, es=rC, var=rC_SE, cor=0.5, data=datMeta_rC_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_rC_males <- datMeta_agg0.5_study_rC_males[!duplicated(datMeta_agg0.5_study_rC_males$id),]
agg_by_study_cohorts_rC_females <- datMeta_agg0.5_study_rC_females[!duplicated(datMeta_agg0.5_study_rC_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_rC_males$cohort <- agg_by_study_cohorts_rC_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_rC_males$ref <- agg_by_study_cohorts_rC_males$Ref

datMeta_agg0.5_study_rC_females$cohort <- agg_by_study_cohorts_rC_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_rC_females$ref <- agg_by_study_cohorts_rC_females$Ref

# Bind males and females
datMeta_agg0.5_study_rC_males$Sex <- "Males"
datMeta_agg0.5_study_rC_females$Sex <- "Females"

datMeta_agg0.5_study_rC_males_rC_females <- rbind(datMeta_agg0.5_study_rC_males, datMeta_agg0.5_study_rC_females)
datMeta_agg0.5_study_rC_males_rC_females$agg <- "Study"

# ============= Bind all aggregation datasets =============
datMeta_agg0.5_all_agg_rC_males_rC_females <- datMeta_agg0.5_study_rC_males_rC_females

datMeta_agg0.5_all_agg_rC_males_rC_females$disorder <- "DICC"
datMeta_agg0.5_all_agg_rC_males_rC_females$Comp <- "Family rC"

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

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X.White", "Cohort1", "Cohort2", "Cohort", 
                              "Cohort_for_agg_by_study", "Design1", "Design2", "N", "X..White", 
                              "Mean_age", "Age_range", "Age_group", "Cohort_country1", "Cohort_country2", 
                              "Trait.disorder", "Broad.Trait.disorder", "NDD.DICC", "Binary.Continuous", 
                              "Categorical.Continuous", "Measure1", "Measure2", "Measure3", 
                              "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", "Rater1", 
                              "Rater2", "Covariate1", "Covariate2", "Covariate3", "Design", 
                              "Group", "Sex", "Prevalence", "Wave", "Categorical.Continuous.1", 
                              "Specific_phenotype", "Specific_phenotype_category", "Continuous_phenotype_category", 
                              "Group.1", "rE", "rE_SE", "rE", "rE_SE", "rE", "rE_SE", "Model")]

# Extract subset of data that I want to meta-analyse: SNP studies, heritability, sexes combined
datMeta_all$rE <- as.numeric(datMeta_all$rE)
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$rE))
datMeta <- subset(datMeta, NDD.DICC == "DICC")
datMeta_rE_males <- subset(datMeta, Sex == "Males")
datMeta_rE_females <- subset(datMeta, Sex == "Females")

# ============= Using aggregate function to combine non-independent effect sizes by study =============

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_study_rE_males <- agg(id=Full.Ref, es=rE, var=rE_SE, cor=0.5, data=datMeta_rE_males)
datMeta_agg0.5_study_rE_females <- agg(id=Full.Ref, es=rE, var=rE_SE, cor=0.5, data=datMeta_rE_females)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts_rE_males <- datMeta_agg0.5_study_rE_males[!duplicated(datMeta_agg0.5_study_rE_males$id),]
agg_by_study_cohorts_rE_females <- datMeta_agg0.5_study_rE_females[!duplicated(datMeta_agg0.5_study_rE_females$id),]

# Now label cohorts and refs
datMeta_agg0.5_study_rE_males$cohort <- agg_by_study_cohorts_rE_males$Cohort_for_agg_by_study
datMeta_agg0.5_study_rE_males$ref <- agg_by_study_cohorts_rE_males$Ref

datMeta_agg0.5_study_rE_females$cohort <- agg_by_study_cohorts_rE_females$Cohort_for_agg_by_study
datMeta_agg0.5_study_rE_females$ref <- agg_by_study_cohorts_rE_females$Ref

# Bind males and females
datMeta_agg0.5_study_rE_males$Sex <- "Males"
datMeta_agg0.5_study_rE_females$Sex <- "Females"

datMeta_agg0.5_study_rE_males_rE_females <- rbind(datMeta_agg0.5_study_rE_males, datMeta_agg0.5_study_rE_females)
datMeta_agg0.5_study_rE_males_rE_females$agg <- "Study"

# ============= Bind all aggregation datasets =============
datMeta_agg0.5_all_agg_rE_males_rE_females <- datMeta_agg0.5_study_rE_males_rE_females

datMeta_agg0.5_all_agg_rE_males_rE_females$disorder <- "DICC"
datMeta_agg0.5_all_agg_rE_males_rE_females$Comp <- "Family rE"


# ============= Bind A, C and E datasets by study ============= 
all_study_agg_data_0.5_all <- rbind(datMeta_agg0.5_all_agg_rA_males_rA_females,
                                    datMeta_agg0.5_all_agg_rC_males_rC_females,
                                    datMeta_agg0.5_all_agg_rE_males_rE_females)

all_study_agg_data_0.5_all$disorder <- "DICC"

##### Plot #####
dat <- rbind(all_study_agg_data_0.5_all)

# ============= Plot by study =============

dat$disorder <- factor(dat$disorder, levels = c("DICC"),
                       labels = c("Comorbidity between \n NDDs"))

p <- ggplot(dat, aes(x = es, y = Comp, fill = Sex)) +
  geom_density_ridges(aes(fill = Sex, alpha=0.3)) +
  scale_fill_manual(values = c("lightcoral","skyblue1")) +
  theme(legend.direction = "vertical",
        legend.position="right",
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
        axis.text.y = element_text(size=10, colour="black", family = "sans", angle = 0), 
        axis.text.x = element_text(size=10, colour="black", family = "sans", angle = 45, hjust = 1),
        #axis.text.x = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        strip.text.x = element_text(size=10, angle = 0),
        strip.text.y = element_text(size=10, angle = 0),
        plot.title = element_text(size=7, angle = 0),
        strip.background = element_rect(colour="black", fill="white")) + 
  #facet_grid(disorder~., scales = "free", space = "free")+
  facet_wrap(disorder~., scales = "free")+
  ylab("Estimates")+
  #scale_y_discrete(expand = expansion(mult = c(-2, 0)))+
  #scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1.5, by = 0.1))+
  guides(alpha=FALSE)
