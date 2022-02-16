library(metafor)
library(MAd)

##### all NDDs #####
# ============= Load and subset data: combined family h2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- subset(datMeta, Sex == "Combined")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#023e8a", "#023e8a"),
       xlab="Family h2", ylab="", cex=1, cex.axis=2, cex.lab=2)

# ============= Load and subset data: combined family c2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$c2))
datMeta <- subset(datMeta, Sex == "Combined")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#023e8a", "#023e8a"),
       xlab="Family c2", ylab="", cex=1, cex.axis=2, cex.lab=2)

# ============= Load and subset data: combined family e2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$e2))
datMeta <- subset(datMeta, Sex == "Combined")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#023e8a", "#023e8a"),
       xlab="Family e2", ylab="", cex=1, cex.axis=2, cex.lab=2)

# ============= Load and subset data: combined SNP h2 =============

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

# Extract subset of data that I want to meta-analyse: SNP studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="SNP")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- subset(datMeta, Sex == "Combined")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#023e8a", "#023e8a"),
       xlab="SNP h2", ylab="", cex=1, cex.axis=2, cex.lab=2)


##### LD #####
# ============= Load and subset data: combined family h2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "LD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#e63946", "#e63946"),
       xlab="Family h2")

# ============= Load and subset data: combined family e2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$e2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "LD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#e63946", "#e63946"),
       xlab="Family e2", bg="white")


##### CMD #####
# ============= Load and subset data: combined family h2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "Communication Disorder")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#fe7f2d", "#fe7f2d"),
       xlab="Family h2")

# ============= Load and subset data: combined family c2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$c2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "Communication Disorder")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#fe7f2d", "#fe7f2d"),
       xlab="Family c2")


# ============= Load and subset data: combined family e2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$e2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "Communication Disorder")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#fe7f2d", "#fe7f2d"),
       xlab="Family e2", bg="white")


# ============= Load and subset data: combined SNP h2 =============

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

# Extract subset of data that I want to meta-analyse: SNP studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="SNP")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "CMD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#fe7f2d", "#fe7f2d"),
       xlab="SNP h2")


##### ASD #####
# ============= Load and subset data: combined family h2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "ASD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#007f5f", "#007f5f"),
       xlab="Family h2")

# ============= Load and subset data: combined family c2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$c2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "ASD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#007f5f", "#007f5f"),
       xlab="Family c2")


# ============= Load and subset data: combined family e2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$e2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "ASD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#007f5f", "#007f5f"),
       xlab="Family e2", bg="white")


# ============= Load and subset data: combined SNP h2 =============

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

# Extract subset of data that I want to meta-analyse: SNP studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="SNP")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "ASD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#007f5f", "#007f5f"),
       xlab="SNP h2")



##### ADHD #####
# ============= Load and subset data: combined family h2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "ADHD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#b5179e", "#b5179e"),
       xlab="Family h2")

# ============= Load and subset data: combined family c2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$c2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "ADHD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#b5179e", "#b5179e"),
       xlab="Family c2")


# ============= Load and subset data: combined family e2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$e2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "ADHD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#b5179e", "#b5179e"),
       xlab="Family e2", bg="white")


# ============= Load and subset data: combined SNP h2 =============

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

# Extract subset of data that I want to meta-analyse: SNP studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="SNP")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "ADHD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#b5179e", "#b5179e"),
       xlab="SNP h2")



##### SLD #####
# ============= Load and subset data: combined family h2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "SLD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#3f37c9", "#3f37c9"),
       xlab="Family h2")

# ============= Load and subset data: combined family c2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$c2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "SLD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#3f37c9", "#3f37c9"),
       xlab="Family c2")


# ============= Load and subset data: combined family e2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$e2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "SLD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#3f37c9", "#3f37c9"),
       xlab="Family e2", bg="white")


# ============= Load and subset data: combined SNP h2 =============

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

# Extract subset of data that I want to meta-analyse: SNP studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="SNP")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "SLD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#3f37c9", "#3f37c9"),
       xlab="SNP h2")



##### MD #####
# ============= Load and subset data: combined family h2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "Motor Disorder")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#892b64", "#892b64"),
       xlab="Family h2")

# ============= Load and subset data: combined family c2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$c2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "Motor Disorder")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=c2, var=c2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#892b64", "#892b64"),
       xlab="Family c2")


# ============= Load and subset data: combined family e2 =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$e2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "Motor Disorder")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=e2, var=e2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#892b64", "#892b64"),
       xlab="Family e2", bg="white")


# ============= Load and subset data: combined SNP h2 =============

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

# Extract subset of data that I want to meta-analyse: SNP studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="SNP")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, Broad.Trait.Disorder == "Motor Disorder")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#892b64", "#892b64"),
       xlab="SNP h2")



##### NDDsNDDs #####
# ============= Load and subset data: combined family rA =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_comorbidity.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, NDDsNDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$rA))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, NDD.DICC == "NDD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=rA, var=rA.rG_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#4895ef", "#4895ef"),
       xlab="Family rA", ylab="", cex=1, cex.axis=2, cex.lab=2)

# ============= Load and subset data: combined family rC =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, NDDsNDDs
datMeta_all$rC <- is.numeric(datMeta_all$rC)
datMeta_all$rC_SE <- is.numeric(datMeta_all$rC_SE)
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$rC))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, NDD.DICC == "NDD")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=rC, var=rC_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#4895ef", "#4895ef"),
       xlab="Family rC", ylab="", cex=1, cex.axis=2, cex.lab=2)

# ============= Load and subset data: combined family rE =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_comorbidity.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]
datMeta <- subset(datMeta, NDD.DICC == "NDD")

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "MeasurrE", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "CovariatrE", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "rE", "rE_SE", "rE", "rE_SE", "rE", 
                              "rE_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, NDDsNDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$rE))
datMeta <- subset(datMeta, Sex == "Combined")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=rE, var=rE_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#4895ef", "#4895ef"),
       xlab="Family rE", ylab="", cex=1, cex.axis=2, cex.lab=2)

# ============= Load and subset data: combined SNP rA =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_comorbidity.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]
datMeta <- subset(datMeta, NDD.DICC == "NDD")

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "MeasurrE", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "CovariatrE", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "rA", "rA.rG_SE", "rC", "rC_SE", "rE", 
                              "rE_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: SNP studies, heritability, sexes combined, NDDsNDDs
datMeta <- subset(datMeta_all, Design =="SNP")
datMeta <- subset(datMeta, !is.na(datMeta$rA))
datMeta <- subset(datMeta, Sex == "Combined")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=rA, var=rA.rG_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#4895ef", "#4895ef"),
       xlab="SNP rG", ylab="", cex=1, cex.axis=2, cex.lab=2)



##### NDDsDICC #####
# ============= Load and subset data: combined family rA =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_comorbidity.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, NDDsDICC
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$rA))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, NDD.DICC == "DICC")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=rA, var=rA.rG_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#f48c06", "#f48c06"),
       xlab="Family rA", ylab="", cex=1, cex.axis=2, cex.lab=2)

# ============= Load and subset data: combined family rC =============

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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, NDDsDICC
datMeta_all$rC <- is.numeric(datMeta_all$rC)
datMeta_all$rC_SE <- is.numeric(datMeta_all$rC_SE)
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$rC))
datMeta <- subset(datMeta, Sex == "Combined")
datMeta <- subset(datMeta, NDD.DICC == "DICC")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=rC, var=rC_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#f48c06", "#f48c06"),
       xlab="Family rC", ylab="", cex=1, cex.axis=2, cex.lab=2)

# ============= Load and subset data: combined family rE =============

# Set WD
setwd("./")

# Read in data
datMeta_all=read.csv("Extraction_comorbidity.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]
datMeta <- subset(datMeta, NDD.DICC == "DICC")

# Relevant columns only
datMeta_all <- datMeta_all[,c("Full.Ref", "Ref", "X", "X.White", "X.Hispanic", "X.AfrAm", 
                              "X.Asian", "X.Chinese.Han", "Covariate1", "Cohort", "Cohort1", 
                              "Cohort2", "Cohort3", "Design1", "Design2", "Design2.old", "N", 
                              "X.1", "Mean_age", "Age_range", "Age_group", "Cohort_country1", 
                              "Cohort_country2", "Broad.Trait.Disorder", "Trait.Disorder", 
                              "Categorical.Continuous", "Binary.Continuous", "Measure1", "MeasurrE", 
                              "Measure3", "Measure4", "Measure5", "Measure6", "Measure7", "Measure8", 
                              "Rater1", "Rater2", "Covariate1.1", "CovariatrE", "Covariate3", 
                              "Covariate4", "Design", "Specific_phenotype", "Specific_phenotype_category", 
                              "Continuous_phenotype_category", "Binary.Continuous.1", "Group", 
                              "Sex", "Prevalence", "Other", "rE", "rE_SE", "rE", "rE_SE", "rE", 
                              "rE_SE", "Cohort_for_agg_by_study", "Model")]

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, NDDsDICC
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$rE))
datMeta <- subset(datMeta, Sex == "Combined")

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=rE, var=rE_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

### set up 2x2 array for plotting
par(mfrow=c(1,1))

### draw funnel plot
funnel(datMeta_agg0.5$es, datMeta_agg0.5$var, refline=0.5,ylim=c(0, 1),xlim=c(-2.5,2.5),
       col=ifelse(datMeta_agg0.5$es > .5, "#f48c06", "#f48c06"),
       xlab="Family rE", ylab="", cex=1, cex.axis=2, cex.lab=2)

