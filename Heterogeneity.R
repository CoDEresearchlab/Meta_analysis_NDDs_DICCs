# ============= Heterogeneity assessment =============
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

# Extract subset of data that I want to meta-analyse: Family studies, heritability, sexes combined, all NDDs
datMeta <- subset(datMeta_all, Design =="Family")
datMeta <- subset(datMeta, !is.na(datMeta$h2))
datMeta <- subset(datMeta, Sex == "Combined")

# ============= Compute effect size and variance -- Converting h2 into Fisher's Z ==========================
# ============= Treat h2 as correlation and convert to Fisher's Z (SE will be used as variance) =============

# Create a vector of correlations needed for FisherZ function
cors <- datMeta$h2

# Convert correlations to Fisher's Z
cors <- FisherZ(cors)

# Convert vector of Zs into a data frame
cors <- as.data.frame(cors)

# Merge back with the datMeta
datMeta$FZ_h2 <- cors$cors

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=FZ_h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

# Extract grand h2
REM0.5_nocov_h2 <- fisherz2r(coef(REM0.5_nocov))
names(REM0.5_nocov_h2) <- NULL
REM0.5_nocov_h2

#Extract standard error
REM0.5_nocov_se <- fisherz2r(REM0.5_nocov$se)
REM0.5_nocov_se

# ============= Heterogeneity =============
## Overall
W <- diag(1/datMeta_agg0.5$var)
X <- model.matrix(REM0.5_nocov)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(REM0.5_nocov$sigma2) / (sum(REM0.5_nocov$sigma2) + (REM0.5_nocov$k-REM0.5_nocov$p)/sum(diag(P)))

## Split
100 * REM0.5_nocov$sigma2 / (sum(REM0.5_nocov$sigma2) + (REM0.5_nocov$k-REM0.5_nocov$p)/sum(diag(P)))

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

# ============= Compute effect size and variance -- Converting c2 into Fisher's Z ==========================
# ============= Treat c2 as correlation and convert to Fisher's Z (SE will be used as variance) =============

# Create a vector of correlations needed for FisherZ function
cors <- datMeta$c2

# Convert correlations to Fisher's Z
cors <- FisherZ(cors)

# Convert vector of Zs into a data frame
cors <- as.data.frame(cors)

# Merge back with the datMeta
datMeta$FZ_c2 <- cors$cors

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=FZ_c2, var=c2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

# Extract grand c2
REM0.5_nocov_c2 <- fisherz2r(coef(REM0.5_nocov))
names(REM0.5_nocov_c2) <- NULL
REM0.5_nocov_c2

#Extract standard error
REM0.5_nocov_se <- fisherz2r(REM0.5_nocov$se)
REM0.5_nocov_se

# ============= Heterogeneity =============
## Overall
W <- diag(1/datMeta_agg0.5$var)
X <- model.matrix(REM0.5_nocov)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(REM0.5_nocov$sigma2) / (sum(REM0.5_nocov$sigma2) + (REM0.5_nocov$k-REM0.5_nocov$p)/sum(diag(P)))

## Split
100 * REM0.5_nocov$sigma2 / (sum(REM0.5_nocov$sigma2) + (REM0.5_nocov$k-REM0.5_nocov$p)/sum(diag(P)))


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

# ============= Compute effect size and variance -- Converting e2 into Fisher's Z ==========================
# ============= Treat e2 as correlation and convert to Fisher's Z (SE will be used as variance) =============

# Create a vector of correlations needed for FisherZ function
cors <- datMeta$e2

# Convert correlations to Fisher's Z
cors <- FisherZ(cors)

# Convert vector of Zs into a data frame
cors <- as.data.frame(cors)

# Merge back with the datMeta
datMeta$FZ_e2 <- cors$cors

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

# Extract grand e2
REM0.5_nocov_e2 <- fisherz2r(coef(REM0.5_nocov))
names(REM0.5_nocov_e2) <- NULL
REM0.5_nocov_e2

#Extract standard error
REM0.5_nocov_se <- fisherz2r(REM0.5_nocov$se)
REM0.5_nocov_se

# ============= Heterogeneity =============
## Overall
W <- diag(1/datMeta_agg0.5$var)
X <- model.matrix(REM0.5_nocov)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(REM0.5_nocov$sigma2) / (sum(REM0.5_nocov$sigma2) + (REM0.5_nocov$k-REM0.5_nocov$p)/sum(diag(P)))

## Split
100 * REM0.5_nocov$sigma2 / (sum(REM0.5_nocov$sigma2) + (REM0.5_nocov$k-REM0.5_nocov$p)/sum(diag(P)))



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

# ============= Compute effect size and variance -- Converting h2 into Fisher's Z ==========================
# ============= Treat h2 as correlation and convert to Fisher's Z (SE will be used as variance) =============

# Create a vector of correlations needed for FisherZ function
cors <- datMeta$h2

# Convert correlations to Fisher's Z
cors <- FisherZ(cors)

# Convert vector of Zs into a data frame
cors <- as.data.frame(cors)

# Merge back with the datMeta
datMeta$FZ_h2 <- cors$cors

# ============= Using aggregate function to combine non-independent effect sizes (by study) =============
# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.5, r=.3 and r=.9 and compare

datMeta_agg0.5 <- agg(id=Full.Ref, es=FZ_h2, var=h2_SE, cor=0.5, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")

# Extract grand h2
REM0.5_nocov_h2 <- fisherz2r(coef(REM0.5_nocov))
names(REM0.5_nocov_h2) <- NULL
REM0.5_nocov_h2

#Extract standard error
REM0.5_nocov_se <- fisherz2r(REM0.5_nocov$se)
REM0.5_nocov_se

# ============= Heterogeneity =============
## Overall
W <- diag(1/datMeta_agg0.5$var)
X <- model.matrix(REM0.5_nocov)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(REM0.5_nocov$sigma2) / (sum(REM0.5_nocov$sigma2) + (REM0.5_nocov$k-REM0.5_nocov$p)/sum(diag(P)))

## Split
100 * REM0.5_nocov$sigma2 / (sum(REM0.5_nocov$sigma2) + (REM0.5_nocov$k-REM0.5_nocov$p)/sum(diag(P)))


