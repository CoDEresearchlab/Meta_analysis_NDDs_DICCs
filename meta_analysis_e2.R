# ============= Script originally from Tabea Schoeler, edited/adapted by Yasmin Ahmadzadeh for meta-analysis, further edited by Aga Gidziela =============
# ============= October 2019 - April 2020 =============

rm(list=ls())

library("compute.es")
library("metafor")
library("gmodels")
library("MAd")
library("DescTools")
library("psych")
library("dplyr")
library("tidyverse")

R.version.string

# ============================================================================ 
# ============= NAs for easy run =============
rm(list=ls())

REM0.5_childhood_e2 <- NA                   
REM0.5_middlechildhood_e2 <- NA             
REM0.5_adolescence_e2 <- NA                 
REM0.5_childhood_middlechildhood_e2 <- NA   
REM0.5_childhood_adolescence_e2 <- NA       
REM0.5_middlechildhood_adolescence_e2 <- NA 
REM0.5_sibling_categorical_e2 <- NA         
REM0.5_sibling_classical_e2 <- NA           
REM0.5_twin_and_sibling_categorical_e2 <- NA
REM0.5_twin_and_sibling_dfextremes_e2 <- NA 
REM0.5_twin_and_sibling_classical_e2 <- NA  
REM0.5_twin_categorical_e2 <- NA            
REM0.5_twin_dfextremes_e2 <- NA             
REM0.5_twin_classical_e2 <- NA              
REM0.5_Aonly_e2 <- NA                       
REM0.5_BEST_e2 <- NA                        
REM0.5_FULL_e2 <- NA                        
REM0.5_DFAonly_e2 <- NA                     
REM0.5_DFBEST_e2 <- NA                      
REM0.5_DFFULL_e2 <- NA                      
REM0.5_TC_e2 <- NA                          
REM0.5_diagnosis_e2 <- NA                   
REM0.5_parent_e2 <- NA                      
REM0.5_parent_self_e2 <- NA                 
REM0.5_parent_teacher_e2 <- NA              
REM0.5_researcher_e2 <- NA                  
REM0.5_self_e2 <- NA                        
REM0.5_teacher_e2 <- NA                     
REM0.5_test_e2 <- NA                        
REM0.5_categorical_e2 <- NA                 
REM0.5_continuous_e2 <- NA                  
REM0.5_50_e2 <- NA                          
REM0.5_50_75_e2 <- NA                       
REM0.5_75_100_e2 <- NA                      
REM0.5_100_e2 <- NA                         
REM0.5_0_e2 <- NA                           
REM0.5_1_e2 <- NA                           
REM0.5_2_e2 <- NA                           
REM0.5_3_e2 <- NA                           
REM0.5_4_e2 <- NA                           
REM0.5_Australia_e2 <- NA                   
REM0.5_Aus_USA_Nor_Swe_e2 <- NA             
REM0.5_Canada_e2 <- NA                      
REM0.5_China_e2 <- NA                       
REM0.5_Netherlands_e2 <- NA                 
REM0.5_Norway_e2 <- NA                      
REM0.5_Sweden_e2 <- NA                      
REM0.5_UK_e2 <- NA                          
REM0.5_USA_e2 <- NA    

REM0.5_childhood_se <- NA                   
REM0.5_middlechildhood_se <- NA             
REM0.5_adolescence_se <- NA                 
REM0.5_childhood_middlechildhood_se <- NA   
REM0.5_childhood_adolescence_se <- NA       
REM0.5_middlechildhood_adolescence_se <- NA 
REM0.5_sibling_categorical_se <- NA         
REM0.5_sibling_classical_se <- NA           
REM0.5_twin_and_sibling_categorical_se <- NA
REM0.5_twin_and_sibling_dfextremes_se <- NA 
REM0.5_twin_and_sibling_classical_se <- NA  
REM0.5_twin_categorical_se <- NA            
REM0.5_twin_dfextremes_se <- NA             
REM0.5_twin_classical_se <- NA              
REM0.5_Aonly_se <- NA                       
REM0.5_BEST_se <- NA                        
REM0.5_FULL_se <- NA                        
REM0.5_DFAonly_se <- NA                     
REM0.5_DFBEST_se <- NA                      
REM0.5_DFFULL_se <- NA                      
REM0.5_TC_se <- NA                          
REM0.5_diagnosis_se <- NA                   
REM0.5_parent_se <- NA                      
REM0.5_parent_self_se <- NA                 
REM0.5_parent_teacher_se <- NA              
REM0.5_researcher_se <- NA                  
REM0.5_self_se <- NA                        
REM0.5_teacher_se <- NA                     
REM0.5_test_se <- NA                        
REM0.5_categorical_se <- NA                 
REM0.5_continuous_se <- NA                  
REM0.5_50_se <- NA                          
REM0.5_50_75_se <- NA                       
REM0.5_75_100_se <- NA                      
REM0.5_100_se <- NA                         
REM0.5_0_se <- NA                           
REM0.5_1_se <- NA                           
REM0.5_2_se <- NA                           
REM0.5_3_se <- NA                           
REM0.5_4_se <- NA                           
REM0.5_Australia_se <- NA                   
REM0.5_Aus_USA_Nor_Swe_se <- NA             
REM0.5_Canada_se <- NA                      
REM0.5_China_se <- NA                       
REM0.5_Netherlands_se <- NA                 
REM0.5_Norway_se <- NA                      
REM0.5_Sweden_se <- NA                      
REM0.5_UK_se <- NA                          
REM0.5_USA_se <- NA    

REM0.5_childhood_k <- NA                   
REM0.5_middlechildhood_k <- NA             
REM0.5_adolescence_k <- NA                 
REM0.5_childhood_middlechildhood_k <- NA   
REM0.5_childhood_adolescence_k <- NA       
REM0.5_middlechildhood_adolescence_k <- NA 
REM0.5_sibling_categorical_k <- NA         
REM0.5_sibling_classical_k <- NA           
REM0.5_twin_and_sibling_categorical_k <- NA
REM0.5_twin_and_sibling_dfextremes_k <- NA 
REM0.5_twin_and_sibling_classical_k <- NA  
REM0.5_twin_categorical_k <- NA            
REM0.5_twin_dfextremes_k <- NA             
REM0.5_twin_classical_k <- NA              
REM0.5_Aonly_k <- NA                       
REM0.5_BEST_k <- NA                        
REM0.5_FULL_k <- NA                        
REM0.5_DFAonly_k <- NA                     
REM0.5_DFBEST_k <- NA                      
REM0.5_DFFULL_k <- NA                      
REM0.5_TC_k <- NA                          
REM0.5_diagnosis_k <- NA                   
REM0.5_parent_k <- NA                      
REM0.5_parent_self_k <- NA                 
REM0.5_parent_teacher_k <- NA              
REM0.5_researcher_k <- NA                  
REM0.5_self_k <- NA                        
REM0.5_teacher_k <- NA                     
REM0.5_test_k <- NA                        
REM0.5_categorical_k <- NA                 
REM0.5_continuous_k <- NA                  
REM0.5_50_k <- NA                          
REM0.5_50_75_k <- NA                       
REM0.5_75_100_k <- NA                      
REM0.5_100_k <- NA                         
REM0.5_0_k <- NA                           
REM0.5_1_k <- NA                           
REM0.5_2_k <- NA                           
REM0.5_3_k <- NA                           
REM0.5_4_k <- NA                           
REM0.5_Australia_k <- NA                   
REM0.5_Aus_USA_Nor_Swe_k <- NA             
REM0.5_Canada_k <- NA                      
REM0.5_China_k <- NA                       
REM0.5_Netherlands_k <- NA                 
REM0.5_Norway_k <- NA                      
REM0.5_Sweden_k <- NA                      
REM0.5_UK_k <- NA                          
REM0.5_USA_k <- NA    

# ============= Load and subset data: combined family e2 =============

# Set WD
setwd("~/")

# Read in data
datMeta_all=read.csv("Extraction_heritability.csv",header=T, strip.white = T)

# Relevant rows only
datMeta_all <- datMeta_all[!is.na(datMeta_all$Full.Ref),]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != "",]
datMeta_all <- datMeta_all[datMeta_all$Full.Ref != " ",]

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
datMeta_agg0.3 <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.3, data=datMeta)
datMeta_agg0.9 <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.9, data=datMeta)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta[!duplicated(datMeta$Full.Ref),]

# Now label cohorts
datMeta_agg0.5$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study
datMeta_agg0.3$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study
datMeta_agg0.9$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM models       
REM0.5_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5, slab=paste(id, cohort, sep="  /  "), method="REML")
REM0.3_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.3, slab=paste(id, cohort, sep="  /  "), method="REML")
REM0.9_nocov <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.9, slab=paste(id, cohort, sep="  /  "), method="REML")

# Extract grand e2
REM0.5_nocov_e2 <- fisherz2r(coef(REM0.5_nocov))
names(REM0.5_nocov_e2) <- NULL
REM0.5_nocov_e2

REM0.3_nocov_e2 <- fisherz2r(coef(REM0.3_nocov))
names(REM0.3_nocov_e2) <- NULL
REM0.3_nocov_e2

REM0.9_nocov_e2 <- fisherz2r(coef(REM0.9_nocov))
names(REM0.9_nocov_e2) <- NULL
REM0.9_nocov_e2

cbind(REM0.5_nocov_e2, REM0.3_nocov_e2, REM0.9_nocov_e2) #No difference :)

#Extract standard error
REM0.5_nocov_se <- fisherz2r(REM0.5_nocov$se)
REM0.5_nocov_se

REM0.3_nocov_se <- fisherz2r(REM0.3_nocov$se)
REM0.3_nocov_se

REM0.9_nocov_se <- fisherz2r(REM0.9_nocov$se)
REM0.9_nocov_se

cbind(REM0.5_nocov_se, REM0.3_nocov_se, REM0.9_nocov_se) #No difference :)

# ============= Aggregate by cohort instead of by publication - sensitivity check using REM (don't need multilevel structure here - all data on one level; no covariates) =============

datMeta_agg0.5_cohort <- agg(id=Cohort, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta)
datMeta_agg0.3_cohort <- agg(id=Cohort, es=FZ_e2, var=e2_SE, cor=0.3, data=datMeta)
datMeta_agg0.9_cohort <- agg(id=Cohort, es=FZ_e2, var=e2_SE, cor=0.9, data=datMeta)

REM0.5_cohort_nocov <- rma(es, var, data=datMeta_agg0.5_cohort, slab=paste(id), method="REML")
REM0.3_cohort_nocov <- rma(es, var, data=datMeta_agg0.3_cohort, slab=paste(id), method="REML")
REM0.9_cohort_nocov <- rma(es, var, data=datMeta_agg0.9_cohort, slab=paste(id), method="REML")

# Extract grand e2
REM0.5_cohort_nocov_e2 <- fisherz2r(coef(REM0.5_cohort_nocov))
names(REM0.5_cohort_nocov_e2) <- NULL
REM0.5_cohort_nocov_e2

REM0.3_cohort_nocov_e2 <- fisherz2r(coef(REM0.3_cohort_nocov))
names(REM0.3_cohort_nocov_e2) <- NULL
REM0.3_cohort_nocov_e2

REM0.9_cohort_nocov_e2 <- fisherz2r(coef(REM0.9_cohort_nocov))
names(REM0.9_cohort_nocov_e2) <- NULL
REM0.9_cohort_nocov_e2

cbind(REM0.5_cohort_nocov_e2, REM0.3_cohort_nocov_e2, REM0.9_cohort_nocov_e2) #No difference :)


#Extract standard error
REM0.5_cohort_nocov_se <- fisherz2r(REM0.5_cohort_nocov$se)
REM0.5_cohort_nocov_se

REM0.3_cohort_nocov_se <- fisherz2r(REM0.3_cohort_nocov$se)
REM0.3_cohort_nocov_se

REM0.9_cohort_nocov_se <- fisherz2r(REM0.9_cohort_nocov$se)
REM0.9_cohort_nocov_se

cbind(REM0.5_cohort_nocov_se, REM0.3_cohort_nocov_se, REM0.9_cohort_nocov_se) #No difference :)

# RESULTS ARE THE SAME USING REM AT THE COHORT LEVEL VS MREM WITH RANDOM EFFECT FOR COHORT. THIS IS AS WE'D EXPECT: http://www.metafor-project.org/doku.php/tips:rma.uni_vs_rma.mv

# ============= Aggregate by country instead of by publication - sensitivity check using REM (don't need multilevel structure here - all data on one level; no covariates) =============
datMeta_agg0.5_country <- agg(id=Cohort_country1, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta)
datMeta_agg0.3_country <- agg(id=Cohort_country1, es=FZ_e2, var=e2_SE, cor=0.3, data=datMeta)
datMeta_agg0.9_country <- agg(id=Cohort_country1, es=FZ_e2, var=e2_SE, cor=0.9, data=datMeta)

REM0.5_country_nocov <- rma(es, var, data=datMeta_agg0.5_country, slab=paste(id), method="REML")
REM0.3_country_nocov <- rma(es, var, data=datMeta_agg0.3_country, slab=paste(id), method="REML")
REM0.9_country_nocov <- rma(es, var, data=datMeta_agg0.9_country, slab=paste(id), method="REML")

# Extract grand e2
REM0.5_country_nocov_e2 <- fisherz2r(coef(REM0.5_country_nocov))
names(REM0.5_country_nocov_e2) <- NULL
REM0.5_country_nocov_e2

REM0.3_country_nocov_e2 <- fisherz2r(coef(REM0.3_country_nocov))
names(REM0.3_country_nocov_e2) <- NULL
REM0.3_country_nocov_e2

REM0.9_country_nocov_e2 <- fisherz2r(coef(REM0.9_country_nocov))
names(REM0.9_country_nocov_e2) <- NULL
REM0.9_country_nocov_e2

cbind(REM0.5_country_nocov_e2, REM0.3_country_nocov_e2, REM0.9_country_nocov_e2) #No difference :)

#Extract standard error
REM0.5_country_nocov_se <- fisherz2r(REM0.5_country_nocov$se)
REM0.5_country_nocov_se

REM0.3_country_nocov_se <- fisherz2r(REM0.3_country_nocov$se)
REM0.3_country_nocov_se

REM0.9_country_nocov_se <- fisherz2r(REM0.9_country_nocov$se)
REM0.9_country_nocov_se

cbind(REM0.5_country_nocov_se, REM0.3_country_nocov_se, REM0.9_country_nocov_se) #No difference :)

# RESULTS ARE THE SAME USING REM AT THE COHORT LEVEL VS MREM WITH RANDOM EFFECT FOR COHORT. THIS IS AS WE'D EXPECT: http://www.metafor-project.org/doku.php/tips:rma.uni_vs_rma.mv

# ============= Prepare covariates variables =============
# ============= Create Design_detailed covariate =============
# First combine general and detailed designs
datMeta$Design_detailed <- paste(datMeta$Design1, datMeta$Design2, sep = ", ")

# ============= Now create "Number of of Measures" covariate =============
datMeta$NoMes1 <- 1
datMeta$NoMes2 <- with(datMeta, ifelse(is.na(datMeta$Measure2) | datMeta$Measure2 == "" | datMeta$Measure2 == " ", 0, 1))
datMeta$NoMes3 <- with(datMeta, ifelse(is.na(datMeta$Measure3) | datMeta$Measure3 == "" | datMeta$Measure3 == " ", 0, 1))
datMeta$NoMes4 <- with(datMeta, ifelse(is.na(datMeta$Measure4) | datMeta$Measure4 == "" | datMeta$Measure4 == " ", 0, 1))
datMeta$NoMes5 <- with(datMeta, ifelse(is.na(datMeta$Measure5) | datMeta$Measure5 == "" | datMeta$Measure5 == " ", 0, 1))
datMeta$NoMes6 <- with(datMeta, ifelse(is.na(datMeta$Measure6) | datMeta$Measure6 == "" | datMeta$Measure6 == " ", 0, 1))
datMeta$NoMes7 <- with(datMeta, ifelse(is.na(datMeta$Measure7) | datMeta$Measure7 == "" | datMeta$Measure7 == " ", 0, 1))
datMeta$NoMes8 <- with(datMeta, ifelse(is.na(datMeta$Measure8) | datMeta$Measure8 == "" | datMeta$Measure8 == " ", 0, 1))
datMeta$NoMeasure <- datMeta$NoMes1+datMeta$NoMes2+datMeta$NoMes3+datMeta$NoMes4+datMeta$NoMes5+datMeta$NoMes6+datMeta$NoMes7+datMeta$NoMes8

# ============= Create a "narrative" Measure covariate =============

datMeta$NarrMeasure <- with(datMeta, ifelse(datMeta$Measure2 == "" | datMeta$Measure2 == " " | is.na(datMeta$Measure2), datMeta$Measure1, 
                                            ifelse(datMeta$Measure2 != "" | datMeta$Measure2 != " " | !is.na(datMeta$Measure2) |
                                                     datMeta$Measure3 == "" | datMeta$Measure3 == " " | is.na(datMeta$Measure3),  paste(datMeta$Measure1, datMeta$Measure2, sep = ", "),
                                                   ifelse(datMeta$Measure3 != "" | datMeta$Measure3 != " " | !is.na(datMeta$Measure3) |
                                                            datMeta$Measure4 == "" | datMeta$Measure4 == " " | is.na(datMeta$Measure4), paste(datMeta$Measure1, datMeta$Measure2, datMeta$Measure3, sep = ", "),
                                                          ifelse(datMeta$Measure4 != "" | datMeta$Measure4 != " " | !is.na(datMeta$Measure4) |
                                                                   datMeta$Measure5 == "" | datMeta$Measure5 == " " | is.na(datMeta$Measure5), paste(datMeta$Measure1, datMeta$Measure2, datMeta$Measure3, datMeta$Measure4, sep = ", "),
                                                                 ifelse(datMeta$Measure5 != "" | datMeta$Measure5 != " " | !is.na(datMeta$Measure5) |
                                                                          datMeta$Measure6 == "" | datMeta$Measure6 == " " | is.na(datMeta$Measure6), paste(datMeta$Measure1, datMeta$Measure2, datMeta$Measure3, datMeta$Measure4, datMeta$Measure5, sep = ", "),
                                                                        ifelse(datMeta$Measure6 != "" | datMeta$Measure6 != " " | !is.na(datMeta$Measure6) |
                                                                                 datMeta$Measure7 == "" | datMeta$Measure7 == " " | is.na(datMeta$Measure7), paste(datMeta$Measure1, datMeta$Measure2, datMeta$Measure3, datMeta$Measure4, datMeta$Measure5, datMeta$Measure6, sep = ", "),
                                                                               ifelse(datMeta$Measure7 != "" | datMeta$Measure7 != " " | !is.na(datMeta$Measure7) |
                                                                                        datMeta$Measure8 == "" | datMeta$Measure8 == " " | is.na(datMeta$Measure8), paste(datMeta$Measure1, datMeta$Measure2, datMeta$Measure3, datMeta$Measure4, datMeta$Measure5,datMeta$Measure6, datMeta$Measure7, sep = ", "),
                                                                                      ifelse(datMeta$Measure8 != "" | datMeta$Measure8 != " " | !is.na(datMeta$Measure8),paste(datMeta$Measure1, datMeta$Measure2, datMeta$Measure3, datMeta$Measure4, datMeta$Measure5,datMeta$Measure6, datMeta$Measure7, datMeta$Measure8, sep = ", ")
                                                                                      )))))))))


# ============= Now create "Number of of covariates" covariate =============
datMeta$Cov1 <- with(datMeta, ifelse(datMeta$Covariate1 == "Not used", 0, 1))
datMeta$Cov2 <- with(datMeta, ifelse(is.na(datMeta$Covariate2) | datMeta$Covariate2 == "" | datMeta$Covariate2 == " ", 0, 1))
datMeta$Cov3 <- with(datMeta, ifelse(is.na(datMeta$Covariate3) | datMeta$Covariate3 == "" | datMeta$Covariate3 == " ", 0, 1))
datMeta$Cov4 <- with(datMeta, ifelse(is.na(datMeta$Covariate4) | datMeta$Covariate4 == "" | datMeta$Covariate4 == " ", 0, 1))
datMeta$NoCov <- datMeta$Cov1+datMeta$Cov2+datMeta$Cov3+datMeta$Cov4
# ============= Now create "narrative covariates" covariate =============
datMeta$NarrCov <- with(datMeta, ifelse(datMeta$Covariate2 == "" | datMeta$Covariate2 == " " | is.na(datMeta$Covariate2), datMeta$Covariate1, 
                                        ifelse(datMeta$Covariate2 != "" | datMeta$Covariate2 != " " | !is.na(datMeta$Covariate2) |
                                                 datMeta$Covariate3 == "" | datMeta$Covariate3 == " " | is.na(datMeta$Covariate3),  paste(datMeta$Covariate1, datMeta$Covariate2, sep = ", "),
                                               ifelse(datMeta$Covariate3 != "" | datMeta$Covariate3 != " " | !is.na(datMeta$Covariate3) |
                                                        datMeta$Covariate4 == "" | datMeta$Covariate4 == " " | is.na(datMeta$Covariate4), paste(datMeta$Covariate1, datMeta$Covariate2, datMeta$Covariate3, sep = ", "),
                                                      ifelse(datMeta$Covariate4 != "" | datMeta$Covariate4 != " " | !is.na(datMeta$Covariate4),paste(datMeta$Covariate1, datMeta$Covariate2, datMeta$Covariate3, datMeta$Covariate4, sep = ", ")
                                                      )))))

# ============= Create Rater covariate =============
# Create Rater covariate taking into account that missing rater means a test and correct a typo
datMeta$Two_Raters <- with(datMeta, ifelse(datMeta$Rater2 == "" | datMeta$Rater2 == " " | is.na(datMeta$Rater2), datMeta$Rater1, paste(datMeta$Rater1, datMeta$Rater2, sep = ", ")))
datMeta$Rater <- with(datMeta, ifelse(datMeta$Rater1 == "" | datMeta$Rater1 == " " | is.na(datMeta$Rater1), "Test", datMeta$Two_Raters))

# Make order consistent
datMeta$Rater <- with(datMeta, ifelse(datMeta$Rater == "Self, Parent", "Parent, Self", datMeta$Rater))

# ============= Create White covariate =============
# Create meaningful categories of % White
# Tell R the variable is numeric
datMeta$X.White <- as.numeric(datMeta$X.White)

# Create categories
# datMeta$White[datMeta$X.White <= 25] <- "Less than or equal to 25%" Only one study; merge with subsequent category
datMeta$White[datMeta$X.White < 50] <- "Less than 50%"
datMeta$White[datMeta$X.White >= 50 & datMeta$X.White < 75] <- "More than or equal to 50% but less than 75%"
datMeta$White[datMeta$X.White >= 75 & datMeta$X.White < 100] <- "More than or equal to 75% but less than 100%"
datMeta$White[datMeta$X.White == 100] <- "100%"

# ============= Aggregate non-independent effect sizes by study and stratify by levels of moderators=============
# ============= Age  =============
# ============= Testing age: Childhood  =============
# Select Childhood and aggregate within childhood
datMeta_childhood <- datMeta[datMeta$Age_group == "Childhood",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_childhood <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_childhood)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_childhood[!duplicated(datMeta_childhood$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_childhood$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model  
REM0.5_childhood <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_childhood, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_childhood <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_childhood, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_childhood

# Extract grand e2
REM0.5_childhood_e2 <- fisherz2r(coef(REM0.5_childhood))
names(REM0.5_childhood_e2) <- NULL
REM0.5_childhood_e2

#Extract standard error
REM0.5_childhood_se <- fisherz2r(REM0.5_childhood$se)
REM0.5_childhood_se

#Extract no of studies
REM0.5_childhood_k <- REM0.5_childhood$k.all

# ============= Testing age: Middle Childhood  =============
# Select Middle Childhood and aggregate within middlechildhood
datMeta_middlechildhood <- datMeta[datMeta$Age_group == "Middle Childhood",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_middlechildhood <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_middlechildhood)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_middlechildhood[!duplicated(datMeta_middlechildhood$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_middlechildhood$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model
REM0.5_middlechildhood <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_middlechildhood, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_middlechildhood <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_middlechildhood, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_middlechildhood

# Extract grand e2
REM0.5_middlechildhood_e2 <- fisherz2r(coef(REM0.5_middlechildhood))
names(REM0.5_middlechildhood_e2) <- NULL
REM0.5_middlechildhood_e2

#Extract standard error
REM0.5_middlechildhood_se <- fisherz2r(REM0.5_middlechildhood$se)
REM0.5_middlechildhood_se

#Extract no of studies
REM0.5_middlechildhood_k <- REM0.5_middlechildhood$k.all

# ============= Testing age: Adolescence  =============
# Select Adolescence and aggregate within adolescence
datMeta_adolescence <- datMeta[datMeta$Age_group == "Adolescence",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_adolescence <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_adolescence)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_adolescence[!duplicated(datMeta_adolescence$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_adolescence$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model   
REM0.5_adolescence <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_adolescence, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_adolescence <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_adolescence, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_adolescence

# Extract grand e2
REM0.5_adolescence_e2 <- fisherz2r(coef(REM0.5_adolescence))
names(REM0.5_adolescence_e2) <- NULL
REM0.5_adolescence_e2

#Extract standard error
REM0.5_adolescence_se <- fisherz2r(REM0.5_adolescence$se)
REM0.5_adolescence_se

#Extract no of studies
REM0.5_adolescence_k <- REM0.5_adolescence$k.all

# ============= Testing age: Childhood & Middle Childhood   =============
# Select Childhood & Middle Childhood and aggregate within childhood_middlechildhood
datMeta_childhood_middlechildhood <- datMeta[datMeta$Age_group == "Childhood & Middle Childhood",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_childhood_middlechildhood <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_childhood_middlechildhood)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_childhood_middlechildhood[!duplicated(datMeta_childhood_middlechildhood$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_childhood_middlechildhood$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model     
REM0.5_childhood_middlechildhood <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_childhood_middlechildhood, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_childhood_middlechildhood <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_childhood_middlechildhood, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_childhood_middlechildhood

# Extract grand e2
REM0.5_childhood_middlechildhood_e2 <- fisherz2r(coef(REM0.5_childhood_middlechildhood))
names(REM0.5_childhood_middlechildhood_e2) <- NULL
REM0.5_childhood_middlechildhood_e2

#Extract standard error
REM0.5_childhood_middlechildhood_se <- fisherz2r(REM0.5_childhood_middlechildhood$se)
REM0.5_childhood_middlechildhood_se

#Extract no of studies
REM0.5_childhood_middlechildhood_k <- REM0.5_childhood_middlechildhood$k.all

# ============= Testing age: Middle Childhood & Adolescence  =============
# Select Middle Childhood & Adolescence and aggregate within middlechildhood_adolescence
datMeta_middlechildhood_adolescence <- datMeta[datMeta$Age_group == "Middle Childhood & Adolescence",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_middlechildhood_adolescence <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_middlechildhood_adolescence)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_middlechildhood_adolescence[!duplicated(datMeta_middlechildhood_adolescence$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_middlechildhood_adolescence$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model   
REM0.5_middlechildhood_adolescence <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_middlechildhood_adolescence, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_middlechildhood_adolescence <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_middlechildhood_adolescence, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_middlechildhood_adolescence

# Extract grand e2
REM0.5_middlechildhood_adolescence_e2 <- fisherz2r(coef(REM0.5_middlechildhood_adolescence))
names(REM0.5_middlechildhood_adolescence_e2) <- NULL
REM0.5_middlechildhood_adolescence_e2

#Extract standard error
REM0.5_middlechildhood_adolescence_se <- fisherz2r(REM0.5_middlechildhood_adolescence$se)
REM0.5_middlechildhood_adolescence_se

#Extract no of studies
REM0.5_middlechildhood_adolescence_k <- REM0.5_middlechildhood_adolescence$k.all

# ============= Testing age: Childhood & Adolescence  =============
# Select Childhood & Adolescence and aggregate within childhood_adolescence
datMeta_childhood_adolescence <- datMeta[datMeta$Age_group == "Childhood & Adolescence",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_childhood_adolescence <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_childhood_adolescence)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_childhood_adolescence[!duplicated(datMeta_childhood_adolescence$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_childhood_adolescence$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model  
REM0.5_childhood_adolescence <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_childhood_adolescence, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_childhood_adolescence <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_childhood_adolescence, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_childhood_adolescence

# Extract grand e2
REM0.5_childhood_adolescence_e2 <- fisherz2r(coef(REM0.5_childhood_adolescence))
names(REM0.5_childhood_adolescence_e2) <- NULL
REM0.5_childhood_adolescence_e2

#Extract standard error
REM0.5_childhood_adolescence_se <- fisherz2r(REM0.5_childhood_adolescence$se)
REM0.5_childhood_adolescence_se

#Extract no of studies
REM0.5_childhood_adolescence_k <- REM0.5_childhood_adolescence$k.all

# ============= Compare estimates for age groups =============
ages_e2 <- rbind(REM0.5_childhood_e2,
                 REM0.5_middlechildhood_e2,
                 REM0.5_adolescence_e2,
                 REM0.5_childhood_middlechildhood_e2,
                 REM0.5_childhood_adolescence_e2,
                 REM0.5_middlechildhood_adolescence_e2)

ages_se <- rbind(REM0.5_childhood_se,
                 REM0.5_middlechildhood_se,
                 REM0.5_adolescence_se,
                 REM0.5_childhood_middlechildhood_se,
                 REM0.5_childhood_adolescence_se,
                 REM0.5_middlechildhood_adolescence_se)

ages_k <- rbind(REM0.5_childhood_k,
                REM0.5_middlechildhood_k,
                REM0.5_adolescence_k,
                REM0.5_childhood_middlechildhood_k,
                REM0.5_childhood_adolescence_k,
                REM0.5_middlechildhood_adolescence_k)

ages <- cbind(ages_e2, ages_se, ages_k)
colnames(ages) <- c("est","se","k")
ages <- data.frame(ages)
ages$sig <- with(ages, ifelse(is.na(ages$est), NA,
                              ifelse(REM0.5_agecov_pval > 0.05, "nosig", "sig")))
ages$type <- "ages"
ages$sex <- "Combined"
ages

# ============= Design  =============
# ============= Testing design: Sibling study, Categorical threshold  =============
# Select Sibling study, Categorical threshold and aggregate within Sibling study, Categorical threshold
datMeta_sibling_categorical <- datMeta[datMeta$Design_detailed == "Sibling study, Categorical threshold",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_sibling_categorical <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_sibling_categorical)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_sibling_categorical[!duplicated(datMeta_sibling_categorical$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_sibling_categorical$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model   
REM0.5_sibling_categorical <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_sibling_categorical, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_sibling_categorical <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_sibling_categorical, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_sibling_categorical

# Extract grand e2
REM0.5_sibling_categorical_e2 <- fisherz2r(coef(REM0.5_sibling_categorical))
names(REM0.5_sibling_categorical_e2) <- NULL
REM0.5_sibling_categorical_e2

#Extract standard error
REM0.5_sibling_categorical_se <- fisherz2r(REM0.5_sibling_categorical$se)
REM0.5_sibling_categorical_se

#Extract no of studies
REM0.5_sibling_categorical_k <- REM0.5_sibling_categorical$k.all

# ============= Testing design: Sibling study, Classical  =============
# Select Sibling study, Classical and aggregate within Sibling study, Classical
datMeta_sibling_classical <- datMeta[datMeta$Design_detailed == "Sibling study, Classical",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_sibling_classical <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_sibling_classical)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_sibling_classical[!duplicated(datMeta_sibling_classical$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_sibling_classical$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model    
REM0.5_sibling_classical <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_sibling_classical, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_sibling_classical <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_sibling_classical, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_sibling_classical

# Extract grand e2
REM0.5_sibling_classical_e2 <- fisherz2r(coef(REM0.5_sibling_classical))
names(REM0.5_sibling_classical_e2) <- NULL
REM0.5_sibling_classical_e2

#Extract standard error
REM0.5_sibling_classical_se <- fisherz2r(REM0.5_sibling_classical$se)
REM0.5_sibling_classical_se

#Extract no of studies
REM0.5_sibling_classical_k <- REM0.5_sibling_classical$k.all


# ============= Testing design: Twin and sibling study, Categorical threshold  =============
# Select Twin and sibling study, Categorical threshold and aggregate within Twin and sibling study, Categorical threshold
datMeta_twin_and_sibling_categorical <- datMeta[datMeta$Design_detailed == "Twin and sibling study, Categorical threshold",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_twin_and_sibling_categorical <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_twin_and_sibling_categorical)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_twin_and_sibling_categorical[!duplicated(datMeta_twin_and_sibling_categorical$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_twin_and_sibling_categorical$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model    
REM0.5_twin_and_sibling_categorical <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_twin_and_sibling_categorical, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_twin_and_sibling_categorical <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_twin_and_sibling_categorical, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_twin_and_sibling_categorical

# Extract grand e2
REM0.5_twin_and_sibling_categorical_e2 <- fisherz2r(coef(REM0.5_twin_and_sibling_categorical))
names(REM0.5_twin_and_sibling_categorical_e2) <- NULL
REM0.5_twin_and_sibling_categorical_e2

#Extract standard error
REM0.5_twin_and_sibling_categorical_se <- fisherz2r(REM0.5_twin_and_sibling_categorical$se)
REM0.5_twin_and_sibling_categorical_se

#Extract no of studies
REM0.5_twin_and_sibling_categorical_k <- REM0.5_twin_and_sibling_categorical$k.all


# ============= Testing design: Twin and sibling study, Classical  =============
# Select Twin and sibling study, Classical and aggregate within Twin and sibling study, Classical
datMeta_twin_and_sibling_classical <- datMeta[datMeta$Design_detailed == "Twin and sibling study, Classical",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_twin_and_sibling_classical <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_twin_and_sibling_classical)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_twin_and_sibling_classical[!duplicated(datMeta_twin_and_sibling_classical$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_twin_and_sibling_classical$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model
REM0.5_twin_and_sibling_classical <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_twin_and_sibling_classical, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_twin_and_sibling_classical <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_twin_and_sibling_classical, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_twin_and_sibling_classical

# Extract grand e2
REM0.5_twin_and_sibling_classical_e2 <- fisherz2r(coef(REM0.5_twin_and_sibling_classical))
names(REM0.5_twin_and_sibling_classical_e2) <- NULL
REM0.5_twin_and_sibling_classical_e2

#Extract standard error
REM0.5_twin_and_sibling_classical_se <- fisherz2r(REM0.5_twin_and_sibling_classical$se)
REM0.5_twin_and_sibling_classical_se

#Extract no of studies
REM0.5_twin_and_sibling_classical_k <- REM0.5_twin_and_sibling_classical$k.all



# ============= Testing design: Twin and sibling study, DFextremes  =============
# Select Twin and sibling study, DFextremes and aggregate within Twin and sibling study, DFextremes
datMeta_twin_and_sibling_dfextremes <- datMeta[datMeta$Design_detailed == "Twin and sibling study, DFextremes",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_twin_and_sibling_dfextremes <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_twin_and_sibling_dfextremes)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_twin_and_sibling_dfextremes[!duplicated(datMeta_twin_and_sibling_dfextremes$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_twin_and_sibling_dfextremes$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model     
REM0.5_twin_and_sibling_dfextremes <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_twin_and_sibling_dfextremes, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_twin_and_sibling_dfextremes <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_twin_and_sibling_dfextremes, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_twin_and_sibling_dfextremes

# Extract grand e2
REM0.5_twin_and_sibling_dfextremes_e2 <- fisherz2r(coef(REM0.5_twin_and_sibling_dfextremes))
names(REM0.5_twin_and_sibling_dfextremes_e2) <- NULL
REM0.5_twin_and_sibling_dfextremes_e2

#Extract standard error
REM0.5_twin_and_sibling_dfextremes_se <- fisherz2r(REM0.5_twin_and_sibling_dfextremes$se)
REM0.5_twin_and_sibling_dfextremes_se

#Extract no of studies
REM0.5_twin_and_sibling_dfextremes_k <- REM0.5_twin_and_sibling_dfextremes$k.all

# ============= Testing design: Twin study, Categorical threshold =============
# Select Twin study, Categorical threshold and aggregate within Twin study, Categorical threshold
datMeta_twin_categorical <- datMeta[datMeta$Design_detailed == "Twin study, Categorical threshold",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_twin_categorical <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_twin_categorical)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_twin_categorical[!duplicated(datMeta_twin_categorical$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_twin_categorical$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model 
REM0.5_twin_categorical <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_twin_categorical, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_twin_categorical <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_twin_categorical, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_twin_categorical

# Extract grand e2
REM0.5_twin_categorical_e2 <- fisherz2r(coef(REM0.5_twin_categorical))
names(REM0.5_twin_categorical_e2) <- NULL
REM0.5_twin_categorical_e2

#Extract standard error
REM0.5_twin_categorical_se <- fisherz2r(REM0.5_twin_categorical$se)
REM0.5_twin_categorical_se

#Extract no of studies
REM0.5_twin_categorical_k <- REM0.5_twin_categorical$k.all





# ============= Testing design: Twin study, Classical =============
# Select Twin study, Classical and aggregate within Twin study, Classical
datMeta_twin_classical <- datMeta[datMeta$Design_detailed == "Twin study, Classical",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_twin_classical <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_twin_classical)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_twin_classical[!duplicated(datMeta_twin_classical$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_twin_classical$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model  
REM0.5_twin_classical <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_twin_classical, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_twin_classical <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_twin_classical, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_twin_classical

# Extract grand e2
REM0.5_twin_classical_e2 <- fisherz2r(coef(REM0.5_twin_classical))
names(REM0.5_twin_classical_e2) <- NULL
REM0.5_twin_classical_e2

#Extract standard error
REM0.5_twin_classical_se <- fisherz2r(REM0.5_twin_classical$se)
REM0.5_twin_classical_se

#Extract no of studies
REM0.5_twin_classical_k <- REM0.5_twin_classical$k.all




# ============= Testing design: Twin study, DFextremes  =============
# Select Twin study, DFextremes and aggregate within Twin study, DFextremes
datMeta_twin_dfextremes <- datMeta[datMeta$Design_detailed == "Twin study, DFextremes",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_twin_dfextremes <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_twin_dfextremes)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_twin_dfextremes[!duplicated(datMeta_twin_dfextremes$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_twin_dfextremes$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model
REM0.5_twin_dfextremes <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_twin_dfextremes, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_twin_dfextremes <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_twin_dfextremes, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_twin_dfextremes

# Extract grand e2
REM0.5_twin_dfextremes_e2 <- fisherz2r(coef(REM0.5_twin_dfextremes))
names(REM0.5_twin_dfextremes_e2) <- NULL
REM0.5_twin_dfextremes_e2

#Extract standard error
REM0.5_twin_dfextremes_se <- fisherz2r(REM0.5_twin_dfextremes$se)
REM0.5_twin_dfextremes_se

#Extract no of studies
REM0.5_twin_dfextremes_k <- REM0.5_twin_dfextremes$k.all





# ============= Compare estimates for designs =============

designs_e2 <- rbind(REM0.5_sibling_categorical_e2,
                    REM0.5_sibling_classical_e2,
                    REM0.5_twin_and_sibling_categorical_e2,
                    REM0.5_twin_and_sibling_dfextremes_e2,
                    REM0.5_twin_and_sibling_classical_e2,
                    REM0.5_twin_categorical_e2,
                    REM0.5_twin_dfextremes_e2,
                    REM0.5_twin_classical_e2)

designs_se <- rbind(REM0.5_sibling_categorical_se,
                    REM0.5_sibling_classical_se,
                    REM0.5_twin_and_sibling_categorical_se,
                    REM0.5_twin_and_sibling_dfextremes_se,
                    REM0.5_twin_and_sibling_classical_se,
                    REM0.5_twin_categorical_se,
                    REM0.5_twin_dfextremes_se,
                    REM0.5_twin_classical_se)

designs_k <- rbind(REM0.5_sibling_categorical_k,
                   REM0.5_sibling_classical_k,
                   REM0.5_twin_and_sibling_categorical_k,
                   REM0.5_twin_and_sibling_dfextremes_k,
                   REM0.5_twin_and_sibling_classical_k,
                   REM0.5_twin_categorical_k,
                   REM0.5_twin_dfextremes_k,
                   REM0.5_twin_classical_k)

designs <- cbind(designs_e2, designs_se, designs_k)
designs
colnames(designs) <- c("est","se","k")
designs <- data.frame(designs)
designs$sig <- with(designs, ifelse(is.na(designs$est), NA,
                                    ifelse(REM0.5_agecov_pval > 0.05, "nosig", "sig")))
designs$type <- "designs"
designs$sex <- "Combined"
designs

# ============= Model =============
# ============= Testing model: Aonly  =============
# Select Aonly and aggregate within Aonly
datMeta_Aonly <- datMeta[datMeta$Model == "Aonly",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_Aonly <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_Aonly)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_Aonly[!duplicated(datMeta_Aonly$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_Aonly$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model  
REM0.5_Aonly <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_Aonly, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_Aonly <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_Aonly, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_Aonly

# Extract grand e2
REM0.5_Aonly_e2 <- fisherz2r(coef(REM0.5_Aonly))
names(REM0.5_Aonly_e2) <- NULL
REM0.5_Aonly_e2

#Extract standard error
REM0.5_Aonly_se <- fisherz2r(REM0.5_Aonly$se)
REM0.5_Aonly_se

#Extract no of studies
REM0.5_Aonly_k <- REM0.5_Aonly$k.all

# ============= Testing model: BEST =============
# Select BEST and aggregate within BEST
datMeta_BEST <- datMeta[datMeta$Model == "BEST",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_BEST <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_BEST)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_BEST[!duplicated(datMeta_BEST$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_BEST$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model  
REM0.5_BEST <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_BEST, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_BEST <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_BEST, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_BEST

# Extract grand e2
REM0.5_BEST_e2 <- fisherz2r(coef(REM0.5_BEST))
names(REM0.5_BEST_e2) <- NULL
REM0.5_BEST_e2

#Extract standard error
REM0.5_BEST_se <- fisherz2r(REM0.5_BEST$se)
REM0.5_BEST_se

#Extract no of studies
REM0.5_BEST_k <- REM0.5_BEST$k.all

# ============= Testing model: DFAonly  =============
# Select DFAonly and aggregate within DFAonly
datMeta_DFAonly <- datMeta[datMeta$Model == "DFAonly",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_DFAonly <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_DFAonly)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_DFAonly[!duplicated(datMeta_DFAonly$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_DFAonly$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model 
REM0.5_DFAonly <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_DFAonly, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_DFAonly <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_DFAonly, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_DFAonly

# Extract grand e2
REM0.5_DFAonly_e2 <- fisherz2r(coef(REM0.5_DFAonly))
names(REM0.5_DFAonly_e2) <- NULL
REM0.5_DFAonly_e2

#Extract standard error
REM0.5_DFAonly_se <- fisherz2r(REM0.5_DFAonly$se)
REM0.5_DFAonly_se

#Extract no of studies
REM0.5_DFAonly_k <- REM0.5_DFAonly$k.all

# ============= Testing model: DFBEST  =============
# Select DFBEST and aggregate within DFBEST
datMeta_DFBEST <- datMeta[datMeta$Model == "DFBEST",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_DFBEST <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_DFBEST)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_DFBEST[!duplicated(datMeta_DFBEST$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_DFBEST$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model     
REM0.5_DFBEST <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_DFBEST, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_DFBEST <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_DFBEST, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_DFBEST

# Extract grand e2
REM0.5_DFBEST_e2 <- fisherz2r(coef(REM0.5_DFBEST))
names(REM0.5_DFBEST_e2) <- NULL
REM0.5_DFBEST_e2

#Extract standard error
REM0.5_DFBEST_se <- fisherz2r(REM0.5_DFBEST$se)
REM0.5_DFBEST_se

#Extract no of studies
REM0.5_DFBEST_k <- REM0.5_DFBEST$k.all

# ============= Testing model: DFFULL  =============
# Select DFFULL and aggregate within DFFULL
datMeta_DFFULL <- datMeta[datMeta$Model == "DFFULL",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_DFFULL <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_DFFULL)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_DFFULL[!duplicated(datMeta_DFFULL$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_DFFULL$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model 
REM0.5_DFFULL <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_DFFULL, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_DFFULL <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_DFFULL, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_DFFULL

# Extract grand e2
REM0.5_DFFULL_e2 <- fisherz2r(coef(REM0.5_DFFULL))
names(REM0.5_DFFULL_e2) <- NULL
REM0.5_DFFULL_e2

#Extract standard error
REM0.5_DFFULL_se <- fisherz2r(REM0.5_DFFULL$se)
REM0.5_DFFULL_se

#Extract no of studies
REM0.5_DFFULL_k <- REM0.5_DFFULL$k.all

# ============= Testing model: FULL =============
# Select FULL and aggregate within FULL
datMeta_FULL <- datMeta[datMeta$Model == "FULL",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_FULL <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_FULL)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_FULL[!duplicated(datMeta_FULL$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_FULL$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model         
REM0.5_FULL <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_FULL, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_FULL <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_FULL, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_FULL

# Extract grand e2
REM0.5_FULL_e2 <- fisherz2r(coef(REM0.5_FULL))
names(REM0.5_FULL_e2) <- NULL
REM0.5_FULL_e2

#Extract standard error
REM0.5_FULL_se <- fisherz2r(REM0.5_FULL$se)
REM0.5_FULL_se

#Extract no of studies
REM0.5_FULL_k <- REM0.5_FULL$k.all

# ============= Testing model: TC  =============
# Select TC and aggregate within TC
datMeta_TC <- datMeta[datMeta$Model == "TC",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_TC <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_TC)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_TC[!duplicated(datMeta_TC$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_TC$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model         
REM0.5_TC <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_TC, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_TC <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_TC, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_TC

# Extract grand e2
REM0.5_TC_e2 <- fisherz2r(coef(REM0.5_TC))
names(REM0.5_TC_e2) <- NULL
REM0.5_TC_e2

#Extract standard error
REM0.5_TC_se <- fisherz2r(REM0.5_TC$se)
REM0.5_TC_se

#Extract no of studies
REM0.5_TC_k <- REM0.5_TC$k.all

# ============= Compare estimates for models =============

models_e2 <- rbind(REM0.5_Aonly_e2,
                   REM0.5_BEST_e2,
                   REM0.5_FULL_e2,
                   REM0.5_DFAonly_e2,
                   REM0.5_DFBEST_e2,
                   REM0.5_DFFULL_e2,
                   REM0.5_TC_e2)

models_se <- rbind(REM0.5_Aonly_se,
                   REM0.5_BEST_se,
                   REM0.5_FULL_se,
                   REM0.5_DFAonly_se,
                   REM0.5_DFBEST_se,
                   REM0.5_DFFULL_se,
                   REM0.5_TC_se)

models_k <- rbind(REM0.5_Aonly_k,
                  REM0.5_BEST_k,
                  REM0.5_FULL_k,
                  REM0.5_DFAonly_k,
                  REM0.5_DFBEST_k,
                  REM0.5_DFFULL_k,
                  REM0.5_TC_k)


models <- cbind(models_e2, models_se, models_k)
models
colnames(models) <- c("est","se","k")
models <- data.frame(models)
models$sig <- with(models, ifelse(is.na(models$est), NA,
                                  ifelse(REM0.5_agecov_pval > 0.05, "nosig", "sig")))
models$type <- "models"
models$sex <- "Combined"
models

# ============= Rater  =============
# ============= Testing rater: Diagnosis  =============
# Select Diagnosis and aggregate within Diagnosis
datMeta_diagnosis <- datMeta[datMeta$Rater == "Diagnosis",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_diagnosis <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_diagnosis)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_diagnosis[!duplicated(datMeta_diagnosis$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_diagnosis$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model      
REM0.5_diagnosis <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_diagnosis, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_diagnosis <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_diagnosis, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_diagnosis

# Extract grand e2
REM0.5_diagnosis_e2 <- fisherz2r(coef(REM0.5_diagnosis))
names(REM0.5_diagnosis_e2) <- NULL
REM0.5_diagnosis_e2

#Extract standard error
REM0.5_diagnosis_se <- fisherz2r(REM0.5_diagnosis$se)
REM0.5_diagnosis_se

#Extract no of studies
REM0.5_diagnosis_k <- REM0.5_diagnosis$k.all

# ============= Testing rater: Parent =============
# Select Parent and aggregate within Parent
datMeta_parent <- datMeta[datMeta$Rater == "Parent",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_parent <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_parent)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_parent[!duplicated(datMeta_parent$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_parent$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model  
REM0.5_parent <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_parent, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_parent <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_parent, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_parent

# Extract grand e2
REM0.5_parent_e2 <- fisherz2r(coef(REM0.5_parent))
names(REM0.5_parent_e2) <- NULL
REM0.5_parent_e2

#Extract standard error
REM0.5_parent_se <- fisherz2r(REM0.5_parent$se)
REM0.5_parent_se

#Extract no of studies
REM0.5_parent_k <- REM0.5_parent$k.all

# ============= Testing rater: Parent, Self  =============
# Select Parent, Self and aggregate within Parent, Self
datMeta_parent_self <- datMeta[datMeta$Rater == "Parent, Self",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_parent_self <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_parent_self)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_parent_self[!duplicated(datMeta_parent_self$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_parent_self$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model
REM0.5_parent_self <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_parent_self, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_parent_self <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_parent_self, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_parent_self

# Extract grand e2
REM0.5_parent_self_e2 <- fisherz2r(coef(REM0.5_parent_self))
names(REM0.5_parent_self_e2) <- NULL
REM0.5_parent_self_e2

#Extract standard error
REM0.5_parent_self_se <- fisherz2r(REM0.5_parent_self$se)
REM0.5_parent_self_se

#Extract no of studies
REM0.5_parent_self_k <- REM0.5_parent_self$k.all

# ============= Testing rater: Parent, Teacher  =============
# Select Parent, Teacher and aggregate within Parent, Teacher
datMeta_parent_teacher <- datMeta[datMeta$Rater == "Parent, Teacher",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_parent_teacher <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_parent_teacher)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_parent_teacher[!duplicated(datMeta_parent_teacher$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_parent_teacher$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model  
REM0.5_parent_teacher <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_parent_teacher, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_parent_teacher <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_parent_teacher, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_parent_teacher

# Extract grand e2
REM0.5_parent_teacher_e2 <- fisherz2r(coef(REM0.5_parent_teacher))
names(REM0.5_parent_teacher_e2) <- NULL
REM0.5_parent_teacher_e2

#Extract standard error
REM0.5_parent_teacher_se <- fisherz2r(REM0.5_parent_teacher$se)
REM0.5_parent_teacher_se

#Extract no of studies
REM0.5_parent_teacher_k <- REM0.5_parent_teacher$k.all

# ============= Testing rater: Researcher  =============
# Select Researcher and aggregate within Researcher
datMeta_researcher <- datMeta[datMeta$Rater == "Researcher",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_researcher <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_researcher)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_researcher[!duplicated(datMeta_researcher$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_researcher$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model 
REM0.5_researcher <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_researcher, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_researcher <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_researcher, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_researcher

# Extract grand e2
REM0.5_researcher_e2 <- fisherz2r(coef(REM0.5_researcher))
names(REM0.5_researcher_e2) <- NULL
REM0.5_researcher_e2

#Extract standard error
REM0.5_researcher_se <- fisherz2r(REM0.5_researcher$se)
REM0.5_researcher_se

#Extract no of studies
REM0.5_researcher_k <- REM0.5_researcher$k.all

# ============= Testing rater: Self  =============
# Select Self and aggregate within Self
datMeta_self <- datMeta[datMeta$Rater == "Self",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_self <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_self)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_self[!duplicated(datMeta_self$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_self$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model  
REM0.5_self <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_self, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_self <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_self, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_self

# Extract grand e2
REM0.5_self_e2 <- fisherz2r(coef(REM0.5_self))
names(REM0.5_self_e2) <- NULL
REM0.5_self_e2

#Extract standard error
REM0.5_self_se <- fisherz2r(REM0.5_self$se)
REM0.5_self_se

#Extract no of studies
REM0.5_self_k <- REM0.5_self$k.all

# ============= Testing rater: Teacher  =============
# Select Teacher and aggregate within Teacher
datMeta_teacher <- datMeta[datMeta$Rater == "Teacher",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_teacher <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_teacher)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_teacher[!duplicated(datMeta_teacher$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_teacher$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model 
REM0.5_teacher <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_teacher, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_teacher <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_teacher, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_teacher

# Extract grand e2
REM0.5_teacher_e2 <- fisherz2r(coef(REM0.5_teacher))
names(REM0.5_teacher_e2) <- NULL
REM0.5_teacher_e2

#Extract standard error
REM0.5_teacher_se <- fisherz2r(REM0.5_teacher$se)
REM0.5_teacher_se

#Extract no of studies
REM0.5_teacher_k <- REM0.5_teacher$k.all

# ============= Testing rater: Test    =============
# Select Test and aggregate within Test
datMeta_test <- datMeta[datMeta$Rater == "Test",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_test <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_test)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_test[!duplicated(datMeta_test$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_test$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model
REM0.5_test <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_test, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_test <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_test, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_test

# Extract grand e2
REM0.5_test_e2 <- fisherz2r(coef(REM0.5_test))
names(REM0.5_test_e2) <- NULL
REM0.5_test_e2

#Extract standard error
REM0.5_test_se <- fisherz2r(REM0.5_test$se)
REM0.5_test_se

#Extract no of studies
REM0.5_test_k <- REM0.5_test$k.all

# ============= Compare estimates for raters =============

raters_e2 <- rbind(REM0.5_diagnosis_e2,
                   REM0.5_parent_e2,
                   REM0.5_parent_self_e2,
                   REM0.5_parent_teacher_e2,
                   REM0.5_researcher_e2,
                   REM0.5_self_e2,
                   REM0.5_teacher_e2,
                   REM0.5_test_e2)

raters_se <- rbind(REM0.5_diagnosis_se,
                   REM0.5_parent_se,
                   REM0.5_parent_self_se,
                   REM0.5_parent_teacher_se,
                   REM0.5_researcher_se,
                   REM0.5_self_se,
                   REM0.5_teacher_se,
                   REM0.5_test_se)

raters_k <- rbind(REM0.5_diagnosis_k,
                  REM0.5_parent_k,
                  REM0.5_parent_self_k,
                  REM0.5_parent_teacher_k,
                  REM0.5_researcher_k,
                  REM0.5_self_k,
                  REM0.5_teacher_k,
                  REM0.5_test_k)

raters <- cbind(raters_e2, raters_se, raters_k)
colnames(raters) <- c("est","se","k")
raters <- data.frame(raters)
raters$sig <- with(raters, ifelse(is.na(raters$est), NA,
                                  ifelse(REM0.5_agecov_pval > 0.05, "nosig", "sig")))
raters$type <- "raters"
raters$sex <- "Combined"
raters

# ============= Scale   =============
# ============= Testing scale: Categorical  =============
# Select Categorical and aggregate within Categorical
datMeta_categorical <- datMeta[datMeta$Categorical.Continuous == "Categorical",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_categorical <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_categorical)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_categorical[!duplicated(datMeta_categorical$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_categorical$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model 
REM0.5_categorical <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_categorical, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_categorical <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_categorical, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_categorical

# Extract grand e2
REM0.5_categorical_e2 <- fisherz2r(coef(REM0.5_categorical))
names(REM0.5_categorical_e2) <- NULL
REM0.5_categorical_e2

#Extract standard error
REM0.5_categorical_se <- fisherz2r(REM0.5_categorical$se)
REM0.5_categorical_se

#Extract no of studies
REM0.5_categorical_k <- REM0.5_categorical$k.all

# ============= Testing scale: Continuous =============
# Select Continuous and aggregate within Continuous
datMeta_continuous <- datMeta[datMeta$Categorical.Continuous == "Continuous",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_continuous <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_continuous)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_continuous[!duplicated(datMeta_continuous$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_continuous$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model 
REM0.5_continuous <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_continuous, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_continuous <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_continuous, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_continuous

# Extract grand e2
REM0.5_continuous_e2 <- fisherz2r(coef(REM0.5_continuous))
names(REM0.5_continuous_e2) <- NULL
REM0.5_continuous_e2

#Extract standard error
REM0.5_continuous_se <- fisherz2r(REM0.5_continuous$se)
REM0.5_continuous_se

#Extract no of studies
REM0.5_continuous_k <- REM0.5_continuous$k.all


# ============= Compare estimates for scales =============

scales_e2 <- rbind(REM0.5_categorical_e2,
                   REM0.5_continuous_e2)

scales_se <- rbind(REM0.5_categorical_se,
                   REM0.5_continuous_se)

scales_k <- rbind(REM0.5_categorical_k,
                  REM0.5_continuous_k)

scales <- cbind(scales_e2, scales_se, scales_k)
scales
colnames(scales) <- c("est","se","k")
scales <- data.frame(scales)
scales$sig <- with(scales, ifelse(is.na(scales$est), NA,
                                  ifelse(REM0.5_agecov_pval > 0.05, "nosig", "sig")))
scales$type <- "scales"
scales$sex <- "Combined"
scales

# ============= % White  =============
# ============= Testing ethnicity: Less than 50%  =============

# Select Less than 50% and aggregate within Less than 50%
datMeta_50 <- datMeta[datMeta$White == "Less than 50%",]
datMeta_50 <- datMeta_50[!is.na(datMeta_50$White),]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_50 <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_50)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_50[!duplicated(datMeta_50$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_50$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model         
REM0.5_50 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_50, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_50 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_50, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_50

# Extract grand e2
REM0.5_50_e2 <- fisherz2r(coef(REM0.5_50))
names(REM0.5_50_e2) <- NULL
REM0.5_50_e2

#Extract standard error
REM0.5_50_se <- fisherz2r(REM0.5_50$se)
REM0.5_50_se

#Extract no of studies
REM0.5_50_k <- REM0.5_50$k.all

# ============= Testing ethnicity: More than or equal to 50% but less than 75%  =============
# Select More than or equal to 50% but less than 75% and aggregate within More than or equal to 50% but less than 75%
datMeta_50_75 <- datMeta[datMeta$White == "More than or equal to 50% but less than 75%",]
datMeta_50_75 <- datMeta_50_75[!is.na(datMeta_50_75$White),]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_50_75 <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_50_75)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_50_75[!duplicated(datMeta_50_75$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_50_75$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model  
REM0.5_50_75 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_50_75, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_50_75 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_50_75, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_50_75

# Extract grand e2
REM0.5_50_75_e2 <- fisherz2r(coef(REM0.5_50_75))
names(REM0.5_50_75_e2) <- NULL
REM0.5_50_75_e2

#Extract standard error
REM0.5_50_75_se <- fisherz2r(REM0.5_50_75$se)
REM0.5_50_75_se

#Extract no of studies
REM0.5_50_75_k <- REM0.5_50_75$k.all

# ============= Testing ethnicity: More than or equal to 75% but less than 100%  =============
# Select More than or equal to 75% but less than 100% and aggregate within More than or equal to 75% but less than 100%
datMeta_75_100 <- datMeta[datMeta$White == "More than or equal to 75% but less than 100%",]
datMeta_75_100 <- datMeta_75_100[!is.na(datMeta_75_100$White),]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_75_100 <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_75_100)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_75_100[!duplicated(datMeta_75_100$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_75_100$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model     
REM0.5_75_100 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_75_100, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_75_100 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_75_100, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_75_100

# Extract grand e2
REM0.5_75_100_e2 <- fisherz2r(coef(REM0.5_75_100))
names(REM0.5_75_100_e2) <- NULL
REM0.5_75_100_e2

#Extract standard error
REM0.5_75_100_se <- fisherz2r(REM0.5_75_100$se)
REM0.5_75_100_se

#Extract no of studies
REM0.5_75_100_k <- REM0.5_75_100$k.all

# ============= Testing ethnicity: 100%  =============
# Select 100% and aggregate within 100%
datMeta_100 <- datMeta[datMeta$White == "100%",]
datMeta_100 <- datMeta_100[!is.na(datMeta_100$White),]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_100 <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_100)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_100[!duplicated(datMeta_100$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_100$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model        
REM0.5_100 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_100, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_100 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_100, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_100

# Extract grand e2
REM0.5_100_e2 <- fisherz2r(coef(REM0.5_100))
names(REM0.5_100_e2) <- NULL
REM0.5_100_e2

#Extract standard error
REM0.5_100_se <- fisherz2r(REM0.5_100$se)
REM0.5_100_se

#Extract no of studies
REM0.5_100_k <- REM0.5_100$k.all


# ============= Compare estimates for ethnicities =============

ethnicities_e2 <- rbind(REM0.5_50_e2,
                        REM0.5_50_75_e2,
                        REM0.5_75_100_e2,
                        REM0.5_100_e2)

ethnicities_se <- rbind(REM0.5_50_se,
                        REM0.5_50_75_se,
                        REM0.5_75_100_se,
                        REM0.5_100_se)

ethnicities_k <- rbind(REM0.5_50_k,
                       REM0.5_50_75_k,
                       REM0.5_75_100_k,
                       REM0.5_100_k)


ethnicities <- cbind(ethnicities_e2, ethnicities_se, ethnicities_k)
ethnicities
colnames(ethnicities) <- c("est","se","k")
ethnicities <- data.frame(ethnicities)
ethnicities$sig <- with(ethnicities, ifelse(is.na(ethnicities$est), NA,
                                            ifelse(REM0.5_agecov_pval > 0.05, "nosig", "sig")))
ethnicities$type <- "ethnicities"
ethnicities$sex <- "Combined"
ethnicities

# ============= NoCov  =============
# ============= Testing nocov: 0  =============

# Select 0 and aggregate within 0
datMeta_0 <- datMeta[datMeta$NoCov == 0,]
datMeta_0 <- datMeta_0[!is.na(datMeta_0$NoCov),]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_0 <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_0)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_0[!duplicated(datMeta_0$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_0$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model    
REM0.5_0 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_0, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_0 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_0, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_0

# Extract grand e2
REM0.5_0_e2 <- fisherz2r(coef(REM0.5_0))
names(REM0.5_0_e2) <- NULL
REM0.5_0_e2

#Extract standard error
REM0.5_0_se <- fisherz2r(REM0.5_0$se)
REM0.5_0_se

#Extract no of studies
REM0.5_0_k <- REM0.5_0$k.all

# ============= Testing nocov: 1 =============

# Select 1 and aggregate within 1
datMeta_1 <- datMeta[datMeta$NoCov == 1,]
datMeta_1 <- datMeta_1[!is.na(datMeta_1$NoCov),]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_1 <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_1)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_1[!duplicated(datMeta_1$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_1$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model  
REM0.5_1 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_1, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_1 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_1, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_1

# Extract grand e2
REM0.5_1_e2 <- fisherz2r(coef(REM0.5_1))
names(REM0.5_1_e2) <- NULL
REM0.5_1_e2

#Extract standard error
REM0.5_1_se <- fisherz2r(REM0.5_1$se)
REM0.5_1_se

#Extract no of studies
REM0.5_1_k <- REM0.5_1$k.all

# ============= Testing nocov: 2  =============

# Select 2 and aggregate within 2
datMeta_2 <- datMeta[datMeta$NoCov == 2,]
datMeta_2 <- datMeta_2[!is.na(datMeta_2$NoCov),]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_2 <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_2)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_2[!duplicated(datMeta_2$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_2$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model        
REM0.5_2 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_2, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_2 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_2, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_2

# Extract grand e2
REM0.5_2_e2 <- fisherz2r(coef(REM0.5_2))
names(REM0.5_2_e2) <- NULL
REM0.5_2_e2

#Extract standard error
REM0.5_2_se <- fisherz2r(REM0.5_2$se)
REM0.5_2_se

#Extract no of studies
REM0.5_2_k <- REM0.5_2$k.all

# ============= Testing nocov: 3  =============

# Select 3 and aggregate within 3
datMeta_3 <- datMeta[datMeta$NoCov == 3,]
datMeta_3 <- datMeta_3[!is.na(datMeta_3$NoCov),]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_3 <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_3)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_3[!duplicated(datMeta_3$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_3$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model     
REM0.5_3 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_3, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_3 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_3, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_3

# Extract grand e2
REM0.5_3_e2 <- fisherz2r(coef(REM0.5_3))
names(REM0.5_3_e2) <- NULL
REM0.5_3_e2

#Extract standard error
REM0.5_3_se <- fisherz2r(REM0.5_3$se)
REM0.5_3_se

#Extract no of studies
REM0.5_3_k <- REM0.5_3$k.all

# ============= Testing nocov: 4   =============

# Select 4 and aggregate within 4
datMeta_4 <- datMeta[datMeta$NoCov == 4,]
datMeta_4 <- datMeta_4[!is.na(datMeta_4$NoCov),]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_4 <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_4)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_4[!duplicated(datMeta_4$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_4$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model     
REM0.5_4 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_4, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_4 <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_4, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_4

# Extract grand e2
REM0.5_4_e2 <- fisherz2r(coef(REM0.5_4))
names(REM0.5_4_e2) <- NULL
REM0.5_4_e2

#Extract standard error
REM0.5_4_se <- fisherz2r(REM0.5_4$se)
REM0.5_4_se

#Extract no of studies
REM0.5_4_k <- REM0.5_4$k.all


# ============= Compare estimates for covs =============
nocovs_e2 <- rbind(REM0.5_0_e2,
                   REM0.5_1_e2,
                   REM0.5_2_e2,
                   REM0.5_3_e2,
                   REM0.5_4_e2)

nocovs_se <- rbind(REM0.5_0_se,
                   REM0.5_1_se,
                   REM0.5_2_se,
                   REM0.5_3_se,
                   REM0.5_4_se)

nocovs_k <- rbind(REM0.5_0_k,
                  REM0.5_1_k,
                  REM0.5_2_k,
                  REM0.5_3_k,
                  REM0.5_4_k)


nocovs <- cbind(nocovs_e2, nocovs_se, nocovs_k)
nocovs
colnames(nocovs) <- c("est","se","k")
nocovs <- data.frame(nocovs)
nocovs$sig <- with(nocovs, ifelse(is.na(nocovs$est), NA,
                                  ifelse(REM0.5_agecov_pval > 0.05, "nosig", "sig")))
nocovs$type <- "nocovs"
nocovs$sex <- "Combined"
nocovs

# ============= Country =============
# ============= Testing country: Australia   =============
# Select Australia and aggregate within Australia
datMeta_Australia <- datMeta[datMeta$Cohort_country1 == "Australia",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_Australia <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_Australia)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_Australia[!duplicated(datMeta_Australia$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_Australia$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model  
REM0.5_Australia <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_Australia, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_Australia <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_Australia, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_Australia

# Extract grand e2
REM0.5_Australia_e2 <- fisherz2r(coef(REM0.5_Australia))
names(REM0.5_Australia_e2) <- NULL
REM0.5_Australia_e2

#Extract standard error
REM0.5_Australia_se <- fisherz2r(REM0.5_Australia$se)
REM0.5_Australia_se

#Extract no of studies
REM0.5_Australia_k <- REM0.5_Australia$k.all

# ============= Testing country: Australia, USA, Norway, Sweden  =============
# Select Australia, USA, Norway, Sweden and aggregate within Australia, USA, Norway, Sweden
datMeta_Aus_USA_Nor_Swe <- datMeta[datMeta$Cohort_country1 == "Australia, USA, Norway, Sweden",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_Aus_USA_Nor_Swe <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_Aus_USA_Nor_Swe)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_Aus_USA_Nor_Swe[!duplicated(datMeta_Aus_USA_Nor_Swe$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_Aus_USA_Nor_Swe$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model      
REM0.5_Aus_USA_Nor_Swe <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_Aus_USA_Nor_Swe, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_Aus_USA_Nor_Swe <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_Aus_USA_Nor_Swe, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_Aus_USA_Nor_Swe

# Extract grand e2
REM0.5_Aus_USA_Nor_Swe_e2 <- fisherz2r(coef(REM0.5_Aus_USA_Nor_Swe))
names(REM0.5_Aus_USA_Nor_Swe_e2) <- NULL
REM0.5_Aus_USA_Nor_Swe_e2

#Extract standard error
REM0.5_Aus_USA_Nor_Swe_se <- fisherz2r(REM0.5_Aus_USA_Nor_Swe$se)
REM0.5_Aus_USA_Nor_Swe_se

#Extract no of studies
REM0.5_Aus_USA_Nor_Swe_k <- REM0.5_Aus_USA_Nor_Swe$k.all

# ============= Testing country: Canada  =============
# Select Canada and aggregate within Canada
datMeta_Canada <- datMeta[datMeta$Cohort_country1 == "Canada",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_Canada <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_Canada)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_Canada[!duplicated(datMeta_Canada$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_Canada$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model         
REM0.5_Canada <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_Canada, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_Canada <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_Canada, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_Canada

# Extract grand e2
REM0.5_Canada_e2 <- fisherz2r(coef(REM0.5_Canada))
names(REM0.5_Canada_e2) <- NULL
REM0.5_Canada_e2

#Extract standard error
REM0.5_Canada_se <- fisherz2r(REM0.5_Canada$se)
REM0.5_Canada_se

#Extract no of studies
REM0.5_Canada_k <- REM0.5_Canada$k.all

# ============= Testing country: China  =============
# Select China and aggregate within China
datMeta_China <- datMeta[datMeta$Cohort_country1 == "China",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_China <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_China)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_China[!duplicated(datMeta_China$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_China$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model   
REM0.5_China <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_China, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_China <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_China, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_China

# Extract grand e2
REM0.5_China_e2 <- fisherz2r(coef(REM0.5_China))
names(REM0.5_China_e2) <- NULL
REM0.5_China_e2

#Extract standard error
REM0.5_China_se <- fisherz2r(REM0.5_China$se)
REM0.5_China_se

#Extract no of studies
REM0.5_China_k <- REM0.5_China$k.all

# ============= Testing country: Netherlands  =============
# Select Netherlands and aggregate within Netherlands
datMeta_Netherlands <- datMeta[datMeta$Cohort_country1 == "Netherlands",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_Netherlands <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_Netherlands)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_Netherlands[!duplicated(datMeta_Netherlands$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_Netherlands$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model       
REM0.5_Netherlands <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_Netherlands, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_Netherlands <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_Netherlands, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_Netherlands

# Extract grand e2
REM0.5_Netherlands_e2 <- fisherz2r(coef(REM0.5_Netherlands))
names(REM0.5_Netherlands_e2) <- NULL
REM0.5_Netherlands_e2

#Extract standard error
REM0.5_Netherlands_se <- fisherz2r(REM0.5_Netherlands$se)
REM0.5_Netherlands_se

#Extract no of studies
REM0.5_Netherlands_k <- REM0.5_Netherlands$k.all

# ============= Testing country: Norway  =============
# Select Norway and aggregate within Norway
datMeta_Norway <- datMeta[datMeta$Cohort_country1 == "Norway",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_Norway <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_Norway)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_Norway[!duplicated(datMeta_Norway$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_Norway$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model   
REM0.5_Norway <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_Norway, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_Norway <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_Norway, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_Norway

# Extract grand e2
REM0.5_Norway_e2 <- fisherz2r(coef(REM0.5_Norway))
names(REM0.5_Norway_e2) <- NULL
REM0.5_Norway_e2

#Extract standard error
REM0.5_Norway_se <- fisherz2r(REM0.5_Norway$se)
REM0.5_Norway_se

#Extract no of studies
REM0.5_Norway_k <- REM0.5_Norway$k.all

# ============= Testing country: Sweden =============
# Select Sweden and aggregate within Sweden
datMeta_Sweden <- datMeta[datMeta$Cohort_country1 == "Sweden",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_Sweden <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_Sweden)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_Sweden[!duplicated(datMeta_Sweden$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_Sweden$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model     
REM0.5_Sweden <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_Sweden, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_Sweden <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_Sweden, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_Sweden

# Extract grand e2
REM0.5_Sweden_e2 <- fisherz2r(coef(REM0.5_Sweden))
names(REM0.5_Sweden_e2) <- NULL
REM0.5_Sweden_e2

#Extract standard error
REM0.5_Sweden_se <- fisherz2r(REM0.5_Sweden$se)
REM0.5_Sweden_se

#Extract no of studies
REM0.5_Sweden_k <- REM0.5_Sweden$k.all

# ============= Testing country: UK  =============
# Select UK and aggregate within UK
datMeta_UK <- datMeta[datMeta$Cohort_country1 == "UK",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_UK <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_UK)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_UK[!duplicated(datMeta_UK$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_UK$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model     
REM0.5_UK <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_UK, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_UK <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_UK, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_UK

# Extract grand e2
REM0.5_UK_e2 <- fisherz2r(coef(REM0.5_UK))
names(REM0.5_UK_e2) <- NULL
REM0.5_UK_e2

#Extract standard error
REM0.5_UK_se <- fisherz2r(REM0.5_UK$se)
REM0.5_UK_se

#Extract no of studies
REM0.5_UK_k <- REM0.5_UK$k.all

# ============= Testing country: USA  =============

# Select USA and aggregate within USA
datMeta_USA <- datMeta[datMeta$Cohort_country1 == "USA",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_USA <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_USA)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_USA[!duplicated(datMeta_USA$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_USA$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model       
REM0.5_USA <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_USA, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_USA <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_USA, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_USA

# Extract grand e2
REM0.5_USA_e2 <- fisherz2r(coef(REM0.5_USA))
names(REM0.5_USA_e2) <- NULL
REM0.5_USA_e2

#Extract standard error
REM0.5_USA_se <- fisherz2r(REM0.5_USA$se)
REM0.5_USA_se

#Extract no of studies
REM0.5_USA_k <- REM0.5_USA$k.all

# ============= Compare estimates for countries =============

countries_e2 <- rbind(REM0.5_Australia_e2,
                      REM0.5_Aus_USA_Nor_Swe_e2,
                      REM0.5_Canada_e2,
                      REM0.5_China_e2,
                      REM0.5_Netherlands_e2,
                      REM0.5_Norway_e2,
                      REM0.5_Sweden_e2,
                      REM0.5_UK_e2,
                      REM0.5_USA_e2)

countries_se <- rbind(REM0.5_Australia_se,
                      REM0.5_Aus_USA_Nor_Swe_se,
                      REM0.5_Canada_se,
                      REM0.5_China_se,
                      REM0.5_Netherlands_se,
                      REM0.5_Norway_se,
                      REM0.5_Sweden_se,
                      REM0.5_UK_se,
                      REM0.5_USA_se)

countries_k <- rbind(REM0.5_Australia_k,
                     REM0.5_Aus_USA_Nor_Swe_k,
                     REM0.5_Canada_k,
                     REM0.5_China_k,
                     REM0.5_Netherlands_k,
                     REM0.5_Norway_k,
                     REM0.5_Sweden_k,
                     REM0.5_UK_k,
                     REM0.5_USA_k)

countries <- cbind(countries_e2, countries_se, countries_k)
countries
colnames(countries) <- c("est","se","k")
countries <- data.frame(countries)
countries$sig <- with(countries, ifelse(is.na(countries$est), NA,
                                        ifelse(REM0.5_agecov_pval > 0.05, "nosig", "sig")))
countries$type <- "countries"
countries$sex <- "Combined"
countries

# ============= Disorder =============
# ============= Testing disorder: ADHD   =============
# Select ADHD and aggregate within ADHD
datMeta_ADHD <- datMeta[datMeta$Broad.Trait.Disorder == "ADHD",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_ADHD <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_ADHD)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_ADHD[!duplicated(datMeta_ADHD$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_ADHD$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model  
REM0.5_ADHD <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_ADHD, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_ADHD <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_ADHD, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_ADHD

# Extract grand e2
REM0.5_ADHD_e2 <- fisherz2r(coef(REM0.5_ADHD))
names(REM0.5_ADHD_e2) <- NULL
REM0.5_ADHD_e2

#Extract standard error
REM0.5_ADHD_se <- fisherz2r(REM0.5_ADHD$se)
REM0.5_ADHD_se

#Extract no of studies
REM0.5_ADHD_k <- REM0.5_ADHD$k.all

# ============= Testing disorder: ASD  =============
# Select ASD and aggregate within ASD
datMeta_ASD <- datMeta[datMeta$Broad.Trait.Disorder == "ASD",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_ASD <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_ASD)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_ASD[!duplicated(datMeta_ASD$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_ASD$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model         
REM0.5_ASD <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_ASD, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_ASD <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_ASD, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_ASD

# Extract grand e2
REM0.5_ASD_e2 <- fisherz2r(coef(REM0.5_ASD))
names(REM0.5_ASD_e2) <- NULL
REM0.5_ASD_e2

#Extract standard error
REM0.5_ASD_se <- fisherz2r(REM0.5_ASD$se)
REM0.5_ASD_se

#Extract no of studies
REM0.5_ASD_k <- REM0.5_ASD$k.all

# ============= Testing disorder: CMD  =============
# Select CMD and aggregate within CMD
datMeta_CMD <- datMeta[datMeta$Broad.Trait.Disorder == "Communication Disorder",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_CMD <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_CMD)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_CMD[!duplicated(datMeta_CMD$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_CMD$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model   
REM0.5_CMD <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_CMD, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_CMD <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_CMD, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_CMD

# Extract grand e2
REM0.5_CMD_e2 <- fisherz2r(coef(REM0.5_CMD))
names(REM0.5_CMD_e2) <- NULL
REM0.5_CMD_e2

#Extract standard error
REM0.5_CMD_se <- fisherz2r(REM0.5_CMD$se)
REM0.5_CMD_se

#Extract no of studies
REM0.5_CMD_k <- REM0.5_CMD$k.all

# ============= Testing disorder: SLD  =============
# Select SLD and aggregate within SLD
datMeta_SLD <- datMeta[datMeta$Broad.Trait.Disorder == "SLD",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_SLD <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_SLD)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_SLD[!duplicated(datMeta_SLD$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_SLD$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model       
REM0.5_SLD <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_SLD, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_SLD <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_SLD, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_SLD

# Extract grand e2
REM0.5_SLD_e2 <- fisherz2r(coef(REM0.5_SLD))
names(REM0.5_SLD_e2) <- NULL
REM0.5_SLD_e2

#Extract standard error
REM0.5_SLD_se <- fisherz2r(REM0.5_SLD$se)
REM0.5_SLD_se

#Extract no of studies
REM0.5_SLD_k <- REM0.5_SLD$k.all

# ============= Testing disorder: LD  =============
# Select LD and aggregate within LD
datMeta_LD <- datMeta[datMeta$Broad.Trait.Disorder == "LD",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_LD <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_LD)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_LD[!duplicated(datMeta_LD$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_LD$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model   
REM0.5_LD <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_LD, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_LD <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_LD, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_LD

# Extract grand e2
REM0.5_LD_e2 <- fisherz2r(coef(REM0.5_LD))
names(REM0.5_LD_e2) <- NULL
REM0.5_LD_e2

#Extract standard error
REM0.5_LD_se <- fisherz2r(REM0.5_LD$se)
REM0.5_LD_se

#Extract no of studies
REM0.5_LD_k <- REM0.5_LD$k.all

# ============= Testing disorder: MD =============
# Select MD and aggregate within MD
datMeta_MD <- datMeta[datMeta$Broad.Trait.Disorder == "Motor Disorder",]

# Aggregate non-independent effect sizes within publications (aggregate on 'Full.Ref' variable)
# Set correlation between effect sizes to be r=.05

datMeta_agg0.5_MD <- agg(id=Full.Ref, es=FZ_e2, var=e2_SE, cor=0.5, data=datMeta_MD)

# Label new aggregated effect sizes with correct cohort
# Use a separate cohort variable with cohorts for aggregation by study (if one study used >1 cohort, they would be lost if not combined into a single row)
agg_by_study_cohorts <- datMeta_MD[!duplicated(datMeta_MD$Full.Ref),]

# Now label cohorts
datMeta_agg0.5_MD$cohort <- agg_by_study_cohorts$Cohort_for_agg_by_study

# Run MREM model     
REM0.5_MD <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_MD, slab=paste(id, cohort, sep="  /  "), method="ML")
REM0.5_MD <- rma.mv(es, var, random = ~ 1 | cohort/id, data=datMeta_agg0.5_MD, slab=paste(id, cohort, sep="  /  "), method="REML")

REM0.5_MD

# Extract grand e2
REM0.5_MD_e2 <- fisherz2r(coef(REM0.5_MD))
names(REM0.5_MD_e2) <- NULL
REM0.5_MD_e2

#Extract standard error
REM0.5_MD_se <- fisherz2r(REM0.5_MD$se)
REM0.5_MD_se

#Extract no of studies
REM0.5_MD_k <- REM0.5_MD$k.all

# ============= Compare estimates for disorders =============

disorders_e2 <- rbind(REM0.5_SLD_e2,
                      REM0.5_ADHD_e2,
                      REM0.5_ASD_e2,
                      REM0.5_CMD_e2,
                      REM0.5_MD_e2,
                      REM0.5_LD_e2)

disorders_se <- rbind(REM0.5_SLD_se,
                      REM0.5_ADHD_se,
                      REM0.5_ASD_se,
                      REM0.5_CMD_se,
                      REM0.5_MD_se,
                      REM0.5_LD_se)

disorders_k <- rbind(REM0.5_SLD_k,
                     REM0.5_ADHD_k,
                     REM0.5_ASD_k,
                     REM0.5_CMD_k,
                     REM0.5_MD_k,
                     REM0.5_LD_k)

disorders <- cbind(disorders_e2, disorders_se, disorders_k)
disorders
colnames(disorders) <- c("est","se","k")
disorders <- data.frame(disorders)
disorders$sig <- NA
disorders$type <- "disorders"
disorders$sex <- "Combined"
disorders


# ============= Create a table with stratified estimates =============
stratified <- rbind(ages, designs, models, raters, scales, ethnicities, nocovs, countries, disorders)
stratified <- data.frame(stratified)
stratified$var <- rownames(stratified)
stratified











