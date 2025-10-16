#############################################################################################################
#############################################################################################################

## ---------------------------
##
##
## Exploring the Impact of Neighborhood Islamic Points of Interest on Islamophobia: 
## An Exploratory Study Using OpenStreetMap and Google Maps
## 
##
## Guiding Script Instruction on Data Pre-Processing and Analysis
##
##
## Authors: Dra. Daria Dementeva
##
## Email: daria.dementeva@kuleuven.be
##
##
##  2025
##
## ---------------------------

## Set Working Directory 

# setwd("...")  
# Since the purpose of the script is to guide and instruct, I use my working directory throughout the script.

# For Reproducibility

set.seed(05081997)

################################################################################################################ 
# Install packages
################################################################################################################ 

# Uncomment if necessary

# install.packages("BelgiumMaps.StatBel")
# install.packages("caret")
# install.packages("car")
# install.packages("corrplot")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("glmnet")
# install.packages("Hmisc")
# install.packages("jtools")
# install.packages("lme4")
# install.packages("lmtest")
# install.packages("missMethods")
# install.packages("openxlsx")
# install.packages("performance")
# install.packages("readr")
# install.packages("sandwich")
# install.packages("sjPlot")
# install.packages("sjmisc")
# install.packages("tidyverse")

################################################################################################################ 
# Load packages
################################################################################################################

library(BelgiumMaps.StatBel) # administrative boundaries for Belgium
library(caret) # ML pipeline and workflow, ML helper functions
library(car) # regression diagnostics
library(corrplot) # correlation matrix and data vis
library(dplyr) # data manipulation
library(ggplot2) # data vis
library(glmnet) # regularization methods: elastic net, lasso, regression
library(Hmisc) # miscellaneous for regression and model-based data vis
library(jtools) # regression output printouts and data vis
library(lmtest) # regression diagnostics
library(missMethods) # missing data imputation
library(openxlsx) # import and export in excel 
library(performance) # model assessment and diagnostics
library(readr) # import csv files
library(sandwich) # robust standard errors and diagnostics
library(sjPlot) # model plots
library(sjmisc) # data management 
library(tidyverse) # umbrella package for data manipulation 

################################################################################################################ 
# Read in Data
################################################################################################################ 

# Read in BNES

symbolic_threat_islamophobia <- read_csv("/Users/dariadementeva/symbolic_threat_islamophobia.csv")

symbolic_threat_islamophobia <- symbolic_threat_islamophobia[,-1]

# Read in Census Data
 
contextual_measures_census <- read_csv("/Users/dariadementeva/Desktop/contextual_measures_ss_for_buffers_final.csv")
contextual_measures_census <- contextual_measures_census[, -c(1, 26:238)] # remove osm 
contextual_measures_census_ss <- contextual_measures_census

# Aggregate for Municipalities 

data(BE_ADMIN_SECTORS)

stat_sector_meta_data <- BE_ADMIN_SECTORS@data

contextual_measures_census_municipality <- merge(contextual_measures_census_ss, stat_sector_meta_data,
                                                 by = c("CD_REFNIS_SECTOR", "TX_SECTOR_DESCR_NL"), 
                                                 all.x = TRUE)

contextual_measures_census_municipality_upd <- contextual_measures_census_municipality %>%
  group_by(CD_MUNTY_REFNIS,                                                                                 
           TX_MUNTY_DESCR_NL) %>%
  mutate_if(is.numeric, 
            sum, 
            na.rm = TRUE)

names(contextual_measures_census_municipality_upd)[3:24] <- paste0("mun_", names(contextual_measures_census_municipality_upd)[3:24])

# Read in Islamic POI data

islamic_food_points_ss  <- read_csv("/Users/dariadementeva/Islam_Food_Establishments_summarized_by_ss_2025_06.csv")
mosques_ss <- read_csv("/Users/dariadementeva/mosques_summarized_by_ss_2025_06.csv")
halal_stores_ss <- read_csv("/Users/dariadementeva/halal_stores_summarized_by_ss_2025_06.csv")
islam_edu_ss <- read_csv("/Users/dariadementeva/islam_edu_summarized_by_ss_2025_06.csv")

islamic_food_points_ss <- islamic_food_points_ss[, -1]
mosques_ss <- mosques_ss[, -1]
halal_stores_ss  <- halal_stores_ss[, -1]
islam_edu_ss <- islam_edu_ss[, -1]

# Count Statistical Sectors with Islamic POI in Belgium, Table 1

nrow(islamic_food_points_ss) # N SS = 690
nrow(mosques_ss) # N SS = 109
nrow(halal_stores_ss) # N SS = 311
nrow(islam_edu_ss) # N SS = 41

################################################################################################################ 
# Link Data
################################################################################################################

# Link with Islamic POI data

symbolic_threat_islamophobia_with_poi <- merge(symbolic_threat_islamophobia,
                                               islamic_food_points_ss, by = c("CD_REFNIS_SECTOR", "TX_SECTOR_DESCR_NL"), 
                                               all.x = T)
symbolic_threat_islamophobia_with_poi <- merge(symbolic_threat_islamophobia_with_poi,
                                               mosques_ss, by = c("CD_REFNIS_SECTOR", "TX_SECTOR_DESCR_NL"), 
                                               all.x = T)   

symbolic_threat_islamophobia_with_poi <- merge(symbolic_threat_islamophobia_with_poi,
                                               halal_stores_ss, by = c("CD_REFNIS_SECTOR", "TX_SECTOR_DESCR_NL"), 
                                               all.x = T)     

symbolic_threat_islamophobia_with_poi <- merge(symbolic_threat_islamophobia_with_poi,
                                               islam_edu_ss, by = c("CD_REFNIS_SECTOR", "TX_SECTOR_DESCR_NL"), 
                                               all.x = T)    

# Link with Census Data

symbolic_threat_islamophobia_with_poi_census <- merge(symbolic_threat_islamophobia_with_poi,
                                                      contextual_measures_census_ss, by = c("CD_REFNIS_SECTOR", "TX_SECTOR_DESCR_NL"), 
                                                      all.x = T)

symbolic_threat_islamophobia_with_poi_census <- merge(symbolic_threat_islamophobia_with_poi_census,
                                                      contextual_measures_census_municipality_upd , by = c("CD_REFNIS_SECTOR", "TX_SECTOR_DESCR_NL"), 
                                                      all.x = T)  

symbolic_threat_islamophobia_final <- symbolic_threat_islamophobia_with_poi_census[, -c(5,37,38,59,60,61:86,88:98)]

################################################################################################################ 
# Create Dummies for Islamic POI
################################################################################################################

# Impose 0 if cells are NA

symbolic_threat_islamophobia_final$n_food_establishments[is.na(symbolic_threat_islamophobia_final$n_food_establishments)] <- 0
symbolic_threat_islamophobia_final$mosque[is.na(symbolic_threat_islamophobia_final$mosque)] <- 0
symbolic_threat_islamophobia_final$halal_store[is.na(symbolic_threat_islamophobia_final$halal_store)] <- 0
symbolic_threat_islamophobia_final$islam_edu[is.na(symbolic_threat_islamophobia_final$islam_edu)] <- 0

symbolic_threat_islamophobia_final$dummy_food <- ifelse(symbolic_threat_islamophobia_final$n_food_establishments > 0, 1, 0)
symbolic_threat_islamophobia_final$dummy_mosque <- ifelse(symbolic_threat_islamophobia_final$mosque > 0, 1, 0)
symbolic_threat_islamophobia_final$dummy_halal <- ifelse(symbolic_threat_islamophobia_final$halal_store > 0, 1, 0)
symbolic_threat_islamophobia_final$dummy_edu <- ifelse(symbolic_threat_islamophobia_final$islam_edu > 0, 1, 0)

################################################################################################################ 
# Create Census-based Neighborhood Characteristics for SS: SES, Residential Ethnic Diversity, Pop. Density
################################################################################################################

colSums((is.na(symbolic_threat_islamophobia_final)))

symbolic_threat_islamophobia_final[, c("area_km2_11")] <- impute_mode( as.data.frame(symbolic_threat_islamophobia_final[, c("area_km2_11")]), type = "columnwise", convert_tibble = F)

colSums((is.na(symbolic_threat_islamophobia_final)))

symbolic_threat_islamophobia_final  <- symbolic_threat_islamophobia_final  %>% 
  group_by(CD_REFNIS_SECTOR) %>% 
  mutate(workers_over_20_years_old_ss = overall_total_labmarket_11 - overall_total_under_15yo_11 - working_overall_less_than_20_years_11 - jobseekers_overall_less_than_20_years_11, 
         prop_workers_over_20_years_old_high_status_jobs = ISCO_08_high_status/workers_over_20_years_old_ss)

# Proportion of residents over 25 years old with a BA 

symbolic_threat_islamophobia_final  <- symbolic_threat_islamophobia_final  %>% 
  group_by(CD_REFNIS_SECTOR) %>% 
  mutate(higher_edu_diploma_holders_ss = sum(total_total_higher_education_graduate_edu_11 + total_total_doctorate_edu_11, na.rm = T),
         prop_higher_edu_diploma_holders_over25 = higher_edu_diploma_holders_ss / over_25_years_ss)

symbolic_threat_islamophobia_final$prop_higher_edu_diploma_holders_over25[symbolic_threat_islamophobia_final$prop_higher_edu_diploma_holders_over25 > 1] <- 1 # force upper bound

symbolic_threat_islamophobia_final  <- symbolic_threat_islamophobia_final  %>% 
  group_by(CD_REFNIS_SECTOR) %>% 
  mutate(
    prop_outgroup_residential_diversity = nonbelgian_all_11/total_population_11,
    median_net_taxable_income = ms_median_net_taxable_inc_2019,
    prop_over_25yo_with_BA = prop_higher_edu_diploma_holders_over25,
    prop_over16yo_in_high_status_job = prop_workers_over_20_years_old_high_status_jobs,
    population_density = total_population_11/area_km2_11)

################################################################################################################ 
# Create Dummies for Individual-Level Features
################################################################################################################

symbolic_threat_islamophobia_final <- 
  symbolic_threat_islamophobia_final %>%
  mutate( 
    
    # gender
    gender_man = ifelse(q2==1,1,0),
    gender_woman = ifelse(q2==2,1,0),
    
    # subjective social class    
    
    subjective_sc_working_class = ifelse(q18==1,1,0),
    subjective_sc_low_middle_class = ifelse(q18==2,1,0),
    subjective_sc_higher_middle_class = ifelse(q18==3,1,0),
    subjective_sc_upper_class = ifelse(q18==4,1,0),
    subjective_sc_other_class = 
      ifelse(q18==7 | q18==9, 1, 0),
    
    # age
    
    age_18_24  = ifelse(age6 == 1, 1, 0),
    age_25_34  = ifelse(age6 == 2, 1, 0),
    age_35_44  = ifelse(age6 == 3, 1, 0),
    age_45_54  = ifelse(age6 == 4, 1, 0),
    age_55_64  = ifelse(age6 == 5, 1, 0),
    age_65_93  = ifelse(age6 == 6, 1, 0),
    
    # education
    
    edu_none_lower = ifelse(edu3 == 1, 1, 0),
    edu_higher_secondary = ifelse(edu3 == 2, 1, 0),
    edu_higher_university = ifelse(edu3 == 3, 1, 0),
    
    res_div =  ifelse(prop_outgroup_residential_diversity >= 0.25, 1, 0))


# View(symbolic_threat_islamophobia_final)

colSums(is.na(symbolic_threat_islamophobia_final))

################################################################################################################ 
# Correlations, Appendix
################################################################################################################

# Uncomment

# correlations <-  rcorr(as.matrix(symbolic_threat_islamophobia_final[, c("q49_symbolic",
#                                                                         "q87_cumulative", 
#                                                                         "q43_2", 
#                                                                         "dummy_food", 
#                                                                         "dummy_mosque", 
#                                                                         "dummy_halal", 
#                                                                         "dummy_edu", 
#                                                                         "prop_over16yo_in_high_status_job",
#                                                                         "prop_higher_edu_diploma_holders_over25",
#                                                                         "prop_outgroup_residential_diversity",
#                                                                         "median_net_taxable_income",                        
#                                                                         "population_density",
#                                                                         "gender_man",                                      
#                                                                         "gender_woman",
#                                                                         "subjective_sc_working_class",
#                                                                         "subjective_sc_low_middle_class",  
#                                                                         "subjective_sc_higher_middle_class", 
#                                                                         "subjective_sc_upper_class",
#                                                                         "subjective_sc_other_class",                      
#                                                                         "age_18_24", 
#                                                                         "age_25_34",
#                                                                         "age_35_44",
#                                                                         "age_45_54",
#                                                                         "age_55_64",
#                                                                         "age_65_93", 
#                                                                         "edu_none_lower",
#                                                                         "edu_higher_secondary",
#                                                                         "edu_higher_university")]))
# 
# 
# cor_mat <- correlations$r
# p_mat <- correlations$P
# 
# corrplot(cor_mat, 
#          method = "number", 
#          type = "lower", 
#          order = "hclust", 
#          p.mat = p_mat, 
#          sig.level = c(0.001, 0.01, 0.05),  # Significance levels
#          insig = "label_sig",       # Add significance stars
#          pch.cex = 0.5,             # Star size        #
#          number.cex = 0.7,
#          tl.cex = 0.4)  # Correlation number size
# 
# get_stars <- function(p) {
#   if (is.na(p)) return("")
#   else if (p < 0.001) return("***")
#   else if (p < 0.01)  return("**")
#   else if (p < 0.05)  return("*")
#   else return("")
# }
# 
# 
# formatted_matrix <- matrix(
#   paste0(sprintf("%.2f", cor_mat), mapply(get_stars, p_mat)),
#   nrow = nrow(cor_mat),
#   dimnames = dimnames(cor_mat)
# )
# 
# 
# df <- as.data.frame(formatted_matrix)
# df <- cbind(Variable = rownames(df), df)
# 
# write.xlsx(df, file = "correlation_full.xlsx", rowNames = FALSE)
# 
# 
# summary(symbolic_threat_islamophobia_final[, c("q49_symbolic",
#                                                "q87_cumulative", 
#                                                "q43_2", 
#                                                "dummy_food", 
#                                                "dummy_mosque", 
#                                                "dummy_halal", 
#                                                "dummy_edu", 
#                                                "prop_over16yo_in_high_status_job",
#                                                "prop_higher_edu_diploma_holders_over25",
#                                                "prop_outgroup_residential_diversity",
#                                                "median_net_taxable_income",                        
#                                                "population_density")])
# 
# round(range(symbolic_threat_islamophobia_final$prop_over16yo_in_high_status_job),2)
# round(range(symbolic_threat_islamophobia_final$prop_higher_edu_diploma_holders_over25),2)
# round(range(symbolic_threat_islamophobia_final$prop_outgroup_residential_diversity),2)
# round(range(symbolic_threat_islamophobia_final$median_net_taxable_income),2)
# round(range(symbolic_threat_islamophobia_final$population_density),2)
# 
# 
# round(sd(symbolic_threat_islamophobia_final$q49_symbolic), 2)
# round(sd(symbolic_threat_islamophobia_final$q87_cumulative), 2)
# round(sd(symbolic_threat_islamophobia_final$q43_2), 2)
# round(sd(symbolic_threat_islamophobia_final$prop_over16yo_in_high_status_job), 2)
# round(sd(symbolic_threat_islamophobia_final$prop_higher_edu_diploma_holders_over25), 2)
# round(sd(symbolic_threat_islamophobia_final$prop_outgroup_residential_diversity), 2)
# round(sd(symbolic_threat_islamophobia_final$median_net_taxable_income), 2)
# round(sd(symbolic_threat_islamophobia_final$population_density), 2)
# 
# 

################################################################################################################ 
# Count Statistical Sectors with Islamic POI in BNES, Table 1
################################################################################################################ 

dummy_vars <- c("dummy_food",
                "dummy_mosque",
                "dummy_halal",
                "dummy_edu")

# Count unique sectors with value 1 for each dummy
sapply(dummy_vars, function(var) {
  length(unique(symbolic_threat_islamophobia_final$CD_REFNIS_SECTOR[symbolic_threat_islamophobia_final[[var]] == 1]))
})

symbolic_threat_islamophobia_final_unique_ss <- symbolic_threat_islamophobia_final[!duplicated(symbolic_threat_islamophobia_final$CD_REFNIS_SECTOR), ]

sum(symbolic_threat_islamophobia_final_unique_ss$n_food_establishments, na.rm =  T) # 131
sum(symbolic_threat_islamophobia_final_unique_ss$mosque, na.rm =  T) # 14
sum(symbolic_threat_islamophobia_final_unique_ss$halal_store, na.rm =  T) # 44
sum(symbolic_threat_islamophobia_final_unique_ss$islam_edu, na.rm =  T) # 6

################################################################################################################ 
# Scale Predictors
################################################################################################################

vars <- c("q43_2",
          "prop_higher_edu_diploma_holders_over25", 
          "prop_outgroup_residential_diversity",
          "median_net_taxable_income",
          "prop_over16yo_in_high_status_job", 
          "population_density")
# "mun_prop_workers_over_20_years_old_high_status_jobs", 
# "mun_prop_higher_edu_diploma_holders_over25", 
# "mun_prop_outgroup_residential_diversity",
# "mun_median_net_taxable_income",
# "mun_prop_over16yo_in_high_status_job",               
# "mun_population_density")


scaled_predictors <- as.data.frame(scale(symbolic_threat_islamophobia_final[, vars]))

symbolic_threat_islamophobia_preprocessed <- cbind(symbolic_threat_islamophobia_final[, c("CD_REFNIS_SECTOR",                                   
                                                                                          "TX_SECTOR_DESCR_NL",                                 
                                                                                          "rid",                                                
                                                                                          "TX_MUNTY_DESCR_NL.x",                                
                                                                                          "q49_symbolic",                                       
                                                                                          "q87_cumulative",
                                                                                          "gender_man",                                       
                                                                                          "gender_woman",                                       
                                                                                          "subjective_sc_working_class",                       
                                                                                          "subjective_sc_low_middle_class",                    
                                                                                          "subjective_sc_higher_middle_class",                  
                                                                                          "subjective_sc_upper_class",                         
                                                                                          "subjective_sc_other_class",                          
                                                                                          "age_18_24",                                         
                                                                                          "age_25_34",                                          
                                                                                          "age_35_44",                                          
                                                                                          "age_45_54",                                          
                                                                                          "age_55_64",                                          
                                                                                          "age_65_93",                                          
                                                                                          "edu_none_lower",                                     
                                                                                          "edu_higher_secondary",                               
                                                                                          "edu_higher_university",
                                                                                          "dummy_food",                                         
                                                                                          "dummy_mosque",                                       
                                                                                          "dummy_halal",                                        
                                                                                          "dummy_edu",
                                                                                          "res_div")],scaled_predictors)

colSums(is.na(symbolic_threat_islamophobia_preprocessed))

################################################################################################################
# Linear Regression: POI as dummies (Statistical Sectors)
################################################################################################################

################################################################################################################
# Islamophobia
################################################################################################################

# Prior Checks: Bivariate Relationships

# b1_is <- lm(q87_cumulative ~ 
#               dummy_food,
#             # dummy_mosque +
#             # dummy_halal +
#             # dummy_edu, 
#             data = symbolic_threat_islamophobia_preprocessed)
# summary(b1_is) # sig, negative
# 
# b2_is <- lm(q87_cumulative ~ 
#               # dummy_food +
#               dummy_mosque,
#             # dummy_halal +
#             # dummy_edu, 
#             data = symbolic_threat_islamophobia_preprocessed)
# summary(b2_is) # not sig 
# 
# b3_is <- lm(q87_cumulative ~ 
#               # dummy_food +
#               # dummy_mosque +
#               dummy_halal,
#             # dummy_edu, 
#             data = symbolic_threat_islamophobia_preprocessed)
# summary(b3_is) # not sig 
# 
# 
# b4_is <- lm(q87_cumulative ~ 
#               # dummy_food +
#               # dummy_mosque +
#               # dummy_halal +
#               dummy_edu, 
#             data = symbolic_threat_islamophobia_preprocessed)
# summary(b4_is) # not sig 
# 
# 
# 
# is_m_1d <- lm(q87_cumulative ~ # without moderation #NOT OK
#                 dummy_food +
#                 dummy_mosque +
#                 dummy_halal +
#                 dummy_edu, 
#               data = symbolic_threat_islamophobia_preprocessed)
# 
# summary(is_m_1d)

# Model 1, OLS

is_m_4d <- lm(q87_cumulative ~  # !!!
                dummy_food +
                dummy_mosque +
                dummy_halal +
                dummy_edu +
                prop_over16yo_in_high_status_job +   
                prop_higher_edu_diploma_holders_over25 +           
                prop_outgroup_residential_diversity +               
                median_net_taxable_income +                          
                population_density+  
                gender_woman +
                subjective_sc_working_class +
                subjective_sc_low_middle_class +
                subjective_sc_upper_class + 
                subjective_sc_other_class +
                age_18_24 +
                age_25_34 +
                age_35_44 +
                age_45_54 +
                age_55_64 +
                edu_none_lower +
                edu_higher_secondary,
              data = symbolic_threat_islamophobia_preprocessed)

summary(is_m_4d)
qqnorm(resid(is_m_4d)) #ok
qqline(resid(is_m_4d), col = "red")
shapiro.test(resid(is_m_4d)) # ok
ks.test(scale(residuals(is_m_4d)), "pnorm") # ok
bptest(is_m_4d) # ok
durbinWatsonTest(is_m_4d) # ok
vif(is_m_4d) #ok

actual <- symbolic_threat_islamophobia_preprocessed$q87_cumulative
predicted <- predict(is_m_4d)
mse <- mean((actual - predicted)^2)
rmse <- sqrt(mse)
print(paste("MSE:", round(mse, 3))) # "MSE: 0.449"
print(paste("RMSE:", round(rmse, 3))) # "RMSE: 0.67"

# save(is_m_4d, file="Islamophobia_OLS_printout.RData")

# Model 1, Moderation with Multiculturalism

is_m_4ad  <- lm(q87_cumulative ~  # !!!
                  dummy_food +
                  dummy_mosque +
                  dummy_halal +
                  dummy_edu +
                  q43_2 + # Multiculturalism
                  dummy_food*q43_2 + # Multiculturalism x halal food
                  dummy_mosque*q43_2 + # Multiculturalism x mosque
                  dummy_halal*q43_2 + # Multiculturalism x halal store
                  dummy_edu*q43_2 + # Multiculturalism x Islamic edu and cultural centers
                  prop_over16yo_in_high_status_job +   
                  prop_higher_edu_diploma_holders_over25 +           
                  prop_outgroup_residential_diversity +               
                  median_net_taxable_income +                          
                  population_density+ 
                  gender_woman +
                  subjective_sc_working_class +
                  subjective_sc_low_middle_class +
                  subjective_sc_upper_class +
                  subjective_sc_other_class +
                  age_18_24 +
                  age_25_34 +
                  age_35_44 +
                  age_45_54 +
                  age_55_64 +
                  edu_none_lower +
                  edu_higher_secondary,
                data = symbolic_threat_islamophobia_preprocessed)

summary(is_m_4ad)
qqnorm(resid(is_m_4ad)) #ok
qqline(resid(is_m_4ad), col = "red")
shapiro.test(resid(is_m_4ad)) 
ks.test(scale(residuals(is_m_4ad)), "pnorm") # ok
bptest(is_m_4ad) # ok
durbinWatsonTest(is_m_4ad) # ok
vif(is_m_4ad, type = "predictor") #ok

actual <- symbolic_threat_islamophobia_preprocessed$q87_cumulative
predicted <- predict(is_m_4ad)
mse <- mean((actual - predicted)^2)
rmse <- sqrt(mse)
print(paste("MSE:", round(mse, 3))) # "MSE: 0.399"
print(paste("RMSE:", round(rmse, 3))) # "RMSE: 0.632"

# save(is_m_4ad, file="Islamophobia_Moderation_OLS_printout.RData")

# stargazer(is_m_4d, is_m_4ad, 
#           type = "html",
#           title = "Regression Results",
#           column.labels = c("Model 1", "Model 2"),
#           align = TRUE,
#           no.space = TRUE,
#           digits = 3,
#           out = "Islamophobia_Results_printout.html")  # Save as an HTML file

################################################################################################################
# Interaction Plot, Islamophobia: Presence of Islamic Food Establishments X Multiculturalism
################################################################################################################

int_1 <-interact_plot(model = is_m_4ad, pred = dummy_food, modx = q43_2,
                      x.label = "Presence of Islamic Food Establishments",
                      y.label = "Predicted Islamophobia",
                      legend.main = "Multiculturalism",
                      interval = TRUE,
                      colors = RColorBrewer::brewer.pal(3, "Greys"))

int_1

# ggsave("int_1_new",  # save in jpeg
#        dpi = 300,
#        width = 6,       
#        height = 4,    
#        units = "in",
#        device = "jpeg")

################################################################################################################
# Elastic Net Regression: POI as dummies (Statistical Sectors)
################################################################################################################

################################################################################################################
# Islamophobia
################################################################################################################

# While developing the ML solution, I encountered several errors with the caret package. 
# I resolved the errors by consulting ChatGPT for explanations and suggestions, but all final codebase and code decisions remain my own.

set.seed(05081997) # control randomness in train-test splits

x1 <- model.matrix(q87_cumulative ~
                     dummy_food +
                     dummy_mosque +
                     dummy_halal +
                     dummy_edu +
                     prop_over16yo_in_high_status_job +   
                     prop_higher_edu_diploma_holders_over25 +           
                     prop_outgroup_residential_diversity +               
                     median_net_taxable_income +                          
                     population_density+  
                     gender_woman +
                     subjective_sc_working_class +
                     subjective_sc_low_middle_class +
                     subjective_sc_upper_class + 
                     subjective_sc_other_class +
                     age_18_24 +
                     age_25_34 +
                     age_35_44 +
                     age_45_54 +
                     age_55_64 +
                     edu_none_lower +
                     edu_higher_secondary - 1,
                   data = symbolic_threat_islamophobia_preprocessed)

x2 <- model.matrix(q87_cumulative ~  # !!!
                     dummy_food +
                     dummy_mosque +
                     dummy_halal +
                     dummy_edu +
                     q43_2 +
                     dummy_food*q43_2 +
                     dummy_mosque*q43_2 +
                     dummy_halal*q43_2 +
                     dummy_edu*q43_2 +
                     prop_over16yo_in_high_status_job +   
                     prop_higher_edu_diploma_holders_over25 +           
                     prop_outgroup_residential_diversity +               
                     median_net_taxable_income +                          
                     population_density+ 
                     gender_woman +
                     subjective_sc_working_class +
                     subjective_sc_low_middle_class +
                     subjective_sc_upper_class +
                     subjective_sc_other_class +
                     age_18_24 +
                     age_25_34 +
                     age_35_44 +
                     age_45_54 +
                     age_55_64 +
                     edu_none_lower +
                     edu_higher_secondary - 1,
                   data = symbolic_threat_islamophobia_preprocessed)

y <- symbolic_threat_islamophobia_preprocessed$q87_cumulative
y2 <- symbolic_threat_islamophobia_preprocessed$q49_symbolic


n <- nrow(symbolic_threat_islamophobia_preprocessed)
train_idx <- sample(1:n, floor(0.8 * n))

# For Model 1
x1_train <- x1[train_idx, ]
x1_test <- x1[-train_idx, ]

# For Model 2
x2_train <- x2[train_idx, ]
x2_test <- x2[-train_idx, ]

y_train <- y[train_idx]
y_test <- y[-train_idx]

y2_train <- y2[train_idx]
y2_test <- y2[-train_idx]

################################################################################################################
# Islamophobia
################################################################################################################

# Baseline Model. alpha = 0.5

set.seed(05081997) # control randomness in ML

fit_is_1 <- cv.glmnet(x1_train, y_train,
                      family = c("gaussian"),
                      alpha = 0.5, 
                      nlambda = 1000, 
                      type.measure = "mse", 
                      nfolds = 10)

# Best lambda (penalty parameter)
fit_is_1 # cv-mse  0.4799, # cv-rmse 0.6927482
best_lambda <- fit_is_1$lambda.min #  0.01030964
best_lambda

preds <- predict(fit_is_1, s = best_lambda, newx = x1_test)
mse <- mean((preds - y_test)^2)
rmse <- sqrt(mse)
cat("Test MSE:", round(mse, 3), "\n") # 0.4032 
cat("Test RMSE:", round(rmse, 3), "\n") # 0.6349 
rss <- sum((y_test - preds)^2)                    
tss <- sum((y_test - mean(y_test))^2)                 
r_squared <- 1 - rss / tss
cat("Test R^2:", round(r_squared,3), "\n")
coef(fit_is_1, s = best_lambda)
round(coef(fit_is_1, s = best_lambda), 3)

elastic_net_islamophobia <- round(coef(fit_is_1, s = best_lambda), 3)

# save(elastic_net_islamophobia, file="Islamophobia_Elastic_Net_printout.RData")

# Moderation Model. Alpha  = 0.5

set.seed(05081997) # control randomness in ML

fit_is_2 <- cv.glmnet(x2_train, y_train,
                      family = c("gaussian"),
                      alpha = 0.5, 
                      nlambda = 1000, 
                      type.measure = "mse", 
                      nfolds = 10)

# Best lambda (penalty parameter)
fit_is_2 # cv-mse 0.4290, # cv-rmse 0.6549809
best_lambda_2 <- fit_is_2$lambda.min # 0.01397484
best_lambda_2 

preds_2 <- predict(fit_is_2, s = best_lambda_2, newx = x2_test)
mse_2 <- mean((preds_2 - y_test)^2)
rmse_2 <- sqrt(mse_2)
cat("Test MSE:", round(mse_2, 3), "\n") # 0.3686 
cat("Test RMSE:", round(rmse_2, 3), "\n") # 0.6071 
rss <- sum((y_test - preds_2)^2)                    
tss <- sum((y_test - mean(y_test))^2)                 
r_squared <- 1 - rss / tss
cat("Test R^2:", round(r_squared,3), "\n")
coef(fit_is_2, s = best_lambda_2)
round(coef(fit_is_2, s = best_lambda_2), 3)

elastic_net_islamophobia_moderation <- round(coef(fit_is_2, s = best_lambda_2), 3)

save(elastic_net_islamophobia_moderation, file="Islamophobia_Moderation_Elastic_Net_printout.RData")
