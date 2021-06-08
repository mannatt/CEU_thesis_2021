### Manna Toth
### Data cleaning
### CEU - Thesis

#packages
library(readxl)
library(plyr)
library(dplyr)
library(data.table)
library("xlsx")
library(stringi)
library(Hmisc)
library(ggplot2)
library(plotly)
library(tidyr)
library(corrplot)
library(ggcorrplot)
library(htmlwidgets)
library(BMS)
library(BAS)
library(ineq)
library(ggrepel)
library(stargazer)

setwd("/Users/manna/Desktop/Thesis_202105/")

####ANALYSIS

##############################################################################################
#Correlation
#---------------------------------------------------------------------------------------------
data <- read.csv('DATA/data_for_analysis1.csv') 

#change order of columns
col_order <- c(
#id
"SC_code", "year",

#controls
"ln_export_income" ,"ln_revenues","ln_personal_income", 
"unemployment_ratio", "population_total","population_male", "population_female",  
"gymnasium_ratio", "governing_party","close_run_governing_party",               
"close_run_opposition","overwhelming_victory_governing_party",    
 "overwhelming_victory_non_governing_party", "presciption_exemption_certificate",
"territory_woods", "distance_nearest_50_town", "distance_regional_center",                
"elderely_ratio","distance_county_capitals" ,"precipication", 
"stadium", "ambulance_stations", "particulate_matter", "ozone", "co2",

#healthcare_related
"empty_GP_praxis","GP_percapita","GP_empty_population_ratio",                                        
 "ICU_department", "mean_care_days","one_day_ratio", "hospital_dummy",                          
"hours_outpatient_percapita", "cancer_ratio", "respiratory_ratio" ,"cardio_ratio",                            
"mortality_ratio","infant_mortality_ratio","preventable_female",                      
 "preventable_total","preventable_male" ,"treatable_female",
"treatable_total", "treatable_male","out_patient_budget_ratio","care_days_percapita",                                   
"professionals_total_percapita","ln_budget_outpatient", "ln_budget_CT",                                                    
"ln_budget_total","ln_budget_GP" ,  

#funds
"ln_funds_decision",                       
"ln_funds_effective",                                                   
"ln_funds_equipment",
"ln_funds_hr" )

data <- data[, col_order]

###Correlation

#sapply(data, typeof)
#colnames(data)

#controls
correlation <- cor(data[3:27], use = 'pairwise.complete.obs', method = 'spearman')
correlation
plot <- ggcorrplot(correlation, type = "lower", tl.cex = 10, tl.srt = 60, lab = TRUE, lab_size=3)
plot
plot2 <- ggplotly(plot)
saveWidget(ggplotly(plot2), file = "RESULTS/Descriptives/corrplot_controls_022.html")
plot

#ambulance station, stadium, ozone, particulate matter, co2 have observations for a low number of years,
#I can not include them in the final analysis.
#particulate matter correlates with personal income (-0.46) and elderly_ratio(-0.43)
#ozone correlates with unemployment ratio (-0.49)
#co2 correlates with personal income (0.51)
#stadiums correlate with the territory of woods (0.46)
#ambulance does correlate with any other variables

#controls were removed from final analysis due to high correlation:
#distance_regional_center, distance_nearest_50_town, prescription_exemptions,
#unemployment_ration
#precipitation is removed because it does not correlated with any of the environmental variables
data <- data%>% select(-c(distance_regional_center, distance_nearest_50_town, 
                          unemployment_ratio, presciption_exemption_certificate, particulate_matter,
                          ambulance_stations, stadium, precipication, ozone, co2))


#health-related variables
correlation <- cor(data[18:43], use = 'pairwise.complete.obs', method = 'spearman')
correlation
plot <- ggcorrplot(correlation, type = "lower", tl.cex = 10, tl.srt = 60, lab = TRUE, lab_size=3)
plot
plot2 <- ggplotly(plot)
saveWidget(ggplotly(plot2), file = "RESULTS/Descriptives/corrplot_health_realated_022.html")
plot

#controls were removed from final analysis due to high correlation:
#empty_GP_praxis for now.
data <- data %>% select(-c(empty_GP_praxis, out_patient_budget_ratio))

write.csv(data, 'DATA/data_for_analysis2.csv', row.names = FALSE, na="")

data <- read.csv('DATA/data_for_analysis1.csv') 
data <- data %>% select(c(ln_funds_decision, treatable_total, preventable_total, mortality_ratio,
                         ln_revenues, population_total, ln_budget_outpatient,
                         GP_empty_population_ratio, gymnasium_ratio, ICU_department,
                         one_day_ratio, close_run_governing_party, governing_party, 
                         close_run_governing_party,               
                        close_run_opposition, overwhelming_victory_governing_party,
                        overwhelming_victory_non_governing_party ))

data$ln_mortality = log(data$mortality_ratio)
data$ln_treatable_mortality = log(data$treatable_total)
data$ln_preventable_mortality = log(data$preventable_total)

data <- data %>% select(-c(treatable_total, preventable_total, mortality_ratio ))

correlation <- cor(data, use = 'pairwise.complete.obs', method = 'spearman')
correlation
plot <- ggcorrplot(correlation, type = "lower", tl.cex = 10, tl.srt = 60, lab = TRUE, lab_size=3)
plot
plot2 <- ggplotly(plot)
saveWidget(ggplotly(plot2), file = "RESULTS/Descriptives/corrplot_health_realated_022.html")
plot

data <- read.csv('DATA/data_for_analysis1.csv') 
data <- data %>% select(c(ln_funds_effective, treatable_total, preventable_total,
                          ln_revenues, population_total, ln_budget_total,
                          GP_empty_population_ratio, gymnasium_ratio, ICU_department,
                           governing_party, hours_outpatient_percapita, 
                          cardio_ratio, cancer_ratio, respiratory_ratio))


data$ln_treatable_mortality = log(data$treatable_total)
data$ln_preventable_mortality = log(data$preventable_total)
data$mean_incidence = (data$cancer_ratio+ data$cardio_ratio + data$respiratory_ratio)/3

data <- data %>% select(-c(treatable_total, preventable_total,
                          cardio_ratio, cancer_ratio, respiratory_ratio))

correlation <- cor(data, use = 'pairwise.complete.obs', method = 'spearman')
correlation
plot <- ggcorrplot(correlation, type = "lower", tl.cex = 10, tl.srt = 60, lab = TRUE, lab_size=3)
plot
#----------------------------------------------------------------------------------------------


#Bayesian Model Averaging
#based on: https://modelaveraging.wordpress.com/2011/05/26/bms-and-the-fixed-effects-estimator-a-tutorial/
#----------------------------------------------------------------------------------------------
data <- read.csv('DATA//data_for_analysis_demeaned4.csv', sep = ',')

#variables are demeaned by the annual mean to clean the dataset from year fixed effects
#data2 <- read.csv('DATA/data_for_analysis2.csv')
#colnames(data)
data_bma <- data %>% select(-c("ln_export_income", "ln_revenues",                     
                               "ln_personal_income", "population_total",                
                               "population_male",  "population_female",                
                                "gymnasium_ratio" , "lead_funds_effective",             
                               "lead2_funds_effective" , "lead3_funds_effective",  
                                "cancer_ratio" , "respiratory_ratio",                
                                "cardio_ratio",  "mean_care_days",  "one_day_ratio", 
                                "mortality_ratio",                  
                              "infant_mortality_ratio" , "preventable_female",               
                               "preventable_total" , "preventable_male" ,                
                               "treatable_female" , "treatable_total" ,                 
                               "treatable_male",  "care_days_percapita" ,             
                                "professionals_total_percapita",  "ln_budget_outpatient" ,            
                               "ln_budget_ct",  "ln_budget_total" ,                 
                                "ln_budget_gp" , 'd_male', "d_female",              
                               "ln_funds_effective" , "ln_funds_equipment" ,              
                               "ln_funds_hr",  "elderely_ratio", "d_prev_female" ,                   
                               "d_prev_male",    "d_treat_female" ,  "d_treat_male",
                              'hours_outpatient_percapita', 'ln_budget_ct', 'ln_budget_gp',
                              'mean_care_days', 'one_day_ratio', 'hospital_dummy',
                              'cardio_ratio', 'respiratory_ratio', 'cancer_ratio', 'd_budget_ct',
                              'd_budget_gp', 'd_cancer', 'd_cardio', 'd_respiratory',
                              'd_one_day_ratio', 'd_mean_care_days', 'd_infant_mortality', 'territory_woods',
                              'd_revenues', 'd_professionals',
                              "distance_county_capitals","gp_percapita",                    
                              "gp_empty_population_ratio", "ln_funds_decision" ,              
                              "lag_funds_decision","lag2_funds_decision","d_export_income",                 
                              "d_income" ,"d_gymnasium_ratio","d_population" ,                   
                              "d_elderelyr", "d_mortality" ,"d_prev_total" ,                   
                              "d_treat_total" ,"d_care_days","d_budget_outpatient","d_budget_total" ))

#functions for demeaning and creating dummies 
####
panel_unstack= function(stackeddata, tstep=NULL) {
  # stackeddata is a stacked data frame/matrix ordered in the following way
  #               Variable1  Variable2
  # ctry1_time1
  # ctry1_time2
  # ...
  # ctry2_time1
  # ctry2_time2
  # ...
  # tstep is the number of time points
  # panel_unstack produces a 3-dimensional array out of that
  bigT=nrow(stackeddata);K=ncol(stackeddata);
  if (is.null(tstep)) tstep=bigT
  X1=aperm(array(as.vector(t(as.matrix(stackeddata))),dim=c(K,tstep,bigT/tstep)), perm=c(2,1,3))
  try(dimnames(X1)[[1]] <-  unique(sapply(strsplit(rownames(stackeddata),"_"),
                                          function(x) x[[2]])), silent=TRUE)
  try(dimnames(X1)[[2]] <-  colnames(stackeddata), silent=TRUE)
  try(dimnames(X1)[[3]] <-  unique(sapply(strsplit(rownames(stackeddata),"_"),
                                          function(x) x[[1]])), silent=TRUE)
  return(X1)
}
panel_stack = function(array3d) {
  x1= apply(array3d,2,rbind)
  try(rownames(x1) <-  as.vector(sapply(dimnames(array3d)[[3]],
                                        FUN=function(x) paste(x, dimnames(array3d)[[1]], sep="_"))), silent=TRUE)
  return(as.data.frame(x1))
}
####

# 1. model y: funds 2 lags + all funds
#### data transformation required for the model
bma1 <- data_bma 
bma1 <- bma1 %>% filter(year < 2019)
bma1 <- bma1 %>% filter(year > 2006)
bma1 <- bma1 %>% relocate(c_lag2_funds_decision)
bma1$rn = paste(bma1$year,bma1$sc_code, sep = "_")
rownames(bma1) <- bma1[,"rn"]
bma1 <- bma1 %>% select(-c(year, sc_code, rn))
panelDat=as.matrix(bma1)
dat.array=panel_unstack(panelDat, tstep=12)
countryDat=panel_stack(demean(dat.array,1))

# country dummies
bigT=nrow(panelDat);tstep=12;
countryDummies=kronecker(diag(bigT/tstep),rep(1,tstep));
colnames(countryDummies)=dimnames(dat.array)[[3]]
countryDummies=countryDummies[,-1]
model_SCdummy1=bms(cbind(panelDat,countryDummies),mprior="uniform", g = 'uip', fixed.reg=colnames(countryDummies),user.int=F)
###

# 2. model y funds lag 2
### data transformation required for the model
bma1 <- data_bma %>% select(-c(c_lag_funds_decision, c_ln_funds_decision))
bma1 <- bma1 %>% filter(year < 2019)
bma1 <- bma1 %>% filter(year > 2006)
bma1 <- bma1 %>% relocate(c_lag2_funds_decision)
bma1$rn = paste(bma1$year,bma1$sc_code, sep = "_")
rownames(bma1) <- bma1[,"rn"]
bma1 <- bma1 %>% select(-c(year, sc_code, rn))
panelDat=as.matrix(bma1)
dat.array=panel_unstack(panelDat, tstep=12)
countryDat=panel_stack(demean(dat.array,1))

# country dummies
bigT=nrow(panelDat);tstep=12;
countryDummies=kronecker(diag(bigT/tstep),rep(1,tstep));
colnames(countryDummies)=dimnames(dat.array)[[3]]
countryDummies=countryDummies[,-1]
model_SCdummy2=bms(cbind(panelDat,countryDummies),mprior="uniform", g = 'uip', fixed.reg=colnames(countryDummies),user.int=F)
###

### 3. model y funds lag 1
#### data transformation required for the model
bma1 <- data_bma %>% select(-c(c_lag2_funds_decision, c_ln_funds_decision))
bma1 <- bma1 %>% filter(year < 2019)
bma1 <- bma1 %>% filter(year > 2006)
bma1 <- bma1 %>% relocate(c_lag_funds_decision)
bma1$rn = paste(bma1$year,bma1$sc_code, sep = "_")
rownames(bma1) <- bma1[,"rn"]
bma1 <- bma1 %>% select(-c(year, sc_code, rn))
panelDat=as.matrix(bma1)
dat.array=panel_unstack(panelDat, tstep=12)
countryDat=panel_stack(demean(dat.array,1))

# country dummies
bigT=nrow(panelDat);tstep=12;
countryDummies=kronecker(diag(bigT/tstep),rep(1,tstep));
colnames(countryDummies)=dimnames(dat.array)[[3]]
countryDummies=countryDummies[,-1]
model_SCdummy3=bms(cbind(panelDat,countryDummies), mprior="uniform", g= 'uip',fixed.reg=colnames(countryDummies),user.int=F)
###

### 4. model y: funds no lag
#### data transformation required for the model
bma1 <- data_bma %>% select(-c(c_lag2_funds_decision, c_lag_funds_decision))
bma1 <- bma1 %>% filter(year < 2019)
bma1 <- bma1 %>% filter(year > 2006)
bma1 <- bma1 %>% relocate(c_ln_funds_decision)
bma1$rn = paste(bma1$year,bma1$sc_code, sep = "_")
rownames(bma1) <- bma1[,"rn"]
bma1 <- bma1 %>% select(-c(year, sc_code, rn))
panelDat=as.matrix(bma1)
dat.array=panel_unstack(panelDat, tstep=12)
countryDat=panel_stack(demean(dat.array,1))

# country dummies
#bigT=nrow(data6);tstep=8;
bigT=nrow(panelDat);tstep=12;
countryDummies=kronecker(diag(bigT/tstep),rep(1,tstep));
colnames(countryDummies)=dimnames(dat.array)[[3]]
countryDummies=countryDummies[,-1]
model_SCdummy4=bms(cbind(panelDat,countryDummies), mprior="uniform", g= 'uip',fixed.reg=colnames(countryDummies),user.int=F)
###

#exporting results
a <- coef(model_SCdummy1)
#write.xlsx(a, 'a.xlsx', row.names = TRUE)

b <- coef(model_SCdummy2)
#write.xlsx(b, 'b.xlsx', row.names = TRUE)

c <- coef(model_SCdummy3)
#write.xlsx(c, 'c.xlsx', row.names = TRUE)

d <- coef(model_SCdummy4)
#write.xlsx(d, 'd.xlsx', row.names = TRUE)

#-------------------------------------------------------------------------------------------

#GINI
#-----------------------------------------------------------------------------------------------


gini_dt <-read.csv('DATA/full_dataset_cleaned.csv')
colnames(gini_dt)
  
gini_dt <- gini_dt %>% select(c(SC_code, year, GP_percapita, gymnasium_ratio, one_day_ratio, hours_outpatient_percapita,
                     infant_mortality_ratio, treatable_total, preventable_total, budget_total_percapita,
                     professionals_total_percapita, revenues_percapita, personal_income_percapita, cancer_ratio, 
                     cardio_ratio, respiratory_ratio, budget_CT_percapita, budget_GP_percapita))

gn = NULL
dt <- data.table(gn)

dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <-  gini_dt %>% filter(year==i)
  b <- ineq(a$respiratory_ratio, type = 'Gini', na.rm = TRUE)
  m = data.table('year' = i, 'respiratory' = b)
  dt <- rbind(dt,m) 
}
gn <- cbind(gn,dt)

dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <-  gini_dt %>% filter(year==i)
  b <- ineq(a$cancer_ratio, type = 'Gini', na.rm = TRUE)
  m = data.table('year' = i, 'cancer' = b)
  dt <- rbind(dt,m) 
}

gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)
dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <-  gini_dt %>% filter(year==i)
  b <- ineq(a$GP_percapita, type = 'Gini', na.rm = TRUE)
  m = data.table('year' = i, 'gp_percapita' = b)
  dt <- rbind(dt,m) 
}

gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)
dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <- gini_dt %>% filter(year==i)
  b <- ineq(a$gymnasium_ratio, type = 'Gini', na.rm = TRUE)
  m = data.table('year' = i, 'gymnasium_ratio' = b)
  dt <- rbind(dt,m) 
}
gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)
dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <-  gini_dt %>% filter(year==i)
  b <- ineq(a$one_day_ratio, type = 'Gini', na.rm = TRUE)
  m = data.table('year' = i, 'one_day_ratio' = b)
  dt <- rbind(dt,m) 
}
gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)
dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <-  gini_dt %>% filter(year==i)
  b <- ineq(a$hours_outpatient_percapita, type = 'Gini', na.rm = TRUE)
  m = data.table('year' = i, 'hours_outpatient_percapita' = b)
  dt <- rbind(dt,m) 
}

gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)
dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <-  gini_dt %>% filter(year==i)
  b <- ineq(a$infant_mortality_ratio, type = 'Gini', na.rm = TRUE)
  m = data.table('year' = i, 'infant_mortality_ratio' = b)
  dt <- rbind(dt,m) 
}
gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)
dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <-  gini_dt%>% filter(year==i)
  b <- ineq(a$treatable_total, type = 'Gini', na.rm = TRUE)
  m = data.table('year' = i, 'treatable_total' = b)
  dt <- rbind(dt,m) 
}
gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)
dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <-  gini_dt %>% filter(year==i)
  b <- ineq(a$preventable_total, type = 'Gini', na.rm = TRUE)
  m = data.table('year' = i, 'preventable_total' = b)
  dt <- rbind(dt,m) 
}

gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)
dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <- gini_dt %>% filter(year==i)
  b <- ineq(a$budget_total_percapita, type = 'Gini', na.rm = TRUE)
  m = data.table('year' = i, 'budget_total_percapita' = b)
  dt <- rbind(dt,m) 
}
gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)
dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <-   gini_dt%>% filter(year==i)
  b <- ineq(a$professionals_total_percapita, type = 'Gini', na.rm = TRUE)
  m = data.table('year' = i, 'professionals_total_percapita' = b)
  dt <- rbind(dt,m) 
}
gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)
dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <-  gini_dt %>% filter(year==i)
  b <- ineq(a$revenues_percapita, type = 'Gini', na.rm = TRUE)
  m = data.table('year' = i, 'revenues_percapita' = b)
  dt <- rbind(dt,m) 
}
gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)
dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <-  gini_dt %>% filter(year==i)
  b <- ineq(a$personal_income_percapita, type = 'Gini', na.rm = TRUE)
  m = data.table('year' = i, 'personal_income_percapita' = b)
  dt <- rbind(dt,m) 
}

gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)
dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <-  gini_dt %>% filter(year==i)
  b <- ineq(a$budget_CT_percapita, type = 'Gini', na.rm = TRUE)
  m = data.table('year' = i, 'budget_CT_percapita' = b)
  dt <- rbind(dt,m) 
}
gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)
dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <-  gini_dt %>% filter(year==i)
  b <- ineq(a$budget_GP_percapita, type = 'Gini',na.rm = TRUE)
  m = data.table('year' = i, 'budget_GP_percapita' = b)
  dt <- rbind(dt,m) 
}
gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)

gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)
dt = NULL
dt <- data.table(dt)
for(i in 2004:2019) {
  a <-  gini_dt %>% filter(year==i)
  b <- ineq(a$cardio_ratio, type = 'Gini',na.rm = TRUE)
  m = data.table('year' = i, 'cardio_ratio' = b)
  dt <- rbind(dt,m) 
}
gn <- merge(gn, dt, by = 'year', all.x = TRUE, all.y = TRUE)


write.csv(gn, 'DATA/gn.csv')

gn2 <- read.csv('DATA/gn22.csv', sep = ';')



ggplot(gn2, aes(x = last, y = first)) +
geom_point(aes(color = factor(trend)), size = 4, alpha = 0.5) +
#geom_text_repel(aes(label = variable), size = 3) +
geom_text_repel(aes(label = variable, color = factor(trend)), size = 4) +
scale_color_manual(values = c("#666999", "#CC6633")) +
scale_y_continuous(limits = c(0, 1), breaks = c(0.02, 0.06, 0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
scale_x_continuous(limits = c(0, 1), breaks = c(0.02, 0.06, 0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
labs(col="Trend of GINI coefficient", x = 'Gini coefficient in the last available year',
     y = "GINI coefficient in the first available year") 

gn2 <- read.csv('DATA/gn22.csv', sep = ';')

df2 <- melt(gn2, id.vars=c('names', 'trend'))
head(df2)

df_small <- filter(df2, value < 0.2)
df_big  <- filter(df2, value > 0.2)


ggplot(df_small, aes(names, value)) + 
  geom_bar(aes(fill = trend), stat = 'identity', position = "dodge2") +
  scale_fill_manual(values = c("#666999", "#CC6633")) +
  theme(text = element_text(size=20),
      axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x = 'Name of variable',
     y = "Gini coefficient") 


ggplot(df_big, aes(names, value)) + 
  geom_bar(aes(fill = trend), stat = 'identity', position = "dodge2") +
  scale_fill_manual(values = c("#666999", "#CC6633")) +
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x = 'Name of variable',
       y = "Gini coefficient") 

   
ggplot(df_small, aes(x=names, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')


ggplot(df_small, aes(names, value)) + 
  geom_bar(aes(fill = variable), stat = 'identity', position = "dodge2") +
  facet_grid(.~trend) 

ggplot(df_small, aes(names, value)) + 
  geom_bar(aes(fill = trend), stat = 'identity', position = "dodge2") +
  
  
  
  expand_limits(y = c(1,30))


+
  expand_limits(y = c(1,30))




df1 <- melt(gn2[1:8,][,c('first', 'last')], id.vars = variable)

ggplot(gn2, aes(x = variable, y = value)) + 
  geom_bar(aes(fill = period), stat = 'identity')

colnames(gn2)
ggplot(gn2[1:8,], aes(x = variable, y = value)) + 
  geom_bar(aes(fill = period, stat = 'identity'))





ineq(a$treatable_male, type = 'Gini') 
plot(Lc(master2$treatable_male))

colnames(gini_dt)











#-----------------------------------------------------------------------------------------------

