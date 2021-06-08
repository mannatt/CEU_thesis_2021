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

setwd("/Users/manna/Desktop/Thesis_202105/")

#CPI
#-------------------------------------------------------------------------------
CPI <- read_excel('Data/ready_to_use/SC/CPI.xlsx')
CPI <- CPI %>% select(year, CPI_basis_2014)
write.csv(CPI,'Data/ready_to_use/SC/CPI.csv', row.names = FALSE)
#-------------------------------------------------------------------------------

#village key and data
#-------------------------------------------------------------------------------
village_codes <- read.csv("Data/ready_to_use/village_codes_final.csv" )
village_key <- village_codes %>% select(village_name_no_accents, SC_code)
#write.csv(village_key, 'Data/ready_to_use/village_key_merge.csv')

SC_data <- read_excel('Data/ready_to_use/SC_data.xlsx')
SC_data$SC_name <- tolower(SC_data$SC_name)
SC_data$SC_capital <- tolower(SC_data$SC_capital)
SC_data$SC_name  <- stri_trans_general(SC_data$SC_name, "Latin-ASCII")
SC_data$SC_capital  <- stri_trans_general(SC_data$SC_capital, "Latin-ASCII")
SC_data$SC_name <- str_remove_all(SC_data$SC_name, "\\jaras")
SC_data$SC_capital_dummy <- 5

SC_key <- SC_data %>% select(SC_capital, SC_capital_dummy)

village_codes <- merge(village_codes, SC_key, by.x="village_name_no_accents", by.y = 'SC_capital', all.x = TRUE)
village_codes$type_code[village_codes$type_code == 0 & village_codes$SC_capital_dummy == 5] <- 5

geographic_role <- village_codes %>% select(SC_code, type_code)
geographic_role <- geographic_role %>% rename(geographic_role = type_code)

#geographic_role <- read.csv('Data/ready_to_use/SC/geographic_role.csv')
geographic_role <- geographic_role %>% filter(geographic_role > 0)

write.csv(village_codes, 'Data/ready_to_use/village_codes_final.csv', row.names = FALSE)
write.csv(geographic_role, 'Data/ready_to_use/SC/geographic_role.csv', row.names = FALSE)
#-------------------------------------------------------------------------------

#outpatient care
#-------------------------------------------------------------------------------
outpatient <- read_excel('Data/ready_to_use/out_patient_hcp_202101.xlsx')
outpatient$`Telephely v?rosa` <- tolower(outpatient$`Telephely v?rosa`)
outpatient$`Telephely v?rosa`  <- stri_trans_general(outpatient$`Telephely v?rosa`, "Latin-ASCII")

village_key <- read.csv('Data/ready_to_use/village_key_merge.csv')

outpatients <- merge(outpatient, village_key, by.x='Telephely v?rosa', by.y = "village_name_no_accents", all.x = TRUE)

outpatients <- outpatients [!duplicated(outpatients[c(2,4)]),]

outpatient_available <- outpatients %>% count(SC_code)
mean <- mean(outpatient_available$n)
median <- median(outpatient_available$n)

#data was created manually + google search was also conducted
#-------------------------------------------------------------------------------

#health care fund financing data
#-------------------------------------------------------------------------------
#National Health care Fund's budget data
#reading raw data
budget_2006 <- read_excel("raw_data/OEP_FIN_2006_evi_uj.xls")
budget_2007 <- read_excel("raw_data/OEP_FIN_2007_evi_uj.xls")
budget_2008 <- read_excel("raw_data/OEP_FIN_2008_evi_uj.xls")
budget_2009 <- read_excel("raw_data/OEP_FIN_2009_evi_uj.xls")
budget_2010 <- read_excel("raw_data/OEP_FIN_2010_evi_uj.xls")
budget_2011 <- read_excel("raw_data/OEP_FIN_2011_evi_uj.xls")
budget_2012 <- read_excel("raw_data/OEP_FIN_2012_evi_uj.xls")
budget_2013 <- read_excel("raw_data/OEP_FIN_2013_evi_uj.xls")
budget_2014 <- read_excel("raw_data/OEP_Fin_2014.eves_uj.xlsx")
budget_2015 <- read_excel("raw_data/OEP_Fin_2015_ev_osszesen_uj.xlsx")
budget_2016 <- read_excel("raw_data/OEP_Fin_2016_ev_osszesen_uj.xlsx")
budget_2017 <- read_excel("raw_data/NEAK_Fin_2017_ev_osszesen_uj.xlsx")
budget_2018 <- read_excel("raw_data/NEAK_Fin_2018.evi_osszesen.vegleges.xlsx")
budget_2019 <- read_excel("raw_data/NEAK_Fin_2019._evi_vegleges.xlsx")

#creating a data table
d2006 <-as.data.table(colnames(budget_2006))
d2007 <-as.data.table(colnames(budget_2007))
d2008 <-as.data.table(colnames(budget_2008))
d2009 <-as.data.table(colnames(budget_2009))
d2010 <-as.data.table(colnames(budget_2010))
d2011 <-as.data.table(colnames(budget_2011))
d2012 <-as.data.table(colnames(budget_2012))
d2013 <-as.data.table(colnames(budget_2013))
d2014 <-as.data.table(colnames(budget_2014))
d2015 <-as.data.table(colnames(budget_2015))
d2016 <-as.data.table(colnames(budget_2016))
d2017 <-as.data.table(colnames(budget_2017))
d2018 <-as.data.table(colnames(budget_2018))
d2019 <-as.data.table(colnames(budget_2019))

#cn <- qpcR:::cbind.na(d2006,d2007,d2008,d2009, d2010, d2011,d2012, d2013, d2014,
#                 d2015,d2016, d2017,d2018,d2019)
#adding year
budget_2006$year <- 2006
budget_2007$year <- 2007
budget_2008$year <- 2008
budget_2009$year <- 2009
budget_2010$year <- 2010
budget_2011$year <- 2011
budget_2012$year <- 2012
budget_2013$year <- 2013
budget_2014$year <- 2014
budget_2015$year <- 2015
budget_2016$year <- 2016
budget_2017$year <- 2017
budget_2018$year <- 2018
budget_2019$year <- 2019

#colSums(is.na(budget_2007))
#merge years
budget <- rbind.fill(budget_2006, 
                     budget_2007, 
                     budget_2008,
                     budget_2009, 
                     budget_2010,
                     budget_2011,
                     budget_2012, 
                     budget_2013, 
                     budget_2014, 
                     budget_2015, 
                     budget_2016, 
                     budget_2017,
                     budget_2018, 
                     budget_2019 )

budget <- budget%>% rename(int_code = `Int. Kód`) #156 984

#saveRDS(budget, file = "health_budget.rds")

#creating institution and village keys

#checking and cleaning unique villages
budget <- readRDS("health_budget.rds")

budget_villages <- budget %>%
  select(int_code, Irányítószám, Település)

colSums(is.na(budget_villages)) #156 984 rows
budget_villages <- budget_villages[!is.na(budget_villages$int_code), ] #156 973 rows
budget_villages  <- budget_villages %>% mutate(Település= tolower(Település))

unique_int_codes <- group_by(budget_villages,int_code) %>% slice(1)
saveRDS (unique_int_codes, "unique_int_codes.rds")

#creating village keys
unique_villages <- group_by(budget_villages, Település) %>% slice(1)

unique_villages$Település2 <- unique_villages$Település

#adding village names for colums where post code was miscoded as village name
unique_villages$Település2[unique_villages$Település == "6455"] <- "katymar" 
unique_villages$Település2[unique_villages$Település == "3300. eger"] <- "eger"
unique_villages$Település2[unique_villages$Település == "3132 nógrádmegyer"] <- "nogradmegyer"

#removing rows where village name was not given, removing punctuation marks
unique_villages <- unique_villages[!(unique_villages$Település2 =="NA"),]
unique_villages$Település2 = gsub("//.", "-",unique_villages$Település2)
unique_villages$Település2= gsub("//,", "-", unique_villages$Település2)
unique_villages$Település2 = gsub("-.*", "", unique_villages$Település2)
unique_villages$Település2 = gsub(" .*", "", unique_villages$Település2)
unique_villages$Település2 = gsub(",.*", "", unique_villages$Település2)

#coding all postcodes starting with 1 as Budapest 
unique_villages$Település2[grepl("^1", unique_villages$Irányítószám)] <- "budapest"
#creating a komoró and kömörő variable
unique_villages$Település2[unique_villages$Település2 == "komoró"] <- "komoro1"
unique_villages$Település2[unique_villages$Település2 == "vp."] <- "veszprem"

#removing hungarian accents
unique_villages$Település2 <- stri_trans_general(unique_villages$Település2, "Latin-ASCII")

#correcting typos and some villages district which were coded to their own name instead of the village they belong to
unique_villages$Település2[unique_villages$Település2 == "agard"] <- "gardony"
unique_villages$Település2[unique_villages$Település2 == "ajkarendek"] <- "ajka"
unique_villages$Település2[unique_villages$Település2 == "batorterenye"] <- "batonyterenye"
unique_villages$Település2[unique_villages$Település2 == "blatonszentgyorgy"] <- "balatonszentgyorgy"
unique_villages$Település2[unique_villages$Település2 == "bukfurdo"] <- "buk"
unique_villages$Település2[unique_villages$Település2 == "debrecn"] <- "debrecen"
unique_villages$Település2[unique_villages$Település2 == "derekegyhaza"] <- "derekegyhaz"
unique_villages$Település2[unique_villages$Település2 == "fuzfogyartelep"] <- "balatonfuzfo"
unique_villages$Település2[unique_villages$Település2 == "hmvhely"] <- "hodmezovasarhely"
unique_villages$Település2[unique_villages$Település2 == "kazinbarcika"] <- "kazincbarcika"
unique_villages$Település2[unique_villages$Település2 == "matrafured"] <- "gyongyos"
unique_villages$Település2[unique_villages$Település2 == "matrahaza"] <- "gyongyos"
unique_villages$Település2[unique_villages$Település2 == "miksolc"] <- "miskolc"
unique_villages$Település2[unique_villages$Település2 == "mozs"] <- "tolna"
unique_villages$Település2[unique_villages$Település2 == "debrcen"] <- "debrecen"
unique_villages$Település2[unique_villages$Település2 == "nyhaza"] <- "nyiregyhaza"
unique_villages$Település2[unique_villages$Település2 == "orsoshaza"] <- "oroshaza"
unique_villages$Település2[unique_villages$Település2 == "paradfurdo"] <- "parad"
unique_villages$Település2[unique_villages$Település2 == "peremartongyartelep"] <- "berhida"
unique_villages$Település2[unique_villages$Település2 == "pusztaegres"] <- "sarbogard"
unique_villages$Település2[unique_villages$Település2 == "satoraljaujjhely"] <- "satoraljaujhely"
unique_villages$Település2[unique_villages$Település2 == "selyp"] <- "lorinci"
unique_villages$Település2[unique_villages$Település2 == "sikonda"] <- "komlo"
unique_villages$Település2[unique_villages$Település2 == "sostofurdo"] <- "nyiregyhaza"
unique_villages$Település2[unique_villages$Település2 == "sostogyogyfurdo"] <- "nyiregyhaza"
unique_villages$Település2[unique_villages$Település2 == "szekesfeherfvar"] <- "szekesfehervar"
unique_villages$Település2[unique_villages$Település2 == "szekesfehevar"] <- "szekesfehervar"
unique_villages$Település2[unique_villages$Település2 == "szilasliget"] <- "kerepes"
unique_villages$Település2[unique_villages$Település2== "vassarosnameny"] <- "vasarosnameny"
unique_villages$Település2[unique_villages$Település2== "aatistvanfalva"] <- "apatistvanfalva"
unique_villages$Település2[unique_villages$Település2== "bakonycsenye"] <- "bakonycsernye"
unique_villages$Település2[unique_villages$Település2== "bekecsaba"] <- "bekescsaba"
unique_villages$Település2[unique_villages$Település2== "biatobagy"] <- "biatorbagy"
unique_villages$Település2[unique_villages$Település2== "domdod"] <- "domsod"
unique_villages$Település2[unique_villages$Település2== "gyorszentivan"] <- "gyor"
unique_villages$Település2[unique_villages$Település2== "jeszfenyszaru"] <- "jaszfenyszaru"
unique_villages$Település2[unique_villages$Település2== "ketegyaza"] <- "ketegyhaza"
unique_villages$Település2[unique_villages$Település2== "kiskunkachaza"] <- "kiskunlachaza"
unique_villages$Település2[unique_villages$Település2== "negyecsed"] <- "nagyecsed"
unique_villages$Település2[unique_villages$Település2== "nyergesujfaalu"] <- "nyergesujfalu"
unique_villages$Település2[unique_villages$Település2== "pannonhalna"] <- "pannonhalma"
unique_villages$Település2[unique_villages$Település2== "satoraljaujjhely"] <- "satoraljaujhely"
unique_villages$Település2[unique_villages$Település2== "taktaharhany"] <- "taktaharkany"
unique_villages$Település2[unique_villages$Település2== "veresegyhaza"] <- "veresegyhaz"
unique_villages$Település2[unique_villages$Település2== "tatarszentgyogy"] <- "tatarszentgyorgy"
unique_villages$Település2[unique_villages$Település2== "szentbekalla"] <- "szentbekkalla"

saveRDS (unique_villages, "unique_villages.rds")

#checking if recoding was okay
unique_villages2 <-  unique_villages %>% 
  group_by(Település2) %>% 
  summarise(Exp11=paste(Település, collapse=' °% '))

# KSH village database for sub-county

#merging with sub-county codes
key_master <- read_excel("cleaned_data/data_for_analysis/village_codes_post_codes_2.xlsx")
key_master <- key_master %>% mutate(village_name = tolower(village_name))
key_master$village_name[key_master$village_name == "komoró"] <- "komoro1"
key_master$village_name <- stri_trans_general(key_master$village_name, "Latin-ASCII")

colSums(is.na(village_key_master))
#missing values are non-hungarian villages so we can delete them
village_key_master <- village_key_master[!is.na(village_key_master$SC_code), ]
#colSums(is.na(village_key_master))

saveRDS (village_key_master, "village_key.rds")

#opening village keys
budget <- readRDS("health_budget.rds")
village_key <- readRDS("village_key.rds")
unique_villages_raw <- readRDS("unique_villages.rds")

unique_villages_raw$Település <- stri_trans_general(unique_villages_raw$Település, "Latin-ASCII")
unique_villages_raw$Település2[unique_villages_raw$int_code == "4946"] <- "godollo"

#unique_villages33 <-  unique_villages_raw2 %>% #number of unique int_codes match 2158
#  group_by(int_code) %>% 
#  summarise(Exp11=paste(Település2, collapse=' °% '))

#village_key_master2 <- merge(unique_villages_raw2, village_key, by = "Település2", all.x = TRUE)
village_key_master2 <- merge(unique_villages_raw, village_key, by = "Település2", all.x = TRUE)

#unique_villages3 <-  village_key_master2 %>% #number of unique int_codes match 2158
#  group_by(int_code) %>% 
#  summarise(Exp11=paste(Település2, collapse=' °% '))

#unique_villages3 <-  village_key_master2 %>% 
#  group_by(Település2) %>% 
#  summarise(Exp11=paste(int_code, collapse=' °% '))

#remove missing SC codes (non-hungarian villages)
village_key_master2 <- village_key_master2[!is.na(village_key_master2$SC_code), ]

unique_master_villages <- group_by(village_key_master2, Település) %>% slice(1)

unique_master_key <- unique_master_villages %>%
  select(int_code, Település2, Település, SC_code)

key_5 <- budget %>% mutate(Település = tolower(Település))
key_5$Település <- stri_trans_general(key_5$Település, "Latin-ASCII")

village_key_merged <- merge(key_5, unique_master_key, by = "Település", all.x = TRUE)

unique_master_key5 <- village_key_merged %>%
  select(int_code.x, Irányítószám, Település2, Település, SC_code)

#missing values have no location data available (post_code, village_name), so I can delete them
unique_master_keys <- unique_master_key5[!is.na(unique_master_key5$SC_code), ] 
#previously: 156 984 rows, after #145 575rows

unique_master_keys_villages <- group_by(unique_master_keys, int_code.x) %>% slice(1)

#institution codes which changed village 
unique_master_keys_villages$Település2[unique_master_keys_villages$int_code.x == "0634" |
                                       unique_master_keys_villages$int_code.x == "1902"|
                                       unique_master_keys_villages$int_code.x == "5831" |
                                       unique_master_keys_villages$int_code.x == "K984" ] <- NA

unique_master_keys_villages$SC_code[unique_master_keys_villages$int_code.x == "0634" |
                                         unique_master_keys_villages$int_code.x == "1902"|
                                         unique_master_keys_villages$int_code.x == "5831" |
                                         unique_master_keys_villages$int_code.x == "K984" ] <- NA

institution_codes <- unique_master_keys_villages %>%
  select(int_code.x, Település2, SC_code)

institution_codes <- institution_codes %>% rename(int_code = int_code.x, village_name = Település2)

saveRDS(institution_codes, "institution_codes.rds")


budget <- readRDS("health_budget.rds")
institution_codes <- readRDS("institution_codes.rds")
#institution codes which changes village
changer_int_codes <- read_xlsx("changer_int_codes.xlsx")

colSums(is.na(institution_codes_merged_raw))

institution_codes_merged_raw <- merge(budget, institution_codes, by = "int_code", all.x = TRUE)

institution_codes_merged_raw2 <- merge(institution_codes_merged_raw, changer_int_codes,
                                      by = c("int_code", "year"), all.x = TRUE)

check <- institution_codes_merged_raw2 %>%
  select(int_code, village_name.x, village_name.y, SC_code, SC_codes)

institution_codes_merged_raw2 <- institution_codes_merged_raw2 %>% 
  mutate(village_name.x = coalesce(village_name.x,village_name.y),
         SC_code = coalesce(SC_code, SC_codes))

colSums(is.na(institution_codes_merged_raw2))

int_codes_raw <- institution_codes_merged_raw2[!is.na(institution_codes_merged_raw2$SC_code), ]

colSums(is.na(int_codes_raw ))

#write.csv(int_codes_raw, "int_codes_raw.csv")

int_codes_raw_cleaned <- int_codes_raw
int_codes_raw_cleaned$out_patient <- int_codes_raw_cleaned$`Járóbetet-szakellátás`

int_codes_raw_cleaned <- int_codes_raw_cleaned %>% 
  mutate(out_patient = coalesce(out_patient,`Járóbeteg-szakellátás...33`))

int_codes_raw_cleaned <- int_codes_raw_cleaned %>% 
  mutate(out_patient = coalesce(out_patient,`Járóbeteg-szakellátás...32`))

colSums(is.na(int_codes_raw_cleaned))

int_codes_raw_cleaned$active_in_patients <- int_codes_raw_cleaned$`Aktív fekvőbeteg-szakellátás`
int_codes_raw_cleaned <- int_codes_raw_cleaned %>% 
  mutate(active_in_patients = coalesce(active_in_patients,`Aktív fekvobeteg-szakellátás...33`))

int_codes_raw_cleaned <- int_codes_raw_cleaned %>% 
  mutate(active_in_patients = coalesce(active_in_patients,`Aktív fekvobeteg-szakellátás...35`))

int_codes_raw_cleaned <- int_codes_raw_cleaned %>% 
  mutate(active_in_patients = coalesce(active_in_patients,`Aktív fekvobeteg-szakellátás...34`))

colSums(is.na(int_codes_raw_cleaned))

int_codes_raw_cleaned$extra_financing <- int_codes_raw_cleaned$`Extra-finanszírozás`
int_codes_raw_cleaned <- int_codes_raw_cleaned %>% 
  mutate(extra_financing = coalesce(extra_financing,`Extra finanszírozás`))

int_codes_raw_cleaned$advance_financing <- int_codes_raw_cleaned$`Működési költségelőleg`
int_codes_raw_cleaned <- int_codes_raw_cleaned %>% 
  mutate(advance_financing = coalesce(advance_financing,`Müködési költségelőleg`))

int_codes_raw_cleaned$dialysis <- int_codes_raw_cleaned$`Művese-kezelés`
int_codes_raw_cleaned <- int_codes_raw_cleaned %>% 
  mutate(dialysis = coalesce(dialysis,Művesekezelés))

int_codes_raw_cleaned$deceased_transport <- int_codes_raw_cleaned$Halottszállítás
int_codes_raw_cleaned <- int_codes_raw_cleaned %>% 
  mutate(deceased_transport  = coalesce(deceased_transport, Halottszálítás))

int_codes_raw_cleaned$care <- int_codes_raw_cleaned$Gondozás

int_codes_raw_cleaned$law_enforcement <- int_codes_raw_cleaned$`IM, BVOP`

int_codes_raw_cleaned$extraordinary_financing <- int_codes_raw_cleaned$`Rendkívüli kiadás`
int_codes_raw_cleaned <- int_codes_raw_cleaned %>% 
  mutate(extraordinary_financing   = coalesce(extraordinary_financing, Rend.kiad.))

int_codes_raw_cleaned$transformation <- int_codes_raw_cleaned$`Intézeti átalakítás`

int_codes_raw_cleaned$structural_transformation2 <- int_codes_raw_cleaned$`Strukt.átalakítás`

int_codes_raw_cleaned$debt_support <- int_codes_raw_cleaned$`Adósság konsz. tám.`

int_codes_raw_cleaned$waiting_list <- int_codes_raw_cleaned$`Várólista csökk. és szakmapol. célok`

int_codes_raw_cleaned$expensive_medication <- int_codes_raw_cleaned$`Nagyértékű gyógyszerfin.`


int_codes_cleaned <- int_codes_raw_cleaned %>%
 select(int_code, year, village_name.x, SC_code, Szolgáltató, 
         out_patient, active_in_patients, extra_financing,
         `Háziorvosi szolgálat`, `Háziorvosi ügyelet`, `Iskola egészségügyi ellátás`,
         `Védonoi ellátás`, `Anya-gyermek-és csecsemovédelem`, `Mozgó szakorvosi szolgálat`, 
         `Fogászati ellátás`,  Betegszállítás, `Otthoni szakápolás`, 
         Céleloirányzat, Mentés, `Laboratóriumi ellátás`, CT, `Speciális finanszírozás`,
         `Krónikus fekvobeteg-szakellátás`, expensive_medication, waiting_list, debt_support,
         extraordinary_financing, extra_financing, law_enforcement,
        care, deceased_transport, dialysis, advance_financing, Összesen)

int_codes_cleaned$transformation <- int_codes_raw_cleaned$`Intézeti
átalakítás`

int_codes_cleaned$structural_transformation <- int_codes_raw_cleaned$`Strukt.
átalakítás`

colSums(is.na(int_codes_cleaned))

#saveRDS(int_codes_cleaned, "int_codes_clened.rds")

merged_budget_raw <- readRDS("int_codes_clened.rds")

#rename variables
int_codes_cleaned_final <- merged_budget_raw  %>% 
  rename(village_name = village_name.x,
         healthcare_provider = Szolgáltató,
         GP = `Háziorvosi szolgálat`,
         GP_extra_shift = `Háziorvosi ügyelet`,
         school_healthcare = `Iskola egészségügyi ellátás`,
         family_health_visitor = `Védonoi ellátás`,
         newborn_maternal_care = `Anya-gyermek-és csecsemovédelem`,
         moving_healthcare_prof = `Mozgó szakorvosi szolgálat`,
         dental_care = `Fogászati ellátás`,
         patient_transport = `Betegszállítás`,
         financial_target = `Céleloirányzat`,
         home_care = `Otthoni szakápolás`,
         ambulance = Mentés,
         laboratory = `Laboratóriumi ellátás`,
         special_financing = `Speciális finanszírozás`,
         chronic_in_patient = `Krónikus fekvobeteg-szakellátás`,
         total_finance = `Összesen`)


colSums(is.na(int_codes_cleaned_final))

int_codes_cleaned_final[is.na(int_codes_cleaned_final)] <- 0

saveRDS(int_codes_cleaned_final, "cleaned_data/data_for_analysis/int_codes_cleaned_final.rds")


int_codes_cleaned_final <- int_codes_cleaned_final %>%
  mutate(sum = extra_financing + active_in_patients + out_patient + GP + GP_extra_shift +
  school_healthcare + family_health_visitor + newborn_maternal_care + moving_healthcare_prof +
  dental_care + patient_transport + financial_target + home_care + ambulance + laboratory +
  CT + special_financing + chronic_in_patient + expensive_medication + waiting_list +
  debt_support + extraordinary_financing + law_enforcement + care + deceased_transport +                 
  dialysis + advance_financing + transformation + structural_transformation)

#check wheather the cleaned data is correct
int_codes_cleaned_final$sum <- as.numeric(as.character(int_codes_cleaned_final$sum))
int_codes_cleaned_final$total_finance <- as.numeric(as.character(int_codes_cleaned_final$total_finance))
int_codes_cleaned_final <- int_codes_cleaned_final %>%
  mutate(check = sum == total_finance )

false <- int_codes_cleaned_final[int_codes_cleaned_final$check == FALSE]
#no variables

#Cleaning Koml?/K?mlo and Komor?/K?m?ro

fund <- readRDS('/Users/User/Documents/Thesis/Data/ready_to_use/budget_health_care_fund.rds')
komlo <- filter(fund, village_name == "komlo2")
komoro <- filter(fund, village_name == "komoro2")

fund$village_name[fund$int_code == 1802] <- 'komlo2'
fund$SC_code[fund$int_code == 1802] <- 108
fund$village_name[fund$int_code == 'E723'] <- 'komlo2'
fund$SC_code[fund$int_code == 'E723'] <- 108

fund$village_name[fund$int_code == 'R939'] <- 'komoro2'
fund$SC_code[fund$int_code == 'R939'] <- 160

stadium_dummy$Megnevez?s[stadium_dummy$K?d == 2361] <- 'komoro2'

saveRDS(fund, "/Users/User/Documents/Thesis/Data/ready_to_use/budget_health_care_fund.rds")

fund <- readRDS('/Users/User/Documents/Thesis/Data/ready_to_use/budget_health_care_fund.rds')


fund_SC = fund %>%
  group_by(SC_code, year) %>%
  dplyr::summarize(out_patient = sum(out_patient),
                   active_in_patients = sum(active_in_patients),
                   extra_financing = sum(extra_financing),
                   GP_extra_shift = sum(GP_extra_shift),
                   school_healthcare = sum(school_healthcare),
                   family_health_visitor = sum(family_health_visitor),
                   newborn_maternal_care = sum(newborn_maternal_care),
                   moving_healthcare_prof= sum(moving_healthcare_prof),
                   dental_care = sum(dental_care),
                   patient_transport = sum(patient_transport),
                   home_care = sum(home_care),
                   financial_target = sum(financial_target),
                   ambulance = sum(ambulance),
                   laboratory = sum(laboratory),
                   CT = sum(CT),
                   special_financing = sum(special_financing),
                   chronic_in_patient = sum(chronic_in_patient),
                   expensive_medication = sum(expensive_medication),
                   waiting_list = sum(waiting_list),
                   debt_support = sum(debt_support),
                   extraordinary_financing = sum(extraordinary_financing),
                   law_enforcement = sum(law_enforcement),
                   care = sum(care),
                   deceased_transport = sum(deceased_transport),
                   dialysis = sum(dialysis),
                   advance_financing = sum(advance_financing),
                   total_finance = sum(total_finance),
                   transformation= sum(transformation),
                   structural_transformation = sum(structural_transformation),
                   GP = sum(GP))

saveRDS(fund_SC, "/Users/User/Documents/Thesis/Data/ready_to_use/SC/budget_health_care_fund_SC.rds")

budget_key <- budget_health_care_fund %>% select(int_code, year, village_name, healthcare_provider)

budget_key <- budget_key[!duplicated(budget_key$int_code), ]
budget_key$healthcare_provider <- tolower(budget_key$healthcare_provider)
budget_key$healthcare_provider <- stri_trans_general(budget_key$healthcare_provider, "Latin-ASCII")

hospital <- dplyr::filter(budget_key, grepl('kiskunfelegyhaza', village_name))
hospital_keys <- dplyr::filter(budget_key, grepl('korhaz|vkh|kh', healthcare_provider))

budget <- readRDS('Data/ready_to_use/SC/budget_health_care_fund_SC.rds')

budget$GP_total <- budget$GP + budget$GP_extra_shift
budget$GP_ratio <- budget$GP_total/budget$total_finance
budget$out_patient_ratio <- budget$out_patient/budget$total_finance
budget$year <- as.numeric(budget$year)
budget$SC_code <- as.numeric(budget$SC_code)


population <- read.csv('Data/ready_to_use/SC/population.csv')
population$SC_code <- as.numeric(population$SC_code)
population$year <- as.numeric(population$year)


budget <- merge(budget, population, by = c('year', 'SC_code'))

budget$GP_percapita = budget$GP_total/budget$population_total*1000
budget$outpatient_percapita = budget$out_patient/budget$population_total*1000
budget$total_percapita = budget$total_finance/budget$population_total*1000

CPI <- read.csv('Data/ready_to_use/SC/CPI.csv')

budget <- merge(budget, CPI, by = 'year')

budget$GP_percapita_CPI = budget$GP_percapita*budget$CPI_basis_2014
budget$outpatient_percapita_CPI = budget$outpatient_percapita*budget$CPI_basis_2014
budget$total_percapita_CPI = budget$total_percapita*budget$CPI_basis_2014

budget_SC <- budget %>% select(SC_code, year, GP_ratio, out_patient_ratio, GP_percapita,
                               outpatient_percapita, total_percapita, GP_percapita_CPI,
                               outpatient_percapita_CPI, total_percapita_CPI)

write.csv(budget_SC, 'Data/ready_to_use/SC/budget_SC.csv', row.names = FALSE)

####

budget_SC = budget  %>%
  group_by(SC_code, year) %>%
  dplyr::summarize(out_patient = sum(out_patient),
                   active_in_patients = sum(active_in_patients),
                   extra_financing = sum(extra_financing),
                   GP_extra_shift = sum(GP_extra_shift),
                   school_healthcare = sum(school_healthcare),
                   family_health_visitor = sum(family_health_visitor),
                   newborn_maternal_care = sum(newborn_maternal_care),
                   moving_healthcare_prof= sum(moving_healthcare_prof),
                   dental_care = sum(dental_care),
                   patient_transport = sum(patient_transport),
                   home_care = sum(home_care),
                   financial_target = sum(financial_target),
                   ambulance = sum(ambulance),
                   laboratory = sum(laboratory),
                   CT = sum(CT),
                   special_financing = sum(special_financing),
                   chronic_in_patient = sum(chronic_in_patient),
                   expensive_medication = sum(expensive_medication),
                   waiting_list = sum(waiting_list),
                   debt_support = sum(debt_support),
                   extraordinary_financing = sum(extraordinary_financing),
                   law_enforcement = sum(law_enforcement),
                   care = sum(care),
                   deceased_transport = sum(deceased_transport),
                   dialysis = sum(dialysis),
                   advance_financing = sum(advance_financing),
                   total_finance = sum(total_finance),
                   transformation= sum(transformation),
                   structural_transformation = sum(structural_transformation),
                   GP = sum(GP))

budget_SC$GP_total <- budget_SC$GP + budget_SC$GP_extra_shift
budget_SC$GP_budget_ratio <- budget_SC$GP_total/budget_SC$total_finance
budget_SC$out_patient_budget_ratio <- budget_SC$out_patient/budget_SC$total_finance
budget_SC$CT_budget_ratio <- budget_SC$CT/budget_SC$total_finance
budget_SC$laboratory_budget_ratio <- budget_SC$laboratory/budget_SC$total_finance

budget_SC$year <- as.numeric(budget_SC$year)
budget_SC$SC_code <- as.numeric(budget_SC$SC_code)

budget_SC <- merge(budget_SC, population, by = c('year', 'SC_code'), all.x = TRUE)
budget_SC <- merge(budget_SC, CPI, by = 'year')

budget_SC$budget_total_percapita <- (budget_SC$total_finance*budget_SC$CPI_basis_2014)/budget_SC$population_total
budget_SC$budget_outpatient_percapita<- (budget_SC$GP_total*budget_SC$CPI_basis_2014)/budget_SC$population_total
budget_SC$budget_GP_percapita <-(budget_SC$out_patient*budget_SC$CPI_basis_2014)/budget_SC$population_total
budget_SC$budget_CT_percapita <- (budget_SC$CT*budget_SC$CPI_basis_2014)/budget_SC$population_total
budget_SC$budget_laboratory_percapita<- (budget_SC$laboratory*budget_SC$CPI_basis_2014)/budget_SC$population_total

budget_SC <- budget_SC %>% select(year, SC_code, GP_budget_ratio, out_patient_budget_ratio, CT_budget_ratio, laboratory_budget_ratio,
                                  budget_total_percapita, budget_outpatient_percapita, budget_GP_percapita, budget_CT_percapita, budget_laboratory_percapita)


write.csv(budget_SC, 'DATA/ready_to_use/budget_SC.csv')


#-------------------------------------------------------------------------------

#tax payers
#-------------------------------------------------------------------------------
####tax payers data
setwd("/Users/User/Documents/Thesis/Data/raw_data/tax_payers")

d2004 <- read_excel("tax_payers/adozok_2004.xls")
d2005 <- read_excel("tax_payers/adozok_2005.xls")
d2006 <- read_excel("tax_payers/adozok_2006.xls")
d2007 <- read_excel("tax_payers/adozok_2007.xls")
d2008 <- read_excel("tax_payers/adozok_2008.xls")
d2009 <- read_excel("tax_payers/adozok_2009.xls")
d2010 <- read_excel("tax_payers/adozok_2010.xls")
d2011 <- read_excel("tax_payers/adozok_2011.xls")
d2012 <- read_excel("tax_payers/adozok_2012.xls")
d2013 <- read_excel("tax_payers/adozok_2013.xls")
d2014 <- read_excel("tax_payers/adozok_2014.xls")
d2015 <- read_excel("tax_payers/adozok_2015.xls")
d2016 <- read_excel("tax_payers/adozok_2016.xls")
d2017 <- read_excel("tax_payers/adozok_2017.xls")
d2018 <- read_excel("tax_payers/adozok_2018.xls")

d2004$year <- 2004
d2005$year <- 2005
d2006$year <- 2006
d2007$year <- 2007
d2008$year <- 2008
d2009$year <- 2009
d2010$year <- 2010
d2011$year <- 2011
d2012$year <- 2012
d2013$year <- 2013
d2014$year <- 2014
d2015$year <- 2015
d2016$year <- 2016
d2017$year <- 2017
d2018$year <- 2018

tax_payers <- rbind.fill(d2004, d2005, d2006, d2007, d2008, d2009, d2010,
                         d2011,d2012,d2013,d2014,d2015,d2016,d2017,d2018)

tax_payers <- tax_payers %>% 
  rename(
    SC_name = Megnevezés,
    SC_code = Kód )

write.csv(tax_payers, "/Users/User/Documents/Thesis/Data/cleaned_data/data_for_analysis/tax_payers.csv")

#-------------------------------------------------------------------------------

#HDI 
#-------------------------------------------------------------------------------
setwd("/Users/User/Documents/Thesis")

HDI <- as.data.table(read_excel("Data/ready_to_use/sub_county_HDI.xlsx", sheet = 'GVI' ))

#histogramm of HDIs
hist.data.frame(HDI, nclass=10)

#round them up to 2 digits
HDI <- HDI %>% mutate(across(3:6, round, 2))

#calculate means
mean_2005 <- round(mean(HDI$a2005),2)
mean_2012 <- round(mean(HDI$a2012),2)
mean_2014 <- round(mean(HDI$a2014, na.rm = TRUE),2)
mean_2017 <- round(mean(HDI$a2017, na.rm = TRUE),2)

#difference from means
HDI <- HDI  %>% mutate(diff_2005 = a2005-mean_2005)
HDI <- HDI  %>% mutate(diff_2012 = a2012-mean_2012)
HDI <- HDI  %>% mutate(diff_2014 = a2014-mean_2014)
HDI <- HDI  %>% mutate(diff_2017 = a2017-mean_2017)

#dpositive and negative differences
HDI <- HDI %>% mutate(positive2005 = case_when(diff_2005 <= 0  ~ 0,
                                               diff_2005 > 0   ~ 1))

HDI <- HDI %>% mutate(positive2012 = case_when(diff_2012 <= 0  ~ 0,
                                               diff_2012 > 0   ~ 1))

HDI <- HDI %>% mutate(positive2014 = case_when(diff_2014 <= 0  ~ 0,
                                               diff_2014 > 0   ~ 1))

HDI <- HDI %>% mutate(positive2017 = case_when(diff_2017 <= 0  ~ 0,
                                               diff_2017 > 0   ~ 1))

#calculate average differences
average_difference_2005 <- HDI %>%             
  group_by(positive2005) %>%                         
  summarise_at(vars(diff_2005),              
               list(mean = mean))

average_difference_2012 <- HDI %>%             
  group_by(positive2012) %>%                         
  summarise_at(vars(diff_2012),              
               list(mean = mean))

average_difference_2014 <- HDI %>%             
  group_by(positive2014) %>%                         
  summarise_at(vars(diff_2014),              
               list(mean = mean))

average_difference_2017 <- HDI %>%             
  group_by(positive2017) %>%                         
  summarise_at(vars(diff_2017),              
               list(mean = mean))
average_difference_2005$mean[1]
average_difference_2005$mean[2]

#creating categories
HDI <- HDI %>% mutate(HDI_cat2005 = case_when(
  a2005 <= mean_2005+average_difference_2005$mean[1] ~ 1,
  a2005 < mean_2005 & a2005 >= mean_2005+average_difference_2005$mean[1] ~ 2,
  a2005 >= mean_2005 & a2005 < mean_2005+average_difference_2005$mean[2] ~ 3,
  a2005 >= mean_2005+average_difference_2005$mean[2] ~ 4))

HDI <- HDI %>% mutate(HDI_cat2012 = case_when(
  a2012 <= mean_2012+average_difference_2012$mean[1] ~ 1,
  a2012 < mean_2012 & a2012 >= mean_2012+average_difference_2012$mean[1] ~ 2,
  a2012 >= mean_2012 & a2012 < mean_2012+average_difference_2012$mean[2] ~ 3,
  a2012 >= mean_2012+average_difference_2012$mean[2] ~ 4))

HDI <- HDI %>% mutate(HDI_cat2014 = case_when(
  a2014 <= mean_2014+average_difference_2014$mean[1] ~ 1,
  a2014 < mean_2014 & a2014 >= mean_2014+average_difference_2014$mean[1] ~ 2,
  a2014 >= mean_2014 & a2014 < mean_2014+average_difference_2014$mean[2] ~ 3,
  a2014 >= mean_2014+average_difference_2014$mean[2] ~ 4))

HDI <- HDI %>% mutate(HDI_cat2017 = case_when(
  a2017 <= mean_2014+average_difference_2017$mean[1] ~ 1,
  a2017 < mean_2014 & a2017 >= mean_2017+average_difference_2017$mean[1] ~ 2,
  a2017 >= mean_2014 & a2017 < mean_2017+average_difference_2017$mean[2] ~ 3,
  a2017 >= mean_2014+average_difference_2017$mean[2] ~ 4))

HDI_cat = HDI %>% select(SC_code,SC_name, HDI_cat2005, HDI_cat2012, HDI_cat2014, HDI_cat2017)

HDI_cat = rename(HDI_cat, 2005 = HDI_cat2005,
                 2012 = HDI_cat2012,
                 2014 = HDI_cat2014,
                 2017 = HDI_cat2017)

names(HDI_cat) <- c('SC_code', 'SC_name', 2005, 2012, 2014, 2017)

HDI_cat = melt(HDI_cat, id=1:2, variable.name = 'year', value.name = 'HDI')

write.csv(HDI_cat, 'Data/ready_to_use/HDI_cat.csv')

#HDI <- read.csv('Data/ready_to_use/HDI_cat.csv')

#HDI$SC_name <- tolower(HDI$SC_name)
#HDI$SC_name <- stri_trans_general(HDI$SC_name, "Latin-ASCII")

#village_key <- as.data.table(read.csv('Data/ready_to_use/village_codes_final.csv'))

#SC_key_village <- village_key %>% select(SC_code, SC_name)
#SC_key_village$SC_name <- tolower(SC_key_village$SC_name)
#SC_key_village$SC_name <- stri_trans_general(SC_key_village$SC_name, "Latin-ASCII")

#SC_key_village  <- unique( SC_key_village[] )

#HDI <- merge(HDI, SC_key_village, by='SC_name')
#-------------------------------------------------------------------------------

#environmental data
#-------------------------------------------------------------------------------
aid_data <- as.data.table(read.csv("Data/ready_to_use_copy/aid_data_final.csv"))

colnames(aid_data)

aid_data <- aid_data %>% rename(
                ozone_2005 = ambient_air_pollution_2013_o3.2005.mean, 
                ozone_2010 = ambient_air_pollution_2013_o3.2010.mean,
                ozone_2011 = ambient_air_pollution_2013_o3.2011.mean, 
                ozone_2012 = ambient_air_pollution_2013_o3.2012.mean,
                ozone_2013 = ambient_air_pollution_2013_o3.2013.mean,
                
                particulate_matter_2005 = ambient_air_pollution_2013_fus_calibrated.2005.mean, 
                particulate_matter_2010 = ambient_air_pollution_2013_fus_calibrated.2010.mean,
                particulate_matter_2011 = ambient_air_pollution_2013_fus_calibrated.2011.mean, 
                particulate_matter_2012 = ambient_air_pollution_2013_fus_calibrated.2012.mean,
                particulate_matter_2013 = ambient_air_pollution_2013_fus_calibrated.2013.mean,
                
                precipication_2004 = cru_pre_yearly_mean.2004.mean, 
                precipication_2005 = cru_pre_yearly_mean.2005.mean,
                precipication_2006 = cru_pre_yearly_mean.2006.mean, 
                precipication_2007 = cru_pre_yearly_mean.2007.mean,
                precipication_2008 = cru_pre_yearly_mean.2008.mean,
                precipication_2009 = cru_pre_yearly_mean.2009.mean, 
                precipication_2010 = cru_pre_yearly_mean.2010.mean,
                precipication_2011 = cru_pre_yearly_mean.2011.mean, 
                precipication_2012 = cru_pre_yearly_mean.2012.mean,
                precipication_2013 = cru_pre_yearly_mean.2013.mean,
                precipication_2014 = cru_pre_yearly_mean.2014.mean, 
                precipication_2015 = cru_pre_yearly_mean.2015.mean,
                precipication_2016 = cru_pre_yearly_mean.2016.mean,
                
                co2_2015 = oco2_xco2_yearly.2015.mean, 
                co2_2016 = oco2_xco2_yearly.2016.mean,
                co2_2017 = oco2_xco2_yearly.2017.mean,
                co2_2018 = oco2_xco2_yearly.2018.mean)

aid_data_cleaned <- aid_data %>% select(village_name_no_accents, SC_code,
                                        ozone_2005, 
                                        ozone_2010,
                                        ozone_2011, 
                                        ozone_2012,
                                        ozone_2013,
                                        
                                        particulate_matter_2005, 
                                        particulate_matter_2010,
                                        particulate_matter_2011, 
                                        particulate_matter_2012,
                                        particulate_matter_2013,
                                        
                                        precipication_2004, 
                                        precipication_2005,
                                        precipication_2006, 
                                        precipication_2007,
                                        precipication_2008,
                                        precipication_2009, 
                                        precipication_2010,
                                        precipication_2011, 
                                        precipication_2012,
                                        precipication_2013,
                                        precipication_2014, 
                                        precipication_2015,
                                        precipication_2016,
                                        
                                        co2_2015, 
                                        co2_2016,
                                        co2_2017,
                                        co2_2018)

aid_data_cleaned$SC_code[aid_data_cleaned$SC_code == 17] <- 999

ozone <- aid_data_cleaned %>% select(village_name_no_accents, SC_code,
                                        ozone_2005, 
                                        ozone_2010,
                                        ozone_2011, 
                                        ozone_2012,
                                        ozone_2013)

particulate_matter <- aid_data_cleaned %>% select(village_name_no_accents, SC_code,
                                        particulate_matter_2005, 
                                        particulate_matter_2010,
                                        particulate_matter_2011, 
                                        particulate_matter_2012,
                                        particulate_matter_2013)

precipication <- aid_data_cleaned %>% select(village_name_no_accents, SC_code,
                                        precipication_2004, 
                                        precipication_2005,
                                        precipication_2006, 
                                        precipication_2007,
                                        precipication_2008,
                                        precipication_2009, 
                                        precipication_2010,
                                        precipication_2011, 
                                        precipication_2012,
                                        precipication_2013,
                                        precipication_2014, 
                                        precipication_2015,
                                        precipication_2016)

co2 <- aid_data_cleaned %>% select(village_name_no_accents, SC_code,
                                        co2_2015, 
                                        co2_2016,
                                        co2_2017,
                                        co2_2018)

ozone <- ozone %>% rename(
  '2005' = ozone_2005, 
  '2010' = ozone_2010,
  '2011' = ozone_2011, 
  '2012' = ozone_2012,
  '2013' = ozone_2013)

particulate_matter <- particulate_matter %>% rename(
  '2005' = particulate_matter_2005, 
  '2010' = particulate_matter_2010,
  '2011' = particulate_matter_2011, 
  '2012' = particulate_matter_2012,
  '2013' = particulate_matter_2013)

precipication <- precipication %>% rename(
  '2004' = precipication_2004, 
  '2005' = precipication_2005,
  '2006' = precipication_2006, 
  '2007' = precipication_2007,
  '2008' = precipication_2008,
  '2009' = precipication_2009, 
  '2010' = precipication_2010,
  '2011' = precipication_2011, 
  '2012' = precipication_2012,
  '2013' = precipication_2013,
  '2014' = precipication_2014, 
  '2015' = precipication_2015,
  '2016' = precipication_2016)

co2 <- co2 %>% rename(
  '2015' = co2_2015, 
  '2016' = co2_2016,
  '2017' = co2_2017,
  '2018' = co2_2018)

ozone = melt(ozone, id=1:2, variable.name = 'year', value.name = 'ozone')
ozone = ozone %>%
  group_by_at(c('village_name_no_accents', 'year')) %>%
  #group_by(.dots=c('village_name_no_accents', 'year')) %>%
  dplyr::summarize(ozone = mean(ozone))

particulate_matter = melt(particulate_matter, id=1:2, variable.name = 'year', value.name = 'particulate_matter')
particulate_matter = particulate_matter %>%
  group_by(village_name_no_accents, year) %>%
  dplyr::summarize(particulate_matter = mean(particulate_matter))

precipication = melt(precipication, id=1:2, variable.name = 'year', value.name = 'precipication')
precipication = precipication %>%
  group_by(village_name_no_accents, year) %>%
  dplyr::summarize(precipication = mean(precipication))

co2 = melt(co2, id=1:2, variable.name = 'year', value.name = 'co2')
co2 = co2 %>%
  group_by(village_name_no_accents, year) %>%
  dplyr::summarize(co2 = mean(co2))

full <- merge(ozone, particulate_matter, by = c('village_name_no_accents', 'year'), all = TRUE)
full2 <- merge(full, precipication, by = c('village_name_no_accents', 'year'), all = TRUE)
full3 <- merge(full2, co2, by = c('village_name_no_accents','year'), all = TRUE)

aid_data_cleaned_final <- full3 %>% rename(
  village_name = village_name_no_accents)

write.csv(aid_data_cleaned_final, 'Data/ready_to_use/aid_data_cleaned_final.csv')

env_data <- read.csv('Data/ready_to_use/SC/aid_data_cleaned_final.csv')
village_key <- read.csv('Data/ready_to_use/SC/village_key_merge.csv')

env_data <- merge(env_data, village_key, by.x = 'village_name', by.y = 'village_name_no_accents')

#write.csv(env_data, 'Data/ready_to_use/aid_data_cleaned_final.csv')

env_data_SC =  env_data %>%
  group_by(SC_code, year) %>%
  dplyr::summarize(ozone = mean(ozone),
                   particulate_matter = mean(particulate_matter),
                   precipication = mean(precipication),
                   co2 = mean(co2))

write.csv(env_data_SC, 'Data/ready_to_use/SC/aid_data_cleaned_final_SC.csv')


#-------------------------------------------------------------------------------

#unemployment ratio
#-------------------------------------------------------------------------------
unemployment <- as.data.table(read_excel("Data/ready_to_use_copy/unemployment_ratio.xls" ))
unemployment = melt(unemployment, id=1:2, variable.name = 'year', value.name = 'unemployment_ratio')
write.csv(unemployment, 'Data/ready_to_use/unemployment.csv')
#-------------------------------------------------------------------------------

#territory of woods
#-------------------------------------------------------------------------------
territory_woods <- as.data.table(read_excel("Data/ready_to_use_copy/territory_woods.xls" ))
territory_woods = melt(territory_woods, id=1:2, variable.name = 'year', value.name = 'territory_woods')
write.csv(territory_woods, 'Data/ready_to_use/territory_woods.csv')
#-------------------------------------------------------------------------------

#state owned roads
#-------------------------------------------------------------------------------
state_owned_roads <- as.data.table(read_excel("Data/ready_to_use_copy/state_owned_rows_km.xls" ))
state_owned_roads = melt(state_owned_roads, id=1:2, variable.name = 'year', value.name = 'state_owned_roads')
write.csv(state_owned_roads, 'Data/ready_to_use/state_owned_roads.csv')
#-------------------------------------------------------------------------------

#stadiums and sport centers
#-------------------------------------------------------------------------------
stadium_dummy <- as.data.table(read_excel("Data/ready_to_use_copy/stadium_dummy.xls" ))
stadium_dummy = melt(stadium_dummy, id=1:2, variable.name = 'year', value.name = 'stadium')
stadium_dummy <- stadium_dummy[ , Megnevez?s := tolower(Megnevez?s) ]
stadium_dummy$Megnevez?s[stadium_dummy$K?d == 1453] <- 'komlo2'
stadium_dummy$Megnevez?s[stadium_dummy$K?d == 2361] <- 'komoro2'
stadium_dummy$Megnevez?s <- stri_trans_general(stadium_dummy$Megnevez?s, "Latin-ASCII")
village_key_total <- read.csv("Data/ready_to_use/village_codes_final.csv" )
village_key <- village_key_total %>% select(village_name_no_accents, SC_code)

stadiums_dummy_2 <- merge(stadium_dummy, village_key, by.x = 'Megnevez?s', by.y = "village_name_no_accents")

stadiums =  stadiums_dummy_2 %>%
  group_by(SC_code, year) %>%
  dplyr::summarize(stadium = sum(stadium))

write.csv(stadiums, 'Data/ready_to_use/stadiums.csv')
#-------------------------------------------------------------------------------

#respiratory_patients
#-------------------------------------------------------------------------------
respiratory_patients <- as.data.table(read_excel("Data/ready_to_use_copy/respiratory_patients.xls" ))
respiratory_patients = melt(respiratory_patients, id=1:2, variable.name = 'year', value.name = 'respiratory_patients')
respiratory_patients <- respiratory_patients[ , Megnevez?s := tolower(Megnevez?s) ]
respiratory_patients$Megnevez?s[respiratory_patients$K?d == 1453] <- 'komlo2'
respiratory_patients$Megnevez?s[respiratory_patients$K?d == 2361] <- 'komoro2'
respiratory_patients$Megnevez?s <- stri_trans_general(respiratory_patients$Megnevez?s, "Latin-ASCII")
write.csv(respiratory_patients, 'Data/ready_to_use/respiratory_patients.csv')
#-------------------------------------------------------------------------------

#prescription exemption certificate
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/cleaned_data/Prescription_exemption_certificate.xls" ))
data = melt(data, id=1:2, variable.name = 'year', value.name = 'presciption_exemption_certificate')
data$ratio = data$presciption_exemption_certificate/1000
data <- data[ , N?v := tolower(N?v) ]
data$N?v <- stri_trans_general(data$N?v, "Latin-ASCII")
write.csv(data, 'Data/ready_to_use/SC/prescription_exemption_certificate.csv', row.names = FALSE)
#-------------------------------------------------------------------------------

#population
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/ready_to_use_copy/actual_population.xls" ))
data = melt(data, id=1:2, variable.name = 'year', value.name = 'population_total')

data2 <- as.data.table(read_excel("Data/ready_to_use_copy/population_male.xls" ))
data2 = melt(data2, id=1:2, variable.name = 'year', value.name = 'population_male')

data <- merge(data, data2, by=c('Megnevez?s', 'K?d', "year"))
data <- data[ , Megnevez?s := tolower(Megnevez?s) ]
data$Megnevez?s <- stri_trans_general(data$Megnevez?s, "Latin-ASCII")
write.csv(data, 'Data/ready_to_use/population.csv')
#-------------------------------------------------------------------------------

#migration
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/ready_to_use_copy/outward_migration.xls" ))
data = melt(data, id=1:2, variable.name = 'year', value.name = 'outward_migration')

data2 <- as.data.table(read_excel("Data/ready_to_use_copy/inward_migration.xls" ))
data2 = melt(data2, id=1:2, variable.name = 'year', value.name = 'inward_migration')

data <- merge(data, data2, by=c('Megnevez?s', 'K?d', "year"))
data <- data[ , Megnevez?s := tolower(Megnevez?s) ]
data$Megnevez?s <- stri_trans_general(data$Megnevez?s, "Latin-ASCII")
write.csv(data, 'Data/ready_to_use/migration.csv')
#-------------------------------------------------------------------------------

#healthcare workers
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/ready_to_use_copy/number_healtcare_workers_higher_degree_male.xls" ))
data = melt(data, id=1:2, variable.name = 'year', value.name = 'healtcare_workers_male_higher')

data2 <- as.data.table(read_excel("Data/ready_to_use_copy/number_healtcare_workers_higher_degree_female.xls" ))
data2 = melt(data2, id=1:2, variable.name = 'year', value.name = 'healthcare_workers_female_higher')

data3 <- as.data.table(read_excel("Data/ready_to_use_copy/healthcare_workers_male.xls" ))
data3 = melt(data3, id=1:2, variable.name = 'year', value.name = 'healtcare_workers_male')

data4 <- as.data.table(read_excel("Data/ready_to_use_copy/healtcare_workers_female.xls" ))
data4 = melt(data4, id=1:2, variable.name = 'year', value.name = 'healthcare_workers_female')

data <- merge(data, data2, by=c('Megnevez?s', 'K?d', "year"))
data <- merge(data, data3, by=c('Megnevez?s', 'K?d', "year"))
data <- merge(data, data4, by=c('Megnevez?s', 'K?d', "year"))

data <- data  %>% mutate(healthcare_workers_total = healtcare_workers_male_higher+healthcare_workers_female_higher+healtcare_workers_male+healthcare_workers_female)
data <- data  %>% mutate(healthcare_workers_higher_total = healtcare_workers_male_higher+healthcare_workers_female_higher)
data <- data %>% select(Megnevez?s, K?d, year, healthcare_workers_total, healthcare_workers_higher_total)
data <- data[ , Megnevez?s := tolower(Megnevez?s) ]
data$Megnevez?s <- stri_trans_general(data$Megnevez?s, "Latin-ASCII")
write.csv(data, 'Data/ready_to_use/healthcare_workers.csv')
#-------------------------------------------------------------------------------

#population
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/ready_to_use_copy/actual_population.xls" ))
data = melt(data, id=1:2, variable.name = 'year', value.name = 'population_total')

data2 <- as.data.table(read_excel("Data/ready_to_use_copy/population_male.xls" ))
data2 = melt(data2, id=1:2, variable.name = 'year', value.name = 'population_male')

data <- merge(data, data2, by=c('Megnevez?s', 'K?d', "year"))
data <- data[ , Megnevez?s := tolower(Megnevez?s) ]
data$Megnevez?s <- stri_trans_general(data$Megnevez?s, "Latin-ASCII")
write.csv(data, 'Data/ready_to_use/population.csv')
#-------------------------------------------------------------------------------

#ambulance stations
#-------------------------------------------------------------------------------
amblance_dummy <- as.data.table(read_excel("Data/ready_to_use_copy/ambulance_dummy.xls" ))
amblance_dummy = melt(amblance_dummy, id=1:2, variable.name = 'year', value.name = 'ambulance')
amblance_dummy <- amblance_dummy[ , Megnevez?s := tolower(Megnevez?s) ]
amblance_dummy$Megnevez?s[amblance_dummy$K?d == 1453] <- 'komlo2'
amblance_dummy$Megnevez?s[amblance_dummy$K?d == 2361] <- 'komoro2'
amblance_dummy$Megnevez?s <- stri_trans_general(amblance_dummy$Megnevez?s, "Latin-ASCII")

ambulance_dummy_2 <- merge(amblance_dummy, village_key, by.x = 'Megnevez?s', by.y = "village_name_no_accents")

ambulance_stations =  ambulance_dummy_2 %>%
  group_by(SC_code, year) %>%
  dplyr::summarize(ambulance_stations = sum(ambulance))

write.csv(ambulance_stations, 'Data/ready_to_use/ambulance_stations.csv')
#-------------------------------------------------------------------------------

#cancer patients
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/ready_to_use_copy/cancer_patients.xls" ))
data = melt(data, id=1:2, variable.name = 'year', value.name = 'cancer_patients')
data <- data[ , Megnevez?s := tolower(Megnevez?s) ]
data$Megnevez?s <- stri_trans_general(data$Megnevez?s, "Latin-ASCII")
write.csv(data, 'Data/ready_to_use/cancer_patients.csv')
#-------------------------------------------------------------------------------

#cardiovascular patients
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/ready_to_use_copy/cardiovascular_patients.xls" ))
data = melt(data, id=1:2, variable.name = 'year', value.name = 'cardiovascular_patients')
data <- data[ , Megnevez?s := tolower(Megnevez?s) ]
data$Megnevez?s <- stri_trans_general(data$Megnevez?s, "Latin-ASCII")
write.csv(data, 'Data/ready_to_use/cardiovascular_patients.csv')
#-------------------------------------------------------------------------------

#mean distance
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/ready_to_use_copy/distance_county_capitals_minute.xls" ))
data = melt(data, id=1:2, variable.name = 'year', value.name = 'distance_county_capitals')

data2 <- as.data.table(read_excel("Data/ready_to_use_copy/distance_nearest_50_town_minutes_SC.xls" ))
data2 = melt(data2, id=1:2, variable.name = 'year', value.name = 'distance_nearest_50_town')

data3 <- as.data.table(read_excel("Data/ready_to_use_copy/distance_regional_center_minute.xls" ))
data3 = melt(data3, id=1:2, variable.name = 'year', value.name = 'distance_regional_center')

data <- merge(data, data2, by=c('Megnevez?s', 'K?d', 'year'))
data <- merge(data, data3, by=c('Megnevez?s', 'K?d', 'year'))

data <- data[ , Megnevez?s := tolower(Megnevez?s) ]
data$Megnevez?s <- stri_trans_general(data$Megnevez?s, "Latin-ASCII")
write.csv(data, 'Data/ready_to_use/distances_mean_minutes.csv')
#-------------------------------------------------------------------------------

#company incomes and taxes
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/ready_to_use_copy/net_company_revenues.xls" ))
data = melt(data, id=1:2, variable.name = 'year', value.name = 'company_revenues')

data2 <- as.data.table(read_excel("Data/ready_to_use_copy/net_income_from_exports.xls" ))
data2 = melt(data2, id=1:2, variable.name = 'year', value.name = 'income_from_exports')

data <- merge(data, data2, by=c('N?v', 'K?d', 'year'))

data <- data[ , N?v := tolower(N?v) ]
data$N?v <- stri_trans_general(data$N?v, "Latin-ASCII")
#write.csv(data, 'Data/ready_to_use/company_income.csv')

company_CPI <- merge(company_income, CPI, by = 'year')
company_CPI <- company_CPI %>% mutate(revenues_CPI = company_revenues*CPI_basis_2014,
                                      export_income_CPI = income_from_exports*CPI_basis_2014)
company_CPI  <- subset(company_CPI, select = -c(CPI_basis_2014))
write.csv(company_CPI, 'Data/ready_to_use/SC/company_income.csv', row.names = FALSE)

#-------------------------------------------------------------------------------

#personal income taxes
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/ready_to_use_copy/personal_income_before_tax.xls" ))
data = melt(data, id=1:2, variable.name = 'year', value.name = 'personal_income_before_tax')

data2 <- as.data.table(read_excel("Data/ready_to_use_copy/tax_payers_personal_income_tax.xls" ))
data2 = melt(data2, id=1:2, variable.name = 'year', value.name = 'tax_payers_number_of')

data <- merge(data, data2, by=c('Megnevez?s', 'K?d', 'year'))

data <- data[ , Megnevez?s:= tolower(Megnevez?s) ]
data$Megnevez?s <- stri_trans_general(data$Megnevez?s, "Latin-ASCII")
write.csv(data, 'Data/ready_to_use/personal_income.csv')

income_CPI <- merge(income_personal, CPI, by = 'year')
income_CPI <- income_CPI %>% mutate(personal_income_before_tax_CPI = personal_income_before_tax*CPI_basis_2014)
income_CPI  <- subset(income_CPI, select = -c(CPI_basis_2014))
write.csv(income_CPI, 'Data/ready_to_use/SC/personal_income.csv', row.names = FALSE)
#-------------------------------------------------------------------------------

#number of GPs
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/ready_to_use_copy/number_GP.xls" ))
data = melt(data, id=1:2, variable.name = 'year', value.name = 'GPs_number')
data <- data[ , Megnevez?s := tolower(Megnevez?s) ]
data$Megnevez?s <- stri_trans_general(data$Megnevez?s, "Latin-ASCII")


write.csv(data, 'Data/ready_to_use/number_of_GPs.csv')

#-------------------------------------------------------------------------------

#empty GP praxis as of 2021 January
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/cleaned_data/empty_GP_praxis.xlsx" ))

#extracting years
data$dates <- as.POSIXct(data$`Bet?ltetlens?g kezdete`, format = "%Y-%m-%d")
data$year <- format(data$dates, format = '%Y')

#remove empty praxis from and later than 2020
data = data[data$year <2020,]

#subsetting data
data <- data %>% select(2,7, 9:15, 17:18)

#remove numbers from village data
data <- data %>% 
  mutate(village_1 = trimws(str_remove(`Ell?tand? telep?l?sek KSH k?djai`, "(\\s+[A-Za-z]+)?[0-9-]+")))

data <- data %>% 
  mutate(village_2 = trimws(str_remove(`2`, "(\\s+[A-Za-z]+)?[0-9-]+")))

data <- data %>% 
  mutate(village_3 = trimws(str_remove(`3`, "(\\s+[A-Za-z]+)?[0-9-]+")))

data <- data %>% 
  mutate(village_4 = trimws(str_remove(`4`, "(\\s+[A-Za-z]+)?[0-9-]+")))

data <- data %>% 
  mutate(village_5 = trimws(str_remove(`5`, "(\\s+[A-Za-z]+)?[0-9-]+")))

data <- data %>% 
  mutate(village_6 = trimws(str_remove(`...14`, "(\\s+[A-Za-z]+)?[0-9-]+")))

data <- data %>% 
  mutate(village_7 = trimws(str_remove(`...15`, "(\\s+[A-Za-z]+)?[0-9-]+")))

data <- data %>% select (-c(3:10))

#write.csv(data, 'Data/cleaned_data/empty_gp_villages.csv')

data <- data %>% 
  mutate(village_1 = trimws(str_remove(`village_1`, "(\\s+[A-Za-z]+)?[0-9-]+")))

data1 <- data %>% select(1:4)

data1$empty_GP_dummy <-1

data1<- data1[ , village_1 := tolower(village_1) ]
data1$village_1 <- stri_trans_general(data1$village_1, "Latin-ASCII")

village_key <- read.csv("Data/ready_to_use/village_key_merge.csv" )
data_m <- merge(data1, village_key, by.x = 'village_1', by.y = 'village_name_no_accents')

#number of empty praxis by SC
empty_GP_praxis_SC = data_m %>%
  group_by(SC_code, year) %>%
  dplyr::summarize(empty_GP_praxis = sum(empty_GP_dummy))

#number of people without GP by SC
empty_GP_population_SC = data_m %>%
  group_by(SC_code, year) %>%
  dplyr::summarize(empty_GP_praxis_population = sum(`Ell?tand? lakoss?gsz?m`))

empty_GP <- merge(empty_GP_praxis_SC, empty_GP_population_SC, by=c('SC_code', 'year'))

write.csv(empty_GP, 'Data/ready_to_use/empty_GP.csv')

GP_total <- merge(GP, GP_empty, by=c('year', 'SC_code'), all.x = TRUE)
GP_total <- merge(GP_total, population, by= c('year', 'SC_code'))
GP_total[is.na(GP_total)] <- 0

GP_total$GP_percapita <- GP_total$GPs_number/GP_total$population_total *10000
GP_total$GP_empty_population_ratio <- GP_total$empty_GP_praxis_population/GP_total$population_total

GP_total <- GP_total %>% select(SC_code, year, empty_GP_praxis, GP_percapita, GP_empty_population_ratio)

write.csv(GP_total, 'Data/ready_to_use/SC/GP.csv', row.names = FALSE)

#-------------------------------------------------------------------------------

#mortality
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/ready_to_use_copy/infant_mortality.xls" ))
data = melt(data, id=1:2, variable.name = 'year', value.name = 'infant_mortality_crude')

data1 <- as.data.table(read_excel("Data/ready_to_use_copy/live_births.xls" ))
data1 = melt(data1, id=1:2, variable.name = 'year', value.name = 'live_births')

data2 <- as.data.table(read_excel("Data/ready_to_use_copy/mortality_crude.xls" ))
data2 = melt(data2, id=1:2, variable.name = 'year', value.name = 'mortality_crude_total')

data3 <- as.data.table(read_excel("Data/ready_to_use_copy/male_mortality_crude.xls" ))
data3 = melt(data3, id=1:2, variable.name = 'year', value.name = 'mortality_crude_male')

data <- merge(data, data1, by=c('Megnevez?s', 'K?d', 'year'), all.x = TRUE)
data <- merge(data, data2, by=c('Megnevez?s', 'K?d', 'year'), all.x = TRUE)
data <- merge(data, data3, by=c('Megnevez?s', 'K?d', 'year'), all.x = TRUE)

data$infant_mortality_ratio = data$infant_mortality_crude/data$live_births

data <- data[ , Megnevez?s:= tolower(Megnevez?s) ]
data$Megnevez?s <- stri_trans_general(data$Megnevez?s, "Latin-ASCII")

write.csv(data, 'Data/ready_to_use/SC/mortality.csv', row.names = FALSE)

mortality <- read.csv('Data/ready_to_use/SC/mortality.csv')
population <- read.csv('Data/ready_to_use/SC/population.csv')

mortality_ratio <- merge(mortality, population, by = c('SC_code', 'year'))

mortality_ratio <- mortality_ratio %>% mutate(mortality_ratio = mortality_crude_total/population_total)
mortality_ratio <- mortality_ratio %>% select(SC_code, year, mortality_ratio, infant_mortality_ratio)

write.csv(mortality_ratio, 'Data/ready_to_use/SC/mortality_SC.csv', row.names = FALSE)

#### Treatable and preventable
#SMA from KSH
treat_total  <- read.csv('DATA/cleaned/KSH/treat_SC.csv', sep = ';')
treat_gender <- read.csv('DATA/cleaned/KSH/treat_SC_gender_adv.csv', sep = ';')
prev_total   <- read.csv('DATA/cleaned/KSH/prev_SC.csv', sep = ';')
prev_gender  <- read.csv('DATA/cleaned/KSH/prev_SC_gender.csv', sep = ';')

prev_male <- filter(prev_gender, gender == 1)
prev_male <- prev_male %>% rename(preventable_male = preventable_gender)
prev_female <- filter(prev_gender, gender == 2)
prev_female <- prev_female %>% rename(preventable_female = preventable_gender)

treat_male <- filter(treat_gender, gender == 1)
treat_male <- treat_male %>% rename(treatable_male = s_PMR)
treat_female <- filter(treat_gender, gender == 2)
treat_female <- treat_female %>% rename(treatable_female = s_PMR)

preventable <- merge(prev_female,prev_total,  by.y = c('SC_code_bp', 'year'),  by.x = c('SC_code', 'year'))
preventable <- merge(preventable, prev_male, by = c('SC_code', 'year'))
preventable <- preventable %>% select(-c(gender.x, gender.y))

treatable <- merge(treat_female,treat_total,  by.y = c('SC_code_bp', 'year'),  by.x = c('SC_code_bp', 'year'))
treatable <- merge(treatable, treat_male, by = c('SC_code_bp', 'year'))
treatable <- treatable %>% select(-c(gender.x, gender.y))

avoidable <- merge(preventable, treatable, by.y = c('SC_code_bp', 'year'),  by.x = c('SC_code', 'year'))

write.csv(avoidable, 'DATA/ready_to_use/avoidable_KSH_SC.csv', row.names = FALSE)


#SMH from NKK
#treatable <- read.csv('DATA/ready_to_use/treatable.csv', sep = ',')
#preventable <- read.csv('DATA/ready_to_use/preventable.csv', sep = ',')

#SC_codes <- read_excel('DATA/SC_codes.xlsx')
#SC_codes <- SC_codes %>% mutate(Megnevezés = tolower(Megnevezés))
#SC_codes$Megnevezés <- stri_trans_general(SC_codes$Megnevezés, "Latin-ASCII")

#preventable <- merge(preventable, SC_codes, by.x= 'SC', by.y= 'Megnevezés', all.x = TRUE)
#treatable <- merge(treatable, SC_codes, by.x= 'SC', by.y= 'Megnevezés', all.x = TRUE) 

#preventable <- preventable %>% rename(SC_code = Kód, preventable_female = women_SHH, preventable_male = men_SHH)
#treatable <- treatable%>% rename(SC_code = Kód, treatable_female = women_SHH, treatable_male = men_SHH)

#preventable <- preventable %>% select(SC_code, year, preventable_female, preventable_male)
#treatable <- treatable %>% select(SC_code, year, treatable_female, treatable_male)
#-------------------------------------------------------------------------------

#hours in outpatient care
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/ready_to_use_copy/hours_outpatient.xls" ))
#0 values are obviously missing values
data[data == 0] <- NA
data = melt(data, id=1:2, variable.name = 'year', value.name = 'hours_outpatient')
data <- data[ , Megnevez?s := tolower(Megnevez?s) ]
data$Megnevez?s <- stri_trans_general(data$Megnevez?s, "Latin-ASCII")
write.csv(data, 'Data/ready_to_use/hours_outpatient_care.csv')

hours_outpatient_pc <- merge(hours_outpatient, population, by = c('year', 'SC_code'))
hours_outpatient_pc$hours_outpatient_percapita = hours_outpatient_pc$hours_outpatient/hours_outpatient_pc$population_total

hours_outpatient_pc <- hours_outpatient_pc %>% select(-c(population_total,population_male))
write.csv(hours_outpatient_pc,'Data/ready_to_use/SC/hours_outpatient_care.csv', row.names = FALSE )

#-------------------------------------------------------------------------------

#gymnasium students
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("DATA/cleaned/high_school_students_number.xls" ))
data1 <- as.data.table(read_excel("DATA/cleaned/high_school_students_2019.xls" ))
highschool_population <- read.csv("DATA/cleaned/KSH/highschool_population.csv")

#0 values 
data = melt(data, id=1:2, variable.name = 'year', value.name = 'gymnasium_students')
data1 = melt(data1, id=1:2, variable.name = 'year', value.name = 'gymnasium_students')

data1 <- data1 %>% rename(name = Megnevezés)
data <- data %>% rename(name = Név)
data = rbind(data,data1)

data <- data[ , name := tolower(name) ]
data$name <- stri_trans_general(data$name, "Latin-ASCII")
data <- data %>% rename(SC_code =Kód)
data$year <- as.numeric(as.character(data$year))
data$SC_code <- as.numeric(as.character(data$SC_code))

data <- merge(data, highschool_population, by.x= c('year', 'SC_code'), by.y= c('year', 'SC_code_bp'))
data$gymnasium_ratio <- data$gymnasium_students/data$pop14_20

data <- data %>% select(c(year, SC_code, gymnasium_ratio)) 

write.csv(data, 'DATA/ready_to_use/gymnasium_students.csv', row.names = FALSE)

#-------------------------------------------------------------------------------

#official population age categories
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/ready_to_use_copy/official_population_total.xls" ))
data = melt(data, id=1:2, variable.name = 'year', value.name = 'official_population_total')

data1 <- as.data.table(read_excel("Data/ready_to_use_copy/official_population_18_59.xls" ))
data1 = melt(data1, id=1:2, variable.name = 'year', value.name = 'official_population_18_59')

data2 <- as.data.table(read_excel("Data/ready_to_use_copy/official_population_60plus.xls" ))
data2 = melt(data2, id=1:2, variable.name = 'year', value.name = 'official_population_60plus')

data <- merge(data, data1, by=c('Megnevez?s', 'K?d', 'year'), all.x = TRUE)
data <- merge(data, data2, by=c('Megnevez?s', 'K?d', 'year'), all.x = TRUE)

data$official_population_18 = data$official_population_total- (data$official_population_18_59+data$official_population_60plus)
data$elderely_ratio = data$official_population_60plus/data$official_population_total

data <- data[ , Megnevez?s:= tolower(Megnevez?s) ]
data$Megnevez?s <- stri_trans_general(data$Megnevez?s, "Latin-ASCII")
write.csv(data, 'Data/ready_to_use/official_population.csv')

population_o <- read.csv('Data/ready_to_use/SC/official_population.csv')

#-------------------------------------------------------------------------------

#tax paying companies 
#-------------------------------------------------------------------------------
data <- as.data.table(read_excel("Data/cleaned_data/tax_paying_companies_IFSR.xls" ))

data1 <- as.data.table(read_excel("Data/cleaned_data/tax_paying_companies_hun.xls" ))

data = melt(data, id=1:2, variable.name = 'year', value.name = 'tax_paying_companies')
data1 = melt(data1, id=1:2, variable.name = 'year', value.name = 'tax_paying_companies')

data = rbind(data,data1)
data2 = data[complete.cases(data), ]

data2 = as.data.table(data2 %>%
  group_by(Megnevez?s, K?d, year) %>%
  dplyr::summarize(tax_paying_companies = sum(tax_paying_companies)))


data2 <- data2[ , Megnevez?s := tolower(Megnevez?s) ]
data2$Megnevez?s <- stri_trans_general(data2$Megnevez?s, "Latin-ASCII")
write.csv(data2, 'Data/ready_to_use/tax_paying_companies.csv')

#-------------------------------------------------------------------------------

#EU fund
#-------------------------------------------------------------------------------
EU_fund <- read_excel('DATA/eu_health_funds_20210515.xlsx')
type_codes <- read_excel ('DATA/type_codes.xlsx', sheet = "Sheet3")
village_key <- read.csv('DATA/ready_to_use/village_key_merge.csv')
CPI <- read.csv('DATA/ready_to_use/CPI.csv')

EU_fund <- EU_fund %>% select(-c(...1, date_decision, date_last_payment, date_effective, amount_awarded...20))
EU_fund$village_name <- str_remove_all(EU_fund$village_name, "\\(.*$")

EU_fund <- merge(EU_fund, type_codes, by.x = 'program_code', by.y = 'code')
EU_fund$village_name <- tolower(EU_fund$village_name)
EU_fund$village_name  <- stri_trans_general(EU_fund$village_name, "Latin-ASCII")
EU_fund$village_name <- str_squish(EU_fund$village_name)

EU_fund <- merge(EU_fund, village_key, by.x = 'village_name', by.y = 'village_name_no_accents', all.x = TRUE)
EU_fund$id <- as.character(EU_fund$id)
EU_fund$id<- str_squish(EU_fund$id)

#national-level developments
EU_fund$SC_code[EU_fund$id == '13696402'|EU_fund$id == '1307540201' |
                EU_fund$id == '1209740201'|EU_fund$id == '453120201' |
                EU_fund$id == '18381802'|EU_fund$id == '3595902' |
                EU_fund$id == '17024102'|EU_fund$id == '11622902' |
                EU_fund$id == '12675302'|EU_fund$id == '9285502' |
                EU_fund$id == '4812202'|EU_fund$id == '2680202' |
                EU_fund$id == '4333502'|EU_fund$id == '14300402' |
                EU_fund$id == '4878702'|EU_fund$id == '4913002' |
                EU_fund$id == '9184302'|EU_fund$id == '7771802' |
                EU_fund$id == '1755502'|EU_fund$id == '14885102' |
                EU_fund$id == '12636702'|EU_fund$id == '16878002' |
                EU_fund$id == '15446302'|EU_fund$id == '2065702' |
                EU_fund$id == '13301202'|EU_fund$id == '1455680201']<- 'national'

colSums(is.na(EU_fund))
#write.xlsx(EU_fund, 'EU_fund_healthcare_cleaned_total.xlsx')

#total amount (calculated during the cleaning in python)
total_amount <- 20547990976931
total_healthcare_funds <- sum(EU_fund$amount_awarded2)
ratio_of_healthcare <- total_healthcare_funds/total_amount 

EU_fund_SC_effective = EU_fund %>%
  group_by(SC_code, year_effective, type) %>%
  dplyr::summarize(amount_awarded_effective = sum(amount_awarded2))
EU_fund_SC_effective <- merge(EU_fund_SC_effective, CPI, by.x = 'year_effective', by.y = 'year')
EU_fund_SC_effective <- EU_fund_SC_effective %>% mutate(effective_CPI = amount_awarded_effective*CPI_basis_2014 )
EU_fund_SC_effective <- EU_fund_SC_effective %>% select(-c(CPI_basis_2014))
write.csv(EU_fund_SC_effective, 'EU_fund_SC_effective_0521.csv', row.names = FALSE)


EU_fund_SC_decision = EU_fund %>%
  group_by(SC_code, year_decision, type) %>%
  dplyr::summarize(amount_awarded_decision = sum(amount_awarded2))
EU_fund_SC_decision <- merge(EU_fund_SC_decision, CPI, by.x = 'year_decision', by.y = 'year')
EU_fund_SC_decision <- EU_fund_SC_decision  %>% mutate(decision_CPI = amount_awarded_decision*CPI_basis_2014 )
EU_fund_SC_decision <- EU_fund_SC_decision  %>% select(-c(CPI_basis_2014))
write.csv(EU_fund_SC_decision, 'EU_fund_SC_decision_0521.csv', row.names = FALSE)


EU_fund_decision <- read.csv('DATA/ready_to_use/EU_fund_SC_decision_0521.csv')
EU_fund_effective <- read.csv('DATA/ready_to_use/EU_fund_SC_effective_0521.csv')


EU_fund_effective = EU_fund_effective %>%
  group_by(SC_code, year_effective) %>%
  dplyr::summarize(funds_effective = sum(amount_awarded_effective),
                   funds_e_cpi = sum(effective_CPI))

EU_fund_decision = EU_fund_decision  %>%
  group_by(SC_code, year_decision) %>%
  dplyr::summarize(funds_decision = sum(amount_awarded_decision),
                   funds_d_cpi = sum(decision_CPI))


EU_fund_SC <-  merge(EU_fund_effective, EU_fund_decision, by.x = c('year_effective', 'SC_code'),
                     by.y = c('year_decision', 'SC_code'), all.x = TRUE, all.y = TRUE)

EU_fund_SC <- EU_fund_SC %>% rename(year = year_effective)
#write.csv(EU_fund_SC, 'DATA/ready_to_use/EU_fund_SC', row.names = FALSE)
write.csv(EU_fund_SC, 'DATA/ready_to_use/EU_fund_SC.csv', row.names = FALSE)


EU_fund_hr <-  filter(EU_fund_SC_effective, type == 'human_resource')
EU_fund_hr <- EU_fund_hr %>% select(year_effective, SC_code, effective_CPI)
EU_fund_hr <- EU_fund_hr %>% rename(year = year_effective, hr_effective = effective_CPI)
write.csv(EU_fund_hr, 'DATA/ready_to_use/EU_fund_hr.csv', row.names = FALSE )

public_awareness <- c('health_awareness', 'public_health', 'public_health_primary_care')
EU_fund_public_awareness<-  filter(EU_fund_SC_effective, type %in% public_awareness )
EU_fund_public_awareness <- EU_fund_public_awareness %>% select(year_effective, SC_code, effective_CPI)
EU_fund_public_awareness <- EU_fund_public_awareness %>% rename(year = year_effective, public_a_effective = effective_CPI)
EU_fund_public_awareness <- EU_fund_public_awareness  %>% group_by(SC_code, year) %>%
  dplyr::summarize(public_a_effective = sum(public_a_effective))
write.csv(EU_fund_public_awareness, 'DATA/ready_to_use/EU_fund_public_a.csv', row.names = FALSE )

outpatient_care <- c ('outpatient_care', 'outpatient_care_microregions', 'outpatient_microregions', 'structural_outpatient', 'primary_outpatient_marginalized')
EU_fund_outpatient <- filter(EU_fund_SC_effective, type %in% outpatient_care )
EU_fund_outpatient <- EU_fund_outpatient %>% select(year_effective, SC_code, effective_CPI)
EU_fund_outpatient <- EU_fund_outpatient %>% rename(year = year_effective, outpatient_effective = effective_CPI)
EU_fund_outpatient <- EU_fund_outpatient  %>% group_by(SC_code, year) %>%
  dplyr::summarize(outpatient_effective = sum(outpatient_effective))
write.csv(EU_fund_outpatient, 'DATA/ready_to_use/EU_fund_outpatient.csv', row.names = FALSE )

equipment <- c ('equipment')
EU_fund_equipment <- filter(EU_fund_SC_effective, type %in% equipment )
EU_fund_equipment <- EU_fund_equipment %>% select(year_effective, SC_code, effective_CPI)
EU_fund_equipment<- EU_fund_equipment %>% rename(year = year_effective, equipment_effective = effective_CPI)
write.csv(EU_fund_equipment, 'DATA/ready_to_use/EU_fund_equipment.csv', row.names = FALSE )
#-------------------------------------------------------------------------------

#election data
#-------------------------------------------------------------------------------
election_data <- read.csv('Data/ready_to_use/local_elections_final_202105.csv')
village_codes <- read.csv('Data/ready_to_use/village_codes_final.csv')
SC_data <- read_excel('Data/ready_to_use/SC_data.xlsx')

village_codes$SC_name_no_accents <- village_codes$SC_name

SC_data$SC_name <- tolower(SC_data$SC_name)
SC_data$SC_capital <- tolower(SC_data$SC_capital)
village_codes$SC_name_no_accents <- tolower(village_codes$SC_name_no_accents)

SC_data$SC_name  <- stri_trans_general(SC_data$SC_name, "Latin-ASCII")
SC_data$SC_capital  <- stri_trans_general(SC_data$SC_capital, "Latin-ASCII")
village_codes$SC_name_no_accents  <- stri_trans_general(village_codes$SC_name_no_accents, "Latin-ASCII")
election_data$village_name <- str_squish(election_data$village_name)

SC_data$SC_name <- str_remove_all(SC_data$SC_name, "\\jaras")
SC_data$SC_capital_dummy <- 5

SC_key <- SC_data %>% select(SC_capital, SC_capital_dummy)

village_keys <- merge(village_codes, SC_key, by.x="village_name_no_accents", by.y = 'SC_capital', all.x = TRUE)

elections_merged <- merge(election_data, SC_key, by.x = 'village_name',
                          by.y="SC_capital", all.x = TRUE)

local_election <- subset(elections_merged, SC_capital_dummy==5)

local_election <- local_election %>% select(village_name, margin, year, governing_pary, independent)

SC_key <- read.csv('Data/ready_to_use/SC/village_key_merge.csv')

local_election <- merge(local_election, SC_key, by.x= 'village_name', by.y = 'village_name_no_accents')

local_election <- local_election %>% select(-c(X,village_name))

local_election <- read.csv('Data/ready_to_use/SC/local_election_SC.csv')

election_margin <- local_election %>% select(SC_code, year, margin)
election_margin <- reshape(election_margin, idvar = "SC_code", timevar = "year", direction = "wide")
election_margin$a2003 <- election_margin$margin.2002
election_margin$a2004 <- election_margin$margin.2002
election_margin$a2005 <- election_margin$margin.2002
election_margin$a2007 <- election_margin$margin.2006
election_margin$a2008 <- election_margin$margin.2006
election_margin$a2009 <- election_margin$margin.2006
election_margin$a2011 <- election_margin$margin.2010
election_margin$a2012 <- election_margin$margin.2010
election_margin$a2013 <- election_margin$margin.2010
election_margin$a2015 <- election_margin$margin.2014
election_margin$a2016 <- election_margin$margin.2014
election_margin$a2017 <- election_margin$margin.2014
election_margin$a2018 <- election_margin$margin.2014
colnames(election_margin)
election_margin <- as.data.table(election_margin %>% rename('2010' = margin.2010,
                                              '2006' = margin.2006,
                                              '2014' = margin.2014,
                                              '2019' = margin.2019,
                                              '2002' = margin.2002,
                                              '2003' = a2003,
                                              '2004' = a2004,
                                              '2005' = a2005,
                                              '2007' = a2007,
                                              '2008' = a2008,
                                              '2009' = a2009,
                                              '2011' = a2011,
                                              '2012' = a2012,
                                              '2013' = a2013,
                                              '2015' = a2015,
                                              '2016' = a2016,
                                              '2017' = a2017,
                                              '2018' = a2018))
election_margin = melt(election_margin, id=1, variable.name = 'year', value.name = 'election_margin')

election_governing_party <- local_election  %>% select(SC_code, year, governing_pary)
election_governing_party <- reshape(election_governing_party, idvar = "SC_code", timevar = "year", direction = "wide")
election_governing_party$a2003 <- election_governing_party$governing_pary.2002
election_governing_party$a2004 <- election_governing_party$governing_pary.2002
election_governing_party$a2005 <- election_governing_party$governing_pary.2002
election_governing_party$a2007 <- election_governing_party$governing_pary.2006
election_governing_party$a2008 <- election_governing_party$governing_pary.2006
election_governing_party$a2009 <- election_governing_party$governing_pary.2006
election_governing_party$a2011 <- election_governing_party$governing_pary.2010
election_governing_party$a2012 <- election_governing_party$governing_pary.2010
election_governing_party$a2013 <- election_governing_party$governing_pary.2010
election_governing_party$a2015 <- election_governing_party$governing_pary.2014
election_governing_party$a2016 <- election_governing_party$governing_pary.2014
election_governing_party$a2017 <- election_governing_party$governing_pary.2014
election_governing_party$a2018 <- election_governing_party$governing_pary.2014
colnames(election_governing_party)
election_governing_party <- as.data.table(election_governing_party %>% rename('2010' = governing_pary.2010,
                                              '2006' = governing_pary.2006,
                                              '2014' = governing_pary.2014,
                                              '2019' = governing_pary.2019,
                                              '2002' = governing_pary.2002,
                                              '2003' = a2003,
                                              '2004' = a2004,
                                              '2005' = a2005,
                                              '2007' = a2007,
                                              '2008' = a2008,
                                              '2009' = a2009,
                                              '2011' = a2011,
                                              '2012' = a2012,
                                              '2013' = a2013,
                                              '2015' = a2015,
                                              '2016' = a2016,
                                              '2017' = a2017,
                                              '2018' = a2018))
election_governing_party = melt(election_governing_party, id=1, variable.name = 'year', value.name = 'governing_party')


election_independent <- local_election  %>% select(SC_code, year, independent)
election_independent <- reshape(election_independent, idvar = "SC_code", timevar = "year", direction = "wide")
election_independent$a2003 <- election_independent$independent.2002
election_independent$a2004 <- election_independent$independent.2002
election_independent$a2005 <- election_independent$independent.2002
election_independent$a2007 <- election_independent$independent.2006
election_independent$a2008 <- election_independent$independent.2006
election_independent$a2009 <- election_independent$independent.2006
election_independent$a2011 <- election_independent$independent.2010
election_independent$a2012 <- election_independent$independent.2010
election_independent$a2013 <- election_independent$independent.2010
election_independent$a2015 <- election_independent$independent.2014
election_independent$a2016 <- election_independent$independent.2014
election_independent$a2017 <- election_independent$independent.2014
election_independent$a2018 <- election_independent$independent.2014
colnames(election_independent)
election_independent <- as.data.table(election_independent %>% rename(
                                                                '2010' = independent.2010,
                                                                '2006' = independent.2006,
                                                                '2014' = independent.2014,
                                                                '2019' = independent.2019,
                                                                '2002' = independent.2002,
                                                                '2003' = a2003,
                                                                '2004' = a2004,
                                                                '2005' = a2005,
                                                                '2007' = a2007,
                                                                '2008' = a2008,
                                                                '2009' = a2009,
                                                                '2011' = a2011,
                                                                '2012' = a2012,
                                                                '2013' = a2013,
                                                                '2015' = a2015,
                                                                '2016' = a2016,
                                                                '2017' = a2017,
                                                                '2018' = a2018))

election_independent = melt(election_independent, id=1, variable.name = 'year', value.name = 'independent')

election_margin$year <- as.numeric(as.character(election_margin$year))
election_governing_party$year <- as.numeric(as.character(election_governing_party$year))
election_independent$year <- as.numeric(as.character(election_independent$year))

local_elections <- merge(election_margin, election_governing_party, by = c('year','SC_code'))
local_elections <- merge(local_elections, election_independent, by = c('year','SC_code'))

local_elections <- filter(local_elections, year > 2003)
write.csv(local_elections, 'DATA/ready_to_use/local_election_SC.csv', row.names = FALSE)

local_election <- read.csv('DATA/ready_to_use/local_election_SC.csv')

local_election <- local_election %>% mutate(close_run_governing_party = 
                                                               case_when(election_margin <= 10 & governing_party ==1   ~ 1))
local_election <- local_election %>% mutate(close_run_opposition = 
                                              case_when(election_margin <= 10 & governing_party ==0   ~ 1))

local_election <- local_election %>% mutate(overwhelming_victory_governing_party = 
                                              case_when(election_margin >= 50 & governing_party ==1   ~ 1))

local_election <- local_election %>% mutate(overwhelming_victory_non_governing_party = 
                                              case_when(election_margin >= 50 & governing_party ==0   ~ 1))

write.csv(local_election, 'DATA/ready_to_use/local_election_SC.csv', row.names = FALSE)

#-------------------------------------------------------------------------------

#one day care (from hospital report)
#-------------------------------------------------------------------------------
hospital_one_day <- read.csv('/Users/manna/Desktop/Thesis_202105/DATA/ready_to_use/hospital_oneday_SC_0521.csv', sep = ';')
hospital_ICU <- read.csv('/Users/manna/Desktop/Thesis_202105/DATA/ready_to_use/hospital_ICU_department_SC_0521.csv', sep = ';')
hospital_ICU <- hospital_ICU%>% select (-c(patients))
#-------------------------------------------------------------------------------

#incidence
#-------------------------------------------------------------------------------
cancer <- read.csv('Data/ready_to_use/SC/cancer_patients.csv')
respiratory<- read.csv('Data/ready_to_use/SC/respiratory_patients.csv')
cardio <- read.csv('Data/ready_to_use/SC/cardiovascular_patients.csv')

incidence <- merge(cancer, respiratory, by = c('year', 'SC_code'))
incidence <-merge(incidence, cardio, by = c('year', 'SC_code'))
incidence <- merge(incidence, population, by = c('year', 'SC_code'))

incidence$cancer_ratio = incidence$cancer_patients/incidence$population_total
incidence$respiratory_ratio = incidence$respiratory_patients/incidence$population_total
incidence$cardio_ratio = incidence$cardiovascular_patients/incidence$population_total

incidence <- incidence %>% select(year, SC_code, cancer_patients, respiratory_patients, cardiovascular_patients,
                                  cancer_ratio, respiratory_ratio, cardio_ratio)

write.csv(incidence, 'Data/ready_to_use/SC/incidence.csv', row.names = FALSE)

#------------------------------------------------------------------------------

##############################################################################################

#Data cleaning for analysis
#-----------------------------------------------------------------------------------------------------

#opening data
#CPI
CPI <- read.csv('DATA/ready_to_use/CPI.csv')

#sub-county HDI (not available for each year)
HDI <- read.csv('DATA/ready_to_use/HDI_cat.csv', sep = ';')

#stadium (not available for each year)
stadium<- read.csv ('DATA/ready_to_use/stadiums.csv')

#ambulance (not available for each year)
ambulance <- read.csv ('DATA/ready_to_use/ambulance_stations.csv')

#environment (not available for each year)
environment <- read.csv ('DATA/ready_to_use/aid_data_cleaned_final_SC.csv', sep = ';')

#distance (not available for each year)
distance <- read.csv ('DATA/ready_to_use/distances_mean_minutes.csv')

#geographic role (only available for 2020)
geo_role <- read.csv('DATA/ready_to_use/geographic_role.csv')

#availability of outpatient_care (only available for 2021)
availability <- read_excel('DATA/ready_to_use/hospital_outpatient_availability_SC.xlsx')

#personal income
income_personal <- read.csv('DATA/ready_to_use/personal_income.csv')

#unemployment ratio
unemployment <- read.csv('DATA/ready_to_use/unemployment.csv')

#company revenues and foreign exports
company_income <- read.csv('DATA/ready_to_use/company_income.csv')

#tax paying companies
#tax_paying_companies <- read.csv('DATA/ready_to_use/tax_paying_companies.csv')

#gymnasium students & elderly ratio
gymnasium_ratio <- read.csv('DATA/ready_to_use/gymnasium_students.csv', sep = ',')

#population 
population <- read.csv('DATA/ready_to_use/population.csv')
population$population_female <- population$population_total-population$population_male

#elderly ratio
elderly_ratio <- read.csv('DATA/ready_to_use/elderly_ratio.csv', sep = ';')

#local elections
local_election <- read.csv('DATA/ready_to_use/local_election_SC.csv')

#mortalities
mortality <- read.csv('DATA/ready_to_use/mortality_SC.csv')
#preventable <- read.csv('DATA/ready_to_use/preventable_SC.csv', sep = ';')
#treatable <-read.csv('DATA/ready_to_use/treatable_SC.csv', sep = ';')
avoidable_mortality <- read.csv('DATA/ready_to_use/avoidable_KSH_SC.csv')

#hours in outpatient care
hours_outpatient <- read.csv('DATA/ready_to_use/hours_outpatient_care.csv')

#GP number
GP <- read.csv('DATA/ready_to_use/GP.csv')

#actual care days
care_days <- read.csv('DATA/ready_to_use/actual_care_days.csv')

#incidence (oncology, cardio-vascular, respiratory)
incidence <- read.csv ('DATA/ready_to_use/incidence.csv')

#stadium
stadium<- read.csv ('DATA/ready_to_use/stadiums.csv')

#ambulance
ambulance <- read.csv ('DATA/ready_to_use/ambulance_stations.csv')

#environment
environment <- read.csv ('DATA/ready_to_use/aid_data_cleaned_final_SC.csv', sep = ';')

#distance
distance <- read.csv ('DATA/ready_to_use/distances_mean_minutes.csv')

#woods
woods <- read.csv('DATA/ready_to_use/territory_woods.csv')

#EU funds
EU_fund_SC <- read.csv('DATA/ready_to_use/EU_fund_SC.csv')

#budget
budget <- read.csv('DATA/ready_to_use/budget_SC.csv', sep =';')

#prescription exemption certificates
prexc <- read.csv('DATA/ready_to_use/prescription_exemption_certificate.csv', sep = ',')

#employees
employees <- read.csv('DATA/ready_to_use/healthcare_workers.csv')
#employees <- employees %>% select(-c(SC_name))

#funds hr
EU_fund_hr <- read.csv('DATA/ready_to_use/EU_fund_hr.csv')
EU_fund_public_awareness<- read.csv('DATA/ready_to_use/EU_fund_public_a.csv')
EU_fund_outpatient<- read.csv( 'DATA/ready_to_use/EU_fund_outpatient.csv')
EU_fund_equipment<- read.csv('DATA/ready_to_use/EU_fund_equipment.csv')

#hospital data
hospital_one_day <- read.csv('/Users/manna/Desktop/Thesis_202105/DATA/ready_to_use/hospital_oneday_SC_0521.csv', sep = ';')
hospital_one_day <- hospital_one_day %>% select (c(year, SC_code, mean_care_days, one_day_ratio, hospital_dummy))
hospital_ICU <- read.csv('/Users/manna/Desktop/Thesis_202105/DATA/ready_to_use/hospital_ICU_department_SC_0521.csv', sep = ';')
hospital_ICU <- hospital_ICU%>% select (-c(patients))

####
master <- merge(unemployment, population, by = c('year', 'SC_code'), all.x= TRUE, all.y = TRUE)
master <- merge(master, geo_role, by = c('SC_code'), all.x= TRUE, all.y = TRUE)
master <- merge(master, GP, by = c('year', 'SC_code'), all= TRUE, all.y = TRUE)
master <- merge(master, gymnasium_ratio, by = c('year', 'SC_code'), all.x= TRUE, all.y = TRUE)
master <- merge(master, HDI, by = c('year', 'SC_code'), all.x= TRUE, all.y = TRUE)
master <- merge(master, hospital_ICU, by = c('year', 'SC_code'), all.x= TRUE, all.y = TRUE)
master <- merge(master, hospital_one_day, by = c('year', 'SC_code'), all.x= TRUE, all.y = TRUE)
master <- merge(master, hours_outpatient, by = c('year', 'SC_code'), all.x= TRUE, all.y = TRUE)
master <- merge(master, incidence, by = c('year', 'SC_code'), all.x= TRUE, all.y = TRUE)
master <- merge(master, income_personal, by = c('year', 'SC_code'), all.x= TRUE, all.y = TRUE)
master <- merge(master, local_election, by = c('year', 'SC_code'),all.x= TRUE, all.y = TRUE)
master <- merge(master, mortality, by = c('year', 'SC_code'), all.x= TRUE, all.y = TRUE)
master <- merge(master, prexc, by = c('year', 'SC_code'),all.x= TRUE, all.y = TRUE)
master <- merge(master, stadium, by = c('year', 'SC_code'),all.x= TRUE, all.y = TRUE)
master <- merge(master, woods, by = c('year', 'SC_code'),all.x= TRUE, all.y = TRUE)
master <- merge(master, ambulance, by = c('year', 'SC_code'),all.x= TRUE, all.y = TRUE)
master <- merge(master, availability, by = c('SC_code'), all.x= TRUE, all.y = TRUE)
master <- merge(master, avoidable_mortality, by = c('SC_code', 'year'), all.x= TRUE, all.y = TRUE)
master <- merge(master, budget, by = c( 'SC_code', 'year'), all.x= TRUE, all.y = TRUE)
master <- merge(master, care_days, by = c( 'SC_code', 'year'), all.x= TRUE, all.y = TRUE)
master <- merge(master, company_income, by = c('SC_code', 'year'), all.x= TRUE, all.y = TRUE)
master <- merge(master, CPI, by = c('year'), all.x= TRUE, all.y = TRUE)
master <- merge(master, distance, by = c('SC_code', 'year'), all.x= TRUE, all.y = TRUE)
master <- merge(master, elderly_ratio, by = c( 'SC_code', 'year'), all.x= TRUE, all.y = TRUE)
master <- merge(master, employees, by = c('SC_code', 'year'), all.x= TRUE, all.y = TRUE)
master <- merge(master, environment, by = c('SC_code', 'year'), all.x= TRUE, all.y = TRUE)
master <- merge(master, EU_fund_public_awareness, by = c( 'SC_code', 'year'), all.x= TRUE, all.y = TRUE)
master <- merge(master, EU_fund_outpatient, by = c('SC_code', 'year'), all.x= TRUE, all.y = TRUE)
master <- merge(master, EU_fund_equipment, by = c('SC_code', 'year'), all.x= TRUE, all.y = TRUE)
master <- merge(master, EU_fund_hr, by = c( 'SC_code', 'year'), all.x= TRUE, all.y = TRUE)
master <- merge(master, EU_fund_SC, by = c('SC_code', 'year'), all.x= TRUE, all.y = TRUE)

master$personal_income_percapita <- master$personal_income_before_tax_CPI/master$population_total
master$revenues_percapita <- master$revenues_CPI/master$population_total
master$export_percapita <- master$export_income_CPI/master$population_total
master$professionals_total_percapita <- master$healthcare_workers_total/master$population_total
master$professionals_higher_percapita <- master$healthcare_workers_higher_total/master$population_total

colnames(master)

#change missing values to 0 where it is relevant
master$funds_d_cpi[is.na(master$funds_d_cpi)] <- 0
master$funds_e_cpi[is.na(master$funds_e_cpi)] <- 0
master$hr_effective[is.na(master$hr_effective)] <- 0
master$equipment_effective[is.na(master$equipment_effective)] <- 0
master$public_a_effective[is.na(master$public_a_effective)] <- 0
master$outpatient_effective[is.na(master$outpatient_effective)] <- 0
master$funds_effective[is.na(master$funds_effective)] <- 0
master$funds_decision[is.na(master$funds_decision)] <- 0
master$hospital_dummy[is.na(master$hospital_dummy) & master$year <2020] <- 0
master$ICU_department[is.na(master$ICU_department) & master$year <2020] <- 0
master$one_day_ratio[is.na(master$one_day_ratio) & master$year <2020] <- 0
master$close_run_governing_party[is.na(master$close_run_governing_party)] <- 0
master$close_run_opposition[is.na(master$close_run_opposition)] <- 0
master$overwhelming_victory_governing_party[is.na(master$overwhelming_victory_governing_party)] <- 0
master$overwhelming_victory_non_governing_party[is.na(master$overwhelming_victory_non_governing_party)] <- 0
master$mean_care_days[is.na(master$mean_care_days)& master$year <2020] <- 0

#histograms
#hist_2006 <-master[(master$year==2006),]
#hist_2010 <-master[(master$year==2010),]
#hist_2016 <-master[(master$year==2016),]
#hist_2018 <-master[(master$year==2018),]

#hist.data.frame(hist_2016[3:13], bins = 15)
#hist.data.frame(hist_2016[14:25])
#hist.data.frame(hist_2016[26:30])
#hist.data.frame(hist_2016[31:40])
#hist.data.frame(hist_2016[41:50])
#hist.data.frame(hist_2016[51:60])
#hist.data.frame(hist_2016[60:64])
#hist.data.frame(hist_2016[95:96])

#hist.data.frame(hist_2018[3:13])
#hist.data.frame(hist_2018[14:25])
#hist.data.frame(hist_2018[26:30])

#master2 <- master %>% filter(year < 2020)

##to delete: company_revenues, income_from_export, revenues_CPI, export_income_CPI,
#cancer_patients, respiratory_patients, cardiovascular_patients, personal_income_before_tax
#tax payers numbers of, personal income before tax

#taking the logs 1 was added to variables that contain 0 values

##healthcare fundbudget
master$ln_budget_outpatient <- log(master$budget_outpatient_percapita+1)
master$ln_budget_CT         <- log(master$budget_CT_percapita+1)
master$ln_budget_GP         <- log(master$budget_GP_percapita +1)
master$ln_budget_total      <- log(master$budget_total_percapita)

##funds
master$ln_funds_decision   <- log(master$funds_d_cpi +1)
master$ln_funds_effective  <- log(master$funds_e_cpi +1)
master$ln_funds_hr               <- log(master$hr_effective +1)
master$ln_funds_outpatient       <- log(master$outpatient_effective +1)
master$ln_funds_public_health    <- log(master$public_a_effective + 1)
master$ln_funds_equipment        <- log(master$equipment_effective + 1)

##company and private income
master$ln_export_income   <- log(master$export_percapita)
master$ln_revenues        <- log(master$revenues_percapita)
master$ln_personal_income <- log(master$personal_income_percapita)

colnames(master)

write.csv(master, 'DATA/full_dataset_cleaned.csv', row.names = FALSE, na="")

master2 <- master %>% select(-c(geographic_role, HDI, hours_outpatient, cancer_patients, respiratory_patients, personal_income_percapita,
                                cardiovascular_patients, personal_income_before_tax, personal_income_before_tax_CPI,professionals_higher_percapita,
                                independent, SC_name.x, hospital_available, poor_out_patient_availability, good_out_patient_care,
                                election_margin,GP_budget_ratio, CT_budget_ratio, budget_total_percapita,budget_GP_percapita,
                                laboratory_budget_ratio, budget_laboratory_percapita, budget_CT_percapita,budget_outpatient_percapita,
                                actual_care_days, company_revenues, income_from_exports, revenues_CPI,ln_funds_outpatient,
                                export_income_CPI,CPI_basis_2014, healthcare_workers_total, healthcare_workers_higher_total,
                                public_a_effective, outpatient_effective, equipment_effective, hr_effective, funds_effective, ln_funds_public_health,
                                funds_decision, funds_e_cpi, funds_d_cpi,SC_name.y, export_percapita, revenues_percapita, tax_payers_number_of))
colnames(master2)

master2 <- master2[!(master2$SC_code=='national'),]

missing_year <- master2 %>% 
  group_by(year) %>% 
  summarise_all(~sum(is.na(.))) 

#the following variables were tried for a subset of years, but due to the high number of missing years were excluded from the final model
#master3 <- master2 %>% select(-c(stadium, ambulance_stations, particulate_matter, ozone, co2))
write.csv(master2, 'DATA/data_for_analysis1.csv', row.names = FALSE, na="")


#adding region codes
master2 <- read.csv("/Users/manna/Desktop/Thesis_202105/DATA/data_for_analysis2.csv")
region_code <- read.csv("/Users/manna/Desktop/Thesis_202105/DATA/ready_to_use/region_codes.csv", sep = ';')

master2 <- merge(master2, region_code, by = 'SC_code')

master2 <- master2 %>% select(-c(region_name))
write.csv(master2, '/Users/manna/Desktop/Thesis_202105/DATA/ready_to_use/data_for_analysis2.csv', row.names = FALSE, na="")


#master_stata <- master3 %>% mutate_all(funs(replace_na(.,88888)))
#write.csv(master_stata, 'DATA/master_stata.csv', row.names = FALSE)
#---------------------------------------------------------------------------------------------
###########################################################################################################
###########################################################################################################

