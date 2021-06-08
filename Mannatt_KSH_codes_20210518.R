#Manna Toth
#KSH data extraction codes
#Thesis
################################################################################################################################
#Megjegyzesek a KSH munkatarsai szamara

#1. Budapestet egy egysegkent kezelem, ezert az SC_codes_bp.csv kodok alapjan sorolom be a jarasokat

#2. Bizonyos halalokokat 0.5-os szorzoval kell szamitani. Az eredeti elemszamokat a "patient_0" oszlopok tartalmazzak a
# KSH szamara keszult ellenorzotablakban. A ratak kiszamitasahoz a patient oszlopot hasznaltam,
# amely a szukseges halalok szorzoval korrigalt betegszamokat tartalmazza.

#3.A BNO kodok szerinti halalozasi adattabla eloallitasa viszonylag sok ideg fut (tobb mint 1 ora volt a kutatoszobai gepen)

#Koszonom szepen, ha barmi kerdesuk van, keszseggel allok rendelkezesukre!

################################################################################################################################
#OUTPUT

#Kivinni kivant eredmenyek helye: X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Kutatasi_eredmenyek
#KSH szamara keszitett kapcsolodo eredmenytablak helye: X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Adv_ellenorzeshez
#A KSH reszere keszitett tablak elnevezese megyezik a kivinni kivant eredmenyek elnevezesevel, de a vegen '_KSH' is szerepel

#1. highschool_population.csv: a kozepiskolas koruak szama jarasonkent es evenkent. Ehhez nem keszult ellenorzo tabla, mivel az elemszamokat szeretnem kivinni.
#2. treat_countRy.csv: az orszagos elkerulheto halalozasi rata
#3. prev_countRy.csv: az országos megelozheto halalozasi rata
#4. treat_countRy_gender.csv: az orszagos elkerulheto halalozasi rata nemenkent
#5. prev_countRy_gender.csv: az orszagos megelozheto halalozasi rata nemenket
#6. treat_SC.csv: jarasi elkerulheto halalozasi rata
#7. treat_SC_gender.csv: jarasi elkerulheto halalozasi rata nemenkent
#8. prev_SC.csv: jarasi megelozheto halalozasi rata
#9. prev_SC_gender.csv: jarasi megelozheto halalozasi rata nemenkent


################################################################################################################################
################################################################################################################################
#Preparations


#Setting working directories 
#setwd("/Users/manna/Desktop/untitled folder") #home pc
setwd ("X:/Kimeno/kutato31/kutato31_Toth_Manna_20210518/Munkafajlok") #folder at KSH's pc
#getwd()

#opening budapest codes
sc_codes_bp <- read.csv('SC_codes_bp.csv', sep = ';')


#Required packages 
library(readxl)
library (data.table)
library(dplyr)

 
#Data preparation
#--------------------------------------------------------------------------------------------------------------------------------------------
#Reading data
#1. Population data
#// a nepessegi adatok leirasanak (nepesseg_allomanyleiras.xls) Jaras2015 fulet excelben elmentettem a 20210518/Munkafajlok helyre subcounty_codes neven
#// a nepessegi adatokat (Nepesseg.csv) atmasoltam a napidatum/Munkafajlok helyre

population_data <- read.csv("raw_data//Nepesseg.csv", header = TRUE, sep = ";")
setnames(population_data, old=c("ESEV", "JARAS15", "KOREV", "NEM", "EVK" ),
                        new = c("year", "subcounty", "age", "gender", "population"))
population_data <- as.data.table(population_data)
county_codes <- read.csv("raw_data//subcounty_county_codes.csv", header=TRUE, sep=";")

#merging population data with county data
population_data <- merge(population_data,county_codes,by.x=c("subcounty"), by.y=c("subcounty"))
saveRDS(population_data, "rds//population_data.rds")
#subcounty_codes <- read_excel("Subcounty_codes.xls")



#1. Mortality data
#// a halalozasi adatokat (Halalozas.csv) atmasoltam a 20210518/Munkafajlok helyre
mortality_data <- read.csv("raw_data//Halalozas.csv", header = TRUE, sep = ";")
setnames(mortality_data, old=c("ESEV","NEM", "OK1", "KOREV", "FJARAS15","JARAS15" ),
                       new = c("year","gender", "BNO", "age", "subcounty_living",  "subcounty_death"))
mortality_data <- as.data.table(mortality_data)
saveRDS(mortality_data, "rds//mortality_data.rds")


#Extracting treatable and preventable mortalities by BNO codes (group and deaseas of causes of mortality)
###
#based on OECD/Eurostat list of preventable and treatable causes of death (November 2019 version)
#source: https://www.oecd.org/health/health-systems/Avoidable-mortality-2019-Joint-OECD-Eurostat-List-preventable-treatable-causes-of-death.pdf

#defining BNO codes we need
intestinal <- mortality_data[grepl(paste0(c("A0"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
intestinal <- intestinal[, prev_treat:="prev"]
intestinal <- intestinal[, group:="infectious"]
intestinal <- intestinal[, disease:="intestinal_diseases"]

diphtheria <- mortality_data[grepl(paste0(c("A35", "A36", "A80"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
diphtheria <- diphtheria[, prev_treat:="prev"]
diphtheria <- diphtheria[, group:="infectious"]
diphtheria <- diphtheria[, disease:="diphtheria"]

whooping <- mortality_data[grepl(paste0(c("A37"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
whooping <- whooping[, prev_treat:="prev"]
whooping <- whooping[, group:="infectious"]
whooping <- whooping[, disease:="whooping"]

meningococcal <- mortality_data[grepl(paste0(c("A39"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
meningococcal <- meningococcal[, prev_treat:="prev"]
meningococcal <- meningococcal[, group:="infectious"]
meningococcal <- meningococcal[, disease:="meningococcal"]

sepsis_1 <- mortality_data[grepl(paste0( c("A403", "A413"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
sepsis_1 <- sepsis_1[, prev_treat:="prev"]
sepsis_1 <- sepsis_1[, group:="infectious"]
sepsis_1 <- sepsis_1[, disease:="sepsis_streptococcus_influenzae"]

haemophilus_inflenza <- mortality_data[grepl(paste0(c("A492"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
haemophilus_inflenza <- haemophilus_inflenza[, prev_treat:="prev"]
haemophilus_inflenza <- haemophilus_inflenza[, group:="infectious"]
haemophilus_inflenza <- haemophilus_inflenza[, disease:="haemophilus_inflenza"]

STD <- mortality_data[grepl(paste0(c("A5", "A6", "A63", "A64"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
STD <- STD[, prev_treat:="prev"]
STD <- STD[, group:="infectious"]
STD <- STD[, disease:="STD"]

varicella <- mortality_data[grepl(paste0(c("B01"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
varicella <- varicella[, prev_treat:="prev"]
varicella <- varicella[, group:="infectious"]
varicella <- varicella[, disease:="varicella"]

measles <- mortality_data[grepl(paste0(c("B05"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
measles <- measles[, prev_treat:="prev"]
measles <- measles[, group:="infectious"]
measles <- measles[, disease:="measles"]

rubella <- mortality_data[grepl(paste0(c("B06"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
rubella <- rubella[, prev_treat:="prev"]
rubella <- rubella[, group:="infectious"]
rubella <- rubella[, disease:="rubella"]

hepatitis <- mortality_data[grepl(paste0(c("B15", "B16", "B17", "B18", "B19"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
hepatitis <- hepatitis[, prev_treat:="prev"]
hepatitis <- hepatitis[, group:="infectious"]
hepatitis <- hepatitis[, disease:="hepatitis"]

HIV <- mortality_data[grepl(paste0(c("B20", "B21", "B22", "B23", "B24"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
HIV <- HIV[, prev_treat:="prev"]
HIV <- HIV[, group:="infectious"]
HIV <- HIV[, disease:="HIV"]

malaria <- mortality_data[grepl(paste0(c("B50", "B51", "B52", "B53", "B54"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
malaria <- malaria[, prev_treat:="prev"]
malaria <- malaria[, group:="infectious"]
malaria <- malaria[, disease:="malaria"]

meningitis <- mortality_data[grepl(paste0(c("G000", "G001"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
meningitis <- meningitis [, prev_treat:="prev"]
meningitis <- meningitis[, group:="infectious"]
meningitis <- meningitis[, disease:="meningitis"]

tuberculosis  <- mortality_data[grepl(paste0(c("A15", "A16", "A17",
"A18", "A19", "B90", "J65"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
tuberculosis  <- tuberculosis[, prev_treat:="prev_treat"]
tuberculosis  <- tuberculosis[, group:="infectious"]
tuberculosis  <- tuberculosis[, disease:="tuberculosis"]

scarlet  <- mortality_data[grepl(paste0(c("A38"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
scarlet  <- scarlet[, prev_treat:="treat"]
scarlet  <- scarlet[, group:="infectious"]
scarlet  <- scarlet[, disease:="scarlet"]

sepsis_2 <- mortality_data[grepl(paste0(c("A40", "A41"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
sepsis_2 <- sepsis_2[!grepl(paste0(c("A403","A413"),  collapse='|'), sepsis_2$BNO,ignore.case = TRUE), ]
sepsis_2 <- sepsis_2[, prev_treat:="treat"]
sepsis_2 <- sepsis_2[, group:="infectious"]
sepsis_2 <- sepsis_2[, disease:="sepsis"]

cellulitis <- mortality_data[grepl(paste0(c("A46", "L03"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
cellulitis <- cellulitis[, prev_treat:="treat"]
cellulitis <- cellulitis[, group:="infectious"]
cellulitis <- cellulitis[, disease:="cellulitis"]

legionnaires <- mortality_data[grepl(paste0(c("A481"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
legionnaires <- legionnaires[, prev_treat:="treat"]
legionnaires <- legionnaires[, group:="infectious"]
legionnaires <- legionnaires[, disease:="legionnaires"]

s_e_infection <- mortality_data[grepl(paste0(c("A491"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
s_e_infection <- s_e_infection[, prev_treat:="treat"]
s_e_infection <- s_e_infection[, group:="infectious"]
s_e_infection <- s_e_infection[, disease:="s_e_infection"]

other_meningitis <- mortality_data[grepl(paste0(c("G002", "G003", "G008", "G009"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
other_meningitis <- other_meningitis[, prev_treat:="treat"]
other_meningitis <- other_meningitis[, group:="infectious"]
other_meningitis <- other_meningitis[, disease:="other_meningitis"]

unspecified_meningitis <- mortality_data[grepl(paste0(c("G03"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
unspecified_meningitis <- unspecified_meningitis[, prev_treat:="treat"]
unspecified_meningitis <- unspecified_meningitis[, group:="infectious"]
unspecified_meningitis <- unspecified_meningitis[, disease:="unspecified_meningitis"]

oral_cancer <- mortality_data[grepl(paste0(c("C0", "C11", "C12", "C13", "C14"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
oral_cancer <- oral_cancer[, prev_treat:="prev"]
oral_cancer <- oral_cancer[, group:="cancer"]
oral_cancer <- oral_cancer[, disease:="oral_cancer"]

oesophageal_cancer <- mortality_data[grepl(paste0(c("C15"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
oesophageal_cancer <- oesophageal_cancer[, prev_treat:="prev"]
oesophageal_cancer <- oesophageal_cancer[, group:="cancer"]
oesophageal_cancer <- oesophageal_cancer[, disease:="oesophageal_cancer"]

stomach_cancer <- mortality_data[grepl(paste0(c("C16"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
stomach_cancer <- stomach_cancer[, prev_treat:="prev"]
stomach_cancer <- stomach_cancer[, group:="cancer"]
stomach_cancer <- stomach_cancer[, disease:="stomach_cancer"]

liver_cancer <- mortality_data[grepl(paste0(c("C22"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
liver_cancer <- liver_cancer[, prev_treat:="prev"]
liver_cancer <- liver_cancer[, group:="cancer"]
liver_cancer <- liver_cancer[, disease:="liver_cancer"]

lung_cancer <- mortality_data[grepl(paste0(c("C33", "C34"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
lung_cancer <- lung_cancer[, prev_treat:="prev"]
lung_cancer <- lung_cancer[, group:="cancer"]
lung_cancer <- lung_cancer[, disease:="lung_cancer"]

mesothelioma <- mortality_data[grepl(paste0(c("C45"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
mesothelioma <- mesothelioma[, prev_treat:="prev"]
mesothelioma <- mesothelioma[, group:="cancer"]
mesothelioma <- mesothelioma[, disease:="mesothelioma"]

melanoma <- mortality_data[grepl(paste0(c("C43"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
melanoma <- melanoma[, prev_treat:="prev"]
melanoma <- melanoma[, group:="cancer"]
melanoma <- melanoma[, disease:="melanoma"]

bladder_cancer <- mortality_data[grepl(paste0(c("C67"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
bladder_cancer <- bladder_cancer[, prev_treat:="prev"]
bladder_cancer <- bladder_cancer[, group:="cancer"]
bladder_cancer <- bladder_cancer[, disease:="bladder_cancer"]

cervical_cancer <- mortality_data[grepl(paste0(c("C53"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
cervical_cancer <- cervical_cancer[, prev_treat:="prev_treat"]
cervical_cancer <- cervical_cancer[, group:="cancer"]
cervical_cancer <- cervical_cancer[, disease:="cervical_cancer"]

colorectal_cancer <- mortality_data[grepl(paste0(c("C18", "C19", "C20", "C21"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
colorectal_cancer <- colorectal_cancer[, prev_treat:="treat"]
colorectal_cancer <- colorectal_cancer[, group:="cancer"]
colorectal_cancer <- colorectal_cancer[, disease:="colorectal_cancer"]
  
breast_cancer <- mortality_data[grepl(paste0(c("C50"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
breast_cancer<- breast_cancer[breast_cancer$gender == 2,] #including women only
breast_cancer <- breast_cancer[, prev_treat:="treat"]
breast_cancer <- breast_cancer[, group:="cancer"]
breast_cancer <- breast_cancer[, disease:="breast_cancer"]

uterus_cancer <- mortality_data[grepl(paste0(c("C54", "C55"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
uterus_cancer <- uterus_cancer[, prev_treat:="treat"]
uterus_cancer <- uterus_cancer[, group:="cancer"]
uterus_cancer <- uterus_cancer[, disease:="uterus_cancer"]

testicular_cancer <- mortality_data[grepl(paste0(c("C62"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
testicular_cancer <- testicular_cancer[, prev_treat:="treat"]
testicular_cancer <- testicular_cancer[, group:="cancer"]
testicular_cancer <- testicular_cancer[, disease:="testicular_cancer"]

thyroid_cancer <- mortality_data[grepl(paste0(c("C73"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
thyroid_cancer <- thyroid_cancer[, prev_treat:="treat"]
thyroid_cancer <- thyroid_cancer[, group:="cancer"]
thyroid_cancer <- thyroid_cancer[, disease:="thyroid_cancer"]

hodgkins <- mortality_data[grepl(paste0(c("C81"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
hodgkins <- hodgkins[, prev_treat:="treat"]
hodgkins <- hodgkins[, group:="cancer"]
hodgkins <- hodgkins[, disease:="hodgkins"]

leukaemia <- mortality_data[grepl(paste0(c("C910", "C911"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
leukaemia <- leukaemia[, prev_treat:="treat"]
leukaemia <- leukaemia[, group:="cancer"]
leukaemia <- leukaemia[, disease:="leukaemia"]

benign_neoplasm <- mortality_data[grepl(paste0(c("D1", "D2",
"D31", "D32", "D33", "D34", "D35", "D36"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
benign_neoplasm <- benign_neoplasm[, prev_treat:="treat"]
benign_neoplasm <- benign_neoplasm[, group:="cancer"]
benign_neoplasm <- benign_neoplasm[, disease:="benign_neoplasm"]

nutritional_deficiency <- mortality_data[grepl(paste0(c("D50", "D51",
                          "D52", "D53"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
nutritional_deficiency <- nutritional_deficiency[, prev_treat:="prev"]
nutritional_deficiency <- nutritional_deficiency[, group:="endocrine"]
nutritional_deficiency <- nutritional_deficiency[, disease:="nutritional_deficiency"]

diabetes <- mortality_data[grepl(paste0(c("E10", "E11",
"E12", "E13", "E14"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
diabetes <- diabetes[, prev_treat:="prev_treat"]
diabetes <- diabetes[, group:="endocrine"]
diabetes <- diabetes[, disease:="diabetes"]

thyroid_disorders <- mortality_data[grepl(paste0(c("E00", "E01",
"E02", "E03", "E04", "E05", "E06", "E07"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
thyroid_disorders <- thyroid_disorders[, prev_treat:="treat"]
thyroid_disorders <- thyroid_disorders[, group:="endocrine"]
thyroid_disorders <- thyroid_disorders[, disease:="thyroid_disorders"]

adrenal_disorders <- mortality_data[grepl(paste0(c("E24", "E25",
"E27"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
adrenal_disorders <- adrenal_disorders[!grepl(paste0(c("E244"),  collapse='|'), adrenal_disorders$BNO,ignore.case = TRUE), ]
adrenal_disorders <- adrenal_disorders[, prev_treat:="treat"]
adrenal_disorders <- adrenal_disorders[, group:="endocrine"]
adrenal_disorders <- adrenal_disorders[, disease:="adrenal_disorders"]

epilepsy <- mortality_data[grepl(paste0(c("G40", "G41"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
epilepsy <- epilepsy[, prev_treat:="treat"]
epilepsy <- epilepsy[, group:="nervous"]
epilepsy <- epilepsy[, disease:="epilepsy"]

aortic_aneurysm <- mortality_data[grepl(paste0(c("I71"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
aortic_aneurysm <- aortic_aneurysm[, prev_treat:="prev_treat"]
aortic_aneurysm <- aortic_aneurysm[, group:="circulatory"]
aortic_aneurysm <- aortic_aneurysm[, disease:="aortic_aneurysm"]

hypertensive <- mortality_data[grepl(paste0(c("I10", "I11", "I12", "I13", "I15"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
hypertensive <- hypertensive[, prev_treat:="prev_treat"]
hypertensive <- hypertensive[, group:="circulatory"]
hypertensive <- hypertensive[, disease:="hypertensive"]

IHD <- mortality_data[grepl(paste0(c("I20", "I21", "I22", "I23", "I24", "I25"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
IHD <- IHD[, prev_treat:="prev_treat"]
IHD <- IHD[, group:="circulatory"]
IHD <- IHD[, disease:="IHD"]

cerebrovascular <- mortality_data[grepl(paste0(c("I6"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
cerebrovascular <- cerebrovascular[, prev_treat:="prev_treat"]
cerebrovascular <- cerebrovascular[, group:="circulatory"]
cerebrovascular <- cerebrovascular[, disease:="cerebrovascular"]

atherosclerosis <- mortality_data[grepl(paste0(c("I70", "I739"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
atherosclerosis <- atherosclerosis[, prev_treat:="prev_treat"]
atherosclerosis <- atherosclerosis[, group:="circulatory"]
atherosclerosis <- atherosclerosis[, disease:="atherosclerosis"]

rheumatic <- mortality_data[grepl(paste0(c("I0"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
rheumatic <- rheumatic[, prev_treat:="treat"]
rheumatic <- rheumatic[, group:="circulatory"]
rheumatic <- rheumatic[, disease:="rheumatic"]

thromboembolism <- mortality_data[grepl(paste0(c("I26", "I80", "I829"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
thromboembolism <- thromboembolism[, prev_treat:="treat"]
thromboembolism <- thromboembolism[, group:="circulatory"]
thromboembolism <- thromboembolism[, disease:="thromboembolism"]

influenza <- mortality_data[grepl(paste0(c("J09", "J10", "J11"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
influenza <- influenza[, prev_treat:="prev"]
influenza <- influenza[, group:="respiratory"]
influenza <- influenza[, disease:="influenza"]

pneumonia_sh <- mortality_data[grepl(paste0(c("J13", "J14"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
pneumonia_sh <- pneumonia_sh[, prev_treat:="prev"]
pneumonia_sh <- pneumonia_sh[, group:="respiratory"]
pneumonia_sh <- pneumonia_sh[, disease:="pneumonia_sh"]

chronic_lower <- mortality_data[grepl(paste0(c("J40", "J41", "J42", "J43", "J44"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
chronic_lower <- chronic_lower[, prev_treat:="prev"]
chronic_lower <- chronic_lower[, group:="respiratory"]
chronic_lower <- chronic_lower[, disease:="chronic_lower"]

lung_diseases <- mortality_data[grepl(paste0(c("J6", "J70", "J82", "J92"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
lung_diseases <- lung_diseases[!grepl(paste0(c("J65"),  collapse='|'), lung_diseases$BNO,ignore.case = TRUE), ]
lung_diseases <- lung_diseases[, prev_treat:="prev"]
lung_diseases <- lung_diseases[, group:="respiratory"]
lung_diseases <- lung_diseases[, disease:="lung_diseases"]

upper_respiratory <- mortality_data[grepl(paste0(c("J00", "J01", "J02",
"J03", "J04", "J05", "J06", "J3"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
upper_respiratory <- upper_respiratory[, prev_treat:="treat"]
upper_respiratory <- upper_respiratory[, group:="respiratory"]
upper_respiratory <- upper_respiratory[, disease:="upper_respiratory_infections"]

unspecified_pneumonia <- mortality_data[grepl(paste0(c("J12", "J15", "J16",
"J17", "J18"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
unspecified_pneumonia <- unspecified_pneumonia[, prev_treat:="treat"]
unspecified_pneumonia <- unspecified_pneumonia[, group:="respiratory"]
unspecified_pneumonia <- unspecified_pneumonia[, disease:="unspecified_pneumonia"]

lower_respiratory <- mortality_data[grepl(paste0(c("J20", "J21", "J22"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
lower_respiratory <- lower_respiratory[, prev_treat:="treat"]
lower_respiratory <- lower_respiratory[, group:="respiratory"]
lower_respiratory <- lower_respiratory[, disease:="lower_respiratory"]

asthma <- mortality_data[grepl(paste0(c("J45", "J46", "J47"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
asthma <- asthma[, prev_treat:="treat"]
asthma <- asthma[, group:="respiratory"]
asthma <- asthma[, disease:="asthma"]

respiratory_distress_syndrome <- mortality_data[grepl(paste0(c("J80"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
respiratory_distress_syndrome <- respiratory_distress_syndrome[, prev_treat:="treat"]
respiratory_distress_syndrome <- respiratory_distress_syndrome[, group:="respiratory"]
respiratory_distress_syndrome <- respiratory_distress_syndrome[, disease:="respiratory_distress_syndrome"]

pulmonary_oedema <- mortality_data[grepl(paste0(c("J81"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
pulmonary_oedema <- pulmonary_oedema[, prev_treat:="treat"]
pulmonary_oedema <- pulmonary_oedema[, group:="respiratory"]
pulmonary_oedema <- pulmonary_oedema[, disease:="pulmonary_oedema"]

pyothorax <- mortality_data[grepl(paste0(c("J85", "J86"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
pyothorax <- pyothorax[, prev_treat:="treat"]
pyothorax <- pyothorax[, group:="respiratory"]
pyothorax <- pyothorax[, disease:="abscess_of_lung_pyothorax"]

pleural_disorders <- mortality_data[grepl(paste0(c("J90", "J93", "J94"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
pleural_disorders <- pleural_disorders[, prev_treat:="treat"]
pleural_disorders <- pleural_disorders[, group:="respiratory"]
pleural_disorders <- pleural_disorders[, disease:="pleural_disorders"]

g_ulcer <- mortality_data[grepl(paste0(c("K25", "K26", "K27", "K28"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
g_ulcer <- g_ulcer[, prev_treat:="treat"]
g_ulcer <- g_ulcer[, group:="digestive"]
g_ulcer <- g_ulcer[, disease:="g_ulcer"]

appendicitis <- mortality_data[grepl(paste0(c("K35", "K36", "K37", "K38"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
appendicitis  <- appendicitis [, prev_treat:="treat"]
appendicitis  <- appendicitis [, group:="digestive"]
appendicitis  <- appendicitis [, disease:="appendicitis"]

abdominal_hernia <- mortality_data[grepl(paste0(c("K40", "K41", "K42", "K43", "K44", "K45", "K46"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
abdominal_hernia <- abdominal_hernia[, prev_treat:="treat"]
abdominal_hernia <- abdominal_hernia[, group:="digestive"]
abdominal_hernia <- abdominal_hernia[, disease:="abdominal_hernia"]

cholecystitis <- mortality_data[grepl(paste0(c("K80", "K81"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
cholecystitis <- cholecystitis[, prev_treat:="treat"]
cholecystitis <- cholecystitis[, group:="digestive"]
cholecystitis <- cholecystitis[, disease:="Cholelithiasis_cholecystitis"]

gallbladder <- mortality_data[grepl(paste0(c("K82", "K83"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
gallbladder <- gallbladder[, prev_treat:="treat"]
gallbladder <- gallbladder[, group:="digestive"]
gallbladder <- gallbladder[, disease:="gallbladder_biliary_tract"]

pancreatitis <- mortality_data[grepl(paste0(c("K850", "K851", "K853", "K858", "K859"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
pancreatitis <- pancreatitis[, prev_treat:="treat"]
pancreatitis <- pancreatitis[, group:="digestive"]
pancreatitis <- pancreatitis[, disease:="acute_pancreatitis"]

other_pancreas <- mortality_data[grepl(paste0(c("K861", "K862", "K863", "K868", "K869"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
other_pancreas <- other_pancreas[, prev_treat:="treat"]
other_pancreas <- other_pancreas[, group:="digestive"]
other_pancreas <- other_pancreas[, disease:="other_pancreas"]

nephrosis <- mortality_data[grepl(paste0(c("N00", "N01", "N02",
"N03", "N04", "N05", "N06", "N07"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
nephrosis <- nephrosis[, prev_treat:="treat"]
nephrosis <- nephrosis[, group:="genitourinary"]
nephrosis <- nephrosis[, disease:="nephrosis"]

uropathy <- mortality_data[grepl(paste0(c("N13", "N20", "N21",
"N35"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
uropathy <- uropathy[, prev_treat:="treat"]
uropathy <- uropathy[, group:="genitourinary"]
uropathy <- uropathy[, disease:="uropathy"]

renal_failure <- mortality_data[grepl(paste0(c("N17", "N18", "N19"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
renal_failure <- renal_failure[, prev_treat:="treat"]
renal_failure <- renal_failure[, group:="genitourinary"]
renal_failure <- renal_failure[, disease:="renal_failure"]

renal_colic <- mortality_data[grepl(paste0(c("N23"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
renal_colic <- renal_colic[, prev_treat:="treat"]
renal_colic <- renal_colic[, group:="genitourinary"]
renal_colic <- renal_colic[, disease:="renal_colic"]

renal_dysfunction <- mortality_data[grepl(paste0(c("N25"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
renal_dysfunction <- renal_dysfunction[, prev_treat:="treat"]
renal_dysfunction <- renal_dysfunction[, group:="genitourinary"]
renal_dysfunction <- renal_dysfunction[, disease:="renal_dysfunction"]

contracted_kidney <- mortality_data[grepl(paste0(c("N26","N27"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
contracted_kidney <- contracted_kidney[, prev_treat:="treat"]
contracted_kidney <- contracted_kidney[, group:="genitourinary"]
contracted_kidney <- contracted_kidney[, disease:="contracted_kidney"]

inflammatory_diseases <- mortality_data[grepl(paste0(c("N341","N70","N71","N72","N73",
"N750","N751","N764","N766"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
inflammatory_diseases <- inflammatory_diseases[, prev_treat:="treat"]
inflammatory_diseases <- inflammatory_diseases[, group:="genitourinary"]
inflammatory_diseases <- inflammatory_diseases[, disease:="inflammatory_diseases"]

prostatic_hyperplasia <- mortality_data[grepl(paste0(c("N40"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
prostatic_hyperplasia <- prostatic_hyperplasia[, prev_treat:="treat"]
prostatic_hyperplasia <- prostatic_hyperplasia[, group:="genitourinary"]
prostatic_hyperplasia <- prostatic_hyperplasia[, disease:="prostatic_hyperplasia"]

tetanus_neonatorum <- mortality_data[grepl(paste0(c("A33"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
tetanus_neonatorum <- tetanus_neonatorum[, prev_treat:="prev"]
tetanus_neonatorum <- tetanus_neonatorum[, group:="pregnancy"]
tetanus_neonatorum <- tetanus_neonatorum[, disease:="tetanus_neonatorum"]

obstetrical_tetanus <- mortality_data[grepl(paste0(c("A34"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
obstetrical_tetanus <- obstetrical_tetanus[, prev_treat:="prev"]
obstetrical_tetanus <- obstetrical_tetanus[, group:="pregnancy"]
obstetrical_tetanus <- obstetrical_tetanus[, disease:="obstetrical_tetanus"]

pregnancy_puerperium <- mortality_data[grepl(paste0(c("O"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
pregnancy_puerperium <- pregnancy_puerperium[, prev_treat:="treat"]
pregnancy_puerperium <- pregnancy_puerperium[, group:="pregnancy"]
pregnancy_puerperium <- pregnancy_puerperium[, disease:="pregnancy_puerperium"]

perinatal_period <- mortality_data[grepl(paste0(c("P"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
perinatal_period <- perinatal_period[!grepl(paste0(c("P97","P98","P99"),  collapse='|'), perinatal_period$BNO,ignore.case = TRUE), ]
perinatal_period <- perinatal_period[, prev_treat:="treat"]
perinatal_period <- perinatal_period[, group:="pregnancy"]
perinatal_period <- perinatal_period[, disease:="perinatal_period"]

neural_tube_defects <- mortality_data[grepl(paste0(c("Q00", "Q01", "Q05"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
neural_tube_defects <- neural_tube_defects[, prev_treat:="prev"]
neural_tube_defects <- neural_tube_defects[, group:="congenital"]
neural_tube_defects <- neural_tube_defects[, disease:="neural_tube_defects"]

heart_defects <- mortality_data[grepl(paste0(c("Q2"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
heart_defects <- heart_defects[!grepl(paste0(c("Q29"),  collapse='|'), heart_defects$BNO,ignore.case = TRUE), ]
heart_defects <- heart_defects[, prev_treat:="treat"]
heart_defects <- heart_defects[, group:="congenital"]
heart_defects <- heart_defects[, disease:="heart_defects"]

drugs_a <- mortality_data[grepl(paste0(c("Y4", "Y5"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
drugs_a <- drugs_a[, prev_treat:="treat"]
drugs_a <- drugs_a[, group:="adverse_effects"]
drugs_a <- drugs_a[, disease:="drugs_a"]

misadventures <- mortality_data[grepl(paste0(c("Y6", "Y83", "Y84"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
misadventures <- misadventures[, prev_treat:="treat"]
misadventures <- misadventures[, group:="adverse_effects"]
misadventures <- misadventures[, disease:="misadventures"]

medical_devices <- mortality_data[grepl(paste0(c("Y7", "Y81", "Y82"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
medical_devices <- medical_devices[, prev_treat:="treat"]
medical_devices <- medical_devices[, group:="adverse_effects"]
medical_devices <- medical_devices[, disease:="medical_devices"]

transport_accidents <- mortality_data[grepl(paste0(c("V"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]

transport_accidents <- transport_accidents[!grepl(paste0(c("V00"),  collapse='|'), transport_accidents$BNO,ignore.case = TRUE), ]
transport_accidents <- transport_accidents[, prev_treat:="prev"]
transport_accidents <- transport_accidents[, group:="injuries"]
transport_accidents <- transport_accidents[, disease:="transport_accidents"]

accidental_injuries <- mortality_data[grepl(paste0(c("W", "X0", "X1", "X2", "X3",
"X46","X47","X48","X49","X5"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
accidental_injuries  <- accidental_injuries [, prev_treat:="prev"]
accidental_injuries  <- accidental_injuries[, group:="injuries"]
accidental_injuries  <- accidental_injuries[, disease:="accidental_injuries"]

self_harm <- mortality_data[grepl(paste0(c("X66", "X67", "X68", "X69", "X7",
"X80","X81","X82","X83","X84"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
self_harm  <- self_harm[, prev_treat:="prev"]
self_harm  <- self_harm[, group:="injuries"]
self_harm  <- self_harm[, disease:="self_harm"]

undetermined_intent <- mortality_data[grepl(paste0(c("Y16", "Y17", "Y18", "Y19", "Y2",
"Y30","Y31","Y32","Y33","Y34"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
undetermined_intent  <- undetermined_intent[, prev_treat:="prev"]
undetermined_intent  <- undetermined_intent[, group:="injuries"]
undetermined_intent  <- undetermined_intent[, disease:="undetermined_intent"]

assault <- mortality_data[grepl(paste0(c("X86", "X87", "X88",
"X89", "X9","Y0"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
assault  <- assault[, prev_treat:="prev"]
assault  <- assault[, group:="injuries"]
assault  <- assault[, disease:="assault"]

alcohol <- mortality_data[grepl(paste0(c("E244", "F10", "G312", "G621", "G721",
"I426", "K292", "K70", "K852", "K860", "Q860", "R780", "X45", "X65", "Y15"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
alcohol  <- alcohol[, prev_treat:="prev"]
alcohol  <- alcohol[, group:="alcohol"]
alcohol  <- alcohol[, disease:="alcohol"]

alcohol_other <- mortality_data[grepl(paste0(c("K73", "K740", "K741", "K742",
"K746" ), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
alcohol_other <- alcohol_other[, prev_treat:="prev"]
alcohol_other <- alcohol_other[, group:="alcohol"]
alcohol_other <- alcohol_other[, disease:="alcohol_other"]

drugs <- mortality_data[grepl(paste0(c("F1", "X40", "X41", "X42",
"X43", "X44", "X85", "Y10", "Y11", "Y12", "Y13", "Y14"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
drugs <- drugs[!grepl(paste0(c("F10","F17"),  collapse='|'), drugs$BNO,ignore.case = TRUE), ]
drugs <- drugs[, prev_treat:="prev"]
drugs <- drugs[, group:="drugs"]
drugs <- drugs[, disease:="drugs"]


self_poisonings <- mortality_data[grepl(paste0(c("X60", "X61", "X62", "X63",
"X64"), collapse='|'), mortality_data$BNO, ignore.case = TRUE),]
self_poisonings <- self_poisonings[, prev_treat:="prev"]
self_poisonings <- self_poisonings[, group:="drugs"]
self_poisonings <- self_poisonings[, disease:="self_poisonings"]

#rbinding disease tables
full_avoidable <- rbind(
intestinal, diphtheria, whooping, meningococcal, sepsis_1, haemophilus_inflenza, STD, varicella, measles, 
rubella, hepatitis, HIV, malaria, meningitis, tuberculosis, scarlet, sepsis_2, cellulitis, legionnaires, s_e_infection,
other_meningitis, unspecified_meningitis, oral_cancer, oesophageal_cancer, stomach_cancer, liver_cancer, lung_cancer, 
mesothelioma, melanoma, bladder_cancer, cervical_cancer, colorectal_cancer, breast_cancer, uterus_cancer,testicular_cancer,
thyroid_cancer, hodgkins, leukaemia, benign_neoplasm, nutritional_deficiency, diabetes, thyroid_disorders, adrenal_disorders, 
epilepsy, aortic_aneurysm, hypertensive, IHD, cerebrovascular, atherosclerosis, rheumatic, thromboembolism, influenza,
pneumonia_sh, chronic_lower, lung_diseases, upper_respiratory, unspecified_pneumonia, lower_respiratory, asthma, 
respiratory_distress_syndrome, pulmonary_oedema, pyothorax, pleural_disorders, g_ulcer, appendicitis, abdominal_hernia,
cholecystitis, gallbladder, pancreatitis, other_pancreas, nephrosis, uropathy, renal_failure, renal_colic,renal_dysfunction, 
contracted_kidney, inflammatory_diseases, prostatic_hyperplasia, tetanus_neonatorum, obstetrical_tetanus, pregnancy_puerperium, 
perinatal_period, neural_tube_defects, heart_defects, drugs_a, misadventures, medical_devices,transport_accidents, 
accidental_injuries, self_harm, undetermined_intent, assault, alcohol, alcohol_other, drugs, self_poisonings)

saveRDS(full_avoidable, "rds//full_avoidable_data.rds")
#write.csv(full_avoidable, "full_avoidable_data.csv", header = TRUE, sep = ";")
#----------------------------------------------------------------------------------------------------------------------------

## I.MIDYEAR POPULATION
#####


#reading population and county codes data
population_data  <- readRDS('rds//population_data.rds')

#creating new sub-county codes for taking Budapest as one unit as a whole
population_data <- merge(population_data, sc_codes_bp, by.x = 'subcounty', by.y = 'Kód')
population_data <- population_data %>% select(-c(subcounty, county, Járás.megnevezés))

#keeping ppl who are younger than 75
pop_75 <- population_data[age < 75]

#creating agegroups
pop_75 <- pop_75 %>% mutate(agegroup = case_when(age < 1 ~ 1, #//korcsoportokat kepzek a sztandardizalashoz
                                                                                 age >= 1   & age < 5  ~ 2,
                                                                                 age >= 5   & age < 10 ~ 3,
                                                                                 age >= 10  & age < 15 ~ 4,
                                                                                 age >= 15  & age < 20 ~ 5,
                                                                                 age >= 20  & age < 25 ~ 6,
                                                                                 age >= 25  & age < 30 ~ 7,
                                                                                 age >= 30  & age < 35 ~ 8,
                                                                                 age >= 35  & age < 40 ~ 9,
                                                                                 age >= 40  & age < 45 ~ 10,
                                                                                 age >= 45  & age < 50 ~ 11,
                                                                                 age >= 50  & age < 55 ~ 12,
                                                                                 age >= 55  & age < 60 ~ 13,
                                                                                 age >= 60  & age < 65 ~ 14,
                                                                                 age >= 65  & age < 70 ~ 15,
                                                                                 age >= 70  & age < 75 ~ 16))
saveRDS(pop_75, 'rds\\pop_75.rds')


#highschool population
#// output 1.
highschool_population <- filter(population_data, age >= 14 & age < 21)
highschool_population <-   highschool_population %>%
  group_by(SC_code_bp, year) %>%
  summarize(pop14_20=sum(population)) 


write.csv(highschool_population,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Kutatasi_eredmenyek\\highschool_population.csv", row.names = FALSE) #///// OUTPUT

#----------------------------------------------------------------------------------------------------------------------------

## II. MORTALITY RATIOS
#----------------------------------------------------------------------------------------------------------------------------
#Preparing data
#Data cleaning and prepartion
#####
#opening previously cleaned population and avoidable mortality data
full_avoidable_data <- readRDS("rds//full_avoidable_data.rds")

#opening budapest codes
sc_codes_bp <- read.csv('SC_codes_bp.csv', sep = ';')

full_avoidable_data <- merge(full_avoidable_data, sc_codes_bp, by.x = 'subcounty_living', by.y = 'Kód')
full_avoidable_data <- full_avoidable_data %>% select(-c(subcounty_living, subcounty_death, Járás.megnevezés, disease))


#creating avoidable subsets and calculating number of dead people (in the current data 1 row stands for 1 person)
#0-74
avoidable_75_data <- full_avoidable_data[age < 75]

avoidable_75_data <- avoidable_75_data[!(avoidable_75_data$SC_code_bp== "888" | avoidable_75_data$SC_code_bp == "666"|
                                         avoidable_75_data$SC_code_bp == "777"  ),]

avoidable_75_data <- avoidable_75_data  %>%
  group_by(year, SC_code_bp, gender, age, group, BNO, prev_treat)  %>%  tally()
colnames(avoidable_75_data)[colnames(avoidable_75_data) == "n"] <- "patient_0" #patient_0 = original number of individual observations
avoidable_75_data <- avoidable_75_data %>% mutate(patient =  #patient = patient number for treatable and preventable mortality ratios (certain diseases only worth 0.5)
                                                    case_when(
                                                      prev_treat == "prev"       ~ patient_0 * 1,
                                                      prev_treat == "treat"      ~ patient_0 * 1,
                                                      prev_treat == "prev_treat" ~ patient_0 * 0.5))


avoidable_75_data_agegroups <- avoidable_75_data %>% mutate(agegroup = case_when(age < 1 ~ 1, #//korcsoportokat kepzek a sztandardizalashoz
                                                                                 age >= 1   & age < 5  ~ 2,
                                                                                 age >= 5   & age < 10 ~ 3,
                                                                                 age >= 10  & age < 15 ~ 4,
                                                                                 age >= 15  & age < 20 ~ 5,
                                                                                 age >= 20  & age < 25 ~ 6,
                                                                                 age >= 25  & age < 30 ~ 7,
                                                                                 age >= 30  & age < 35 ~ 8,
                                                                                 age >= 35  & age < 40 ~ 9,
                                                                                 age >= 40  & age < 45 ~ 10,
                                                                                 age >= 45  & age < 50 ~ 11,
                                                                                 age >= 50  & age < 55 ~ 12,
                                                                                 age >= 55  & age < 60 ~ 13,
                                                                                 age >= 60  & age < 65 ~ 14,
                                                                                 age >= 65  & age < 70 ~ 15,
                                                                                 age >= 70  & age < 75 ~ 16))

treatable_75_data2 <- avoidable_75_data_agegroups[avoidable_75_data_agegroups$prev_treat == "treat"| avoidable_75_data_agegroups$prev_treat == "prev_treat",]
preventable_75_data2 <- avoidable_75_data_agegroups[avoidable_75_data_agegroups$prev_treat == "prev"| avoidable_75_data_agegroups$prev_treat == "prev_treat",]

saveRDS(treatable_75_data2, 'rds//full_treatable_75_data.rds')
saveRDS(preventable_75_data2, 'rds//full_preventable_75_data.rds')

#Merging population and mortality tables
pop_75 <- readRDS("rds//pop_75.rds")
treat_75_data <- readRDS('rds//full_treatable_75_data.rds')
prev_75_data <- readRDS('rds//full_preventable_75_data.rds')


pop_75 <- pop_75 %>%  
  group_by(year, SC_code_bp, agegroup, gender) %>%
  summarize(population=sum(population))

prev_data <- prev_75_data %>%
  group_by(year, SC_code_bp, agegroup, gender) %>%
  summarize(patient=sum(patient), patient_0= sum(patient_0))

treat_data <- treat_75_data %>%
  group_by(year, SC_code_bp, agegroup, gender) %>%
  summarize(patient=sum(patient), patient_0= sum(patient_0))


prev_pop <- merge(pop_75, prev_data, by.x=c("year", "SC_code_bp", "agegroup", "gender"),
                  by.y=c("year", "SC_code_bp", "agegroup", "gender"), all.x = TRUE)

prev_pop$patient[is.na(prev_pop$patient)] <- 0
prev_pop$patient_0[is.na(prev_pop$patient_0)] <- 0


treat_pop <- merge(pop_75, treat_data, by.x=c("year", "SC_code_bp", "agegroup", "gender"),
                   by.y=c("year", "SC_code_bp", "agegroup", "gender"), all.x = TRUE)

treat_pop$patient[is.na(treat_pop$patient)] <- 0
treat_pop$patient_0[is.na(treat_pop$patient_0)] <- 0


saveRDS(prev_pop, 'rds//prev_pop_75_agegroup_data.rds') #the basis of further preventable mortality calculations
saveRDS(treat_pop, 'rds//treat_pop_75_agegroup_data.rds') #the basis of further treatable mortality calculations


#######

## 1. CountRy
#####
#countRy total treatable

treat_pop <- readRDS ('rds//treat_pop_75_agegroup_data.rds')
pop_agegroups <- ungroup(treat_pop) %>%  #//levalogatom az egyesitett, korcsoportok szerinti adatokbol, amire szuksegem van
  group_by(year, agegroup) %>%
  summarize(patient=sum(patient), patient_0= sum(patient_0), population=sum(population)) 


pop_agegroups <- ungroup(pop_agegroups)  %>% mutate(PMR = patient/population) 


pop_agegroups <- as.data.table(ungroup(pop_agegroups)  %>%#//halalozasi rata /100 000 fo
                                 mutate(PMR100000 = PMR *100000))

pop_agegroups <- ungroup(pop_agegroups) %>% mutate(stdr_PMR100000 = #100 000 fore szamitott rata beszorzasa a sulyokkal (forr?s ESP2013)
                                                     case_when(agegroup == 1     ~ PMR100000 * 0.010989011,
                                                               agegroup == 2     ~ PMR100000 * 0.043956044,
                                                               agegroup == 3     ~ PMR100000 * 0.06043956,
                                                               agegroup == 4     ~ PMR100000 * 0.06043956,
                                                               agegroup == 5     ~ PMR100000 * 0.06043956,
                                                               agegroup == 6     ~ PMR100000 * 0.065934066,
                                                               agegroup == 7     ~ PMR100000 * 0.065934066,
                                                               agegroup == 8     ~ PMR100000 * 0.071428571,
                                                               agegroup == 9     ~ PMR100000 * 0.076923077,
                                                               agegroup == 10     ~ PMR100000 * 0.076923077,
                                                               agegroup == 11     ~ PMR100000 * 0.076923077,
                                                               agegroup == 12     ~ PMR100000 * 0.076923077,
                                                               agegroup == 13     ~ PMR100000 * 0.071428571,
                                                               agegroup == 14     ~ PMR100000 * 0.065934066,
                                                               agegroup == 15     ~ PMR100000 * 0.06043956,
                                                               agegroup == 16     ~ PMR100000 * 0.054945055))

pop_fin <- ungroup(pop_agegroups) %>% #//adatok aggregalasa a szukseges csoportok szerint
  group_by(year) %>%
  summarize(population=sum(population), patient=sum(patient), patient_0=sum(patient_0), s_PMR=sum(stdr_PMR100000))

KSH_treat_contRy <- pop_fin #//ellenorzo tabla a KSH szamara, 
#//patient_0=halalozasi esetszam, population=nepesseg szama
a <- KSH_treat_contRy %>% filter_at(vars(patient_0), any_vars(. %in% c(1,2,3))) #//nincs ilyen
b <- KSH_treat_contRy %>% filter_at(vars(population), any_vars(. %in% c(1,2,3))) #//nincs ilyen
OUT_treat_contRy <- KSH_treat_contRy[, c("year", "s_PMR", "patient")] 
write.csv(OUT_treat_contRy,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Kutatasi_eredmenyek\\treat_countRy.csv", row.names = FALSE) #////////OUTPUT
write.csv(KSH_treat_contRy,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Adv_ellenorzeshez\\treat_countRy_KSH.csv", row.names = FALSE) #KSH
####

#countRy gender treatable
####
treat_pop <- readRDS ('rds//treat_pop_75_agegroup_data.rds')
pop_agegroups <- ungroup(treat_pop) %>%  #//levalogatom az egyesitett, korcsoportok szerinti adatokbol, amire szuksegem van
  group_by(year, agegroup, gender) %>%
  summarize(patient=sum(patient), patient_0= sum(patient_0), population=sum(population)) 


pop_agegroups <- ungroup(pop_agegroups)  %>% mutate(PMR = patient/population) 


pop_agegroups <- as.data.table(ungroup(pop_agegroups)  %>%#//halalozasi rata /100 000 fo
                                 mutate(PMR100000 = PMR *100000))

pop_agegroups <- ungroup(pop_agegroups) %>% mutate(stdr_PMR100000 = #100 000 fore szamitott rata beszorzasa a sulyokkal (forr?s ESP2013)
                                                     case_when(agegroup == 1     ~ PMR100000 * 0.010989011,
                                                               agegroup == 2     ~ PMR100000 * 0.043956044,
                                                               agegroup == 3     ~ PMR100000 * 0.06043956,
                                                               agegroup == 4     ~ PMR100000 * 0.06043956,
                                                               agegroup == 5     ~ PMR100000 * 0.06043956,
                                                               agegroup == 6     ~ PMR100000 * 0.065934066,
                                                               agegroup == 7     ~ PMR100000 * 0.065934066,
                                                               agegroup == 8     ~ PMR100000 * 0.071428571,
                                                               agegroup == 9     ~ PMR100000 * 0.076923077,
                                                               agegroup == 10     ~ PMR100000 * 0.076923077,
                                                               agegroup == 11     ~ PMR100000 * 0.076923077,
                                                               agegroup == 12     ~ PMR100000 * 0.076923077,
                                                               agegroup == 13     ~ PMR100000 * 0.071428571,
                                                               agegroup == 14     ~ PMR100000 * 0.065934066,
                                                               agegroup == 15     ~ PMR100000 * 0.06043956,
                                                               agegroup == 16     ~ PMR100000 * 0.054945055))

pop_fin <- ungroup(pop_agegroups) %>% #//adatok aggregalasa a szukseges csoportok szerint
  group_by(year, gender) %>%
  summarize(population=sum(population), patient=sum(patient), patient_0=sum(patient_0), s_PMR=sum(stdr_PMR100000))

KSH <- pop_fin #//ellenorzo tabla a KSH szamara, 
#//patient_0=halalozasi esetszam, population=nepesseg szama
a <- KSH %>% filter_at(vars(patient_0), any_vars(. %in% c(1,2,3))) #//nincs ilyen
b <- KSH %>% filter_at(vars(population), any_vars(. %in% c(1,2,3))) #//nincs ilyen
OUT_treat_contRy_gender <- KSH[, c("year", "s_PMR", "gender", "patient")] #
write.csv(OUT_treat_contRy_gender,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Kutatasi_eredmenyek\\treat_countRy_gender.csv", row.names = FALSE) #////////OUTPUT
write.csv(KSH,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Adv_ellenorzeshez\\treat_countRy_gender_KSH.csv", row.names = FALSE) #KSH
####

#countRy total preventable
####
prev_pop <- readRDS ('rds/prev_pop_75_agegroup_data.rds')
pop_agegroups <- ungroup(prev_pop) %>%  #//levalogatom az egyesitett, korcsoportok szerinti adatokbol, amire szuksegem van
  group_by(year, agegroup) %>%
  summarize(patient=sum(patient), patient_0= sum(patient_0), population=sum(population)) 


pop_agegroups <- ungroup(pop_agegroups)  %>% mutate(PMR = patient/population) 


pop_agegroups <- as.data.table(ungroup(pop_agegroups)  %>%#//halalozasi rata /100 000 fo
                                 mutate(PMR100000 = PMR *100000))

pop_agegroups <- ungroup(pop_agegroups) %>% mutate(stdr_PMR100000 = #100 000 fore szamitott rata beszorzasa a sulyokkal (forr?s ESP2013)
                                                     case_when(agegroup == 1     ~ PMR100000 * 0.010989011,
                                                               agegroup == 2     ~ PMR100000 * 0.043956044,
                                                               agegroup == 3     ~ PMR100000 * 0.06043956,
                                                               agegroup == 4     ~ PMR100000 * 0.06043956,
                                                               agegroup == 5     ~ PMR100000 * 0.06043956,
                                                               agegroup == 6     ~ PMR100000 * 0.065934066,
                                                               agegroup == 7     ~ PMR100000 * 0.065934066,
                                                               agegroup == 8     ~ PMR100000 * 0.071428571,
                                                               agegroup == 9     ~ PMR100000 * 0.076923077,
                                                               agegroup == 10     ~ PMR100000 * 0.076923077,
                                                               agegroup == 11     ~ PMR100000 * 0.076923077,
                                                               agegroup == 12     ~ PMR100000 * 0.076923077,
                                                               agegroup == 13     ~ PMR100000 * 0.071428571,
                                                               agegroup == 14     ~ PMR100000 * 0.065934066,
                                                               agegroup == 15     ~ PMR100000 * 0.06043956,
                                                               agegroup == 16     ~ PMR100000 * 0.054945055))

pop_fin <- ungroup(pop_agegroups) %>% #//adatok aggregalasa a szukseges csoportok szerint
  group_by(year) %>%
  summarize(population=sum(population), patient=sum(patient), patient_0=sum(patient_0), s_PMR=sum(stdr_PMR100000))

KSH <- pop_fin #//ellenorzo tabla a KSH szamara, 
#//patient_0=halalozasi esetszam, population=nepesseg szama
a <- KSH %>% filter_at(vars(patient_0), any_vars(. %in% c(1,2,3))) #//nincs ilyen
b <- KSH %>% filter_at(vars(population), any_vars(. %in% c(1,2,3))) #//nincs ilyen
OUT_prev_contRy <- KSH[, c("year", "s_PMR", "patient")] #
write.csv(OUT_prev_contRy,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Kutatasi_eredmenyek\\prev_countRy.csv", row.names = FALSE) #////////OUTPUT
write.csv(KSH,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Adv_ellenorzeshez\\prev_countRy_KSH.csv", row.names = FALSE) #KSH
####

#countRy gender preventable
####
prev_pop <- readRDS ('rds/prev_pop_75_agegroup_data.rds')
pop_agegroups <- ungroup(prev_pop) %>%  #//levalogatom az egyesitett, korcsoportok szerinti adatokbol, amire szuksegem van
  group_by(year, agegroup, gender) %>%
  summarize(patient=sum(patient), patient_0= sum(patient_0), population=sum(population)) 


pop_agegroups <- ungroup(pop_agegroups)  %>% mutate(PMR = patient/population) 


pop_agegroups <- as.data.table(ungroup(pop_agegroups)  %>%#//halalozasi rata /100 000 fo
                                 mutate(PMR100000 = PMR *100000))

pop_agegroups <- ungroup(pop_agegroups) %>% mutate(stdr_PMR100000 = #100 000 fore szamitott rata beszorzasa a sulyokkal (forr?s ESP2013)
                                                     case_when(agegroup == 1     ~ PMR100000 * 0.010989011,
                                                               agegroup == 2     ~ PMR100000 * 0.043956044,
                                                               agegroup == 3     ~ PMR100000 * 0.06043956,
                                                               agegroup == 4     ~ PMR100000 * 0.06043956,
                                                               agegroup == 5     ~ PMR100000 * 0.06043956,
                                                               agegroup == 6     ~ PMR100000 * 0.065934066,
                                                               agegroup == 7     ~ PMR100000 * 0.065934066,
                                                               agegroup == 8     ~ PMR100000 * 0.071428571,
                                                               agegroup == 9     ~ PMR100000 * 0.076923077,
                                                               agegroup == 10     ~ PMR100000 * 0.076923077,
                                                               agegroup == 11     ~ PMR100000 * 0.076923077,
                                                               agegroup == 12     ~ PMR100000 * 0.076923077,
                                                               agegroup == 13     ~ PMR100000 * 0.071428571,
                                                               agegroup == 14     ~ PMR100000 * 0.065934066,
                                                               agegroup == 15     ~ PMR100000 * 0.06043956,
                                                               agegroup == 16     ~ PMR100000 * 0.054945055))

pop_fin <- ungroup(pop_agegroups) %>% #//adatok aggregalasa a szukseges csoportok szerint
  group_by(year, gender) %>%
  summarize(population=sum(population), patient=sum(patient), patient_0=sum(patient_0), s_PMR=sum(stdr_PMR100000))

KSH <- pop_fin #//ellenorzo tabla a KSH szamara, 
#//patient_0=halalozasi esetszam, population=nepesseg szama
a <- KSH %>% filter_at(vars(patient_0), any_vars(. %in% c(1,2,3))) #//nincs ilyen
b <- KSH %>% filter_at(vars(population), any_vars(. %in% c(1,2,3))) #//nincs ilyen
OUT_prev_contRy_gender <- KSH[, c("year", "s_PMR", "gender", "patient")] #
write.csv(OUT_prev_contRy_gender,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Kutatasi_eredmenyek\\prev_countRy_gender.csv", row.names = FALSE) #////////OUTPUT
write.csv(KSH,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Adv_ellenorzeshez\\prev_countRy_gender_KSH.csv", row.names = FALSE) #////////OUTPUT


#----------------------------------------------------------------------------------------------------------------------------

## 2. SUB-COUNTY
#####
#sub-county treatable total
####
treat_pop <- readRDS ('rds//treat_pop_75_agegroup_data.rds')

treat_pop_agegroups <- ungroup(treat_pop) %>%  #//levalogatom az egyesitett, korcsoportok szerinti adatokbol, amire szuksegem van
  group_by(year, SC_code_bp, agegroup) %>%
  summarize(patient=sum(patient), patient_0= sum(patient_0), population=sum(population)) 

treat_pop_agegroups <- ungroup(treat_pop_agegroups)  %>% mutate(PMR = patient/population) 
#//nyers halalozasi rata

treat_pop_agegroups <- as.data.table(ungroup(treat_pop_agegroups)  %>%#//halalozasi rata /100 000 fo
                                       mutate(PMR100000 = PMR *100000))



treat_pop_agegroups <- ungroup(treat_pop_agegroups) %>% mutate(stdr_PMR100000 = #100 000 fore szamitott rata beszorzasa a sulyokkal (forr?s ESP2013)
                                                                 case_when(agegroup == 1     ~ PMR100000 * 0.010989011,
                                                                           agegroup == 2     ~ PMR100000 * 0.043956044,
                                                                           agegroup == 3     ~ PMR100000 * 0.06043956,
                                                                           agegroup == 4     ~ PMR100000 * 0.06043956,
                                                                           agegroup == 5     ~ PMR100000 * 0.06043956,
                                                                           agegroup == 6     ~ PMR100000 * 0.065934066,
                                                                           agegroup == 7     ~ PMR100000 * 0.065934066,
                                                                           agegroup == 8     ~ PMR100000 * 0.071428571,
                                                                           agegroup == 9     ~ PMR100000 * 0.076923077,
                                                                           agegroup == 10     ~ PMR100000 * 0.076923077,
                                                                           agegroup == 11     ~ PMR100000 * 0.076923077,
                                                                           agegroup == 12     ~ PMR100000 * 0.076923077,
                                                                           agegroup == 13     ~ PMR100000 * 0.071428571,
                                                                           agegroup == 14     ~ PMR100000 * 0.065934066,
                                                                           agegroup == 15     ~ PMR100000 * 0.06043956,
                                                                           agegroup == 16     ~ PMR100000 * 0.054945055))

treat_pop_fin <- ungroup(treat_pop_agegroups) %>% #//adatok aggregalasa a szukseges csoportok szerint
  group_by(year, SC_code_bp) %>%
  summarize(population=sum(population), patient=sum(patient), patient_0=sum(patient_0), s_PMR=sum(stdr_PMR100000))

KSH_treat_SC <- treat_pop_fin #//ellenorzo tabla a KSH szamara, 
#//patient_0=halalozasi esetszam, population=nepesseg szama
a <- KSH_treat_SC %>% filter_at(vars(patient_0), any_vars(. %in% c(1,2,3))) #//nincs ilyen
b <- KSH_treat_SC %>% filter_at(vars(population), any_vars(. %in% c(1,2,3))) #//nincs ilyen
OUT_treat_SC <- KSH_treat_SC[, c("year", "SC_code_bp", "s_PMR")] #
write.csv(OUT_treat_SC,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Kutatasi_eredmenyek\\treat_SC.csv", row.names = FALSE) #////////OUTPUT
write.csv(KSH_treat_SC,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Adv_ellenorzeshez\\treat_SC_KSH.csv", row.names = FALSE) #KSH


###

#Sub-county gender treatable
###
treat_pop <- readRDS ('rds//treat_pop_75_agegroup_data.rds')

treat_pop_agegroups <- ungroup(treat_pop) %>%  #//levalogatom az egyesitett, korcsoportok szerinti adatokbol, amire szuksegem van
  group_by(year, SC_code_bp, gender, agegroup) %>%
  summarize(patient=sum(patient), patient_0= sum(patient_0), population=sum(population)) 

treat_pop_agegroups <- ungroup(treat_pop_agegroups)  %>% mutate(PMR = patient/population) 
#//nyers halalozasi rata

treat_pop_agegroups <- as.data.table(ungroup(treat_pop_agegroups)  %>%#//halalozasi rata /100 000 fo
                                       mutate(PMR100000 = PMR *100000))



treat_pop_agegroups <- ungroup(treat_pop_agegroups) %>% mutate(stdr_PMR100000 = #100 000 fore szamitott rata beszorzasa a sulyokkal (forr?s ESP2013)
                                                                 case_when(agegroup == 1     ~ PMR100000 * 0.010989011,
                                                                           agegroup == 2     ~ PMR100000 * 0.043956044,
                                                                           agegroup == 3     ~ PMR100000 * 0.06043956,
                                                                           agegroup == 4     ~ PMR100000 * 0.06043956,
                                                                           agegroup == 5     ~ PMR100000 * 0.06043956,
                                                                           agegroup == 6     ~ PMR100000 * 0.065934066,
                                                                           agegroup == 7     ~ PMR100000 * 0.065934066,
                                                                           agegroup == 8     ~ PMR100000 * 0.071428571,
                                                                           agegroup == 9     ~ PMR100000 * 0.076923077,
                                                                           agegroup == 10     ~ PMR100000 * 0.076923077,
                                                                           agegroup == 11     ~ PMR100000 * 0.076923077,
                                                                           agegroup == 12     ~ PMR100000 * 0.076923077,
                                                                           agegroup == 13     ~ PMR100000 * 0.071428571,
                                                                           agegroup == 14     ~ PMR100000 * 0.065934066,
                                                                           agegroup == 15     ~ PMR100000 * 0.06043956,
                                                                           agegroup == 16     ~ PMR100000 * 0.054945055))

treat_pop_fin <- ungroup(treat_pop_agegroups) %>% #//adatok aggregalasa a szukseges csoportok szerint
  group_by(year, SC_code_bp, gender) %>%
  summarize(population=sum(population), patient=sum(patient), patient_0=sum(patient_0), s_PMR=sum(stdr_PMR100000))

KSH_treat_SC_gender <- treat_pop_fin #//ellenorzo tabla a KSH szamara, 
#//patient_0=halalozasi esetszam, population=nepesseg szama
a <- KSH_treat_SC_gender %>% filter_at(vars(patient_0), any_vars(. %in% c(1,2,3))) #//nincs ilyen
b <- KSH_treat_SC_gender %>% filter_at(vars(population), any_vars(. %in% c(1,2,3))) #//nincs ilyen
OUT_treat_SC_gender <- KSH_treat_SC_gender[, c("year", "SC_code_bp", "gender", "s_PMR")] #//KSH
write.csv(OUT_treat_SC_gender,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Kutatasi_eredmenyek\\treat_SC_gender.csv", row.names = FALSE) #////////OUTPUT
write.csv(KSH_treat_SC_gender,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Adv_ellenorzeshez\\treat_SC_gender_KSH.csv", row.names = FALSE) #//KSH
#check <- OUT_treat_SC_gender %>%
#  group_by(year, gender) %>%
#  summarize(mean=mean(s_PMR))
####

#Sub-county total preventable

####
prev_pop <- readRDS ('rds//prev_pop_75_agegroup_data.rds')

prev_pop_agegroups <- ungroup(prev_pop) %>%  #//levalogatom az egyesitett, korcsoportok szerinti adatokbol, amire szuksegem van
  group_by(year, SC_code_bp, agegroup) %>%
  summarize(patient=sum(patient), patient_0= sum(patient_0), population=sum(population)) 

prev_pop_agegroups <- ungroup(prev_pop_agegroups)  %>% mutate(PMR = patient/population) 
#//nyers halalozasi rata

prev_pop_agegroups <- as.data.table(ungroup(prev_pop_agegroups)  %>%#//halalozasi rata /100 000 fo
                                      mutate(PMR100000 = PMR *100000))



prev_pop_agegroups <- ungroup(prev_pop_agegroups) %>% mutate(stdr_PMR100000 = #100 000 fore szamitott rata beszorzasa a sulyokkal (forr?s ESP2013)
                                                               case_when(agegroup == 1     ~ PMR100000 * 0.010989011,
                                                                         agegroup == 2     ~ PMR100000 * 0.043956044,
                                                                         agegroup == 3     ~ PMR100000 * 0.06043956,
                                                                         agegroup == 4     ~ PMR100000 * 0.06043956,
                                                                         agegroup == 5     ~ PMR100000 * 0.06043956,
                                                                         agegroup == 6     ~ PMR100000 * 0.065934066,
                                                                         agegroup == 7     ~ PMR100000 * 0.065934066,
                                                                         agegroup == 8     ~ PMR100000 * 0.071428571,
                                                                         agegroup == 9     ~ PMR100000 * 0.076923077,
                                                                         agegroup == 10     ~ PMR100000 * 0.076923077,
                                                                         agegroup == 11     ~ PMR100000 * 0.076923077,
                                                                         agegroup == 12     ~ PMR100000 * 0.076923077,
                                                                         agegroup == 13     ~ PMR100000 * 0.071428571,
                                                                         agegroup == 14     ~ PMR100000 * 0.065934066,
                                                                         agegroup == 15     ~ PMR100000 * 0.06043956,
                                                                         agegroup == 16     ~ PMR100000 * 0.054945055))

prev_pop_fin <- ungroup(prev_pop_agegroups) %>% #//adatok aggregalasa a szukseges csoportok szerint
  group_by(year, SC_code_bp) %>%
  summarize(population=sum(population), patient=sum(patient), patient_0=sum(patient_0), s_PMR=sum(stdr_PMR100000))

KSH_prev_SC <- prev_pop_fin #//ellenorzo tabla a KSH szamara, 
#//patient_0=halalozasi esetszam, population=nepesseg szama
a <- KSH_prev_SC %>% filter_at(vars(patient_0), any_vars(. %in% c(1,2,3))) #//nincs ilyen
b <- KSH_prev_SC %>% filter_at(vars(population), any_vars(. %in% c(1,2,3))) #//nincs ilyen
OUT_prev_SC <- KSH_prev_SC[, c("year", "SC_code_bp", "s_PMR")] #////////OUTPUT
write.csv(OUT_prev_SC,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Kutatasi_eredmenyek\\prev_SC.csv", row.names = FALSE) #////////OUTPUT
write.csv(KSH_prev_SC,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Adv_ellenorzeshez\\prev_SC_KSH.csv", row.names = FALSE) #////////OUTPUT

####

#Sub-county gender preventable
####
prev_pop <- readRDS ('rds//prev_pop_75_agegroup_data.rds')

prev_pop_agegroups <- ungroup(prev_pop) %>%  #//levalogatom az egyesitett, korcsoportok szerinti adatokbol, amire szuksegem van
  group_by(year, SC_code_bp, gender, agegroup) %>%
  summarize(patient=sum(patient), patient_0= sum(patient_0), population=sum(population)) 

prev_pop_agegroups <- ungroup(prev_pop_agegroups)  %>% mutate(PMR = patient/population) 
#//nyers halalozasi rata

prev_pop_agegroups <- as.data.table(ungroup(prev_pop_agegroups)  %>%#//halalozasi rata /100 000 fo
                                      mutate(PMR100000 = PMR *100000))



prev_pop_agegroups <- ungroup(prev_pop_agegroups) %>% mutate(stdr_PMR100000 = #100 000 fore szamitott rata beszorzasa a sulyokkal (forr?s ESP2013)
                                                               case_when(agegroup == 1     ~ PMR100000 * 0.010989011,
                                                                         agegroup == 2     ~ PMR100000 * 0.043956044,
                                                                         agegroup == 3     ~ PMR100000 * 0.06043956,
                                                                         agegroup == 4     ~ PMR100000 * 0.06043956,
                                                                         agegroup == 5     ~ PMR100000 * 0.06043956,
                                                                         agegroup == 6     ~ PMR100000 * 0.065934066,
                                                                         agegroup == 7     ~ PMR100000 * 0.065934066,
                                                                         agegroup == 8     ~ PMR100000 * 0.071428571,
                                                                         agegroup == 9     ~ PMR100000 * 0.076923077,
                                                                         agegroup == 10     ~ PMR100000 * 0.076923077,
                                                                         agegroup == 11     ~ PMR100000 * 0.076923077,
                                                                         agegroup == 12     ~ PMR100000 * 0.076923077,
                                                                         agegroup == 13     ~ PMR100000 * 0.071428571,
                                                                         agegroup == 14     ~ PMR100000 * 0.065934066,
                                                                         agegroup == 15     ~ PMR100000 * 0.06043956,
                                                                         agegroup == 16     ~ PMR100000 * 0.054945055))

prev_pop_fin <- ungroup(prev_pop_agegroups) %>% #//adatok aggregalasa a szukseges csoportok szerint
  group_by(year, SC_code_bp, gender) %>%
  summarize(population=sum(population), patient=sum(patient), patient_0=sum(patient_0), s_PMR=sum(stdr_PMR100000))

KSH_prev_SC_gender <- prev_pop_fin #//ellenorzo tabla a KSH szamara, 
#//patient_0=halalozasi esetszam, population=nepesseg szama
a <- KSH_prev_SC_gender %>% filter_at(vars(patient_0), any_vars(. %in% c(1,2,3))) #//nincs ilyen
b <- KSH_prev_SC_gender %>% filter_at(vars(population), any_vars(. %in% c(1,2,3))) #//nincs ilyen
OUT_prev_SC_gender <- KSH_prev_SC_gender[, c("year", "gender", "SC_code_bp", "s_PMR")] #
write.csv(OUT_prev_SC_gender,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Kutatasi_eredmenyek\\prev_SC_gender.csv", row.names = FALSE) #////////OUTPUT
write.csv(KSH_prev_SC_gender,"X:\\Kimeno\\kutato31\\kutato31_Toth_Manna_20210518\\Adv_ellenorzeshez\\prev_SC_gender_KSH.csv", row.names = FALSE) #KSH





##############################################################################################################################
##############################################################################################################################
