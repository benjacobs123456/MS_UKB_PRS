print(paste0("Started at ",Sys.time()))
#############################################
#               Load packages 
#############################################
library(RNOmni)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

#############################################
#               read in data 
#############################################

setwd("/data/Wolfson-UKBB-Dobson/ukb_pheno_911")
selected_vars = read_tsv("ukb_pheno_final_MS_1301")
args=commandArgs(trailingOnly=TRUE)
print(paste0("Number of bootstrap iterations:",args[1]))
trials = as.numeric(as.character(args[1]))


#############################################
#               Get vars in right format 
#############################################

selected_vars$MS_status=factor(selected_vars$MS_status)
selected_vars$Sex.0.0 = factor(selected_vars$Sex.0.0)
selected_vars$`Year of birth.0.0` = as.numeric(selected_vars$`Year of birth.0.0`)
selected_vars$`Month of birth.0.0` = factor(selected_vars$`Month of birth.0.0`)
selected_vars$`Place of birth in UK - north co-ordinate.0.0` = as.numeric(selected_vars$`Place of birth in UK - north co-ordinate.0.0`)
df1= selected_vars %>% 
  filter(`Place of birth in UK - north co-ordinate.0.0`<0) %>%
  mutate (`Place of birth in UK - north co-ordinate.0.0` = NA)
selected_vars = df1 %>% bind_rows(selected_vars %>% filter(!(EID %in% df1$EID)))

#make young starters
young= selected_vars %>% filter(`Smoking status.0.0`=="Previous" &`Age started smoking in former smokers.0.0` < 20 &`Age started smoking in former smokers.0.0` > 0) %>% mutate("young_smoker"="yes")
young2= selected_vars %>% filter(`Smoking status.0.0`=="Current" &`Age started smoking in current smokers.0.0` < 20 &`Age started smoking in former smokers.0.0` > 0) %>% mutate("young_smoker"="yes")
nyoung= selected_vars %>% filter(!EID %in% young$EID) %>% 
  filter(!EID %in% young2$EID) %>% 
  mutate("young_smoker"="no") 
selected_vars = bind_rows(young,nyoung,young2)


df1= selected_vars %>% 
  filter(`Age completed full time education.0.0`<0) %>%
  mutate (`Age completed full time education.0.0` = NA)
selected_vars = df1 %>% bind_rows(selected_vars %>% filter(!(EID %in% df1$EID)))

selected_vars$`Country of birth (UK/elsewhere).0.0`=factor(recode((factor(selected_vars$`Country of birth (UK/elsewhere).0.0`)),
                                                                  "England"="UK",
                                                                  "Republic of Ireland"="UK",
                                                                  "Northern Ireland"="UK",
                                                                  "Scotland"="UK",
                                                                  "Wales"="UK",
                                                                  "Elsewhere"="Non-UK",
                                                                  "Do not know"="NA",
                                                                  "Prefer not to answer"="NA"),levels=c("UK","Non-UK"))


selected_vars$`Breastfed as a baby.0.0` = factor(selected_vars$ `Breastfed as a baby.0.0`,levels=c("No","Yes"))
selected_vars$`Comparative body size at age 10.0.0` = factor(selected_vars$ `Comparative body size at age 10.0.0`, levels=c("Thinner","About average","Plumper"))
selected_vars$`Maternal smoking around birth.0.0` = factor(selected_vars$`Maternal smoking around birth.0.0`,levels=c("No","Yes"))

df1= selected_vars %>% 
  filter(`Age first had sexual intercourse.0.0`<0) %>%
  mutate (`Age first had sexual intercourse.0.0` = NA)
selected_vars = df1 %>% bind_rows(selected_vars %>% filter(!(EID %in% df1$EID)))

selected_vars$`Age when periods started (menarche).0.0` = as.numeric(selected_vars$`Age when periods started (menarche).0.0`)
df1= selected_vars %>% 
  filter(`Age when periods started (menarche).0.0`<0) %>%
  mutate (`Age when periods started (menarche).0.0` = NA)
selected_vars = df1 %>% bind_rows(selected_vars %>% filter(!(EID %in% df1$EID)))


selected_vars$`Relative age voice broke.0.0` = factor(selected_vars$`Relative age voice broke.0.0`,
                                                      levels=c("About average age", "Younger than average","Older than average"))
selected_vars$`Birth weight.0.0` = as.numeric(selected_vars$`Birth weight.0.0`)
selected_vars$`Ethnic background.0.0` = factor(selected_vars$`Ethnic background.0.0`)
selected_vars$raw_ethnicity=selected_vars$`Ethnic background.0.0`
selected_vars$`Ethnic background.0.0`=factor(recode(selected_vars$`Ethnic background.0.0`,African="Non-white",`Any other Asian background`="Non-white",`Other ethnic group`="Non-white",`White and Black Caribbean`="Non-white",`White and Asian`="Non-white",`Any other Black background`="Non-white",`White and Black African`="Non-white",`Any other mixed background`="Non-white",`Bangladeshi`="Non-white",`British Caribbean`="Non-white",`Pakistani`="Non-white",`Indian`="Non-white",`Caribbean`="Non-white",`Chinese`="Non-white",`British`="White",`Irish`="White",`Any other white background`="White",`Do not know`="NA",`Prefer not to answer`="NA"),levels=c("White","Non-white"))


#############################################
#     Additive and multiplicative interaction 
#############################################
selected_vars = selected_vars %>% mutate('cBMI'=recode(selected_vars$`Comparative body size at age 10.0.0`,
                                                       'About average'="Not overweight",
                                                       'Plumper'="Overweight",
                                                       'Thinner'="Not overweight"))

selected_vars$smoking = factor(selected_vars$young_smoker)

selected_vars$DRB1_15 = selected_vars$DRB_15


selected_vars = selected_vars %>% filter(`Ethnic background.0.0`=="White")
selected_vars = selected_vars %>% filter(`Genetic ethnic grouping.0.0`=="Caucasian")
kin = read_table2("/data/Wolfson-UKBB-Dobson/helper_progs_and_key/ukb43101_rel_s488282.dat")
exclusion = kin %>% filter(Kinship>0.0884) %>% select(ID1) %>% rename("EID"="ID1") 
selected_vars = selected_vars %>% filter(!EID %in% exclusion$EID)
#############################################
#     Additive  interaction 
#############################################

# define functions

#choose best prs based on r2 and read it in
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_08/summarised_PRS_results_pval0.4")
selected_vars = selected_vars %>% left_join(prs,by="EID")
#additive interaction PRS
# interaction with PRS as continuous var

selected_vars = selected_vars %>% filter(!is.na(PRS))
selected_vars$PRS=rankNorm(selected_vars$PRS)
  
library(doParallel)
registerDoParallel(cores=40)
df=selected_vars


r = foreach(icount(trials), .combine=cbind) %dopar% {
  df = sample_n(df, size=nrow(df), replace=TRUE)
  model = glm(data=df,
              MS_status~`Age at recruitment.0.0`+
                `Place of birth in UK - north co-ordinate.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
                `Genetic principal components.0.1`+
                `Genetic principal components.0.2`+
                `Genetic principal components.0.3`+
                `Genetic principal components.0.4`+
                `Age when periods started (menarche).0.0`*PRS,
              family=binomial(link="logit"))
  coeffs=as.numeric(coef(model))
  RERI=exp(coeffs[9]+coeffs[10]+coeffs[11])-exp(coeffs[9])-exp(coeffs[10])+1
  AP=RERI/(exp(coeffs[9]+coeffs[10]+coeffs[11]))}

raw_aps_menarche_prs = r

model = glm(data=df,
            MS_status~`Age at recruitment.0.0`+
                `Place of birth in UK - north co-ordinate.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
              `Genetic principal components.0.1`+
              `Genetic principal components.0.2`+
              `Genetic principal components.0.3`+
              `Genetic principal components.0.4`+
              `Age when periods started (menarche).0.0`*PRS,
            family=binomial(link="logit"))
coeffs=as.numeric(coef(model))
  RERI=exp(coeffs[9]+coeffs[10]+coeffs[11])-exp(coeffs[9])-exp(coeffs[10])+1
  AP=RERI/(exp(coeffs[9]+coeffs[10]+coeffs[11]))

menarche_prs=c(AP,quantile(r,c(0.025,0.975)))


r = foreach(icount(trials), .combine=cbind) %dopar% {
  df = sample_n(df, size=nrow(df), replace=TRUE)
  model = glm(data=df,
              MS_status~`Age at recruitment.0.0`+
                `Sex.0.0`+
                `Place of birth in UK - north co-ordinate.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
                `Genetic principal components.0.1`+
                `Genetic principal components.0.2`+
                `Genetic principal components.0.3`+
                `Genetic principal components.0.4`+
                cBMI*PRS,
              family=binomial(link="logit"))
  coeffs=as.numeric(coef(model))
  RERI=exp(coeffs[10]+coeffs[11]+coeffs[12])-exp(coeffs[10])-exp(coeffs[11])+1
  AP=RERI/(exp(coeffs[10]+coeffs[11]+coeffs[12]))}

raw_aps_bmi_prs = r

model = glm(data=df,
            MS_status~`Age at recruitment.0.0`+
                `Sex.0.0`+
                `Place of birth in UK - north co-ordinate.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
              `Genetic principal components.0.1`+
              `Genetic principal components.0.2`+
              `Genetic principal components.0.3`+
              `Genetic principal components.0.4`+
              cBMI*PRS,
            family=binomial(link="logit"))
coeffs=as.numeric(coef(model))
RERI=exp(coeffs[10]+coeffs[11]+coeffs[12])-exp(coeffs[10])-exp(coeffs[11])+1
AP=RERI/(exp(coeffs[10]+coeffs[11]+coeffs[12]))

bmi_prs=c(AP,quantile(r,c(0.025,0.975)))



r = foreach(icount(trials), .combine=cbind) %dopar% {
  df = sample_n(df, size=nrow(df), replace=TRUE)
  model = glm(data=df,
              MS_status~`Age at recruitment.0.0`+
                `Sex.0.0`+
                `Place of birth in UK - north co-ordinate.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
                `Genetic principal components.0.1`+
                `Genetic principal components.0.2`+
                `Genetic principal components.0.3`+
                `Genetic principal components.0.4`+
                smoking*PRS,
              family=binomial(link="logit"))
  coeffs=as.numeric(coef(model))
  RERI=exp(coeffs[10]+coeffs[11]+coeffs[12])-exp(coeffs[10])-exp(coeffs[11])+1
  AP=RERI/(exp(coeffs[10]+coeffs[11]+coeffs[12]))}

raw_aps_smoking_prs = r

model = glm(data=df,
            MS_status~`Age at recruitment.0.0`+
                `Sex.0.0`+
                `Place of birth in UK - north co-ordinate.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
              `Genetic principal components.0.1`+
              `Genetic principal components.0.2`+
              `Genetic principal components.0.3`+
              `Genetic principal components.0.4`+
              smoking*PRS,
            family=binomial(link="logit"))
coeffs=as.numeric(coef(model))
RERI=exp(coeffs[10]+coeffs[11]+coeffs[12])-exp(coeffs[10])-exp(coeffs[11])+1
AP=RERI/(exp(coeffs[10]+coeffs[11]+coeffs[12]))

smoking_prs=c(AP,quantile(r,c(0.025,0.975)))

prs_summary = data.frame(rbind(smoking_prs,bmi_prs,menarche_prs))
write_csv(prs_summary,"prs_APs.csv")



df = data.frame("menarche"=raw_aps_menarche_prs,"bmi"=raw_aps_bmi_prs,"smoking"=raw_aps_smoking_prs)
write_csv(df,"raw_PRS_APs.csv")

print(paste0("Finished at ",Sys.time()))
