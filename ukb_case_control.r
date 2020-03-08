#############################################
#               Load packages 
#############################################

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

#############################################
#               read in data 
#############################################

setwd("/data/Wolfson-UKBB-Dobson/ukb_pheno_911")
selected_vars = read_tsv("ukb_pheno_final_MS_1301")

#selected_vars = read_tsv("H:/UKB_PRS/28-11/ukb_pheno_final_hla")


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

selected_vars$A_02 = factor(selected_vars$A_02)
selected_vars$DRB_15 = factor(selected_vars$DRB_15)

#############################################
#     Work out basic demographics 
#############################################

summ1 = cbind(rbind(
  table(selected_vars$Sex.0.0,selected_vars$MS_status),
  table(selected_vars$`Month of birth.0.0`,selected_vars$MS_status),
  table(selected_vars$`Breastfed as a baby.0.0`,selected_vars$MS_status),
  table(selected_vars$`Comparative body size at age 10.0.0`,selected_vars$MS_status),
  table(selected_vars$`Maternal smoking around birth.0.0`,selected_vars$MS_status),
  table(selected_vars$`Relative age voice broke.0.0`,selected_vars$MS_status),
  table(selected_vars$young_smoker,selected_vars$MS_status),
  table(selected_vars$`Ethnic background.0.0`,selected_vars$MS_status),
  table(selected_vars$A_02,selected_vars$MS_status),
  table(selected_vars$DRB_15,selected_vars$MS_status),
  table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$MS_status),
  table(selected_vars$IM_status,selected_vars$MS_status)),
  c(table(selected_vars$Sex.0.0,selected_vars$MS_status)[,1]/sum(table(selected_vars$Sex.0.0,selected_vars$MS_status)[,1]),
    table(selected_vars$`Month of birth.0.0`,selected_vars$MS_status)[,1]/sum(table(selected_vars$`Month of birth.0.0`,selected_vars$MS_status)[,1]),
    table(selected_vars$`Breastfed as a baby.0.0`,selected_vars$MS_status)[,1]/sum(table(selected_vars$`Breastfed as a baby.0.0`,selected_vars$MS_status)[,1]),
    table(selected_vars$`Comparative body size at age 10.0.0`,selected_vars$MS_status)[,1]/sum(table(selected_vars$`Breastfed as a baby.0.0`,selected_vars$MS_status)[,1]),
    table(selected_vars$`Maternal smoking around birth.0.0`,selected_vars$MS_status)[,1]/sum(table(selected_vars$`Maternal smoking around birth.0.0`,selected_vars$MS_status)[,1]),
    table(selected_vars$`Relative age voice broke.0.0`,selected_vars$MS_status)[,1]/sum(table(selected_vars$`Relative age voice broke.0.0`,selected_vars$MS_status)[,1]),
    table(selected_vars$young_smoker,selected_vars$MS_status)[,1]/sum(table(selected_vars$young_smoker,selected_vars$MS_status)[,1]),
    table(selected_vars$`Ethnic background.0.0`,selected_vars$MS_status)[,1]/sum(table(selected_vars$`Ethnic background.0.0`,selected_vars$MS_status)[,1]),
    table(selected_vars$A_02,selected_vars$MS_status)[,1]/sum(table(selected_vars$A_02,selected_vars$MS_status)[,1]),
    table(selected_vars$DRB_15,selected_vars$MS_status)[,1]/sum(table(selected_vars$DRB_15,selected_vars$MS_status)[,1]),
    table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$MS_status)[,1]/sum(table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$MS_status)[,1]),
    table(selected_vars$IM_status,selected_vars$MS_status)[,1]/sum(table(selected_vars$IM_status,selected_vars$MS_status)[,1])),
  c(table(selected_vars$Sex.0.0,selected_vars$MS_status)[,2]/sum(table(selected_vars$Sex.0.0,selected_vars$MS_status)[,2]),
    table(selected_vars$`Month of birth.0.0`,selected_vars$MS_status)[,2]/sum(table(selected_vars$`Month of birth.0.0`,selected_vars$MS_status)[,2]),
    table(selected_vars$`Breastfed as a baby.0.0`,selected_vars$MS_status)[,2]/sum(table(selected_vars$`Breastfed as a baby.0.0`,selected_vars$MS_status)[,2]),
    table(selected_vars$`Comparative body size at age 10.0.0`,selected_vars$MS_status)[,2]/sum(table(selected_vars$`Breastfed as a baby.0.0`,selected_vars$MS_status)[,2]),
    table(selected_vars$`Maternal smoking around birth.0.0`,selected_vars$MS_status)[,2]/sum(table(selected_vars$`Maternal smoking around birth.0.0`,selected_vars$MS_status)[,2]),
    table(selected_vars$`Relative age voice broke.0.0`,selected_vars$MS_status)[,2]/sum(table(selected_vars$`Relative age voice broke.0.0`,selected_vars$MS_status)[,2]),
    table(selected_vars$young_smoker,selected_vars$MS_status)[,2]/sum(table(selected_vars$young_smoker,selected_vars$MS_status)[,2]),
    table(selected_vars$`Ethnic background.0.0`,selected_vars$MS_status)[,2]/sum(table(selected_vars$`Ethnic background.0.0`,selected_vars$MS_status)[,2]),
    table(selected_vars$A_02,selected_vars$MS_status)[,2]/sum(table(selected_vars$A_02,selected_vars$MS_status)[,2]),
    table(selected_vars$DRB_15,selected_vars$MS_status)[,2]/sum(table(selected_vars$DRB_15,selected_vars$MS_status)[,2]),
    table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$MS_status)[,2]/sum(table(selected_vars$`Country of birth (UK/elsewhere).0.0`,selected_vars$MS_status)[,2]),
    table(selected_vars$IM_status,selected_vars$MS_status)[,2]/sum(table(selected_vars$IM_status,selected_vars$MS_status)[,2])))      
summ1=data.frame(summ1)
summ1$Trait=rownames(summ1)    
summ1$Control=paste(summ1$X0," (",round(summ1$V3*100,2),"%)")
summ1$Cases=paste(summ1$X1," (",round(summ1$V4*100,2),"%)")
summ1=summ1[,c(5:7)]
rownames(summ1)=NULL

summ2 = selected_vars %>% group_by(MS_status) %>%
  summarise('Age'=mean(`Age at recruitment.0.0`,na.rm=TRUE),
            'Age SD'=sd(`Age at recruitment.0.0`,na.rm=TRUE),
            'Birth latitude'=mean(`Place of birth in UK - north co-ordinate.0.0`,na.rm=TRUE),
            'Birth latitude SD'=sd(`Place of birth in UK - north co-ordinate.0.0`,na.rm=TRUE),
            'Age completed full-time education'=mean(`Age completed full time education.0.0`,na.rm=TRUE),
            'Age completed full-time education SD'=sd(`Age completed full time education.0.0`,na.rm=TRUE),
            'Age had sex' =mean(`Age first had sexual intercourse.0.0`,na.rm=TRUE),
            'Age had sex SD' =sd(`Age first had sexual intercourse.0.0`,na.rm=TRUE),
            'Age at menarche' = mean(`Age when periods started (menarche).0.0`,na.rm=TRUE),
            'Age at menarche SD' = sd(`Age when periods started (menarche).0.0`,na.rm=TRUE),
            'Birth weight'=mean(`Birth weight.0.0`,na.rm=TRUE),
            'Birth weight SD'=sd(`Birth weight.0.0`,na.rm=TRUE),
            'Townsend deprivation index'=mean(`Townsend deprivation index at recruitment.0.0`,na.rm=TRUE),
            'Townsend deprivation index SD'=sd(`Townsend deprivation index at recruitment.0.0`,na.rm=TRUE))
summ2 = data.frame(t(data.matrix(summ2)))
summ2=data.frame("Trait"=c("Age","Birth latitude","Age completed full-time education","Age had sexual intercourse","Age at menarche","Birth weight (Kg)","Townsend deprivation index"),
                 "Control"=c(paste0(round(summ2$X1[2],2)," (",round(summ2$X1[3],2),")"),
                             paste0(round(summ2$X1[4],2)," (",round(summ2$X1[5],2),")"),
                             paste0(round(summ2$X1[6],2)," (",round(summ2$X1[7],2),")"),
                             paste0(round(summ2$X1[8],2)," (",round(summ2$X1[9],2),")"),
                             paste0(round(summ2$X1[10],2)," (",round(summ2$X1[11],2),")"),
                             paste0(round(summ2$X1[12],2)," (",round(summ2$X1[13],2),")"),
                             paste0(round(summ2$X1[14],2)," (",round(summ2$X1[15],2),")")),
                 "Cases"=c(paste0(round(summ2$X2[2],2)," (",round(summ2$X2[3],2),")"),
                           paste0(round(summ2$X2[4],2)," (",round(summ2$X2[5],2),")"),
                           paste0(round(summ2$X2[6],2)," (",round(summ2$X2[7],2),")"),
                           paste0(round(summ2$X2[8],2)," (",round(summ2$X2[9],2),")"),
                           paste0(round(summ2$X2[10],2)," (",round(summ2$X2[11],2),")"),
                           paste0(round(summ2$X2[12],2)," (",round(summ2$X2[13],2),")"),
                           paste0(round(summ2$X2[14],2)," (",round(summ2$X2[15],2),")")))

summ3 = rbind(summ1,summ2)
write.csv(summ3,"demographics.csv")

#############################################
#     Build multivariate models 
#############################################



hla_drb_model_multi=glm(data=selected_vars,
                        MS_status~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+DRB_15,
                        family=binomial(link="logit"))
hla_a_model_multi=glm(data=selected_vars,
                      MS_status~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+A_02,
                      family=binomial(link="logit"))

mob_model_multi=glm(data=selected_vars,
                    MS_status~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+`Month of birth.0.0`,
                    family=binomial(link="logit"))
breastfeeding_model_multi=glm(data=selected_vars,
                              MS_status~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+`Breastfed as a baby.0.0`,
                              family=binomial(link="logit"))
cbmi_model_multi=glm(data=selected_vars,
                     MS_status~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+`Comparative body size at age 10.0.0`,
                     family=binomial(link="logit"))
mat_smoking_model_multi=glm(data=selected_vars,
                            MS_status~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+`Maternal smoking around birth.0.0`,
                            family=binomial(link="logit"))
age_had_sex_model_multi=glm(data=selected_vars,
                            MS_status~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+`Age first had sexual intercourse.0.0`,
                            family=binomial(link="logit"))
smoking_model_multi=glm(data=selected_vars,
                        MS_status~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+young_smoker,
                        family=binomial(link="logit"))
menarche_model_multi=glm(data=selected_vars %>% filter (selected_vars$`Age when periods started (menarche).0.0`>0),
                         MS_status~`Ethnic background.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+`Age when periods started (menarche).0.0`,
                         family=binomial(link="logit"))
voicebreak_model_multi=glm(data=selected_vars,
                           MS_status~`Ethnic background.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+`Relative age voice broke.0.0`,
                           family=binomial(link="logit"))
bw_model_multi=glm(data=selected_vars,
                   MS_status~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+`Birth weight.0.0`,
                   family=binomial(link="logit"))
im_model_multi=glm(data=selected_vars,
                   MS_status~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+IM_status,
                   family=binomial(link="logit"))

tbl = rbind(summary(hla_drb_model_multi)$coefficients[-c(1:6),],
            summary(hla_a_model_multi)$coefficients[-c(1:6),],
            summary(mob_model_multi)$coefficients[-c(1:6),],
            summary(breastfeeding_model_multi)$coefficients[-c(1:6),],
            summary(cbmi_model_multi)$coefficients[-c(1:6),],
            summary(mat_smoking_model_multi)$coefficients[-c(1:6),],
            summary(menarche_model_multi)$coefficients[-c(1:5),],
            summary(voicebreak_model_multi)$coefficients[-c(1:5),],
            summary(smoking_model_multi)$coefficients[-c(1:6),],
            summary(age_had_sex_model_multi)$coefficients[-c(1:6),],
            summary(bw_model_multi)$coefficients[-c(1:6),],
            summary(im_model_multi)$coefficients[-c(1:6),])

rownames(tbl)[16]=rownames(summary(breastfeeding_model_multi)$coefficients)[7]
rownames(tbl)[19]=rownames(summary(mat_smoking_model_multi)$coefficients)[7]
rownames(tbl)[20]=rownames(summary(menarche_model_multi)$coefficients)[6]
rownames(tbl)[23]=rownames(summary(smoking_model_multi)$coefficients)[7]
rownames(tbl)[24]=rownames(summary(age_had_sex_model_multi)$coefficients)[7]
rownames(tbl)[25]=rownames(summary(bw_model_multi)$coefficients)[7]
rownames(tbl)[26]=rownames(summary(im_model_multi)$coefficients)[7]

tbl = data.frame(tbl)
tbl$or = exp(tbl$Estimate)
tbl$lower_ci = exp(tbl$Estimate-1.96*tbl$Std..Error)
tbl$upper_ci = exp(tbl$Estimate+1.96*tbl$Std..Error)
tbl$clean_or = paste0(round(tbl$or,2)," (",round(tbl$lower_ci,2)," to ",round(tbl$upper_ci,2),")")
df = tbl
tbl = tbl[,c(8,4)]
colnames(tbl)=c("OR (95% CI)","P Value")


labs = c("MOB","BF","cBMI","Mat smok","Menarche","Voicebreak","Smoking","Age had sex","HLA A","HLA DRB","BW","IM")
lik_rats = c()
anov = anova(mob_model_multi,test="Chisq")
p = anov$`Pr(>Chi)`[length(anov$`Pr(>Chi)`)]
lik_rats=c(lik_rats,p)
anov =anova(breastfeeding_model_multi,test="Chisq")
p = anov$`Pr(>Chi)`[length(anov$`Pr(>Chi)`)]
lik_rats=c(lik_rats,p)
anov =anova(cbmi_model_multi,test="Chisq")
p = anov$`Pr(>Chi)`[length(anov$`Pr(>Chi)`)]
lik_rats=c(lik_rats,p)
anov =anova(mat_smoking_model_multi,test="Chisq")
p = anov$`Pr(>Chi)`[length(anov$`Pr(>Chi)`)]
lik_rats=c(lik_rats,p)
anov =anova(menarche_model_multi,test="Chisq")
p = anov$`Pr(>Chi)`[length(anov$`Pr(>Chi)`)]
lik_rats=c(lik_rats,p)
anov =anova(voicebreak_model_multi,test="Chisq")
p = anov$`Pr(>Chi)`[length(anov$`Pr(>Chi)`)]
lik_rats=c(lik_rats,p)
anov =anova(smoking_model_multi,test="Chisq")
p = anov$`Pr(>Chi)`[length(anov$`Pr(>Chi)`)]
lik_rats=c(lik_rats,p)
anov =anova(age_had_sex_model_multi,test="Chisq")
p = anov$`Pr(>Chi)`[length(anov$`Pr(>Chi)`)]
lik_rats=c(lik_rats,p)
anov =anova(hla_a_model_multi,test="Chisq")
p = anov$`Pr(>Chi)`[length(anov$`Pr(>Chi)`)]
lik_rats=c(lik_rats,p)
anov =anova(hla_drb_model_multi,test="Chisq")
p = anov$`Pr(>Chi)`[length(anov$`Pr(>Chi)`)]
lik_rats=c(lik_rats,p)
anov =anova(bw_model_multi,test="Chisq")
p = anov$`Pr(>Chi)`[length(anov$`Pr(>Chi)`)]
lik_rats=c(lik_rats,p)
anov =anova(im_model_multi,test="Chisq")
p = anov$`Pr(>Chi)`[length(anov$`Pr(>Chi)`)]
lik_rats=c(lik_rats,p)

lik = data.frame(labs,lik_rats)
lik$thresh=lik_rats<0.05/12
write.csv(lik,"lik.csv")

write.csv(tbl,"models.csv")

#model plots


df$trait=factor(c('HLA DRB1*15 heterozygote (vs no DRB1*15 alleles)',
                  'HLA DRB1*15 homozygote (vs no DRB1*15 alleles)',
                  'HLA A*02 heterozygote (vs no A*02 alleles)',
                  'HLA A*02 homozygote (vs no A*02 alleles)',
                  'Born in August (vs April)',
                  'Born in December (vs April)',
                  'Born in February (vs April)',
                  'Born in January (vs April)',
                  'Born in July (vs April)',
                  'Born in June (vs April)',
                  'Born in March (vs April)',
                  'Born in May (vs April)',
                  'Born in November (vs April)',
                  'Born in October (vs April)',
                  'Born in September (vs April)',
                  'Breastfed as a baby (vs not)',
                  'Comparative body size aged 10: average (vs thinner)',
                  'Comparative body size aged 10: plumper (vs thinner)',
                  'Exposed to maternal smoking (vs not)',
                  'Age at menarche (females)',
                  'Age when voice broke (males): younger than average (vs average)',
                  'Age when voice broke (males): older than average (vs average)',
                  'Smoker aged <20',
                  'Age of first sexual intercourse',
                  'Birth weight',
                  'Ever had IM'),levels=c(   'Comparative body size aged 10: average (vs thinner)',
                                             'Comparative body size aged 10: plumper (vs thinner)',
                                             'Age at menarche (females)',
                                             'Age when voice broke (males): younger than average (vs average)',
                                             'Age when voice broke (males): older than average (vs average)',
                                             'Smoker aged <20',
                                             'Age of first sexual intercourse',
                                             'Ever had IM',
                                             'Birth weight',
                                             'Born in August (vs April)',
                                             'Born in December (vs April)',
                                             'Born in February (vs April)',
                                             'Born in January (vs April)',
                                             'Born in July (vs April)',
                                             'Born in June (vs April)',
                                             'Born in March (vs April)',
                                             'Born in May (vs April)',
                                             'Born in November (vs April)',
                                             'Born in October (vs April)',
                                             'Born in September (vs April)',
                                             'Breastfed as a baby (vs not)',
                                             'Exposed to maternal smoking (vs not)',
                                             'HLA DRB1*15 heterozygote (vs no DRB1*15 alleles)',
                                             'HLA DRB1*15 homozygote (vs no DRB1*15 alleles)',
                                             'HLA A*02 heterozygote (vs no A*02 alleles)',
                                             'HLA A*02 homozygote (vs no A*02 alleles)'),order=TRUE)


p=ggplot(df,aes(or,trait))+
  geom_vline(aes(xintercept=1),alpha=0.3)+
  geom_errorbarh(aes(y=trait,xmin=lower_ci,xmax=upper_ci),height=0.3)+
  geom_point(shape=22,size=5,fill="black")+
  scale_x_log10()+
  theme_classic()+
  theme(text=element_text(size = 16))+
  labs(x="Odds Ratio for MS",y="Risk factor",fill="P value")+
  annotate("rect",xmin = 0.4,xmax=6,ymin=0,ymax=8.5,fill="red",alpha=0.1)+
  annotate("rect",xmin = 0.4,xmax=6,ymin=8.5,ymax=22.5,fill="orange",alpha=0.1)+
  annotate("rect",xmin = 0.4,xmax=6,ymin=22.5,ymax=26.5,fill="yellow",alpha=0.1)



png("figure 1.png",height=10,width=10,res=1000,units="in")
p  
dev.off()

overall_model=glm(data=selected_vars,
                  MS_status~`Ethnic background.0.0`+`Age at recruitment.0.0`+selected_vars$Sex.0.0+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+selected_vars$`Comparative body size at age 10.0.0`+selected_vars$young_smoker+selected_vars$DRB_15+selected_vars$A_02,
                  family=binomial(link="logit"))

tbl = summary(overall_model)$coefficients
tbl = data.frame(tbl)
tbl$or = exp(tbl$Estimate)
tbl$lower_ci = exp(tbl$Estimate-1.96*tbl$Std..Error)
tbl$upper_ci = exp(tbl$Estimate+1.96*tbl$Std..Error)
tbl$clean_or = paste0(round(tbl$or,2)," (",round(tbl$lower_ci,2)," to ",round(tbl$upper_ci,2),")")
df = tbl
tbl = tbl[,c(8,4)]
colnames(tbl)=c("OR (95% CI)","P Value")

write.csv(tbl,"multivariate_overall.csv")


overall_model_f=glm(data=selected_vars,
                    MS_status~`Ethnic background.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+selected_vars$`Comparative body size at age 10.0.0`+selected_vars$`Age when periods started (menarche).0.0`+selected_vars$young_smoker+selected_vars$DRB_15+selected_vars$A_02,
                    family=binomial(link="logit"))

tbl = summary(overall_model_f)$coefficients
tbl = data.frame(tbl)
tbl$or = exp(tbl$Estimate)
tbl$lower_ci = exp(tbl$Estimate-1.96*tbl$Std..Error)
tbl$upper_ci = exp(tbl$Estimate+1.96*tbl$Std..Error)
tbl$clean_or = paste0(round(tbl$or,2)," (",round(tbl$lower_ci,2)," to ",round(tbl$upper_ci,2),")")
df = tbl
tbl = tbl[,c(8,4)]
colnames(tbl)=c("OR (95% CI)","P Value")

write.csv(tbl,"multivariate_overall_f.csv")

summary(overall_model_f)

#############################################
#     Additive and multiplicative interaction 
#############################################

selected_vars$A_02 = as.numeric(selected_vars$A_02)
selected_vars$DRB1_15 = as.numeric(selected_vars$DRB_15)

selected_vars = selected_vars %>% mutate('cBMI'=recode(selected_vars$`Comparative body size at age 10.0.0`,
                                                       'About average'="Not overweight",
                                                       'Plumper'="Overweight",
                                                       'Thinner'="Not overweight"))

selected_vars$smoking = factor(selected_vars$young_smoker)

p=ggplot(selected_vars,aes(`Genetic principal components.0.1`,`Genetic principal components.0.2`,col=raw_ethnicity))+geom_point()+labs(x="Genetic PC 1",y="Genetic PC 2",col="Self-reported ethnicity")
png("pcplot.png")
p
dev.off()

selected_vars = selected_vars %>% filter(`Ethnic background.0.0`=="White")
selected_vars = selected_vars %>% filter(`Genetic ethnic grouping.0.0`=="Caucasian")
p=ggplot(selected_vars,aes(`Genetic principal components.0.1`,`Genetic principal components.0.2`,col=raw_ethnicity))+geom_point()+labs(x="Genetic PC 1",y="Genetic PC 2",col="Self-reported ethnicity")

png("pcplot_afterexclusions.png")
p
dev.off()
kin = read_table2("/data/Wolfson-UKBB-Dobson/helper_progs_and_key/ukb43101_rel_s488282.dat")
exclusion = kin %>% filter(Kinship>0.0884) %>% select(ID1) %>% rename("EID"="ID1") 
selected_vars = selected_vars %>% filter(!EID %in% exclusion$EID)


#multiplicative interaction and plots
model = glm(data=selected_vars,
            MS_status~smoking*DRB1_15+
            `Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
            family=binomial(link="logit"))

null_model = glm(data=selected_vars,
                 MS_status~smoking+DRB1_15+
                 `Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
                 family=binomial(link="logit"))
summary(model)

anova(model,null_model,test="Chisq")

model = glm(data=selected_vars,
            MS_status~smoking*A_02+
            `Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
            family=binomial(link="logit"))

null_model = glm(data=selected_vars,
                 MS_status~smoking+A_02+
                 `Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
                 family=binomial(link="logit"))
summary(model)
anova(model,null_model,test="Chisq")


model = glm(data=selected_vars,
            MS_status~cBMI*DRB1_15+
            `Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
            family=binomial(link="logit"))

null_model = glm(data=selected_vars,
                 MS_status~cBMI+DRB1_15+
                 `Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
                 family=binomial(link="logit"))
summary(model)
anova(null_model,model,test="Chisq")

model = glm(data=selected_vars,
            MS_status~cBMI*A_02+
            `Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
            family=binomial(link="logit"))

null_model = glm(data=selected_vars,
                 MS_status~cBMI+A_02+
                 `Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
                 family=binomial(link="logit"))
summary(model)
anova(null_model,model,test="Chisq")

model = glm(data=selected_vars,
            MS_status~DRB1_15*A_02+
            `Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
            family=binomial(link="logit"))

null_model = glm(data=selected_vars,
                 MS_status~DRB1_15+A_02+
                 `Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
                 family=binomial(link="logit"))
summary(model)
anova(null_model,model,test="Chisq")


model = glm(data=selected_vars,
            MS_status~cBMI*smoking+
            `Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
            family=binomial(link="logit"))

null_model = glm(data=selected_vars,
                 MS_status~cBMI+smoking+
                 `Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
                 family=binomial(link="logit"))
summary(model)
anova(null_model,model,test="Chisq")

model = glm(data=selected_vars,
            MS_status~selected_vars$`Age when periods started (menarche).0.0`*A_02+
            `Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
            family=binomial(link="logit"))

null_model = glm(data=selected_vars,
                 MS_status~selected_vars$`Age when periods started (menarche).0.0`+A_02+
                 `Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
                 family=binomial(link="logit"))
summary(model)
anova(null_model,model,test="Chisq")

model = glm(data=selected_vars,
            MS_status~selected_vars$`Age when periods started (menarche).0.0`*DRB1_15+
            `Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
            family=binomial(link="logit"))

null_model = glm(data=selected_vars,
                 MS_status~selected_vars$`Age when periods started (menarche).0.0`+DRB1_15+
                 `Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`,
                 family=binomial(link="logit"))
summary(model)
anova(null_model,model,test="Chisq")



#prs
library(rcompanion)
library(ROCR)
library(RNOmni)


nagelkerke = c()
score_prs=function(){
  selected_vars = selected_vars %>% select(-contains("prs"))
  selected_vars = selected_vars %>% left_join(prs,by="EID")
  selected_vars = selected_vars %>% filter(!(is.na(PRS)))
  
  selected_vars = selected_vars %>% filter(!is.na(PRS))
  selected_vars$PRS=rankNorm(selected_vars$PRS)
  
  
  null_model = glm(data=selected_vars,
                   MS_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Place of birth in UK - north co-ordinate.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`,
                   family=binomial(link="logit"))
  prs_model = glm(data=selected_vars,
                  MS_status~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Place of birth in UK - north co-ordinate.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    PRS,
                  family=binomial(link="logit"))
  summary(prs_model)
  nag <<- nagelkerke(prs_model,null_model)
  nagelkerke <<- c(nagelkerke,nag$Pseudo.R.squared.for.model.vs.null[3])
}

prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_02/summarised_PRS_results_pval1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_02/summarised_PRS_results_pval0.8")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_02/summarised_PRS_results_pval0.6")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_02/summarised_PRS_results_pval0.4")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_02/summarised_PRS_results_pval0.2")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_02/summarised_PRS_results_pval0.1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_02/summarised_PRS_results_pval0.01")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_04/summarised_PRS_results_pval1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_04/summarised_PRS_results_pval0.8")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_04/summarised_PRS_results_pval0.6")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_04/summarised_PRS_results_pval0.4")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_04/summarised_PRS_results_pval0.2")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_04/summarised_PRS_results_pval0.1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_04/summarised_PRS_results_pval0.01")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_06/summarised_PRS_results_pval1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_06/summarised_PRS_results_pval0.8")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_06/summarised_PRS_results_pval0.6")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_06/summarised_PRS_results_pval0.4")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_06/summarised_PRS_results_pval0.2")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_06/summarised_PRS_results_pval0.1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_06/summarised_PRS_results_pval0.01")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_08/summarised_PRS_results_pval1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_08/summarised_PRS_results_pval0.8")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_08/summarised_PRS_results_pval0.6")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_08/summarised_PRS_results_pval0.4")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_08/summarised_PRS_results_pval0.2")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_08/summarised_PRS_results_pval0.1")
score_prs()
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_08/summarised_PRS_results_pval0.01")
score_prs()




pvals = factor(rep(c(1,0.8,0.6,0.4,0.2,0.1,0.01),4))
r2 = factor(c(rep(0.2,7),rep(0.4,7),rep(0.6,7),rep(0.8,7)))
nagel = data.frame(nagelkerke,pvals,r2)
nagel_plot = ggplot(nagel,aes(pvals,nagelkerke,fill=r2))+
  geom_col(position=position_dodge(),col="black")+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x="P value threshold",y="Nagelkerke PseudoR2",fill="Clumping R2 parameter")+
  theme(text=element_text(size=14))

png("figure 3A.png",height=10,width=10,res=1000,units="in")
nagel_plot
dev.off()




#choose best prs based on r2 and read it in
prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/r2_08/summarised_PRS_results_pval0.4")

selected_vars = selected_vars %>% select(-contains("prs"))
selected_vars = selected_vars %>% left_join(prs,by="EID")
selected_vars = selected_vars %>% filter(!is.na(PRS))
selected_vars$PRS=rankNorm(selected_vars$PRS)
library(Hmisc)

selected_vars$prs_decile = cut2(selected_vars$PRS,g=10)
prs_model = glm(data=selected_vars,
                MS_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  prs_decile,
                family=binomial(link="logit"))
tbl = data.frame(summary(prs_model)$coefficients[-c(1:7),])
tbl$decile=c(2:10)
tbl$or=exp(tbl$Estimate)
tbl$lower_ci=exp(tbl$Estimate-1.96*tbl$Std..Error)
tbl$upper_ci=exp(tbl$Estimate+1.96*tbl$Std..Error)
tbl
write_csv(tbl,"decile_ORs.csv")

prs_decile_plot=ggplot(tbl,aes(decile,or,fill=or))+
  geom_errorbar(aes(x=decile,ymin=lower_ci,ymax=upper_ci,width=0.2))+
  geom_point(size=5,shape=22)+
  scale_fill_continuous(type="viridis")+
  theme_classic()+
  theme(legend.position="none",text=element_text(size=16))+
  labs(x="PRS Decile",y="OR for MS (vs lowest decile)")

means = selected_vars %>% group_by(MS_status) %>%
  summarise("mean"=mean(PRS,na.rm=TRUE))
means$mean[1]

png("figure 3B.png",height=10,width=10,res=1000,units="in")
prs_decile_plot
dev.off()
str(selected_vars$MS_status)


hist = ggplot(selected_vars,aes(PRS,fill=MS_status))+
  geom_density(alpha=0.5)+
  scale_fill_brewer(palette ="Set2",labels=c("Controls","MS"))+
  theme_classic()+
  theme(text=element_text(size=16))+
  labs(x="PRS",y="Density",fill="MS status")

png("figure 3d.png",height=10,width=10,res=1000,units="in")
hist
dev.off()

# repeat as binary var
selected_vars$prs_quantile = cut2(selected_vars$PRS,g=2)
prs_model = glm(data=selected_vars,
                MS_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  prs_quantile,
                family=binomial(link="logit"))
levels(selected_vars$prs_quantile)=c("low","high")
summary(prs_model)



#additive interaction PRS


ms = selected_vars %>% filter(MS_status==1) %>%
  filter(!is.na(age_at_ms_diagnosis)) %>% 
  filter(age_at_ms_diagnosis>5)
summary(ms$age_at_ms_diagnosis)

age = selected_vars %>% filter(!is.na(age_at_ms_diagnosis))
age$age_at_ms_diagnosis = rankNorm(age$age_at_ms_diagnosis)
age_model=lm(data=age,
             age_at_ms_diagnosis~`Age at recruitment.0.0`+
               `Sex.0.0`+
               `Genetic principal components.0.1`+
               `Genetic principal components.0.2`+
               `Genetic principal components.0.3`+
               `Genetic principal components.0.4`+
               PRS)
age_model=lm(data=age,
             age_at_ms_diagnosis~PRS)
summary(age_model)

p=ggplot(age,aes((PRS),age_at_ms_diagnosis))+geom_point()+theme_classic()+theme(text=element_text(size=14))+
  labs(y="Age at diagnosis",x="PRS")
png("age_model.png")
p
dev.off()

summary(age_model)

# high vs low decile
levels(selected_vars$prs_decile)=c(1:10)
top_decile = selected_vars %>% filter(prs_decile=="10")
bottom_decile = selected_vars %>% filter(prs_decile=="1")


cbmi_top_model = glm(data=top_decile,
                MS_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  cBMI,
                family=binomial(link="logit"))

cbmi_bottom_model = glm(data=bottom_decile,
                     MS_status~`Age at recruitment.0.0`+
                       `Sex.0.0`+
                       `Genetic principal components.0.1`+
                       `Genetic principal components.0.2`+
                       `Genetic principal components.0.3`+
                       `Genetic principal components.0.4`+
                       cBMI,
                     family=binomial(link="logit"))
summary(cbmi_bottom_model)
summary(cbmi_top_model)


smoking_top_model = glm(data=top_decile,
                     MS_status~`Age at recruitment.0.0`+
                       `Sex.0.0`+
                       `Genetic principal components.0.1`+
                       `Genetic principal components.0.2`+
                       `Genetic principal components.0.3`+
                       `Genetic principal components.0.4`+
                       smoking,
                     family=binomial(link="logit"))
summary(smoking_top_model)

smoking_bottom_model = glm(data=bottom_decile,
                        MS_status~`Age at recruitment.0.0`+
                          `Sex.0.0`+
                          `Genetic principal components.0.1`+
                          `Genetic principal components.0.2`+
                          `Genetic principal components.0.3`+
                          `Genetic principal components.0.4`+
                          smoking,
                        family=binomial(link="logit"))
summary(smoking_bottom_model)
df=data.frame(cbind(rbind(summary(cbmi_top_model)$coefficients[8,c(1:2)],
summary(cbmi_bottom_model)$coefficients[8,c(1:2)],
summary(smoking_top_model)$coefficients[8,c(1:2)],
summary(smoking_bottom_model)$coefficients[8,c(1:2)]),
'PRS_decile'=c("Top","Bottom","Top","Bottom")),
'Trait'=c("Overweight at age 10","Overweight at age 10","Smoker < aged 20","Smoker < aged 20"))

df$Estimate=as.numeric(as.character(df$Estimate))
df$Std..Error=as.numeric(as.character(df$Std..Error))
df$or=exp(df$Estimate)
df$lower_ci=exp(df$Estimate-1.96*df$Std..Error)
df$upper_ci=exp(df$Estimate+1.96*df$Std..Error)
df$exposure=paste0(df$Trait,":",df$PRS_decile," PRS Decile")
strat_plot=ggplot(df,aes(or,exposure,fill=Trait))+
  geom_vline(xintercept=1,alpha=0.3)+
  geom_errorbarh(mapping=aes(y=exposure,xmin=lower_ci,xmax=upper_ci),height=0.3)+
  geom_point(shape=22,size=5)+
  theme_classic()+
  scale_fill_brewer(palette="Set2")+
  labs(y="Group",x="OR for MS",fill="Exposure")+
  theme(text=element_text(size=14))+
  scale_x_log10()


png("figure 2f.png",height=10,width=10,res=1000,units="in")
strat_plot
dev.off()

#model comparison
null_model = glm(data=selected_vars,
                 MS_status~`Age at recruitment.0.0`+
                   `Sex.0.0`+
                   `Genetic principal components.0.1`+
                   `Genetic principal components.0.2`+
                   `Genetic principal components.0.3`+
                   `Genetic principal components.0.4`,
                 family=binomial(link="logit"))

prs_model = glm(data=selected_vars,
                MS_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  PRS,
                family=binomial(link="logit"))

hla_model = glm(data=selected_vars,
                MS_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  DRB1_15+A_02,
                family=binomial(link="logit"))

prs_hla_model = glm(data=selected_vars,
                    MS_status~`Age at recruitment.0.0`+
                      `Sex.0.0`+
                      `Genetic principal components.0.1`+
                      `Genetic principal components.0.2`+
                      `Genetic principal components.0.3`+
                      `Genetic principal components.0.4`+
                      DRB1_15+A_02+PRS,
                    family=binomial(link="logit"))

env_model = glm(data=selected_vars,
                MS_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  cBMI+smoking+IM_status,
                family=binomial(link="logit"))

env_hla_model = glm(data=selected_vars,
                    MS_status~`Age at recruitment.0.0`+
                      `Sex.0.0`+
                      `Genetic principal components.0.1`+
                      `Genetic principal components.0.2`+
                      `Genetic principal components.0.3`+
                      `Genetic principal components.0.4`+
                      cBMI+smoking+IM_status+DRB1_15+A_02,
                    family=binomial(link="logit"))

final_model = glm(data=selected_vars,
                  MS_status~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    cBMI+smoking+DRB1_15+A_02+PRS,
                  family=binomial(link="logit"))

nagel=c(nagelkerke(final_model,null_model)$Pseudo.R.squared.for.model.vs.null[3],
        nagelkerke(env_hla_model,null_model)$Pseudo.R.squared.for.model.vs.null[3],
        nagelkerke(env_model,null_model)$Pseudo.R.squared.for.model.vs.null[3],
        nagelkerke(hla_model,null_model)$Pseudo.R.squared.for.model.vs.null[3],
        nagelkerke(prs_model,null_model)$Pseudo.R.squared.for.model.vs.null[3],
        nagelkerke(prs_hla_model,null_model)$Pseudo.R.squared.for.model.vs.null[3])

HLA = c("HLA","HLA","No HLA","HLA","No HLA","HLA")
prs = c("PRS","No PRS","No PRS","No PRS","PRS","PRS")
env = c("Environment","Environment","Environment","No environment","No environment","No environment")
model = factor(c("HLA+PRS+ENV","HLA+ENV","ENV","HLA","PRS","HLA+PRS"),
               levels=c("PRS","ENV","HLA","HLA+PRS","HLA+ENV","HLA+PRS+ENV"))

nagels = data.frame(nagel,HLA,prs,env,model)

nagel_plot_overall=ggplot(nagels,aes(model,nagel,fill=nagel))+
  geom_col(position=position_dodge(),col="black")+
  theme_classic()+
  scale_y_continuous(breaks=c(0.01,0.02,0.03,0.04,0.05,0.06,0.07))+
  scale_fill_continuous(type="viridis")+
  labs(x="Risk factors",y="Nagelkerke PseudoR2")+
  theme(legend.position="none",text=element_text(size=14))


png("figure 3e.png",height=10,width=10,res=1000,units="in")
nagel_plot_overall
dev.off()


#other disease
ibdnull_model = glm(data=selected_vars,
                    IBD_status~`Age at recruitment.0.0`+
                      `Sex.0.0`+
                      `Genetic principal components.0.1`+
                      `Genetic principal components.0.2`+
                      `Genetic principal components.0.3`+
                      `Genetic principal components.0.4`,
                    family=binomial(link="logit"))

ibd_model = glm(data=selected_vars,
                IBD_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  PRS,
                family=binomial(link="logit"))

ranull_model = glm(data=selected_vars,
                   RA_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`,
                   family=binomial(link="logit"))

ra_model = glm(data=selected_vars,
               RA_status~`Age at recruitment.0.0`+
                 `Sex.0.0`+
                 `Genetic principal components.0.1`+
                 `Genetic principal components.0.2`+
                 `Genetic principal components.0.3`+
                 `Genetic principal components.0.4`+
                 PRS,
               family=binomial(link="logit"))

slenull_model = glm(data=selected_vars,
                    SLE_status~`Age at recruitment.0.0`+
                      `Sex.0.0`+
                      `Genetic principal components.0.1`+
                      `Genetic principal components.0.2`+
                      `Genetic principal components.0.3`+
                      `Genetic principal components.0.4`,
                    family=binomial(link="logit"))

sle_model = glm(data=selected_vars,
                SLE_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  PRS,
                family=binomial(link="logit"))

pbcnull_model = glm(data=selected_vars,
                    PBC_status~`Age at recruitment.0.0`+
                      `Sex.0.0`+
                      `Genetic principal components.0.1`+
                      `Genetic principal components.0.2`+
                      `Genetic principal components.0.3`+
                      `Genetic principal components.0.4`,
                    family=binomial(link="logit"))

pbc_model = glm(data=selected_vars,
                PBC_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  PRS,
                family=binomial(link="logit"))

sjognull_model = glm(data=selected_vars,
                     SJOG_status~`Age at recruitment.0.0`+
                       `Sex.0.0`+
                       `Genetic principal components.0.1`+
                       `Genetic principal components.0.2`+
                       `Genetic principal components.0.3`+
                       `Genetic principal components.0.4`,
                     family=binomial(link="logit"))

sjog_model = glm(data=selected_vars,
                 SJOG_status~`Age at recruitment.0.0`+
                   `Sex.0.0`+
                   `Genetic principal components.0.1`+
                   `Genetic principal components.0.2`+
                   `Genetic principal components.0.3`+
                   `Genetic principal components.0.4`+
                   PRS,
                 family=binomial(link="logit"))


msnull_model = glm(data=selected_vars,
                   MS_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`,
                   family=binomial(link="logit"))

ms_model = glm(data=selected_vars,
               MS_status~`Age at recruitment.0.0`+
                 `Sex.0.0`+
                 `Genetic principal components.0.1`+
                 `Genetic principal components.0.2`+
                 `Genetic principal components.0.3`+
                 `Genetic principal components.0.4`+
                 PRS,
               family=binomial(link="logit"))


pvals=c(anova(ibd_model,test="Chisq")$`Pr(>Chi)`[8],
        anova(ra_model,test="Chisq")$`Pr(>Chi)`[8],
        anova(sjog_model,test="Chisq")$`Pr(>Chi)`[8],
        anova(sle_model,test="Chisq")$`Pr(>Chi)`[8],
        anova(pbc_model,test="Chisq")$`Pr(>Chi)`[8],
        anova(ms_model,test="Chisq")$`Pr(>Chi)`[8])


nagels=c(nagelkerke(ibd_model,ibdnull_model)$Pseudo.R.squared.for.model.vs.null[3],
         nagelkerke(ra_model,ranull_model)$Pseudo.R.squared.for.model.vs.null[3],
         nagelkerke(sjog_model,sjognull_model)$Pseudo.R.squared.for.model.vs.null[3],
         nagelkerke(sle_model,slenull_model)$Pseudo.R.squared.for.model.vs.null[3],
         nagelkerke(pbc_model,pbcnull_model)$Pseudo.R.squared.for.model.vs.null[3],
         nagelkerke(ms_model,msnull_model)$Pseudo.R.squared.for.model.vs.null[3])
disease = c("IBD","RA","Sjogren's","SLE","PBC","MS")
df = data.frame(nagels,disease,pvals)
df$logp=log10(df$pvals)*-1
df$adjusted_p=p.adjust(df$pvals,method="fdr")

df=df[order(df$nagels),]
df$disease=factor(df$disease,levels=df$disease,order=TRUE)
p=ggplot(df,aes(disease,nagels,fill=logp))+
  geom_col(alpha=0.5)+
  scale_fill_continuous(type="viridis")+
  theme_classic()+
  labs(x="Disease",y="Nagelkerke's Pseudo-R2",fill="-log10 P value")+
  theme(text=element_text(size=14))

png("supp_fig_disease.png")
p
dev.off()





