#############################################
#               Load packages
#############################################

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(rcompanion)
library(ROCR)
library(RNOmni)

#############################################
#               read in data
#############################################

args = commandArgs(trailingOnly=TRUE)
print(args[1])
print("iterations:")
print(args[2])


setwd("/data/Wolfson-UKBB-Dobson/ukb_pheno_911")
selected_vars = read_tsv("ukb_pheno_final_MS_1301")
df = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_2410/ms_dates.tsv")
selected_vars = selected_vars %>% left_join(df,by="EID")
im = read_tsv("/data/Wolfson-UKBB-Dobson/ukb_pheno_2410/im_codes.tsv")
colnames(im)=c("EID","date_of_im_report","source_of_im_report")
im = im %>% mutate(IM_status = ifelse(!is.na(source_of_im_report),1,0))
selected_vars = selected_vars %>% select(-IM_status) %>% left_join(im,by="EID")

#############################################
#               Get vars in right format
#############################################

# define MS status using source of report
selected_vars = selected_vars %>% mutate(MS_status = ifelse(!is.na(source_of_ms_report),1,0))

table(selected_vars$MS_status)
############## Extras for sensitivity analysis (not run in main analysis)


# exclude one source only
selected_vars = selected_vars %>% filter(MS_status==0 | (MS_status==1 & source_of_ms_report=="self-report and other source/s") |(MS_status==1 & source_of_ms_report=="primary care and other source/s") |(MS_status==1 & source_of_ms_report=="HES data and other source/s"))
table(selected_vars$MS_status)

##############

# clean up dates
selected_vars = selected_vars %>% mutate(date_of_ms_report=ifelse(date_of_ms_report == "1901-01-01" | date_of_ms_report == "1902-02-02" | date_of_ms_report == "1903-03-03" | date_of_ms_report == "2037-07-07",NA,date_of_ms_report))

selected_vars = selected_vars %>% mutate(date_of_im_report=ifelse(date_of_im_report == "1901-01-01" | date_of_im_report == "1902-02-02" | date_of_im_report == "1903-03-03" | date_of_im_report == "2037-07-07",NA,date_of_im_report))

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

selected_vars = selected_vars %>% mutate(dob = as.numeric(as.Date(paste0(`Year of birth.0.0`,"-01-01")))) %>%
mutate(age_at_ms_dx = (as.numeric(date_of_ms_report) - dob)/365) %>%
mutate(age_at_im_dx = (as.numeric(date_of_im_report) - dob)/365)

# basic demographic plots
hist=ggplot(selected_vars%>%filter(MS_status==1),aes(age_at_ms_dx))+geom_density()+theme_classic()+labs(x="Age at first MS diagnostic code report")
png("age_histo.png",res=300,height=8,width=8,units="in")
hist
dev.off()


# stats for age
summary(selected_vars$age_at_ms_dx)
# exclude MS prior to 20
selected_vars = selected_vars %>% filter(!(MS_status==1 & age_at_ms_dx <20))
selected_vars = selected_vars %>% mutate(IM_status = ifelse(IM_status==1 & age_at_im_dx<20,1,0))


#############################################
#     Work out basic demographics
#############################################

summ1 = cbind(rbind(table(selected_vars$Sex.0.0,selected_vars$MS_status),
  table(selected_vars$`Month of birth.0.0`,selected_vars$MS_status),table(selected_vars$`Breastfed as a baby.0.0`,selected_vars$MS_status),
  table(selected_vars$`Comparative body size at age 10.0.0`,selected_vars$MS_status),table(selected_vars$`Maternal smoking around birth.0.0`,selected_vars$MS_status),
  table(selected_vars$`Relative age voice broke.0.0`,selected_vars$MS_status),table(selected_vars$young_smoker,selected_vars$MS_status),
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
#     Build multivariable models
#############################################

make_model = function(x){
glm(data=selected_vars,MS_status~`Ethnic background.0.0`+`Sex.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+selected_vars[[x]],
family=binomial(link="logit"))
}
make_model2 = function(x){
glm(data=selected_vars,MS_status~`Ethnic background.0.0`+`Age at recruitment.0.0`+`Place of birth in UK - north co-ordinate.0.0`+`Townsend deprivation index at recruitment.0.0`+selected_vars[[x]],
family=binomial(link="logit"))
}


mob_model_multi=make_model("Month of birth.0.0")
breastfeeding_model_multi=make_model("Breastfed as a baby.0.0")
cbmi_model_multi=make_model("Comparative body size at age 10.0.0")
mat_smoking_model_multi=make_model("Maternal smoking around birth.0.0")
age_had_sex_model_multi=make_model("Age first had sexual intercourse.0.0")
smoking_model_multi=make_model("young_smoker")
menarche_model_multi=make_model2("Age when periods started (menarche).0.0")
voicebreak_model_multi=make_model2("Relative age voice broke.0.0")
bw_model_multi=make_model("Birth weight.0.0")
im_model_multi=make_model("IM_status")

tbl = rbind(summary(mob_model_multi)$coefficients[-c(1:6),],
            summary(breastfeeding_model_multi)$coefficients[-c(1:6),],
            summary(cbmi_model_multi)$coefficients[-c(1:6),],
            summary(mat_smoking_model_multi)$coefficients[-c(1:6),],
            summary(menarche_model_multi)$coefficients[-c(1:5),],
            summary(voicebreak_model_multi)$coefficients[-c(1:5),],
            summary(smoking_model_multi)$coefficients[-c(1:6),],
            summary(age_had_sex_model_multi)$coefficients[-c(1:6),],
            summary(bw_model_multi)$coefficients[-c(1:6),],
            summary(im_model_multi)$coefficients[-c(1:6),])

rownames(tbl)[12]=rownames(summary(breastfeeding_model_multi)$coefficients)[7]
rownames(tbl)[15]=rownames(summary(mat_smoking_model_multi)$coefficients)[7]
rownames(tbl)[16]=rownames(summary(menarche_model_multi)$coefficients)[6]
rownames(tbl)[19]=rownames(summary(smoking_model_multi)$coefficients)[7]
rownames(tbl)[20]=rownames(summary(age_had_sex_model_multi)$coefficients)[7]
rownames(tbl)[21]=rownames(summary(bw_model_multi)$coefficients)[7]
rownames(tbl)[22]=rownames(summary(im_model_multi)$coefficients)[7]

tbl = data.frame(tbl)
tbl$or = exp(tbl$Estimate)
tbl$lower_ci = exp(tbl$Estimate-1.96*tbl$Std..Error)
tbl$upper_ci = exp(tbl$Estimate+1.96*tbl$Std..Error)
tbl$clean_or = paste0(round(tbl$or,2)," (",round(tbl$lower_ci,2)," to ",round(tbl$upper_ci,2),")")
df = tbl
tbl = tbl[,c(8,4)]
colnames(tbl)=c("OR (95% CI)","P Value")


labs = c("MOB","BF","cBMI","Mat smok","Menarche","Voicebreak","Smoking","Age had sex","BW","IM")
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
anov =anova(bw_model_multi,test="Chisq")
p = anov$`Pr(>Chi)`[length(anov$`Pr(>Chi)`)]
lik_rats=c(lik_rats,p)
anov =anova(im_model_multi,test="Chisq")
p = anov$`Pr(>Chi)`[length(anov$`Pr(>Chi)`)]
lik_rats=c(lik_rats,p)

lik = data.frame(labs,lik_rats)
lik$thresh=lik_rats<0.05/10
write.csv(lik,"lik.csv")

write.csv(tbl,"models.csv")

#model plots


df$trait=factor(c('Born in August (vs April)',
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
                                             'Exposed to maternal smoking (vs not)'),order=TRUE)


p=ggplot(df,aes(log(or),trait))+
  geom_vline(aes(xintercept=0),alpha=0.3)+
  geom_errorbarh(aes(y=trait,xmin=log(lower_ci),xmax=log(upper_ci)),height=0.3)+
  geom_point(shape=22,size=1,fill="black")+
  theme_classic()+
  theme(text=element_text(size = 12))+
  labs(x="Log odds Ratio for MS (+/- 95% CI)",y="Risk factor")+
  scale_x_continuous(limits=c(-1.5,1.5))



png("figure 1.png",height=6,width=12,res=300,units="in")
p
dev.off()


# png("sensitivity_analysis_no_selfreports.png",height=6,width=12,res=300,units="in")
# p
# dev.off()


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

write.csv(tbl,"multivariable_overall.csv")


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

write.csv(tbl,"multivariable_overall_f.csv")

summary(overall_model_f)

#############################################
#     Additive and multiplicative interaction
#############################################

selected_vars = selected_vars %>% mutate('cBMI'=recode(selected_vars$`Comparative body size at age 10.0.0`,
                                                       'About average'="Not overweight",
                                                       'Plumper'="Overweight",
                                                       'Thinner'="Not overweight"))

selected_vars$smoking = factor(selected_vars$young_smoker)

p=ggplot(selected_vars,aes(`Genetic principal components.0.1`,`Genetic principal components.0.2`,col=raw_ethnicity))+geom_point()+labs(x="Genetic PC 1",y="Genetic PC 2",col="Self-reported ethnicity")+theme_classic()
png("pcplot_before_exclusions.png",height=8,width=8,res=300,units="in")
p
dev.off()

table(selected_vars$MS_status)

# filter ethnicity
selected_vars = selected_vars %>% filter(`Ethnic background.0.0`=="White")
selected_vars = selected_vars %>% filter(`Genetic ethnic grouping.0.0`=="Caucasian")
table(selected_vars$MS_status)

p=ggplot(selected_vars,aes(`Genetic principal components.0.1`,`Genetic principal components.0.2`,col=raw_ethnicity))+geom_point()+labs(x="Genetic PC 1",y="Genetic PC 2",col="Self-reported ethnicity")+theme_classic()

png("pcplot_after_exclusions.png",height=8,width=8,res=300,units="in")
p
dev.off()

table(selected_vars$MS_status)

# filter relatedness
kin = read_table2("/data/Wolfson-UKBB-Dobson/helper_progs_and_key/ukb43101_rel_s488282.dat")
exclusion = kin %>% filter(Kinship>0.0884) %>% select(ID1) %>% rename("EID"="ID1")
selected_vars = selected_vars %>% filter(!EID %in% exclusion$EID)
table(selected_vars$MS_status)

##############
#   prs
##############

set.seed(123456)
prs_tuning_dataset = sample_frac(selected_vars,size=0.3,replace=FALSE)
selected_vars = selected_vars %>% filter(!EID %in% prs_tuning_dataset$EID)
write_tsv(selected_vars,"testing_set.tsv.gz")
write_tsv(prs_tuning_dataset,"training_set.tsv.gz")

# selected_vars = read_tsv("testing_set.tsv.gz",guess_max=10000)
print("cases and controls in training set")
table(prs_tuning_dataset$MS_status)

print("cases and controls in validation set")
table(selected_vars$MS_status)


score_prs = function(pval,r2){
# read in prs
filename = paste0("/data/Wolfson-UKBB-Dobson/ms_prs/summarised_PRS_results_pval",pval,"r2",r2)
prs = read_table2(filename)
colnames(prs) = c("EID","PRS")
prs_tuning_dataset = prs_tuning_dataset %>% select(-contains("prs"),`Age at recruitment.0.0`,`Sex.0.0`,`Townsend deprivation index at recruitment.0.0`,`Genetic principal components.0.1`,`Genetic principal components.0.2`,`Genetic principal components.0.3`,`Genetic principal components.0.4`,MS_status,EID)
prs_tuning_dataset$EID = as.numeric(as.character(prs_tuning_dataset$EID))
prs_tuning_dataset = prs_tuning_dataset %>% left_join(prs,by="EID")
prs_tuning_dataset = prs_tuning_dataset %>% filter(!(is.na(PRS)))
prs_tuning_dataset$PRS=rankNorm(prs_tuning_dataset$PRS)

null_model = glm(data=prs_tuning_dataset,
                   MS_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`,
                   family=binomial(link="logit"))

prs_model = glm(data=prs_tuning_dataset,
                  MS_status~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    PRS,
                  family=binomial(link="logit"))
nag = nagelkerke(prs_model,null_model)$Pseudo.R.squared.for.model.vs.null[3]
return(nag)
}

pvals = c("1","0.8","0.6","0.4","0.2","0.1","0.05","0.00000005")
r2 = c(0.2,0.4,0.6,0.8)
params = expand.grid(pvals,r2)

nags = mapply(score_prs,pval=params[,1],r2=params[,2])

params = data.frame(params)
params$Nagelkerke_Pseudo_R2 = nags
colnames(params)=c("Pval","R2","Nagelkerke_Pseudo_R2")
hla_scores = params

# repeat with hla excluded
score_prs_nohla = function(pval,r2){
# read in prs
filename = paste0("/data/Wolfson-UKBB-Dobson/ms_prs/nomhc_summarised_PRS_results_pval",pval,"r2",r2)
prs = read_table2(filename)
colnames(prs) = c("EID","PRS")
prs_tuning_dataset = prs_tuning_dataset %>% select(-contains("prs"),`Age at recruitment.0.0`,`Sex.0.0`,`Townsend deprivation index at recruitment.0.0`,`Genetic principal components.0.1`,`Genetic principal components.0.2`,`Genetic principal components.0.3`,`Genetic principal components.0.4`,MS_status,EID)
prs_tuning_dataset$EID = as.numeric(as.character(prs_tuning_dataset$EID))
prs_tuning_dataset = prs_tuning_dataset %>% left_join(prs,by="EID")
prs_tuning_dataset = prs_tuning_dataset %>% filter(!(is.na(PRS)))
prs_tuning_dataset$PRS=rankNorm(prs_tuning_dataset$PRS)

null_model = glm(data=prs_tuning_dataset,
                   MS_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`,
                   family=binomial(link="logit"))

prs_model = glm(data=prs_tuning_dataset,
                  MS_status~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    PRS,
                  family=binomial(link="logit"))
nag = nagelkerke(prs_model,null_model)$Pseudo.R.squared.for.model.vs.null[3]
return(nag)
}

pvals = c("1","0.8","0.6","0.4","0.2","0.1","0.05","0.00000005")
r2 = c(0.2,0.4,0.6,0.8)
params = expand.grid(pvals,r2)

nags = mapply(score_prs_nohla,pval=params[,1],r2=params[,2])

params = data.frame(params)
params$Nagelkerke_Pseudo_R2 = nags
colnames(params)=c("Pval","R2","Nagelkerke_Pseudo_R2")
nohla_scores = params

nohla_scores$MHC = "MHC excluded"
hla_scores$MHC = "MHC included"
params = bind_rows(nohla_scores,hla_scores)
training_params = params

nagel_plot = ggplot(params,aes(factor(as.numeric(as.character(Pval))),Nagelkerke_Pseudo_R2,fill=factor(R2)))+
  facet_wrap(~MHC)+
  geom_col(position=position_dodge(),col="black")+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x="P value threshold",y=expression(Nagelkerke~Pseudo-R^{"2"}),fill=expression(Clumping~R^{"2"}~parameter))+
  theme(text=element_text(size=12))

png("nagel_plot.png",height=8,width=8,res=300,units="in")
nagel_plot
dev.off()



png("sensitivity_analysis_no_selfreports_nagel_plot.png",height=8,width=8,res=300,units="in")
nagel_plot
dev.off()

write_csv(params,"prs_nagelkerke_training.csv")


# experiment with different numbers of PCs

# none

score_prs_nopcs = function(pval,r2){
# read in prs
filename = paste0("/data/Wolfson-UKBB-Dobson/ms_prs/summarised_PRS_results_pval",pval,"r2",r2)
prs = read_table2(filename)
colnames(prs) = c("EID","PRS")
prs_tuning_dataset = prs_tuning_dataset %>% select(-contains("prs"),`Age at recruitment.0.0`,`Sex.0.0`,`Townsend deprivation index at recruitment.0.0`,`Genetic principal components.0.1`,`Genetic principal components.0.2`,`Genetic principal components.0.3`,`Genetic principal components.0.4`,MS_status,EID)
prs_tuning_dataset$EID = as.numeric(as.character(prs_tuning_dataset$EID))
prs_tuning_dataset = prs_tuning_dataset %>% left_join(prs,by="EID")
prs_tuning_dataset = prs_tuning_dataset %>% filter(!(is.na(PRS)))
prs_tuning_dataset$PRS=rankNorm(prs_tuning_dataset$PRS)

null_model = glm(data=prs_tuning_dataset,
                   MS_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`,
                   family=binomial(link="logit"))

prs_model = glm(data=prs_tuning_dataset,
                  MS_status~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                    PRS,
                  family=binomial(link="logit"))
nag = nagelkerke(prs_model,null_model)$Pseudo.R.squared.for.model.vs.null[3]
return(nag)
}

pvals = c("1","0.8","0.6","0.4","0.2","0.1","0.05","0.00000005")
r2 = c(0.2,0.4,0.6,0.8)
params = expand.grid(pvals,r2)

nags = mapply(score_prs_nopcs,pval=params[,1],r2=params[,2])

params = data.frame(params)
params$Nagelkerke_Pseudo_R2 = nags
colnames(params)=c("Pval","R2","Nagelkerke_Pseudo_R2")


nagel_plot = ggplot(params,aes(factor(Pval),Nagelkerke_Pseudo_R2,fill=factor(R2)))+
  geom_col(position=position_dodge(),col="black")+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x="P value threshold",y=expression(Nagelkerke~Pseudo-R^{"2"}),fill=expression(Clumping~R^{"2"}~parameter))+
  theme(text=element_text(size=12))

png("nagel_plot_nopcs.png",height=10,width=10,res=1000,units="in")
nagel_plot
dev.off()

# 10 pcs
score_prs_tenpcs = function(pval,r2){
# read in prs
filename = paste0("/data/Wolfson-UKBB-Dobson/ms_prs/summarised_PRS_results_pval",pval,"r2",r2)
prs = read_table2(filename)
colnames(prs) = c("EID","PRS")
prs_tuning_dataset = prs_tuning_dataset %>% select(-contains("prs"),`Age at recruitment.0.0`,`Sex.0.0`,`Townsend deprivation index at recruitment.0.0`,contains("principal components"),MS_status,EID)
prs_tuning_dataset$EID = as.numeric(as.character(prs_tuning_dataset$EID))
prs_tuning_dataset = prs_tuning_dataset %>% left_join(prs,by="EID")
prs_tuning_dataset = prs_tuning_dataset %>% filter(!(is.na(PRS)))
prs_tuning_dataset$PRS=rankNorm(prs_tuning_dataset$PRS)

null_model = glm(data=prs_tuning_dataset,
                   MS_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`+
                      `Genetic principal components.0.5`+
                     `Genetic principal components.0.6`+
                     `Genetic principal components.0.7`+
                     `Genetic principal components.0.8`+
                      `Genetic principal components.0.9`+
                     `Genetic principal components.0.10`,
                   family=binomial(link="logit"))

prs_model = glm(data=prs_tuning_dataset,
                  MS_status~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`+
                      `Genetic principal components.0.5`+
                     `Genetic principal components.0.6`+
                     `Genetic principal components.0.7`+
                     `Genetic principal components.0.8`+
                      `Genetic principal components.0.9`+
                     `Genetic principal components.0.10`+
                    PRS,
                  family=binomial(link="logit"))
nag = nagelkerke(prs_model,null_model)$Pseudo.R.squared.for.model.vs.null[3]
return(nag)
}

pvals = c("1","0.8","0.6","0.4","0.2","0.1","0.05","0.00000005")
r2 = c(0.2,0.4,0.6,0.8)
params = expand.grid(pvals,r2)

nags = mapply(score_prs_tenpcs,pval=params[,1],r2=params[,2])

params = data.frame(params)
params$Nagelkerke_Pseudo_R2 = nags
colnames(params)=c("Pval","R2","Nagelkerke_Pseudo_R2")


nagel_plot = ggplot(params,aes(factor(Pval),Nagelkerke_Pseudo_R2,fill=factor(R2)))+
  geom_col(position=position_dodge(),col="black")+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  labs(x="P value threshold",y=expression(Nagelkerke~Pseudo-R^{"2"}),fill=expression(Clumping~R^{"2"}~parameter))+
  theme(text=element_text(size=12))

png("nagel_plot_tenpcs.png",height=10,width=10,res=1000,units="in")
nagel_plot
dev.off()

##########################
# validation
##########################


#choose best prs based on r2 and read it in

prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/summarised_PRS_results_pval0.4r20.8")

colnames(prs)=c("EID","PRS")
nohla_prs = read_table2("/data/Wolfson-UKBB-Dobson/ms_prs/nomhc_summarised_PRS_results_pval0.05r20.8")
colnames(nohla_prs)=c("EID","PRS")

selected_vars = selected_vars %>% select(-contains("PRS"))
selected_vars = selected_vars %>% left_join(prs,by="EID")
selected_vars = selected_vars %>% filter(!(is.na(PRS)))
selected_vars$HLA_PRS=rankNorm(selected_vars$PRS)
selected_vars = selected_vars %>% select(-PRS)
selected_vars = selected_vars %>% left_join(nohla_prs,by="EID")
selected_vars = selected_vars %>% filter(!(is.na(PRS)))
selected_vars$NOHLA_PRS=rankNorm(selected_vars$PRS)

# nagelkerke

null_model = glm(data=selected_vars,
                   MS_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`,
                   family=binomial(link="logit"))

hlaprs_model = glm(data=selected_vars,
                  MS_status~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    HLA_PRS,
                  family=binomial(link="logit"))

nohla_prs_model = glm(data=selected_vars,
                  MS_status~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    NOHLA_PRS,
                  family=binomial(link="logit"))

print("printing nagelkerke for best PRS in testing set")
print(nagelkerke(hlaprs_model,null_model))
print("printing nagelkerke for best non-HLA PRS in testing set")
print(nagelkerke(nohla_prs_model,null_model))

library(Hmisc)

# decile plot
selected_vars$prs_decile = cut2(selected_vars$HLA_PRS,g=10)
prs_decile_model = glm(data=selected_vars,
                MS_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Townsend deprivation index at recruitment.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  prs_decile,
                family=binomial(link="logit"))

tbl = data.frame(summary(prs_decile_model)$coefficients[-c(1:8),])
tbl$decile=c(2:10)
tbl$or=exp(tbl$Estimate)
tbl$lower_ci=exp(tbl$Estimate-1.96*tbl$Std..Error)
tbl$upper_ci=exp(tbl$Estimate+1.96*tbl$Std..Error)
tbl
write_csv(tbl,"decile_ORs.csv")

prs_decile_plot=ggplot(tbl,aes(decile,or))+
  geom_errorbar(aes(x=decile,ymin=lower_ci,ymax=upper_ci,width=0.2))+
  geom_point(size=3,shape=22,fill="black")+
  theme_classic()+
  theme(legend.position="none",text=element_text(size=12))+
  labs(x="PRS Decile",y="OR for MS (vs lowest decile)")+
  annotate("text",label="MHC Included",x = -Inf, y = Inf, hjust = -0.5, vjust = 1,size=8)+
  ylim(c(0,11))




selected_vars$prs_decile = cut2(selected_vars$NOHLA_PRS,g=10)
prs_decile_model = glm(data=selected_vars,
                MS_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Townsend deprivation index at recruitment.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  prs_decile,
                family=binomial(link="logit"))

tbl = data.frame(summary(prs_decile_model)$coefficients[-c(1:8),])
tbl$decile=c(2:10)
tbl$or=exp(tbl$Estimate)
tbl$lower_ci=exp(tbl$Estimate-1.96*tbl$Std..Error)
tbl$upper_ci=exp(tbl$Estimate+1.96*tbl$Std..Error)
tbl
write_csv(tbl,"nohla_decile_ORs.csv")

no_hla_prs_decile_plot=ggplot(tbl,aes(decile,or))+
  geom_errorbar(aes(x=decile,ymin=lower_ci,ymax=upper_ci,width=0.2))+
  geom_point(size=3,shape=22,fill="black")+
  theme_classic()+
  theme(legend.position="none",text=element_text(size=12))+
  labs(x="PRS Decile",y="OR for MS (vs lowest decile)")+
  annotate("text",label="MHC Excluded",x = -Inf, y = Inf, hjust = -0.5, vjust = 1,size=8)+
  ylim(c(0,11))


library(gridExtra)

png("decile_plot.png",height=8,width=8,res=300,units="in")
grid.arrange(no_hla_prs_decile_plot,prs_decile_plot,nrow=1)
dev.off()

# png("sensitivity_analysis_no_selfreports_decile_plot.png",height=8,width=8,res=300,units="in")
# grid.arrange(no_hla_prs_decile_plot,prs_decile_plot,nrow=1)
# dev.off()

hist_nohla = ggplot(selected_vars,aes(NOHLA_PRS,fill=factor(MS_status)))+
  geom_density(alpha=0.5)+
  scale_fill_brewer(palette ="Set2",labels=c("Controls","MS"))+
  theme_classic()+
  theme(text=element_text(size=12))+
  labs(x="PRS",y="Density",fill="MS status")+
  annotate("text",label="MHC Excluded",x = -Inf, y = Inf, hjust = -0.5, vjust = 1,size=5)

hist_hla = ggplot(selected_vars,aes(HLA_PRS,fill=factor(MS_status)))+
  geom_density(alpha=0.5)+
  scale_fill_brewer(palette ="Set2",labels=c("Controls","MS"))+
  theme_classic()+
  theme(text=element_text(size=12))+
  labs(x="PRS",y="Density",fill="MS status")+
  annotate("text",label="MHC Included",x = -Inf, y = Inf, hjust = -0.5, vjust = 1,size=5)


png("prs_hist.png",height=8,width=8,res=300,units="in")
grid.arrange(hist_nohla,hist_hla,nrow=1)
dev.off()

# png("sensitivity_analysis_no_selfreports_prs_hist.png",height=8,width=8,res=300,units="in")
# grid.arrange(hist_nohla,hist_hla,nrow=1)
# dev.off()

# roc analysis
library(ROCR)
hlaprs_model = glm(data=selected_vars,
                MS_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Townsend deprivation index at recruitment.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  HLA_PRS,
                family=binomial(link="logit"))

nohla_prs_model = glm(data=selected_vars,
                MS_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Townsend deprivation index at recruitment.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  NOHLA_PRS,
                family=binomial(link="logit"))

null_model = glm(data=selected_vars,
                MS_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Townsend deprivation index at recruitment.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`,
                family=binomial(link="logit"))

make_prediction = function(x){
predict(x,newdata=selected_vars,type="response")
}

preds = data.frame("hlaprs_model"=make_prediction(hlaprs_model),
"nohla_prs_model"=make_prediction(nohla_prs_model),
"null_model"=make_prediction(null_model),
"prs_decile"=selected_vars$prs_decile,
"MS_status"=selected_vars$MS_status)

preds_summary = preds %>% group_by(prs_decile) %>% summarise(mean(hlaprs_model,na.rm=TRUE),mean(nohla_prs_model,na.rm=TRUE),mean(null_model,na.rm=TRUE))
tbl = table(selected_vars$prs_decile,selected_vars$MS_status)
obs_risk = tbl[,2]/rowSums(tbl)

pred_df = data.frame(preds_summary,obs_risk)
pred_df$prs_decile = c(1:10)
library(reshape2)
pred_df = melt(pred_df,id="prs_decile")
pred_df$prs_decile = factor(pred_df$prs_decile)

calib_plot = ggplot(pred_df,aes(prs_decile,value,col=variable,group=variable))+geom_point(col="black",shape=22,alpha=0.3)+geom_line()+labs(col="Source of risk estimate",x="PRS decile",y="MS risk (probability scale)")+scale_color_brewer(palette="Set2",labels=c("HLA PRS model","Non-HLA PRS model","Null model (Age, Sex, PCs, Townsend)","Observed risk"))+theme_classic()

png("calibration_plot.png",height=8,width=8,res=300,units="in")
calib_plot
dev.off()


png("sensitivity_analysis_no_selfreports_calibration_plot.png",height=8,width=8,res=300,units="in")
calib_plot
dev.off()

# auc

preds = preds %>% na.omit()
preds$MS=factor(preds$MS_status)
predictions = prediction(list(preds$hlaprs_model,preds$nohla_prs_model,preds$null_model),
list(preds$MS,preds$MS,preds$MS))
roc.perf = ROCR::performance(predictions, measure = "tpr", x.measure = "fpr")

hla_prs_model_auc = data.frame(x=roc.perf@x.values[[1]],y=roc.perf@y.values[[1]],model="HLA PRS model")
nohla_prs_model_auc = data.frame(x=roc.perf@x.values[[2]],y=roc.perf@y.values[[2]],model="Non-HLA PRS model")
null_model_auc = data.frame(x=roc.perf@x.values[[3]],y=roc.perf@y.values[[3]],model="Null model")

df = bind_rows(hla_prs_model_auc,nohla_prs_model_auc,null_model_auc)
auc.perf = performance(predictions, measure = "auc")
auc.perf@y.values

aucplot=ggplot(df,aes(x,y,col=model))+
geom_line()+
scale_color_brewer(palette="Set2")+
labs(x="False Positive Rate",y="True Positive Rate",col="Model")+
theme_classic()+
geom_abline()+
annotate("text",x=0.8,y=0.3,label="AUC",hjust=0)+
annotate("text",x=1,y=0.25,label=paste0("HLA PRS model: ",round(auc.perf@y.values[[1]],3)),hjust=1)+
annotate("text",x=1,y=0.2,label=paste0("Non-HLA PRS model: ",round(auc.perf@y.values[[2]],3)),hjust=1)+
annotate("text",x=1,y=0.15,label=paste0("Null model: ",round(auc.perf@y.values[[3]],3)),hjust=1)

png("discrimination_plot.png",height=8,width=8,res=300,units="in")
aucplot
dev.off()

# png("sensitivity_analysis_no_selfreports_discrimination_plot.png",height=8,width=8,res=300,units="in")
# aucplot
# dev.off()



# relationship with age at report

age = selected_vars %>% filter(!is.na(age_at_ms_dx))
sd(age$age_at_ms_dx)
age$age_at_ms_diagnosis = rankNorm(age$age_at_ms_dx)
age_model=lm(data=age,
             age_at_ms_diagnosis~`Age at recruitment.0.0`+
               `Sex.0.0`+
               `Townsend deprivation index at recruitment.0.0`+
               `Genetic principal components.0.1`+
               `Genetic principal components.0.2`+
               `Genetic principal components.0.3`+
               `Genetic principal components.0.4`+
               HLA_PRS)
summary(age_model)

p=ggplot(age,aes((HLA_PRS),age_at_ms_diagnosis))+geom_point()+theme_classic()+theme(text=element_text(size=12))+
  labs(y="Age at MS report",x="PRS")
png("age_model.png")
p
dev.off()

age_model=lm(data=age,
             age_at_ms_diagnosis~`Age at recruitment.0.0`+
               `Sex.0.0`+
               `Townsend deprivation index at recruitment.0.0`+
               `Genetic principal components.0.1`+
               `Genetic principal components.0.2`+
               `Genetic principal components.0.3`+
               `Genetic principal components.0.4`+
               NOHLA_PRS)
summary(age_model)
p=ggplot(age,aes((NOHLA_PRS),age_at_ms_diagnosis))+geom_point()+theme_classic()+theme(text=element_text(size=12))+
  labs(y="Age at MS report",x="PRS")
png("nohla_age_model.png")
p
dev.off()


##############################
# multiplicative interactions
##############################


multiplic_df = data.frame()
get_multiplicative_int = function(model,rf) {
model1 = glm(data=selected_vars,
                MS_status~`Age at recruitment.0.0`+
                  `Sex.0.0`+
                  `Townsend deprivation index at recruitment.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  selected_vars[[model]]*selected_vars[[rf]],
                family=binomial(link="logit"))
multiplic_df <<- bind_rows(multiplic_df,summary(model1)$coefficients[11,c(1,2,4)])
}

get_multiplicative_int2 = function(model,rf) {
model1 = glm(data=selected_vars,
                MS_status~`Age at recruitment.0.0`+
                  `Townsend deprivation index at recruitment.0.0`+
                  `Genetic principal components.0.1`+
                  `Genetic principal components.0.2`+
                  `Genetic principal components.0.3`+
                  `Genetic principal components.0.4`+
                  selected_vars[[model]]*selected_vars[[rf]],
                family=binomial(link="logit"))
multiplic_df <<- bind_rows(multiplic_df,summary(model1)$coefficients[10,c(1,2,4)])
}



get_multiplicative_int("HLA_PRS","cBMI")
get_multiplicative_int("NOHLA_PRS","cBMI")
get_multiplicative_int("HLA_PRS","young_smoker")
get_multiplicative_int("NOHLA_PRS","young_smoker")
get_multiplicative_int2("HLA_PRS","Age when periods started (menarche).0.0")
get_multiplicative_int2("NOHLA_PRS","Age when periods started (menarche).0.0")

multiplic_df$model = rep(c("MHC PRS","Non-MHC PRS"),3)
multiplic_df$exposure = c("Childhood body size","Childhood body size","Smoking","Smoking","Age at menarche","Age at menarche")
multiplic_df$interaction = paste0(multiplic_df$model," x ",multiplic_df$exposure)

write_csv(multiplic_df,"multiplicative_interaction.csv")


p=ggplot(multiplic_df,aes(Estimate,interaction,fill=model))+geom_vline(xintercept=0,alpha=0.5)+
geom_errorbarh(aes(xmin=Estimate-1.96*`Std. Error`,xmax=Estimate+1.96*`Std. Error`,y=interaction,height=0.3))+
geom_point(shape=22,size=3)+
scale_fill_brewer(palette="Set2")+
theme_classic()+
labs(fill="PRS type",x="Interation term beta")

png("multiplicative_interaction.png",width=8,height=8,units="in",res=300)
p
dev.off()




##############################
# additive interactions
##############################
library(doParallel)
registerDoParallel(cores=args[1])
trials=as.numeric(args[2])

overall_ap_df = data.frame()
df=selected_vars %>% select(`Age at recruitment.0.0`, MS_status,HLA_PRS,NOHLA_PRS,
                `Sex.0.0`,
                `Townsend deprivation index at recruitment.0.0`,
                `Genetic principal components.0.1`,
                `Genetic principal components.0.2`,
                `Genetic principal components.0.3`,
                `Genetic principal components.0.4`,
                cBMI,
                young_smoker,
                `Age when periods started (menarche).0.0`,
                DRB_15)

additive_int = function(rf,prs){
start=Sys.time()
r = foreach(icount(trials),.combine=cbind) %dopar% {
  df = sample_n(df, size=nrow(df), replace=TRUE)
  model = glm(data=df,
              MS_status~`Age at recruitment.0.0`+
                `Sex.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
                `Genetic principal components.0.1`+
                `Genetic principal components.0.2`+
                `Genetic principal components.0.3`+
                `Genetic principal components.0.4`+
                df[[rf]]*df[[prs]],
              family=binomial(link="logit"))
  coeffs=as.numeric(coef(model))
  RERI=exp(coeffs[9]+coeffs[10]+coeffs[11])-exp(coeffs[9])-exp(coeffs[10])+1
  AP=RERI/(exp(coeffs[9]+coeffs[10]+coeffs[11]))}
end=Sys.time()
print(paste0("elapsed time: ",difftime(end,start)))


model = glm(data=df,
            MS_status~`Age at recruitment.0.0`+
              `Sex.0.0`+
              `Townsend deprivation index at recruitment.0.0`+
              `Genetic principal components.0.1`+
              `Genetic principal components.0.2`+
              `Genetic principal components.0.3`+
              `Genetic principal components.0.4`+
              df[[rf]]*df[[prs]],
            family=binomial(link="logit"))
coeffs=as.numeric(coef(model))
RERI=exp(coeffs[9]+coeffs[10]+coeffs[11])-exp(coeffs[9])-exp(coeffs[10])+1
AP=RERI/(exp(coeffs[9]+coeffs[10]+coeffs[11]))

aps = c(AP,
quantile(r,c(0.025,0.975)),
above_0 = sum(r>0),
below_0 = sum(r<0))

if(AP>0) {
pval =(sum(r<0)+1)/(trials+1)
} else {
pval =(sum(r>0)+1)/(trials+1)
}

aps = c(aps,pval)
overall_ap_df <<- rbind(overall_ap_df,aps)
}




additive_int2 = function(rf,prs){
start=Sys.time()
r = foreach(icount(trials),.combine=cbind) %dopar% {
  df = sample_n(df, size=nrow(df), replace=TRUE)
  model = glm(data=df,
              MS_status~`Age at recruitment.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
                `Genetic principal components.0.1`+
                `Genetic principal components.0.2`+
                `Genetic principal components.0.3`+
                `Genetic principal components.0.4`+
                df[[rf]]*df[[prs]],
              family=binomial(link="logit"))
  coeffs=as.numeric(coef(model))
  RERI=exp(coeffs[8]+coeffs[9]+coeffs[10])-exp(coeffs[8])-exp(coeffs[9])+1
  AP=RERI/(exp(coeffs[8]+coeffs[9]+coeffs[10]))}
end=Sys.time()
print(paste0("elapsed time: ",difftime(end,start)))


model = glm(data=df,MS_status~`Age at recruitment.0.0`+
              `Townsend deprivation index at recruitment.0.0`+
              `Genetic principal components.0.1`+
              `Genetic principal components.0.2`+
              `Genetic principal components.0.3`+
              `Genetic principal components.0.4`+
              df[[rf]]*df[[prs]],
            family=binomial(link="logit"))
coeffs=as.numeric(coef(model))
RERI=exp(coeffs[8]+coeffs[9]+coeffs[10])-exp(coeffs[8])-exp(coeffs[9])+1
AP=RERI/(exp(coeffs[8]+coeffs[9]+coeffs[10]))

aps = c(AP,
quantile(r,c(0.025,0.975)),
above_0 = sum(r>0),
below_0 = sum(r<0))

if(AP>0) {
pval =(sum(r<0)+1)/(trials+1)
} else {
pval =(sum(r>0)+1)/(trials+1)
}

aps = c(aps,pval)
overall_ap_df <<- rbind(overall_ap_df,aps)
}


additive_int("cBMI","HLA_PRS")
additive_int("cBMI","NOHLA_PRS")
additive_int("young_smoker","HLA_PRS")
additive_int("young_smoker","NOHLA_PRS")
additive_int2("HLA_PRS","Age when periods started (menarche).0.0")
additive_int2("NOHLA_PRS","Age when periods started (menarche).0.0")

overall_ap_df = data.frame(overall_ap_df)
overall_ap_df$modelname = rep(c("MHC PRS","Non-MHC PRS"),3)
overall_ap_df$exposure = c("Childhood body size","Childhood body size","Smoking","Smoking","Age at menarche","Age at menarche")
overall_ap_df$interaction = paste0(overall_ap_df$model," x ",overall_ap_df$exposure)


colnames(overall_ap_df) = c("AP","lower_ci","upper_ci","above_0","below_0","pval","modelname","exposure","interaction")
overall_ap_df$two_sided_p = overall_ap_df$pval*2

write_csv(overall_ap_df,"overall_ap_df.csv")

# overall_ap_df = read_csv("overall_ap_df.csv")
p=ggplot(overall_ap_df,aes(AP,interaction,fill=modelname))+geom_vline(xintercept=0,alpha=0.5)+
geom_errorbarh(aes(xmin=lower_ci,xmax=upper_ci,y=interaction,height=0.3))+
geom_point(shape=22,size=3)+
scale_fill_brewer(palette="Set2")+
theme_classic()+
labs(fill="PRS type",x="AP due to interaction")

png("additive_interaction.png",width=8,height=8,units="in",res=300)
p
dev.off()





###########################################
#  G x G
###########################################
selected_vars$DRB_15 = as.numeric(as.character(selected_vars$DRB_15))
selected_vars = selected_vars %>% mutate(DRB_15 = ifelse(DRB_15==0,0,1))

print("printing no. of people with DRB 15")
table(selected_vars$DRB_15)

# multiplicative
model = glm(data=selected_vars,
              MS_status~`Age at recruitment.0.0`+
                `Sex.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
                `Genetic principal components.0.1`+
                `Genetic principal components.0.2`+
                `Genetic principal components.0.3`+
                `Genetic principal components.0.4`+
                DRB_15*NOHLA_PRS,
              family=binomial(link="logit"))


print("multiplicative interaction for dominant-coding DRB 1*15 x PRS")
print(summary(model))
print(summary(model)$coefficients[11,])



df=selected_vars %>% select(`Age at recruitment.0.0`, MS_status,HLA_PRS,NOHLA_PRS,
                `Sex.0.0`,
                `Townsend deprivation index at recruitment.0.0`,
                `Genetic principal components.0.1`,
                `Genetic principal components.0.2`,
                `Genetic principal components.0.3`,
                `Genetic principal components.0.4`,
                DRB_15)


r = foreach(icount(trials), .combine=cbind) %dopar% {
  df = sample_n(df, size=nrow(df), replace=TRUE)
  model = glm(data=df,
              MS_status~`Age at recruitment.0.0`+
                `Sex.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
                `Genetic principal components.0.1`+
                `Genetic principal components.0.2`+
                `Genetic principal components.0.3`+
                `Genetic principal components.0.4`+
                DRB_15*NOHLA_PRS,
              family=binomial(link="logit"))
  coeffs=as.numeric(coef(model))
  RERI=exp(coeffs[9]+coeffs[10]+coeffs[11])-exp(coeffs[9])-exp(coeffs[10])+1
  AP=RERI/(exp(coeffs[9]+coeffs[10]+coeffs[11]))}

model = glm(data=df,
            MS_status~`Age at recruitment.0.0`+
              `Sex.0.0`+
              `Townsend deprivation index at recruitment.0.0`+
              `Genetic principal components.0.1`+
              `Genetic principal components.0.2`+
              `Genetic principal components.0.3`+
              `Genetic principal components.0.4`+
              DRB_15*NOHLA_PRS,
            family=binomial(link="logit"))
coeffs=as.numeric(coef(model))
RERI=exp(coeffs[9]+coeffs[10]+coeffs[11])-exp(coeffs[9])-exp(coeffs[10])+1
AP=RERI/(exp(coeffs[9]+coeffs[10]+coeffs[11]))

aps = c(AP,
quantile(r,c(0.025,0.975)),
above_0 = sum(r>0),
below_0 = sum(r<0))

if(AP>0) {
pval =(sum(r<0)+1)/(trials+1)
} else {
pval =(sum(r>0)+1)/(trials+1)
}

aps = c(aps,pval,pval*2)
print("printing aps for DRB1*15 x PRS")
print(aps)



############################
# high vs low decile
############################
selected_vars$young_smoker = factor(selected_vars$young_smoker)
selected_vars$prs_quartile = cut2(selected_vars$NOHLA_PRS,g=10)
levels(selected_vars$prs_quartile)=c(1:10)


or_df = data.frame()
make_decile_models = function(x,rf){
model_df = selected_vars %>% filter(prs_quartile==as.character(x))
model <<- glm(data=model_df,
              MS_status~`Age at recruitment.0.0`+
                `Sex.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
                `Genetic principal components.0.1`+
                `Genetic principal components.0.2`+
                `Genetic principal components.0.3`+
                `Genetic principal components.0.4`+
                model_df[[rf]],
              family=binomial(link="logit"))
or=exp(summary(model)$coefficients[9,1])
lower_ci=exp(summary(model)$coefficients[9,1]-1.96*summary(model)$coefficients[9,2])
upper_ci=exp(summary(model)$coefficients[9,1]+1.96*summary(model)$coefficients[9,2])
or_df <<- rbind(or_df,data.frame(rf,x,or,lower_ci,upper_ci))
}

make_decile_models(1,"cBMI")
make_decile_models(10,"cBMI")
make_decile_models(1,"young_smoker")
make_decile_models(10,"young_smoker")
make_decile_models(1,"DRB_15")
make_decile_models(10,"DRB_15")

make_decile_models2 = function(x,rf){
model_df = selected_vars %>% filter(prs_quartile==as.character(x)) %>% filter(Sex.0.0=="Female")
model <<- glm(data=model_df,
              MS_status~`Age at recruitment.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
                `Genetic principal components.0.1`+
                `Genetic principal components.0.2`+
                `Genetic principal components.0.3`+
                `Genetic principal components.0.4`+
                model_df[[rf]],
              family=binomial(link="logit"))
or=exp(summary(model)$coefficients[8,1])
lower_ci=exp(summary(model)$coefficients[8,1]-1.96*summary(model)$coefficients[8,2])
upper_ci=exp(summary(model)$coefficients[8,1]+1.96*summary(model)$coefficients[8,2])
or_df <<- rbind(or_df,data.frame(rf,x,or,lower_ci,upper_ci))
}

make_decile_models2(1,"Age when periods started (menarche).0.0")
make_decile_models2(10,"Age when periods started (menarche).0.0")



colnames(or_df)=c("Risk factor","PRS quartile","OR","Lower CI","Upper CI")
or_df$PRS_half = recode(factor(or_df$`PRS quartile`),"1"="Bottom 10%","2"="Top 10%")

library(ggstance)
levels(or_df$`Risk factor`)=c("Childhood body size","Smoking prior to age 20","DRB 1*15:01 positivity","Age at menarche")

png("strat_plot.png",res=300,height=8,width=8,units="in")
ggplot(or_df,aes(OR,`Risk factor`,col=PRS_half))+geom_vline(xintercept=1,alpha=0.2)+geom_point(position=position_dodgev(height=0.5))+geom_errorbarh(aes(xmin=`Lower CI`,xmax=`Upper CI`,y=`Risk factor`),position=position_dodgev(height=0.5),height=0.1)+theme_classic()+labs(x="Odds Ratio for MS",y="Risk factor",col="PRS decile")
dev.off()


selected_vars = selected_vars %>% mutate(drb15_doses = as.numeric(as.character(drb15_doses)))

selected_vars = selected_vars %>% filter(drb15_doses < 0.7) %>% mutate(DRB_dose = 0) %>% bind_rows(selected_vars %>% filter(drb15_doses > 0.7 & drb15_doses <1.4) %>% mutate(DRB_dose = 1))%>% bind_rows(selected_vars %>% filter(drb15_doses >1.4) %>% mutate(DRB_dose = 2))


additive_hla_model = function(x){
model = glm(data=selected_vars %>% filter(prs_quartile==as.character(x)),
              MS_status~`Age at recruitment.0.0`+
                `Sex.0.0`+
                `Townsend deprivation index at recruitment.0.0`+
                `Genetic principal components.0.1`+
                `Genetic principal components.0.2`+
                `Genetic principal components.0.3`+
                `Genetic principal components.0.4`+
                DRB_dose,
              family=binomial(link="logit"))
print(paste0("Beta ",
round(summary(model)$coefficients[9,1],3),
" 95% CI ",
round(summary(model)$coefficients[9,1]-1.96*summary(model)$coefficients[9,2],3),
" - ",
round(summary(model)$coefficients[9,1]+1.96*summary(model)$coefficients[9,2],3),
")"))
}

print("highest PRS decile")
additive_hla_model(10)
print("lowest PRS decile")
additive_hla_model(1)


######################################
# MRI
######################################

mri_data = read_table2("mri_data.tsv")
ms_mri = selected_vars %>% filter(EID %in% mri_data$EID) %>% filter(MS_status==1)
ms_mri = ms_mri %>% left_join(mri_data,by="EID") %>% filter(!is.na(wm_lesion_vol))
ms_mri$norm_wm_lesions = rankNorm(ms_mri$wm_lesion_vol)

model = glm(data=ms_mri,norm_wm_lesions~`Age at recruitment.0.0`+Sex.0.0+`Genetic principal components.0.1`+`Genetic principal components.0.2`+`Genetic principal components.0.3`+`Genetic principal components.0.4`+csf_vol_normalised_headvol+grey_and_white_vol_normalised_headvol+HLA_PRS)
summary(model)

model = glm(data=ms_mri,norm_wm_lesions~`Age at recruitment.0.0`+ Sex.0.0+`Genetic principal components.0.1`+`Genetic principal components.0.2`+`Genetic principal components.0.3`+`Genetic principal components.0.4`+csf_vol_normalised_headvol+grey_and_white_vol_normalised_headvol+NOHLA_PRS)
summary(model)

mhc = ggplot(ms_mri,aes(HLA_PRS,norm_wm_lesions))+geom_point()+theme_classic()+
labs(x="MHC PRS",y="Normalised WM T2/FLAIR lesion load")
nomhc = ggplot(ms_mri,aes(NOHLA_PRS,norm_wm_lesions))+geom_point()+theme_classic()+
labs(x="Non-MHC PRS",y="Normalised WM T2/FLAIR lesion load")

png("mri_prs_plot.png",res=300,units="in",height=8,width=8)
grid.arrange(mhc,nomhc,nrow=1)
dev.off()

# repeat with controls
mri_data = read_table2("mri_data.tsv")
overall_mri = selected_vars %>% filter(EID %in% mri_data$EID)
overall_mri = overall_mri %>% left_join(mri_data,by="EID") %>% filter(!is.na(wm_lesion_vol))
overall_mri$norm_wm_lesions = rankNorm(overall_mri$wm_lesion_vol)


mhc = ggplot(overall_mri,aes(HLA_PRS,norm_wm_lesions,col=factor(MS_status)))+geom_point(alpha=0.5)+theme_classic()+
labs(x="MHC PRS",y="Normalised WM T2/FLAIR lesion load",col="MS status")+scale_color_discrete(labels=c("Controls","MS cases"))
nomhc = ggplot(overall_mri,aes(NOHLA_PRS,norm_wm_lesions,col=factor(MS_status)))+geom_point(alpha=0.5)+theme_classic()+
labs(x="Non-MHC PRS",y="Normalised WM T2/FLAIR lesion load",col="MS status")+scale_color_discrete(labels=c("Controls","MS cases"))

png("healthy_indivs_mri_prs_plot.png",res=300,units="in",height=8,width=8)
grid.arrange(mhc,nomhc,nrow=1)
dev.off()

model = glm(data=overall_mri,norm_wm_lesions~`Age at recruitment.0.0`+Sex.0.0+`Genetic principal components.0.1`+`Genetic principal components.0.2`+`Genetic principal components.0.3`+`Genetic principal components.0.4`+csf_vol_normalised_headvol+grey_and_white_vol_normalised_headvol+HLA_PRS)
summary(model)

model = glm(data=overall_mri,norm_wm_lesions~`Age at recruitment.0.0`+ Sex.0.0+`Genetic principal components.0.1`+`Genetic principal components.0.2`+`Genetic principal components.0.3`+`Genetic principal components.0.4`+csf_vol_normalised_headvol+grey_and_white_vol_normalised_headvol+NOHLA_PRS)
summary(model)





######################################
# Disability
######################################

selected_vars$`Attendance/disability/mobility allowance.0.0` %>% table()

disability_df = selected_vars %>% filter(!`Attendance/disability/mobility allowance.0.0`=="Do not know") %>% filter(!`Attendance/disability/mobility allowance.0.0`=="Prefer not to answer")

disability_df = disability_df %>% mutate(disability = ifelse(`Attendance/disability/mobility allowance.0.0`=="Attendance allowance" | `Attendance/disability/mobility allowance.0.0`=="Disability living allowance" | `Attendance/disability/mobility allowance.0.0`=="Blue badge",1,0))

disability_df = disability_df %>% filter(MS_status==1)

mhc=ggplot(disability_df,aes(factor(disability),HLA_PRS,fill=factor(disability)))+geom_violin(alpha=0.5)+theme_classic()+
labs(x="Attendance/disability/mobility allowance claimed",y="MHC PRS")+
scale_x_discrete(labels=c("No","Yes"))+theme(legend.position="none")
nomhc=ggplot(disability_df,aes(factor(disability),NOHLA_PRS,fill=factor(disability)))+geom_violin(alpha=0.5)+theme_classic()+
labs(x="Attendance/disability/mobility allowance claimed",y="Non-MHC PRS")+
scale_x_discrete(labels=c("No","Yes"))+theme(legend.position="none")

png("prs_disability.png",res=300,units="in",height=8,width=8)
grid.arrange(mhc,nomhc,nrow=1)
dev.off()

model = glm(data=disability_df,disability~`Age at recruitment.0.0`+ Sex.0.0+`Genetic principal components.0.1`+`Genetic principal components.0.2`+`Genetic principal components.0.3`+`Genetic principal components.0.4`+NOHLA_PRS,family=binomial(link="logit"))
summary(model)

model = glm(data=disability_df,disability~`Age at recruitment.0.0`+ Sex.0.0+`Genetic principal components.0.1`+`Genetic principal components.0.2`+`Genetic principal components.0.3`+`Genetic principal components.0.4`+HLA_PRS,family=binomial(link="logit"))
summary(model)

# remove autoimmune disease
no_ai_selected_vars = selected_vars %>% filter(IBD_status == 0 & RA_status ==0 & SJOG_status==0 & SLE_status==0 & PBC_status==0 & ank_spond_status==0)


null_model = glm(data=no_ai_selected_vars,
                   MS_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`,
                   family=binomial(link="logit"))

hlaprs_model = glm(data=no_ai_selected_vars,
                  MS_status~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    HLA_PRS,
                  family=binomial(link="logit"))

nohla_prs_model = glm(data=no_ai_selected_vars,
                  MS_status~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    NOHLA_PRS,
                  family=binomial(link="logit"))

print("printing nagelkerke for best PRS in testing set")
print(nagelkerke(hlaprs_model,null_model))
print("printing nagelkerke for best non-HLA PRS in testing set")
print(nagelkerke(nohla_prs_model,null_model))


# specificity
ms_null_model = glm(data=selected_vars,
                   MS_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`,
                   family=binomial(link="logit"))

mshlaprs_model = glm(data=selected_vars,
                  MS_status~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    HLA_PRS,
                  family=binomial(link="logit"))

ra_null_model = glm(data=selected_vars,
                   RA_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`,
                   family=binomial(link="logit"))

rahlaprs_model = glm(data=selected_vars,
                   RA_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`+HLA_PRS,
                   family=binomial(link="logit"))

ibd_null_model = glm(data=selected_vars,
                   IBD_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`,
                   family=binomial(link="logit"))

ibdhlaprs_model = glm(data=selected_vars,
                  IBD_status~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    HLA_PRS,
                  family=binomial(link="logit"))

as_null_model = glm(data=selected_vars,
                   ank_spond_status~`Age at recruitment.0.0`+
                     `Sex.0.0`+
                     `Townsend deprivation index at recruitment.0.0`+
                     `Genetic principal components.0.1`+
                     `Genetic principal components.0.2`+
                     `Genetic principal components.0.3`+
                     `Genetic principal components.0.4`,
                   family=binomial(link="logit"))

ashlaprs_model = glm(data=selected_vars,
                  ank_spond_status~`Age at recruitment.0.0`+
                    `Sex.0.0`+
                    `Townsend deprivation index at recruitment.0.0`+
                    `Genetic principal components.0.1`+
                    `Genetic principal components.0.2`+
                    `Genetic principal components.0.3`+
                    `Genetic principal components.0.4`+
                    HLA_PRS,
                  family=binomial(link="logit"))

df = data.frame("Outcome"=c("MS","RA","IBD","Ank Spond"),"Nagelkerke"=c(nagelkerke(mshlaprs_model,ms_null_model)$Pseudo.R.squared.for.model.vs.null[3],
nagelkerke(rahlaprs_model,ra_null_model)$Pseudo.R.squared.for.model.vs.null[3],
nagelkerke(ibdhlaprs_model,ibd_null_model)$Pseudo.R.squared.for.model.vs.null[3],
nagelkerke(ashlaprs_model,as_null_model)$Pseudo.R.squared.for.model.vs.null[3]))

ggplot(df,aes(Outcome,Nagelkerke))+geom_col()
