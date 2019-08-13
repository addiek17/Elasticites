### CPI cliffs

### Install necessary packages ###
wants <- c("tidyr", "lubridate", "tibble", "data.table","knitr", "caret","boot", "ggplot2",
           "gridExtra", "pander", "ggthemes", "scales", "foreign", "magrittr", "reshape2",
           "glmnet", 'mgcv', 'cvTools', 'rpart', 'class', 'psych', 'stringr','sqldf',
           'rpart.plot', 'randomForest', 'corrplot', 'bit64', "dplyr","readxl",
           'ngram','stringi','stringdist','fuzzyjoin','mgsub','tidytext','plotly','RODBC','tidyverse')
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)
library(nlme)
library(r2glmm)
library(lsmeans)
library(emmeans)
library(lme4)

options(scipen=999)
setwd('\\\\cs01corp/root/Files/Corp/MKT/950240/PUB-DB/Pricing Strategy/Addie/CPI cliffs')
#save.image(file = "str_onln_data.RData")
load('str_onln_data.RData')

str_onln=final
#names(str_onln)

#classes=str_onln%>%select(sku_id,class_id, product_group, fisc_wk_of_mth_id, unit_lift, cpi, chn_id)%>%group_by(class_id)%>%summarise(cpi=sum(cpi))
#class=str_onln%>%filter(class_id==161)

useful=str_onln%>%select(sku_id,class_id, product_group, fisc_wk_of_mth_id, unit_lift, cpi, chn_id)

max(useful$cpi)
min(useful$cpi)


useful=useful%>%mutate(cpi=round(cpi*100,0))%>%filter(cpi <= 100)

useful0=useful%>%mutate(cpi_bin=as.factor(useful$cpi))



##################################
##                              ##
##        Regression            ##
##                              ##
##################################

useful1=useful0

### removing observations less than 40 by chn, class, product group
x=useful1%>%group_by(chn_id,class_id,product_group)%>%dplyr::summarize(count=n())
#sku_count=useful1%>%group_by(sku_id,class_id)%>%dplyr::summarize(sku_count=n())

#sku_data=useful1%>%group_by(sku_id,chn_id)%>%dplyr::summarize(count_sku=n())

useful2=merge(useful1,x)
#useful1=merge(useful1,sku_data)

useful3=useful2%>%filter(count>=30)#%>%select(sku_id,class_id,cpi,cpi_bin,unit_lift,product_group,chn_id,fisc_wk_of_mth_id)

### here checking if all product combinations have more than one distinct cpi
bin_check=useful3%>%distinct(class_id,chn_id,product_group,cpi_bin)%>%
  group_by(class_id,chn_id,product_group)%>%
  dplyr::summarize(count=n())%>%
  filter(count>1)%>%select(-count)
#useful3=useful2%>%distinct(class_id,cpi_bin)%>%group_by(class_id)%>%dplyr::summarize(count=n())%>%filter(count>1)

#vector_x=as.vector(useful3$class_id)

useful4=merge(useful3,bin_check)

#useful4=useful2%>%filter(class_id %in% vector_x)

#y=useful4%>%distinct(class_id,product_group)%>%group_by(class_id)%>%dplyr::summarize(count=n())%>%filter(count>1)

#vector_y=as.vector(y$class_id)

#useful5=useful4%>%filter(class_id %in% vector_y)

#useful3=useful2%>%group_by(class_id,cpi_bin)
#useful_others=useful2%>%filter(!class_id %in% unique(x$class_id))

library(emmeans)

useful5=useful4%>%mutate(class_chn_segment=paste0(class_id,"_",chn_id,"_",product_group))

#z=useful5%>%group_by(class_chn_segment)%>%dplyr::summarize(count=length(unique(cpi_bin)))%>%filter(count>1)
#vector_z=as.vector(z$class_chn_segment)

useful6=useful5#%>%filter(class_chn_segment %in% vector_z)

groups=unique(useful6$class_chn_segment)
ls_means=data.frame()
for (i in groups){
  print(paste0("observation # ",match(c(i),groups)," out of ",length(groups)))
  group=useful6%>%filter(class_chn_segment==i)
  m=lm(unit_lift~cpi_bin,data=group)
  
  adjusted_means = emmeans(m, ~ cpi_bin)
  tidy_results=summary(adjusted_means)
  #getElement(tidy_results,"product_group")
  #getElement(tidy_results,"class_id")
  
  tidy_results$product_group=unique(group$product_group)
  tidy_results$class_id=unique(group$class_id)
  tidy_results$chn_id=unique(group$chn_id)
  tidy_results$observations=nrow(group)
  #tidy_results$class_chn_segment=unique(group$class_chn_segment)
  
  ls_means=rbind(ls_means,tidy_results)
}
ls_means=ls_means%>%mutate(class_chn_segment=paste0(class_id,"_",chn_id,"_",product_group))

#a=as.vector(ls_means$class_chn_segment)

#final=useful6%>%filter(class_chn_segment %in% a)

write.csv(ls_means,'elasticity_v2.csv')