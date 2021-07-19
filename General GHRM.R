# Analyze your questionnaire.
# By Dr. Mohammed Alnahhal.  mjnahhal@yahoo.com


# read data from Excel
library(readxl)
data <- read_excel("raw data.xlsx")


data=as.data.frame(data)

#input parameters

# number of initial questions (such as gender and experience)
IQ=5

# number of main questions
MQ=21

# Average Likert Scale to compare with (for one sample t-test). The dimension is satisfactory applied if average is more than 3.5
ALS=3.5

# Low performance. For example low performance is recognized if the values is less than 3
low_p=3
################################################
# cronbach.alpha and t-test
library( ltm)

# the data columns contain four constructs (each one contains several dimensions which are the heads on the columns):
# Here we have 21 questions grouped in 4 constructs with the given columns headings
GHRM=data[,c("GHRM 1","GHRM 2","GHRM 3","GHRM 4","GHRM 5","GHRM 6")]
ENP=data[,c("ENP 1","ENP 2","ENP 3","ENP 4","ENP 5")]
EP=data[,c("EP 1","EP 2","EP 3","EP 4","EP 5")]
SP=data[,c("SP 1","SP 2","SP 3","SP 4","SP 5")]


#cronbach.alpha
print(cronbach.alpha(GHRM))
print(cronbach.alpha(ENP))
print(cronbach.alpha(EP))
print(cronbach.alpha(SP))
#############################################
# initial estimation by finding low performance percentage
low_performance=matrix(0,nrow=MQ, ncol=1) 
row.names(low_performance)=colnames(data)[(IQ+1):(MQ+IQ)]


for (ii in 1:MQ)
{
  data2=data[,IQ+ii]
  data2=data2[complete.cases(data2)] # to test if there is missing data or not
  
  low_performance[ii]= sum(data2<low_p)/length(data2)*100 # percent of low performance
}

# percentage of low performance
low_performance_final=cbind(rownames(low_performance),as.data.frame(low_performance))

library("writexl")
write_xlsx(low_performance_final,"low_performance_final.xlsx")

###########################################

# t-test for each item (column)
t_testdata=matrix(0,nrow=MQ, ncol=1) 
row.names(t_testdata)=colnames(data)[(IQ+1):(MQ+IQ)]
mu0=ALS

for (ii in 1:MQ)
{
  data2=data[,IQ+ii]
  data2=data2[complete.cases(data2)] # to test if there is missing data or not
 t_testdata[ii]=t.test(data2, mu=mu0, alternative="greater")$p.value 
}

one_sample_t=cbind(rownames(t_testdata),as.data.frame(t_testdata))

write_xlsx(one_sample_t,"one_sample_t.xlsx")


# t-test for a group of columns
GHRM_group=rowMeans(GHRM)
t_GHRM=t.test(GHRM_group, mu=mu0, alternative="greater")$p.value

ENP_group=rowMeans(ENP)
t_ENP=t.test(ENP_group, mu=mu0, alternative="greater")$p.value

EP_group=rowMeans(EP)
t_EP=t.test(EP_group, mu=mu0, alternative="greater")$p.value

SP_group=rowMeans(SP)
t_SP=t.test(SP_group, mu=mu0, alternative="greater")$p.value

#print the p-values for groups one t-test
print(c(t_GHRM,t_ENP,t_EP,t_SP))

################################


#####################
# One sample t-test for each item for particular respondents

### Males: They are written in the excel file as 1 for males and 2 for females
data_Males=data[data$Gender==1,]


t_testdata_Males=matrix(0,nrow=MQ, ncol=1) 
row.names(t_testdata_Males)=colnames(data)[(IQ+1):(MQ+IQ)]
mu0=ALS

for (ii in 1:MQ)
{
  data2=data_Males[,IQ+ii]
  data2=data2[complete.cases(data2)]
  t_testdata_Males[ii]=t.test(data2, mu=mu0, alternative="greater")$p.value 
}

# print one sample t-test for males
print(t_testdata_Males)

### Females
data_Female=data[data$Gender==2,]

t_testdata_Female=matrix(0,nrow=MQ, ncol=1) 
row.names(t_testdata_Female)=colnames(data)[(IQ+1):(MQ+IQ)]
mu0=ALS

for (ii in 1:MQ)
{
  data2=data_Female[,IQ+ii]
  data2=data2[complete.cases(data2)]
  t_testdata_Female[ii]=t.test(data2, mu=mu0, alternative="greater")$p.value 
}

# print one sample t-test for females
print(t_testdata_Female)



#####################
# Two Sample t-test. Test the effect of gender


t_testdata_two_sample=matrix(0,nrow=MQ, ncol=1) 
row.names(t_testdata_two_sample)=colnames(data)[(IQ+1):(MQ+IQ)]


for (ii in 1:MQ)
{
  data2=data_Males[,IQ+ii]
  data2=data2[complete.cases(data2)]
  
  data3=data_Female[,IQ+ii]
  data3=data3[complete.cases(data3)]
  
  t_testdata_two_sample[ii]=t.test(data2, data3)$p.value 
}

Two_sample_t=cbind(rownames(t_testdata_two_sample),as.data.frame(t_testdata_two_sample))

write_xlsx(Two_sample_t,"Two_sample_t.xlsx")
######################

#####################################################
#compare averages
Avg_data_Males=t(t(colMeans(data_Males[(IQ+1):(MQ+IQ)],na.rm = T)))
                      
Avg_data_resident=t(t(colMeans(data_Female[(IQ+1):(MQ+IQ)],na.rm = T)))
####################################################


###########
#ANOVA to test the effect of experience: `Years of experience`
ANOVA_results=t_testdata_two_sample # just to define an array
 

for (ii in (IQ+1):(MQ+IQ))
{
 res.aov1 <-aov(data[,ii] ~ `Years of experience`, 
               data = data) 
ANOVA_results[ii-IQ]=summary(res.aov1)[[1]][["Pr(>F)"]][1] 
}



write_xlsx(ANOVA_Final,"ANOVA_Final.xlsx")


