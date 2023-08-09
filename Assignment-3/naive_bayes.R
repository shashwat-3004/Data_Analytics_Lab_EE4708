library(tidyverse)

data<-read_csv("adult.csv",col_names =FALSE,na="?")

colnames(data)<-c("age","workclass","fnlgwt","education","education_num",
                  "marital_status","Occupation","relationship","race","sex",
                  "capital_gain","capital_loss","hours_per_week","native_country","Income")

ggplot(data)+geom_boxplot(aes(x=Income,y=fnlgwt))+
  labs(title="Boxplot of fnlgwt on the basis of income")+
  my_theme


# "?" is NA



na_check<-function(dataset){
  sapply(dataset,function(x) sum(is.na(x)))
}

na_check(data)
str(data)
summary(data)

data<-data%>% mutate_if(is.character,as.factor)

# fnlwgt, education_num, relationship column remove

data<-data%>%select(-c(fnlgwt,education_num,relationship))

library(VIM)

data_tidy<-kNN(data,variable = c("workclass","Occupation","native_country"),k=sqrt(nrow(data)))

data_tidy<-data_tidy[,1:12]


# % of people earning more or less than 50k
(table(data_tidy$Income)/nrow(data_tidy))*100

# capital_gain and capital_loss data not used becuase of high number of zeroes. 
#the number of zeros in these variables is more than 90%.
#This means that less than 10% of the participants in the survey make any investments
#2nd model uses this



# Visualization

library(ggthemes)

my_theme<-theme_fivethirtyeight()+theme(plot.title = element_text(hjust = 0.5,size=20),
                                        axis.title = element_text(size=20),
                                        axis.text = element_text(size=14),
                                        plot.subtitle = element_text(hjust=0.5),
                                        legend.position = "top",legend.title = element_text(size=15),
                                        legend.text = element_text(size=15))



# fig 1

ggplot(data_tidy)+geom_histogram(aes(age,fill=Income),binwidth=1,col="black")+
  labs(x="Age",y="Count",title = "Income w.r.t to Age")+my_theme+scale_fill_brewer(palette = "Set1")


# fig 2

ggplot(data_tidy)+geom_bar(aes(sex,fill=Income),col="black",position = "dodge")+
  labs(x="Gender",y="No. of people",title = "Income grouped by gender")+
  my_theme+scale_fill_brewer(palette = "Set1")

data_tidy%>%group_by(Income)%>%summarise(sum(sex=="Male"))

6662/sum(data_tidy$sex=="Male")  

data_tidy%>%group_by(Income)%>%summarise(sum(sex=="Female"))

1179/sum(data_tidy$sex=="Female")


# fig 3

data_tidy<-data_tidy%>%mutate(workclass_processed=case_when(
                                            workclass=="State-gov" ~ "Government",
                                            workclass=="Self-emp-not-inc" ~ "Self-emp",
                                            workclass=="Private" ~ "Private",
                                            workclass=="Federal-gov" ~ "Government",
                                            workclass=="Local-gov" ~ "Government",
                                            workclass=="Self-emp-inc" ~ "Self-emp",
                                            workclass=="Without-pay" ~ "Others",
                                            workclass=="Never-worked"~"Others"
))

data_tidy%>%group_by(workclass_processed)%>%summarise(n())

data_tidy%>%group_by(workclass_processed)%>%filter(Income==">50K")%>%summarise(n())


data_tidy%>%ggplot()+geom_bar(aes(workclass_processed,fill=Income),position = "dodge")+
  my_theme+labs(x="Work-Class",y="No. of People",title="Income w.r.t Industry")+
  scale_fill_brewer(palette = "Set1")

# fig 4

data_tidy<-data_tidy%>%mutate(education_processed=case_when(
                                              education=="Bachelors" ~ "Bachelors",
                                              education=="HS-grad" ~ "HS-grad",
                                              education=="11th" ~ "9th-12th",
                                              education=="Masters" ~ "Masters",
                                              education=="9th" ~ "9th-12th",
                                              education=="Some-college" ~ "College",
                                              education=="Assoc-acdm" ~ "Assoc",
                                              education=="Assoc-voc" ~ "Assoc",
                                              education=="7th-8th" ~ "5th-8th",
                                              education=="Doctorate" ~ "Doctorate",
                                              education=="Prof-school" ~ "Prof-school",
                                              education=="5th-6th" ~ "5th-8th",
                                              education=="10th" ~ "9th-12th",
                                              education=="1st-4th" ~ "1st-4th",
                                              education=="Preschool" ~ "Preschool",
                                              education=="12th" ~ "9th-12th"
  
))

ggplot(data_tidy)+geom_bar(aes(education_processed,fill=Income),col="black",position = "dodge")+
  theme_fivethirtyeight()+theme(plot.title = element_text(hjust = 0.5,size=20),
                                axis.title = element_text(size=20),
                                axis.text = element_text(size=14),
                                plot.subtitle = element_text(hjust=0.5),
                                legend.position = "top",legend.title = element_text(size=15),
                                legend.text = element_text(size=15),
                                axis.text.x = element_text(size=12,angle=45,hjust=0.5))+
  labs(x="Education",y="No. of people",title = "Income on the basis of education")+
  scale_fill_brewer(palette = "Set1")
 

# fig 5

marital_data<-data_tidy%>%group_by(marital_status,Income)%>%summarise(count=n())




marital_data<-marital_data%>%arrange((Income))

# Fig 6

data_tidy<-data_tidy%>%mutate(occupation_processed=case_when(
                                                     Occupation=="Adm-clerical" ~ "White-collar",
                                                     Occupation=="Exec-managerial" ~ "White-collar",
                                                     Occupation=="Handlers-cleaners" ~ "Blue-collar",
                                                     Occupation=="Prof-specialty" ~ "Professional",
                                                     Occupation=="Other-service" ~ "Service",
                                                     Occupation=="Sales" ~ "Sales",
                                                     Occupation=="Craft-repair" ~ "Blue-collar",
                                                     Occupation=="Transport-moving" ~ "Blue-collar",
                                                     Occupation=="Farming-fishing" ~ "Blue-collar",
                                                     Occupation=="Machine-op-inspct" ~ "Blue-collar",
                                                     Occupation=="Tech-support" ~ "Service",
                                                     Occupation=="Protective-serv" ~ "Service",
                                                     Occupation=="Armed-Forces" ~ "Service",
                                                     Occupation=="Priv-house-serv" ~ "Service"
                                                  
  
))

ggplot(data_tidy)+geom_bar(aes(occupation_processed,fill=Income),col="black", position = "dodge")+
  labs(x="Occupation",y="No. of People",title = "Income w.r.t Occupation")+my_theme+
  scale_fill_brewer(palette = "Set1")

# Fig 7

data_tidy<-data_tidy%>%mutate(race=case_when(race=="Asian-Pac-Islander" ~ "Asian-Pacific",
                                             race=="Amer-Indian-Eskimo" ~ "Amer-Indian",
                                             race=="White" ~ "White",
                                             race=="Black" ~ "Black",
                                             race=="Other" ~ "Other"))

ggplot(data_tidy)+geom_bar(aes(race,fill=Income),col="black", position = "dodge")+
  labs(x="Race",y="No. of People",title = "Income w.r.t race")+my_theme+
  scale_fill_brewer(palette = "Set1")

###########################################


(sum(data_tidy$capital_gain==0)/nrow(data_tidy))*100
(sum(data_tidy$capital_loss==0)/nrow(data_tidy))*100

p1<-ggplot(data_tidy,aes(x=capital_gain, group=Income, fill=Income)) + 
  geom_histogram(bins=10, color='black') + ggtitle('Histogram of Capital Gain')+
  labs(x="Capital Gain")+my_theme

p2<-ggplot(data_tidy,aes(x=capital_loss, group=Income, fill=Income)) + 
  geom_histogram(bins=10, color='black') + ggtitle('Histogram of Capital Loss')+
  labs(x="Capital loss")+my_theme

ggplot(data_tidy, aes(x=native_country)) + ggtitle("Native Country") + xlab("Native Country") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip()


# Fig 8

data_tidy<-data_tidy%>% mutate(hours_per_week_processed=case_when(
                                         hours_per_week<20~"0-20",
                                         hours_per_week>=20 & hours_per_week<=40 ~ "20-40",
                                         hours_per_week>40 & hours_per_week<=60 ~ "40-60",
                                         hours_per_week>60 ~ "60+"))


ggplot(data_tidy)+geom_bar(aes(hours_per_week_processed,fill=Income),col="black",position = "dodge")+
  labs(x="Hours per week",y="No. of People",title = "Income w.r.t hours")+my_theme+
  scale_fill_brewer(palette = "Set1")


#### Model

# not using capital gain/loss, native_country

data_model_1<-data_tidy[,c(1:5,7,10,12)]

library(e1071)
library(caret)

set.seed(1)

classifier<-naiveBayes(Income~.,data=data_model_1)

predicted_data<-predict(classifier,newdata=data_model_1)


conf_mat<-confusionMatrix(data_model_1$Income, predicted_data)

cm_dataframe<-as.data.frame(conf_mat$table)

library(scales)
ggplot(data =cm_dataframe ,
       aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = log(Freq)), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(x = Reference, y = Prediction, label = Freq),size=6) +
  labs(x="True Class",y="Predicted Class")+
  ggtitle(paste("Accuracy:",percent_format()(conf_mat$overall[1])))+
  theme_fivethirtyeight()+
  theme(legend.position = "none",
        axis.title = element_text(size=20),
        axis.text = element_text(size=14),
        plot.title = element_text(hjust=0.5,size=20))

library(ggthemes)
















