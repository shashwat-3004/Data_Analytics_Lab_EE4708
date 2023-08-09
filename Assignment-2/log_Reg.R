library(tidyverse)

train<-read_csv("train.csv")
test<-read_csv("test.csv")

na_check<-function(dataset){
  sapply(dataset,function(x) sum(is.na(x)))
}
na_check(train)
na_check(test)

full_data<-full_join(train,test)
na_check(full_data)
str(full_data)

boxplot(full_data$Age)
hist(full_data$Age)

#Impute 1 Fare mssing data 
full_data%>%filter(is.na(Fare))

# Embarked from "S" and and was in lower passenger class
fare_values_southampton<-full_data%>%filter(Embarked=="S" & Pclass==3)%>%.$Fare

# Due to presence of outliers, mean would overshoot the data so median data is used to impute the value
full_data[is.na(full_data$Fare),]$Fare=median(fare_values_southampton,na.rm = TRUE)

# Percentage of cabin data missing
(sum(is.na(full_data$Cabin))/nrow(full_data))*100

# Removing Cabin column

full_data<-full_data%>%select(-Cabin)

# Imputing Age

summary(full_data)
# Due to outliers in Age, mean is not good for imputing values, median age is used for filling missing data.

full_data[is.na(full_data$Age),]$Age<-median(full_data$Age,na.rm = TRUE)

# Imputing mssing embark data

# Both person had same fare, we can say that they boarded from the same place and both were in 1st class
full_data%>%filter(is.na(Embarked))

# Finding mean of the fare depending on embarkment and passenger class
full_data%>%group_by(Embarked,Pclass)%>%summarise(Fares=mean(Fare))

# Southampton seems to be the best choice for imputing as average fare is closest

full_data[is.na(full_data$Embarked),]$Embarked="S"

# Factoring

full_data$Pclass<-as.factor(full_data$Pclass)
full_data$Sex<-as.factor(full_data$Sex)
full_data$Embarked<-as.factor(full_data$Embarked)
full_data$Survived<-as.factor(full_data$Survived)
str(full_data)

# Removing ticket column

full_data$family_size<-full_data$Parch+full_data$SibSp

for(i in 1:nrow(full_data)){
  full_data$Alone[i]<-ifelse(full_data$N_Per_Ticket[i]==1,1,0)
}

# Insight
fare_zero<-full_data%>%filter(Fare==0)

# Every person is male, no family boarded and everyone embarked from Southampton

# max_fare

max_fare<-full_data%>%filter(Fare_Per_Person==max(Fare_Per_Person))
# Eveyone embarked from Cherboug, max fare is 512.3292, and one son-mother pair is included in it.

# Ordering by ticket
full_data<-full_data%>%arrange(Ticket)



# Age Group

full_data <- full_data %>% 
  mutate(Age_Group = case_when(Age < 16 ~ "0-15",
                               Age >= 16 & Age < 20 ~ "16-19",
                               Age >= 20 & Age < 24 ~ "20-23",
                               Age >= 24 & Age < 30 ~ "24-29",
                               Age >= 30 & Age < 40 ~ "30-39",
                               Age >= 40 & Age < 55 ~ "40-54",
                               Age >= 55 ~ "55+"))



# No. of person per ticket
n_per_ticket <- full_data %>% 
  group_by(Ticket) %>% 
  count() 

full_data<- merge(x=n_per_ticket, y=full_data, 
                  by.x="Ticket", by.y="Ticket",
                  all.x=TRUE, all.y=TRUE)
colnames(full_data)[colnames(full_data) == "n"] <- "N_Per_Ticket"

#There are many cases of families with the same last name (such as the Taussig above) which 
#leads us to believe that these are not individual prices but group prices under the same ticket. 
#So we keep Ticket only to clean fare

# People travelled on same ticket, we had assumed that fare was that of single person but people travelled 
# in groups, it is reasonable to think that fare was of full group.

full_data$Fare_Per_Person <-  full_data$Fare/full_data$N_Per_Ticket


# Visualizations
library(ggthemes)

my_theme<-theme_fivethirtyeight()+theme(plot.title = element_text(hjust = 0.5,size=20),
                                        axis.title = element_text(size=20),
                                        axis.text = element_text(size=14),
                                        plot.subtitle = element_text(hjust=0.5),
                                        legend.position = "top",legend.title = element_text(size=15),
                                        legend.text = element_text(size=15))




full_data%>%filter(!is.na(Survived))%>%ggplot()+geom_bar(aes(Sex,fill=Survived),position = "dodge",width=0.7)+
  my_theme+ labs(x="Gender",y="No. of people",title="Survivability based on gender")+
  scale_y_continuous(breaks = seq(0,500,100),limits = c(0,500))+ scale_fill_brewer(palette = 'Set1')

survive_gender<-full_data%>%group_by(Sex)%>%filter(Survived==1)%>%summarise(n())
total_male<-sum(train$Sex=="male")
total_female<-sum(train$Sex=="female")
# Female_percentgae_survived- approx 74%, male survived around 19%
# Write percentage of male survived and female survived


full_data%>%filter(!is.na(Survived))%>%ggplot()+geom_bar(aes(Pclass,fill=Survived),position = "dodge",
                                                         width=0.7)+my_theme+
  labs(x="Passenger Class",y="No of people",title="Survivability based on passenger class")+
  scale_y_continuous(breaks = seq(0,400,100),limits = c(0,400))+ scale_fill_brewer(palette = 'Set1')

full_data%>%filter(!is.na(Survived))%>%ggplot()+geom_bar(aes(Pclass,fill=Survived),position = "dodge",
                                                         width=0.7)+my_theme+
  labs(x="Passenger Class",y="No of people",title="Survivability based on passenger class(Gender-wise)")+
  facet_wrap(Sex~.)+ scale_fill_brewer(palette = 'Set1')

full_data%>%filter(!is.na(Survived))%>%ggplot()+geom_bar(aes(family_size,fill=Survived),position = "dodge",
                                                         width=0.7)+my_theme+
  labs(x="Family Size",y="No of people",title="Survivability based on family size")+
  scale_x_continuous(breaks = seq(0,10,2))+ scale_fill_brewer(palette = 'Set1')


full_data%>%filter(!is.na(Survived))%>%ggplot()+geom_bar(aes(Age_Group,fill=Survived),position = "dodge",
                                                         width=0.7)+my_theme+
  labs(x="Age Group",y="No of People",title="Survivability based on Age")+scale_fill_brewer(palette = 'Set1')
  
full_data%>%filter(!is.na(Survived))%>%ggplot()+geom_bar(aes(Embarked,fill=Survived),position = "dodge",
                                                         width=0.7)+my_theme+
  labs(x="Embarkement Place",y="No of People",title="Survivability based on embarkement")+scale_fill_brewer(palette = 'Set1')


## Lot of outliers in the fare data, some amt. of fare of passeneger class 3 is more than 2 
## and even 1st passenger class. 
full_data %>% 
  filter(Fare<50) %>% 
  ggplot(aes(x=Fare, y=Pclass, fill=Pclass))+
  geom_boxplot()+my_theme+labs(x="Fare",y="Passenger Class",title = "Boxplot of fares base on passenger class")

full_data %>% 
  filter(Fare_Per_Person<100)%>%
  ggplot(aes(x=Fare_Per_Person, y=Pclass, fill=Pclass))+
  geom_boxplot()+my_theme+labs(x="Fare",y="Passenger Class",title = "Boxplot of fares base on passenger class")
# 

full_data<-full_data%>% mutate(Fare_group=case_when(
                                   Fare_Per_Person<=5~"0-5",
                                   Fare_Per_Person>5 & Fare_Per_Person<=10 ~ "5-10",
                                   Fare_Per_Person>10 & Fare_Per_Person<=20 ~ "10-20",
                                   Fare_Per_Person>20 & Fare_Per_Person<=30 ~ "20-30",
                                   Fare_Per_Person>30 & Fare_Per_Person<=40 ~ "30-40",
                                   Fare_Per_Person>40 & Fare_Per_Person<=50 ~ "40-50",
                                   Fare_Per_Person>50 ~ "50+" ))

full_data%>%ggplot()+geom_point(aes(N_Per_Ticket,Fare_Per_Person,col=Pclass))


p1<-full_data%>%filter(Pclass==3)%>%ggplot()+geom_point(aes(N_Per_Ticket,Fare),size=2,col="blue",alpha=0.3)+
  labs(x="No. of persons on same ticket",y="Fare",title="Total Fare based on No. of persons",subtitle = "Passenger Class 3 Only")+
  my_theme+scale_x_continuous(breaks = seq(0,10,2))+
  scale_y_continuous(breaks = seq(0,60,10))

  

p2<-full_data%>%filter(Pclass==3)%>%ggplot()+geom_point(aes(N_Per_Ticket,Fare_Per_Person),col="blue",alpha=0.3,size=2)+
  labs(x="No of persons on same ticket",y="Fare per person",title="Fare based on one person",
       subtitle = "Passenger Class 3 only")+
  my_theme+scale_x_continuous(breaks = seq(0,10,2))

library(gridExtra)
grid.arrange(p1,p2)

#######################################
colnames(full_data)
full_data<-full_data%>%arrange(PassengerId)

data_use_model_1<-full_data%>%select(-c(Ticket,N_Per_Ticket,PassengerId,Name,Cabin,Age_Group,
                                        family_size,Fare_Per_Person,Alone))

test_1<-data_use_model_1%>%filter(is.na(Survived))
train_1<-data_use_model_1%>%filter(!is.na(Survived))



log_model_1<-glm(Survived~.,data=train_1,family = binomial(link = "logit"))
summary(log_model_1)


data_use_model_2<-full_data%>%select(-c(Ticket,N_Per_Ticket,PassengerId,Name,Cabin,SibSp,Parch,Fare,Age_Group))

data_use_model_2$Alone<-as.factor(data_use_model_2$Alone)

test_2<-data_use_model_2%>%filter(is.na(Survived))
train_2<-data_use_model_2%>%filter(!is.na(Survived))

log_model_2<-glm(Survived~.,data=train_2,family = binomial(link = "logit"))
summary(log_model_2)

prediction_train <- predict(log_model_2,newdata = train_2,type = "response")
prediction_train <- ifelse(prediction_train > 0.5,1,0)

library(caret)
prediction_train<-as.factor(prediction_train)
cm<-confusionMatrix(prediction_train,train_2$Survived)

cm_dataframe<-as.data.frame(cm$table)


ggplot(data =cm_dataframe ,
       aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = log(Freq)), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(x = Reference, y = Prediction, label = Freq),size=6) +
  labs(x="True Class",y="Predicted Class")+
  ggtitle(paste("Accuracy:",percent_format()(cm$overall[1])))+
  theme_fivethirtyeight()+
  theme(legend.position = "none",
        axis.title = element_text(size=20),
        axis.text = element_text(size=14),
        plot.title = element_text(hjust=0.5,size=20))




test_prediction<-predict(log_model_2,newdata = test_2,type="response")
test_prediction<-ifelse(test_prediction>0.5,1,0)


df_submission<-data.frame(PassengerId=test$PassengerId,Survived=test_prediction)

write.csv(df_submission,"df_predict.csv",row.names = FALSE)
  