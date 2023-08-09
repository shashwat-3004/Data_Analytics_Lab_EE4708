library(tidyverse)

data<-read_csv("car_evaluation.csv",col_names=FALSE)

colnames(data)<-c("buying","maint","doors","persons","lug_boot","safety","Target")

library(stringr)

data$doors<-str_replace_all(data$doors,pattern = "5more",replacement = "more")

data<-data%>% mutate_if(is.character,as.factor)

str(data)
summary(data)



na_check<-function(dataset){
  sapply(dataset,function(x) sum(is.na(x)))
}

na_check(data)

# Visualizations

# Fig-2: Distribution of target variable

library(ggthemes)

my_theme<-theme_fivethirtyeight()+theme(plot.title = element_text(hjust = 0.5,size=20),
                                        axis.title = element_text(size=20),
                                        axis.text = element_text(size=14),
                                        plot.subtitle = element_text(hjust=0.5),
                                        legend.position = "top",legend.title = element_text(size=15),
                                        legend.text = element_text(size=15))


target_var<-as.data.frame(table(data$Target))

colnames(target_var)<-c("Target","Frequency")

target_var%>%mutate(Target=reorder(Target,Frequency))%>%
  ggplot()+geom_col(aes(Target,Frequency),fill="red",col="black")+
  labs(title = "Target Variable Distribution")+
  my_theme+
  geom_text(aes(x=Target,y=Frequency+50,
                label=paste(round((Frequency*100)/sum(Frequency),2),"%")),size=5,col="blue")

# fig 2

p1<-data%>%ggplot()+geom_bar(aes(buying,fill=Target),position = "dodge")+
  labs(x="Buying Cost",y="Frequency",title = "Target w.r.t Buying cost")+
  my_theme+
  scale_fill_brewer(palette = "Set1")

# fig 3

p2<-data%>%ggplot()+geom_bar(aes(maint,fill=Target),position = "dodge")+
  labs(x="Maintenance Cost",y="Frequency",title = "Target w.r.t Maintenance cost")+
  my_theme+
  scale_fill_brewer(palette = "Set1")

# fig 4

data%>%ggplot()+geom_bar(aes(doors,fill=Target),position = "dodge")+
  labs(x="No. of Doors",y="Frequency",title = "Target w.r.t No. of doors")+
  my_theme+
  scale_fill_brewer(palette = "Set1")

# fig 5

data%>%ggplot()+geom_bar(aes(persons,fill=Target),position = "dodge")+
  labs(x="",y="Frequency",title = "Target w.r.t No. of persons")+
  my_theme+
  scale_fill_brewer(palette = "Set1")

# fig 6

data%>%ggplot()+geom_bar(aes(lug_boot,fill=Target),position = "dodge")+
  labs(x="Size of luggae boot",y="Frequency",title = "Target w.r.t Size of luggage boot")+
  my_theme+
  scale_fill_brewer(palette = "Set1")

# fig 7

data%>%ggplot()+geom_bar(aes(safety,fill=Target),position = "dodge")+
  labs(x="Level of Safety",y="Frequency",title = "Target w.r.t Safety")+
  my_theme+
  scale_fill_brewer(palette = "Set1")

library(rpart)
library(caret)
library(rpart.plot)
set.seed(123)
trainIndex <- createDataPartition(data$Target,p=0.8,list=FALSE)

train<-data[trainIndex,]
test<-data[-(trainIndex),]


# Model

model_1<-rpart(Target~buying+maint,data=train,method='class')

predict_model1<-predict(model_1,test,type='class')

test$preicted_target_model1<-predict_model1

# Everything predicted as "unacc"
cm_model1<-confusionMatrix(predict_model1,test$Target)

model_2<-rpart(Target~buying+maint+persons,data=train,method='class')

predict_model2<-predict(model_2,test,type='class')

rpart.plot(model_2)

# No of perons more important
cm_model2<-confusionMatrix(predict_model2,test$Target)


model_3<-rpart(Target~buying+maint+persons+doors,data=train,method='class')

predict_model3<-predict(model_3,test,type='class')

rpart.plot(model_3)

# No. of Doors not that important
cm_model3<-confusionMatrix(predict_model3,test$Target)


# lug_boot important

model_4<-rpart(Target~buying+maint+persons+doors+lug_boot,data=train,method='class')

predict_model4<-predict(model_4,test,type='class')

rpart.plot(model_4)

# No. of Doors not that important
cm_model4<-confusionMatrix(predict_model4,test$Target)


# Without doors

model_5<-rpart(Target~buying+maint+persons+lug_boot+safety,data=train,method='class')

predict_model5<-predict(model_5,test,type='class')

rpart.plot(model_5)

# Safety factor very important
cm_model5<-confusionMatrix(predict_model5,test$Target)



model_6<-rpart(Target~safety+persons+buying+maint,data=train,method='class')

predict_model6<-predict(model_6,test,type='class')

cm_model6<-confusionMatrix(predict_model6,test$Target)

rpart.plot(model_6)

library(gridExtra)

grid.arrange(p1,p2)