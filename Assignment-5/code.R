
library(tidyverse)
library(randomForest)
library(caret)

data<-read_csv("car_evaluation.csv",col_names = FALSE)

colnames(data)<-c("buying", "maint","doors", "persons", "lug_boot","safety", "target")

data$doors<-gsub(pattern = "5more",replacement = "more",x=data$doors)

data<-data%>%mutate_if(is.character,as.factor)


library(ggthemes)

my_theme<-theme_fivethirtyeight()+theme(plot.title = element_text(hjust = 0.5,size=20),
                                        axis.title = element_text(size=20),
                                        axis.text = element_text(size=14),
                                        plot.subtitle = element_text(hjust=0.5),
                                        legend.position = "top",legend.title = element_text(size=15),
                                        legend.text = element_text(size=15))



target_var<-as.data.frame(table(data$target))

colnames(target_var)<-c("Target","Frequency")

target_var%>%mutate(Target=reorder(Target,Frequency))%>%
  ggplot()+geom_col(aes(Target,Frequency),fill="darkgreen",col="black")+
  labs(title = "Target Variable Distribution")+
  my_theme+
  geom_text(aes(x=Target,y=Frequency+50,
                label=paste(round((Frequency*100)/sum(Frequency),2),"%")),size=5,col="blue")+
  scale_y_continuous(breaks = seq(0,1200,300))

data$buying<-factor(data$buying,levels = c("low","med","high","vhigh"))

data%>%ggplot()+geom_bar(aes(buying,fill=target),position = "dodge")+
  labs(x="Buying Cost",y="Frequency",title = "Target w.r.t Buying cost")+
  my_theme+
  scale_fill_brewer(palette = "Set2")

data%>%ggplot()+geom_bar(aes(maint,fill=target),position = "dodge")+
  labs(x="Maintenance Cost",y="Frequency",title = "Target w.r.t Maintenance cost")+
  my_theme+
  scale_fill_brewer(palette = "Set2")



data%>%ggplot()+geom_bar(aes(doors,fill=target),position = "dodge")+
  labs(x="No. of Doors",y="Frequency",title = "Target w.r.t No. of doors")+
  my_theme+
  scale_fill_brewer(palette = "Set2")

# fig 5

data%>%ggplot()+geom_bar(aes(persons,fill=target),position = "dodge")+
  labs(x="No. of persons",y="Frequency",title = "Target w.r.t No. of persons")+
  my_theme+
  scale_fill_brewer(palette = "Set2")

# fig 6

data$lug_boot<-factor(data$lug_boot,levels =c("small","med","big") )

data%>%ggplot()+geom_bar(aes(lug_boot,fill=target),position = "dodge")+
  labs(x="Size of luggage boot",y="Frequency",title = "Target w.r.t Size of luggage boot")+
  my_theme+
  scale_fill_brewer(palette = "Set2")

# fig 7


data$safety<-factor(data$safety,levels = c("low","med","high"))

data%>%ggplot()+geom_bar(aes(safety,fill=target),position = "dodge")+
  labs(x="Level of Safety",y="Frequency",title = "Target w.r.t level of Safety")+
  my_theme+
  scale_fill_brewer(palette = "Set2")




Index <- createDataPartition(data$target,p=0.8,list=FALSE)

train_data<-data[Index,]
test_data<-data[-(Index),]


# Visualizations



# Models

set.seed(123)

model1<-randomForest(target~buying+maint,data=train_data)

print(model1)

print(importance(model1,type = 2))

predict_model1<-predict(model1,test_data,type='class')

test_data$preicted_target_model1<-predict_model1

cm_model1<-confusionMatrix(predict_model1,test_data$target)

---------------------------------

  
model2<-randomForest(target~buying+maint+persons,data=train_data)

print(model2)

print(importance(model2,type = 2))

predict_model2<-predict(model2,test_data,type='class')

test_data$predicted_target_model2<-predict_model2

cm_model2<-confusionMatrix(predict_model2,test_data$target)

---------------------------------
  
model3<-randomForest(target~buying+maint+persons+doors,data=train_data)

print(model3)

print(importance(model3,type = 2))

predict_model3<-predict(model3,test_data,type='class')

test_data$preicted_target_model3<-predict_model3

cm_model3<-confusionMatrix(predict_model3,test_data$target)

----------------------------------
  
  
model4<-randomForest(target~buying+maint+persons+doors+lug_boot,data=train_data)

print(model4)

print(importance(model4,type = 2))

predict_model4<-predict(model4,test_data,type='class')

test_data$preicted_target_model4<-predict_model4

cm_model4<-confusionMatrix(predict_model4,test_data$target)

--------------------------------
  
  
model5<-randomForest(target~buying+maint+persons+doors+lug_boot+safety,data=train_data)

print(model5)

print(importance(model5,type = 2))

varImpPlot(model5, main="Variable Importance Plot",pch=16,col="blue",cex=1.3)

predict_model5<-predict(model5,test_data,type='class')

test_data$preicted_target_model5<-predict_model5

cm_model5<-confusionMatrix(predict_model5,test_data$target)



-----------------------
model6<-randomForest(target~persons+safety+buying+maint+lug_boot,data=train_data)

print(model6)

predict_model6<-predict(model6,test_data,type='class')

test_data$preicted_target_model6<-predict_model6

cm_model6<-confusionMatrix(predict_model6,test_data$target)

