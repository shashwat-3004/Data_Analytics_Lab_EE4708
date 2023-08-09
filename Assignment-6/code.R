library(tidyverse)
library(mice)
library(ggcorrplot)

train<-read_csv("pulsar_data_train.csv")
test<-read_csv("pulsar_data_test.csv")




colnames(train)<-c("Mean_IP","SD_IP","EK_IP","Skew_IP","Mean_DMSNR","SD_DMSNR",
                   "EK_DMSNR","Skew_DMSNR","class")

colnames(test)<-c("Mean_IP","SD_IP","EK_IP","Skew_IP","Mean_DMSNR","SD_DMSNR",
                  "EK_DMSNR","Skew_DMSNR","class")

full_data<-full_join(train,test)

na_check<-function(dataset){
  sapply(dataset,function(x) sum(is.na(x)))
}

str(full_data)
summary(full_data)
# Check total amt of NAs in the dataset

na_check(full_data[,-9])

na_check(train)

na_check(test[,-9])

correlation<-cor(full_data[,-9],use="na.or.complete")

ggcorrplot(correlation, hc.order = TRUE,lab = TRUE)

class<-full_data[,9]
full_dat_without_class<-full_data[-9]

imputed_Data <- mice(full_dat_without_class, m=5, maxit = 50, method = 'pmm', seed = 500)
completeData <- complete(imputed_Data,2)

full_data<-completeData
full_data$class<-class$class

full_data$class<-as.factor(full_data$class)



library(ggthemes)

my_theme<-theme_fivethirtyeight()+theme(plot.title = element_text(hjust = 0.5,size=20),
                                        axis.title = element_text(size=20),
                                        axis.text = element_text(size=14),
                                        plot.subtitle = element_text(hjust=0.5),
                                        legend.position = "top",legend.title = element_text(size=15),
                                        legend.text = element_text(size=15))

library(ggpair)




full_data %>%filter(!is.na(class))%>%
  ggplot( aes(x=Mean_IP, fill=class)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) 

full_data%>%filter(!is.na(class))%>%ggplot()+geom_point(aes(Mean_IP,Skew_IP,col=class),size=2)+
  scale_color_brewer(palette = "Set1")+labs(title="Mean Vs Skewness of Integrated Profile")+
  my_theme


full_data%>%filter(!is.na(class))%>%ggplot()+geom_point(aes(Skew_IP,EK_IP,col=class),size=2)+
  scale_color_brewer(palette = "Set1")+labs(title="Skewness Vs Excess kurtosis of Integrated Profile")+
  my_theme


full_data%>%filter(!is.na(class))%>%ggplot()+geom_point(aes(Mean_DMSNR,SD_DMSNR,col=class),size=2)+
  scale_color_brewer(palette = "Set1")+labs(title="Mean Vs Standard Deviation of DM-SNR curve")+
  my_theme



full_data%>%filter(!is.na(class))%>%ggplot()+geom_point(aes(Mean_IP,SD_IP,col=class),size=2)+
  scale_color_brewer(palette = "Set1")+labs(title="Mean Vs Standard Deviation of Integrated Profile")+
  my_theme



full_data%>%filter(!is.na(class))%>%ggplot()+geom_point(aes(Mean_DMSNR,EK_DMSNR,col=class),size=2)+
  scale_color_brewer(palette = "Set1")+labs(title="Mean Vs Excess kurtosis of DM-SNR curve")+
  my_theme



full_data%>%filter(!is.na(class))%>%ggplot()+geom_point(aes(Skew_DMSNR,EK_DMSNR,col=class),size=2)+
  scale_color_brewer(palette = "Set1")+labs(title="Skewness Vs Excess kurtosis of DM-SNR curve")+
  my_theme




full_data[,1:8]<-scale(full_data[,1:8])

library(e1071)
library(caret)

train<-full_data%>%filter(!is.na(class))

set.seed(1)
Index <- createDataPartition(train$class,p=0.8,list=FALSE)

train_data<-train[Index,]

validation_data<-train[-(Index),]

test<-full_data%>%filter(is.na(class))

classifier_1<- svm(formula = class ~ .,
                  data = train_data,
                  type = 'C-classification',
                  kernel = 'linear')

val_pred<-predict(classifier_1,newdata = validation_data[,-9])

test_pred<- predict(classifier_1, newdata = test[,-9])



cm_model1<-confusionMatrix(val_pred,validation_data$class)



cm_dataframe<-as.data.frame(cm_model1$table)

library(scales)
ggplot(data =cm_dataframe ,
       aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = log(Freq)), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(x = Reference, y = Prediction, label = Freq),size=6) +
  labs(x="True Class",y="Predicted Class")+
  ggtitle(paste("Accuracy:",percent_format()(cm_model1$overall[1])))+
  theme_fivethirtyeight()+
  theme(legend.position = "none",
        axis.title = element_text(size=20),
        axis.text = element_text(size=14),
        plot.title = element_text(hjust=0.5,size=20))
  
  