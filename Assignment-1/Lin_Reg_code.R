# Load the required Libraries
library(readxl)

library(tidyverse) 

library(stringr)

library(mltools)

library(mice)

library(ggthemes)

library(GGally)

library(scales)

library(gridExtra)

library(Metrics)


# Reading the data

all_data <- read_excel("merged_data.xlsx",na=c(" ","**"))

### The FIPS, fips_x fips_y is the same (used for identification of area), so those columns are removed.

all_data<-all_data[,c(-1,-20,-24)]

# Checking basic structure of dataset

summary(all_data)

str(all_data)

head(all_data)

# Data Cleaning and processing

# Some incidence rate data had "#" written at the end with the number, below code extracts out the number.

check_1<-str_extract_all(all_data$Incidence_Rate,pattern = "\\d+.?\\d+")

all_data$Incidence_Rate<-as.numeric(all_data$Incidence_Rate)

all_data$Incidence_Rate<-check_1

##Function to check number of NAs column-wise

sapply(all_data,function(x) sum(is.na(x)))

##Replacing "_", "*" in the columns with NAs, where these are present  

all_data$Incidence_Rate<-str_replace_all(all_data$Incidence_Rate,pattern = "_",replacement=NA_character_)

all_data$Avg_Ann_Incidence<-str_replace_all(all_data$Avg_Ann_Incidence,pattern="_",replacement=NA_character_)

all_data$recent_trend<-str_replace_all(all_data$recent_trend,pattern = "_",replacement=NA_character_)

all_data$Mortality_Rate<-str_replace_all(all_data$Mortality_Rate,pattern = "\\*",replacement=NA_character_)

all_data$Avg_Ann_Deaths<-str_replace_all(all_data$Avg_Ann_Deaths,pattern="\\*",replacement=NA_character_)

## The "*" and NAs present in recent_trend column are replace with "stable" as , those values mean less than 16 reported cases.

all_data$recent_trend<-str_replace_all(all_data$recent_trend,"\\*","stable")

all_data$recent_trend<-replace_na(all_data$recent_trend,"stable")

## One hot encoding on recent trend column to create dummy variable

recent_trend<-as.data.frame(all_data$recent_trend)

recent<-data.table::data.table(recent_trend)

encoder<-one_hot(recent)

colnames(encoder)<-c("falling","rising","stable")

all_data<-cbind(all_data,encoder)

### remove the recent_trend column
all_data<-all_data[,-21]

### Converting incidence rate, average annual incidence, average annual death to numeric data

all_data\$Avg_Ann_Deaths<-as.numeric(all_data$Avg_Ann_Deaths)

all_data\$Mortality_Rate<-as.numeric(all_data$Mortality_Rate)

all_data\$Avg_Ann_Incidence<-str_replace_all(all_data$Avg_Ann_Incidence,"3 or fewer","3")

all_data\$Avg_Ann_Incidence<-as.numeric(all_data$Avg_Ann_Incidence)

### Lots of NAs in Median income by ethnicities, so removed them only kepping Median Income total

all_data<-all_data[,c(-8,-9,-10,-11,-12)]

## Impute values using MICE algorithm

all_data_no_catgr_var<-all_data[,c(-1,-2)]

imputed_Data <- mice(all_data_no_catgr_var, m=5, maxit = 50, method = 'pmm', seed = 500)

completeData <- complete(imputed_Data,2)

# Visualizatons

##Fig 1 
  ggpairs(completeData[,c(1,2,3)])

##Fig 2- 
    completeData%>%ggplot()+geom_histogram(aes(Mortality_Rate,y=..density..),bins=50,fill="grey",
                                                  color="black")+
      geom_density(aes(Mortality_Rate),color="red",size=1.5)+
  theme_fivethirtyeight()+theme(plot.title = element_text(hjust = 0.5,size=20),
                                axis.title=element_text(size=20),
                                axis.text = element_text(size=14),
                                axis.text.x = element_text(hjust=0.8))+
  labs(x="Mortality Rate",y="Denisty",title = "Mortality Rate Values Distribution")+
  scale_x_continuous(breaks = seq(0,125,25))+scale_y_continuous(breaks = seq(0,0.04,0.005))

##Fig  
    gplot(aes(sample=Mortality_Rate),data=completeData)+stat_qq(distribution=qnorm,size=1.6)+
  stat_qq_line(color="blue",size=1.1)+
  theme_fivethirtyeight()+theme(plot.title = element_text(hjust = 0.5,size=20),
                                axis.title = element_text(size=20),
                                axis.text = element_text(size=14),
                                axis.text.x = element_text(hjust=0.8))+
  labs(x="Theoretical",y="Mortality Rate",title = "q-q plot") 

##Fig 4
      recent_trend%>%ggplot()+geom_bar(aes(trend),fill="darkblue",width=0.5)+coord_flip()+ 
        theme_fivethirtyeight()+
        theme(plot.title = element_text(hjust = 0.5,size=20), 
              axis.title = element_text(size=20),axis.text = element_text(size=14), 
              axis.text.x = element_text(hjust=0.8))+
  labs(x="Trend",y="Count",title = "Recent trend of Cancer diagnosis")

##Fig 5 
  
  p1<-all_data%>%group_by(State)%>%
  summarise(mean_mortality_rate=mean(Mortality_Rate,na.rm=TRUE))%>%
  mutate(State=reorder(State,mean_mortality_rate))%>%top_n(5)%>%
  ggplot(aes(State,mean_mortality_rate))+
  geom_col(fill="black")+
  coord_flip()+theme_fivethirtyeight()+theme(plot.title = element_text(hjust = 0.5,size=20),
                                             axis.title = element_text(size=20),
                                             axis.text = element_text(size=14),
                                             axis.text.x = element_text(hjust=0.8))+
  labs(x="Mean Mortality Rate",y="State",title = "5 states with highest mean mortality Rate ")

p2<-all_data%>%group_by(State)%>%
  summarise(mean_incidence_rate=mean(Incidence_Rate,na.rm=TRUE))%>%
  mutate(State=reorder(State,mean_incidence_rate))%>%top_n(5)%>%ggplot(aes(State,mean_incidence_rate))+
  geom_col(fill="red")+
  coord_flip()+theme_fivethirtyeight()+theme(plot.title = element_text(hjust = 0.5,size=20),
                                             axis.title = element_text(size=20),
                                             axis.text = element_text(size=14),
                                             axis.text.x = element_text(hjust=0.8))+
  labs(x="Mean Incidence Rate",y="State",title = "5 states with highest mean incidence Rate ")

grid.arrange(p1,p2,ncol=1)

##Fig 6
  
  p11<-completeData%>%filter(All_Poverty)%>%ggplot(aes(y=Mortality_Rate))+
  
  geom_point(aes(x=All_Poverty),color="red")+
  theme_fivethirtyeight()+theme(plot.title = element_text(hjust = 0.5,size=20),
                                axis.title = element_text(size=20),
                                axis.text = element_text(size=14),
                                axis.text.x = element_text(hjust=0.8))+
  labs(x="Total Poverty",y="Mortality Rate")


p12<-completeData%>%ggplot(aes(y=Mortality_Rate))+geom_point(aes(x=All_With),color="red")+
  scale_x_continuous(labels = comma)+
  theme_fivethirtyeight()+theme(plot.title = element_text(hjust = 0.5,size=20),
                                 axis.title = element_text(size=20),
                                 axis.text = element_text(size=14),
                                 axis.text.x = element_text(hjust=0.8))+
  labs(x="People with insurance",y="Mortality Rate")

grid.arrange(p11,p12,ncol=1)

##Fig 7
  
  completeData%>%ggplot(aes(y=Mortality_Rate))+geom_point(aes(x=Med_Income),color="red")+
  scale_x_continuous(labels = comma)+theme_fivethirtyeight()+
    theme(plot.title = element_text(hjust = 0.5,size=20),
           axis.title = element_text(size=20),
            axis.text = element_text(size=14),
            axis.text.x = element_text(hjust=0.8))+
  labs(x="Income",y="Mortality Rate")


## Fig 8 and 9 are screenshot of the summary of the fitted linear regression models**  
  
## Fig 10  
  
  completeData%>%ggplot(aes(y=predicted,x=Mortality_Rate))+
    geom_point(color="darkblue",size=2,alpha=0.6)+
  geom_smooth(method = lm,fill=NA,color="red")+
  theme_fivethirtyeight()+theme(plot.title = element_text(hjust = 0.5,size=20),
                                axis.title = element_text(size=20),
                                axis.text = element_text(size=14),
                                axis.text.x = element_text(hjust=0.8))+
  labs(x="Actual Mortality Rate",y="Predicted Mortality Rate",title = "Mortality Rate")+
  scale_x_continuous(breaks = seq(0,120,20))+scale_y_continuous(breaks = seq(0,160,20))

##Fig 11 
  
  completeData%>%ggplot(aes(predicted,residual))+geom_point(alpha=0.6,color="darkblue",size=2)+
  geom_hline(yintercept = 0,linetype="dashed",size=1.4,color="red")+
    scale_x_continuous(breaks = seq(20,120,20))+
  scale_y_continuous(breaks = c(-60,-40,-20,0,20,40,60),limits =c(-60,60))+
    theme_fivethirtyeight()+
  theme(plot.title = element_text(hjust = 0.5,size=20),
        axis.title = element_text(size=20),
        axis.text = element_text(size=14),
        axis.text.x = element_text(hjust=0.8))+
  labs(x="Fitted Values",y="Residual")

# The model

## 1st model (Fig 8)

data_needed<-completeData[,c(1,4,9,10,11,12,13,14,15,16)]

lin_reg_model<-lm(Mortality_Rate~.,data=data_needed)

summary(lin_reg_model)

## 2nd model (Fig 9) without insurance data
data_needed_model_2<-data_needed[,c(-3,-4)]

lin_reg_model_2<-lm(Mortality_Rate~.,data=data_needed_model_2)

summary(lin_reg_model_2)

data_needed_model_2$residual<-residuals(lin_reg_model_2)
data_needed_model_2$predicted<-predict(lin_reg_model_2)
### To calulcate mean squared error

mse(data_needed_model_2$Mortality_Rate,data_needed_model_2$predicted)

