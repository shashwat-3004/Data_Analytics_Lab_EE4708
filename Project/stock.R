library(tidyverse)
library(lubridate)
library(scales)

cognizant<-read_csv("Cognizant share prices 2019_2021.csv")

hcl<-read_csv("HCL Technologies share prices 2019_2021.csv")

hdfc<-read_csv("HDFC Bank Share Prices 2019_2021.csv")

icici<-read_csv("ICICI Bank Share Prices 2019_2021.csv")

infosys<-read_csv("Infosys Share Prices 2019_2021.csv")

sbi<-read_csv("SBI Share Prices 2019_2021.csv")

exchange_rate<-read_csv("USD-INR Exchange rate 2019_2021.csv")

hcl<-hcl%>%filter(Close!="null")%>% mutate_if(is.character,as.numeric)
hdfc<-hdfc%>%filter(Close!="null")%>%mutate_if(is.character,as.numeric)
icici<-icici%>%filter(Close!="null")%>%mutate_if(is.character,as.numeric)
infosys<-infosys%>%filter(Close!="null")%>%mutate_if(is.character,as.numeric)
sbi<-sbi%>%filter(Close!="null")%>% mutate_if(is.character,as.numeric)


library(ggthemes)

my_theme<-theme_fivethirtyeight()+theme(plot.title = element_text(hjust = 0.5,size=20),
                                        axis.title = element_text(size=20),
                                        axis.text = element_text(size=14),
                                        plot.subtitle = element_text(hjust=0.5),
                                        legend.position = "top",legend.title = element_text(size=15),
                                        legend.text = element_text(size=15),
                                        axis.text.x = element_text(size=14,angle = 45,hjust=1))



p1<-ggplot()+geom_line(data=cognizant,aes(Date,Close),size=1.1,col="red")+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  my_theme+
  labs(x="Time",y="Closing Price",title = "Cognizant Closing Share Price")
                                                                         

p2<-ggplot()+geom_line(data=hcl,aes(Date,Close),size=1.1,col="red")+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  scale_y_continuous(breaks = seq(800,1300,100))+
  my_theme+
  labs(x="Time",y="Closing Price",title = "HCL Closing Share Price")


p3<-ggplot()+geom_line(data=hdfc,aes(Date,Close),size=1.1,col="red")+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  
  my_theme+
  labs(x="Time",y="Closing Price",title = "HDFC Closing Share Price")

p4<-ggplot()+geom_line(data=icici,aes(Date,Close),size=1.1,col="red")+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  my_theme+
  labs(x="Time",y="Closing Price",title = "ICICI Closing Share Price")

p5<-ggplot()+geom_line(data=infosys,aes(Date,Close),size=1.1,col="red")+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  my_theme+
  scale_y_continuous(breaks = seq(1000,1700,100))+
  labs(x="Time",y="Closing Price",title = "Infosys Closing Share Price")

p6<-ggplot()+geom_line(data=sbi,aes(Date,Close),size=1.1,col="red")+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  my_theme+
  scale_y_continuous(breaks = seq(100,500,100),limits = c(100,500))+
  labs(x="Time",y="Closing Price",title = "SBI Closing Share Price")

p7<-ggplot()+geom_col(data=cognizant,aes(Date,Volume),size=1.1,col="#3aa832")+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  my_theme+
  scale_y_continuous(labels = comma)+
  labs(x="Time",y="Volume",title = "Coznizant Volume of shares traded/contracted")


p8<-ggplot()+geom_col(data=hcl,aes(Date,Volume),size=1.1,col="#3aa832")+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  my_theme+
  scale_y_continuous(labels = comma)+
  labs(x="Time",y="Volume",title = "HCL Volume of shares traded/contracted")


p9<-ggplot()+geom_col(data=hdfc,aes(Date,Volume),size=1.1,col="#3aa832")+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  my_theme+
  scale_y_continuous(labels = comma)+
  labs(x="Time",y="Volume",title = "HDFC Volume of shares traded/contracted")


p10<-ggplot()+geom_col(data=icici,aes(Date,Volume),size=1.1,col="#3aa832")+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  my_theme+
  scale_y_continuous(labels = comma)+
  labs(x="Time",y="Volume",title = "ICICI Volume of shares traded/contracted")


p11<-ggplot()+geom_col(data=infosys,aes(Date,Volume),size=1.1,col="#3aa832")+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  my_theme+
  scale_y_continuous(labels = comma)+
  labs(x="Time",y="Volume",title = "Infosys Volume of shares traded/contracted")


p12<-ggplot()+geom_col(data=sbi,aes(Date,Volume),size=1.1,col="#3aa832")+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  my_theme+
  scale_y_continuous(labels = comma)+
  labs(x="Time",y="Volume",title = "SBI Volume of shares traded/contracted")

# Return

cognizant[2:nrow(cognizant),"Return"]<-log((cognizant[2:nrow(cognizant),5])/cognizant[1:(nrow(cognizant)-1),5])

hcl[2:nrow(hcl),"Return"]<-log(hcl[2:nrow(hcl),5]/hcl[1:(nrow(hcl)-1),5])

hdfc[2:nrow(hdfc),"Return"]<-log(hdfc[2:nrow(hdfc),5]/hdfc[1:(nrow(hdfc)-1),5])

icici[2:nrow(icici),"Return"]<-log(icici[2:nrow(icici),5]/icici[1:(nrow(icici)-1),5])

infosys[2:nrow(infosys),"Return"]<-log(infosys[2:nrow(infosys),5]/infosys[1:(nrow(infosys)-1),5])

sbi[2:nrow(sbi),"Return"]<-log(sbi[2:nrow(sbi),5]/sbi[1:(nrow(sbi)-1),5])

cognizant_less<-cognizant%>%filter(Date>="2020-10-05")
hdfc_less<-hdfc%>%filter(Date>="2020-10-05")
icici_less<-icici%>%filter(Date>="2020-10-05")
sbi_less<-sbi%>%filter(Date>="2020-10-05")


p13<-ggplot()+geom_line(data=cognizant_less,aes(Date,Return),color="blue",size=1.1,alpha=0.7)+
  geom_hline(yintercept = 0,col="red",size=1.1,linetype="dashed")+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  my_theme+
  labs(x="Time",y="Log Return Value",title = "Log Return Value of Cognizant")

p14<-ggplot()+geom_line(data=hcl,aes(Date,Return),color="red",size=1.1,alpha=0.7)+
  geom_hline(yintercept = 0,col="darkblue",size=1.1,linetype="dashed")+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  my_theme+
  labs(x="Time",y="Log Return Value",title = "Log Return Value of HCL")

p15<-ggplot()+geom_line(data=hdfc_less,aes(Date,Return),color="#fc9003",size=1.1,alpha=0.7)+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  geom_hline(yintercept = 0,col="red",size=1.1,linetype="dashed")+
  my_theme+
  labs(x="Time",y="Log Return Value",title = "Log Return Value of HDFC")

p16<-ggplot()+geom_line(data=icici_less,aes(Date,Return),color="#9003fc",size=1.1,alpha=0.7)+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  geom_hline(yintercept = 0,col="red",size=1.1,linetype="dashed")+
  my_theme+
  labs(x="Time",y="Log Return Value",title = "Log Return Value of ICICI")

p17<-ggplot()+geom_line(data=infosys,aes(Date,Return),color="brown",size=1.1,alpha=0.7)+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  geom_hline(yintercept = 0,col="darkblue",size=1.1,linetype="dashed")+
  my_theme+
  labs(x="Time",y="Log Return Value",title = "Log Return Value of Infosys")

p18<-ggplot()+geom_line(data=sbi_less,aes(Date,Return),color="#03cefc",size=1.1,alpha=0.7)+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  geom_hline(yintercept = 0,col="red",size=1.1,linetype="dashed")+
  my_theme+
  labs(x="Time",y="Log Return Value",title = "Log Return Value of SBI")


library(prophet)
library(forecast)

# Only taking close value

cognizant_data<-cognizant[,c(1,5)]
hcl_data<-hcl[,c(1,5)]
hdfc_data<-hdfc[,c(1,5)]
icici_data<-icici[,c(1,5)]
infosys_data<-infosys[,c(1,5)]
sbi_data<-sbi[,c(1,5)]

colnames(cognizant_data)<-c("ds","y")
colnames(hcl_data)<-c("ds","y")
colnames(hdfc_data)<-c("ds","y")
colnames(icici_data)<-c("ds","y")
colnames(infosys_data)<-c("ds","y")
colnames(sbi_data)<-c("ds","y")






###


f_1<-prophet(cognizant_data)
f_2<-prophet(hcl_data)
f_3<-prophet(hdfc_data)
f_4<-prophet(icici_data)
f_5<-prophet(infosys_data)
f_6<-prophet(sbi_data)

future_1 <- make_future_dataframe(f_1, periods = 60)
future_2 <- make_future_dataframe(f_2, periods = 60)
future_3 <- make_future_dataframe(f_3, periods = 60)
future_4 <- make_future_dataframe(f_4, periods = 60)
future_5 <- make_future_dataframe(f_5, periods = 60)
future_6 <- make_future_dataframe(f_6, periods = 60)

forecast_cognizant <- predict(f_1, future_1)
forecast_hcl <- predict(f_2, future_2)
forecast_hdfc <- predict(f_3, future_3)
forecast_icici <- predict(f_4, future_4)
forecast_infosys <- predict(f_5, future_5)
forecast_sbi <- predict(f_6, future_6)


ggplot()+geom_line(data=cognizant_data,aes(ds,y,col="Actual"))+
  geom_line(data=forecast_cognizant[nrow(cognizant_data+1):nrow(forecast_cognizant),],
            aes(as.Date(ds),yhat,col="Forecast"))+
  scale_x_date(date_breaks = "3 month",date_labels = "%B %Y")+
  scale_color_manual(labels=c("Actual","Forecast"),values = c("black","red"))+
  my_theme+
  labs(x="Time",y="Close Value",title = "Forecast share prices(Cognizant)")


ggplot()+geom_line(data=hcl_data,aes(ds,y,col="Actual"))+
  geom_line(data=forecast_hcl[nrow(hcl_data):nrow(forecast_hcl),],
            aes(as.Date(ds),yhat,col="Forecast"))+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  scale_color_manual(labels=c("Actual","Forecast"),values = c("black","red"))+
  my_theme+
  labs(x="Time",y="Close Value",title = "Forecast share prices(HCL)")


ggplot()+geom_line(data=hdfc_data,aes(ds,y,col="Actual"))+
  geom_line(data=forecast_hdfc[nrow(hdfc_data):nrow(forecast_hdfc),],
            aes(as.Date(ds),yhat,col="Forecast"))+
  scale_x_date(date_breaks = "3 month",date_labels = "%B %Y")+
  scale_color_manual(labels=c("Actual","Forecast"),values = c("black","red"))+
  my_theme+
  labs(x="Time",y="Close Value",title = "Forecast share prices(HDFC)")



ggplot()+geom_line(data=icici_data,aes(ds,y,col="Actual"))+
  geom_line(data=forecast_icici[nrow(icici_data):nrow(forecast_icici),],
            aes(as.Date(ds),yhat,col="Forecast"))+
  scale_x_date(date_breaks = "3 month",date_labels = "%B %Y")+
  scale_color_manual(labels=c("Actual","Forecast"),values = c("black","red"))+
  my_theme+
  labs(x="Time",y="Close Value",title = "Forecast share prices(ICICI)")


ggplot()+geom_line(data=infosys_data,aes(ds,y,col="Actual"))+
  geom_line(data=forecast_infosys[nrow(infosys_data+1):nrow(forecast_infosys),],
            aes(as.Date(ds),yhat,col="Forecast"))+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  scale_color_manual(labels=c("Actual","Forecast"),values = c("black","red"))+
  my_theme+
  labs(x="Time",y="Close Value",title = "Forecast share prices(Infosys)")


ggplot()+geom_line(data=sbi_data,aes(ds,y,col="Actual"))+
  geom_line(data=forecast_sbi[nrow(sbi_data+1):nrow(forecast_sbi),],
            aes(as.Date(ds),yhat,col="Forecast"))+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  scale_color_manual(labels=c("Actual","Forecast"),values = c("black","red"))+
  my_theme+
  labs(x="Time",y="Close Value",title = "Forecast share prices(SBI)")




  

plot(f_2,forecast_hcl)

plot(f_3,forecast_hdfc)

plot(f_4,forecast_icici)

plot(f_5,forecast_infosys)

plot(f_6,forecast_sbi)



prophet_plot_components(f_1, forecast_cognizant)


prophet_plot_components(f_2, forecast_hcl)

prophet_plot_components(f_3, forecast_hdfc)

prophet_plot_components(f_4, forecast_icici)

prophet_plot_components(f_5,forecast_infosys)

prophet_plot_components(f_6, forecast_sbi)


cognizant_data$prediction<-forecast_cognizant[(1:nrow(cognizant_data)),"yhat"]

accuracy(cognizant_data$prediction,cognizant_data$y)


hcl_data$prediction<-forecast_hcl[(1:nrow(hcl_data)),"yhat"]

accuracy(hcl_data$prediction,hcl_data$y)


hdfc_data$prediction<-forecast_hdfc[(1:nrow(hdfc_data)),"yhat"]

accuracy(hdfc_data$prediction,hdfc_data$y)


icici_data$prediction<-forecast_icici[(1:nrow(icici_data)),"yhat"]

accuracy(icici_data$prediction,icici_data$y)


infosys_data$prediction<-forecast_infosys[(1:nrow(infosys_data)),"yhat"]

accuracy(infosys_data$prediction,infosys_data$y)


sbi_data$prediction<-forecast_sbi[(1:nrow(sbi_data)),"yhat"]

accuracy(sbi_data$prediction,sbi_data$y)


###
ggplot()+geom_line(data=cognizant,aes(Date,Close,col="Actual"),size=1.1,alpha=0.7)+
  geom_line(data=forecast_cognizant[1:nrow(cognizant_data),],aes(as.Date(ds),yhat,col="Prophet Model"),
            size=1.1)+
  geom_line(data=cognizant_data,aes(ds,dnn,col="Neural Network"),size=1.1,alpha=0.6)+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  scale_color_manual(labels=c("Actual","Neural Network","Prophet Model"),
                     values=c("red","blue","#49eb34"))+
  my_theme+
  labs(x="Time",y="Close Value",title ="Actual Vs Fitted Data(Cognizant)" )

ggplot()+geom_line(data=hcl,aes(Date,Close,col="Actual"),size=1.1,alpha=0.7)+
  geom_line(data=forecast_hcl[1:nrow(hcl_data),],aes(as.Date(ds),yhat,col="Prophet Model"),
            size=1.1)+
  geom_line(data=hcl_data,aes(ds,dnn,col="Neural Network"),size=1.1,alpha=0.6)+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  scale_color_manual(labels=c("Actual","Neural Network","Prophet Model"),
                     values=c("red","blue","#49eb34"))+
  my_theme+
  labs(x="Time",y="Close Value",title ="Actual Vs Fitted Data(HCL)" )



ggplot()+geom_line(data=hdfc,aes(Date,Close,col="Actual"),size=1.1,alpha=0.7)+
  geom_line(data=forecast_hdfc[1:nrow(hdfc_data),],aes(as.Date(ds),yhat,col="Prophet Model"),
            size=1.1)+
  geom_line(data=hdfc_data,aes(ds,dnn,col="Neural Network"),size=1.1,alpha=0.6)+
  scale_x_date(date_breaks = "2 month",date_labels = "%B %Y")+
  scale_color_manual(labels=c("Actual","Neural Network","Prophet Model"),
                     values=c("red","blue","#49eb34"))+
  my_theme+
  labs(x="Time",y="Close Value",title ="Actual Vs Fitted Data(HDFC)" )


  

###

library(tsfknn)

cognizant_data<-cognizant_data[,c(1,2)]
hcl_data<-hcl_data[,c(1,2)]
hdfc_data<-hdfc_data[,c(1,2)]
icici_data<-icici_data[,c(1,2)]
infosys_data<-infosys_data[,c(1,2)]
sbi_data<-sbi_data[,c(1,2)]

predknn_cognizant <- knn_forecasting(cognizant_data$y, h = 60, lags = 1:30, k = 40, msas = "MIMO")

predknn_hcl <- knn_forecasting(hcl_data$y, h = 60, lags = 1:30, k = 40, msas = "MIMO")

predknn_hdfc <- knn_forecasting(hdfc_data$y, h = 60, lags = 1:30, k = 40, msas = "MIMO")

predknn_icici <- knn_forecasting(icici_data$y, h = 60, lags = 1:30, k = 40, msas = "MIMO")

predknn_infosys <- knn_forecasting(infosys_data$y, h = 60, lags = 1:30, k = 40, msas = "MIMO")

predknn_sbi <- knn_forecasting(sbi_data$y, h = 60, lags = 1:30, k = 40, msas = "MIMO")


ro_cognizant <- rolling_origin(predknn_cognizant)
ro_cognizant$global_accu

ro_hcl <- rolling_origin(predknn_hcl)
ro_hcl$global_accu

ro_hdfc <- rolling_origin(predknn_hdfc)
ro_hdfc$global_accu

ro_icici <- rolling_origin(predknn_icici)
ro_icici$global_accu

ro_infosys <- rolling_origin(predknn_infosys)
ro_infosys$global_accu

ro_sbi <- rolling_origin(predknn_sbi)
ro_sbi$global_accu


ro_cognizant$predictions




autoplot(predknn_cognizant)+my_theme+
  labs(x="Time",y="Close Value",title = "Forecast Share Prices(Cognizant)")

autoplot(predknn_hcl)+my_theme+
  labs(x="Time",y="Close Value",title = "Forecast Share Prices(HCL)")


autoplot(predknn_hdfc)+my_theme+
  labs(x="Time",y="Close Value",title = "Forecast Share Prices(HDFC)")


autoplot(predknn_icici)+my_theme+
  labs(x="Time",y="Close Value",title = "Forecast Share Prices(ICICI)")


autoplot(predknn_infosys)+my_theme+
  labs(x="Time",y="Close Value",title = "Forecast Share Prices(Infosys)")



autoplot(predknn_sbi)+my_theme+
  labs(x="Time",y="Close Value",title = "Forecast Share Prices(SBI)")



#####

alpha <- 1.5^(-10)

hn_1 <- length(cognizant_data)/(alpha*(length(cognizant_data)+30))

hn_2 <- length(hcl_data)/(alpha*(length(hcl_data)+30))

hn_3 <- length(hdfc_data)/(alpha*(length(hdfc_data)+30))

hn_4 <- length(icici_data)/(alpha*(length(icici_data)+30))

hn_5 <- length(infosys_data)/(alpha*(length(infosys_data)+30))

hn_6 <- length(sbi_data)/(alpha*(length(sbi_data)+30))


lambda_1 <- BoxCox.lambda(cognizant_data)

dnn_pred_cognizant <- nnetar(cognizant_data$y, size= hn_1, lambda = lambda_1)

dnn_forecast_cognizant <- forecast(dnn_pred_cognizant, h= 60, PI = TRUE)

plot(dnn_forecast_cognizant,xlab="Time",ylab="Close Values",main="Forecast share values(Cognizant)")






lambda_2 <- BoxCox.lambda(hcl_data)

dnn_pred_hcl <- nnetar(hcl_data$y, size= hn_2, lambda = lambda_2)

dnn_forecast_hcl <- forecast(dnn_pred_hcl, h= 60, PI = TRUE)

plot(dnn_forecast_hcl,xlab="Time",ylab="Close Values",main="Forecast share values(HCL)")




lambda_3 <- BoxCox.lambda(hdfc_data)

dnn_pred_hdfc <- nnetar(hdfc_data$y, size= hn_3, lambda = lambda_3)

dnn_forecast_hdfc <- forecast(dnn_pred_hdfc, h= 60, PI = TRUE)

plot(dnn_forecast_hdfc,xlab="Time",ylab="Close Values",main="Forecast share values(HDFC)")




lambda_4 <- BoxCox.lambda(icici_data)

dnn_pred_icici <- nnetar(icici_data$y, size= hn_4, lambda = lambda_4)

dnn_forecast_icici <- forecast(dnn_pred_icici, h= 60, PI = TRUE)

plot(dnn_forecast_icici,xlab="Time",ylab="Close Values",main="Forecast share values(ICICI)")



lambda_5 <- BoxCox.lambda(infosys_data)

dnn_pred_infosys <- nnetar(infosys_data$y, size= hn_5, lambda = lambda_5)

dnn_forecast_infosys <- forecast(dnn_pred_infosys, h= 60, PI = TRUE)

plot(dnn_forecast_infosys,xlab="Time",ylab="Close Values",main="Forecast share values(Infosys)")



lambda_6 <- BoxCox.lambda(sbi_data)

dnn_pred_sbi <- nnetar(sbi_data$y, size= hn_6, lambda = lambda_6)

dnn_forecast_sbi <- forecast(dnn_pred_sbi, h= 60, PI = TRUE)

plot(dnn_forecast_sbi,xlab="Time",ylab="Close Values",main="Forecast share values(SBI)")


cognizant_data$dnn<-dnn_forecast_cognizant$fitted

hcl_data$dnn<-dnn_forecast_hcl$fitted

hdfc_data$dnn<-dnn_forecast_hdfc$fitted







accuracy(cognizant_data$dnn,cognizant_data$y)

accuracy(dnn_forecast_sbi$fitted,sbi_data$y)

accuracy(dnn_forecast_hdfc$fitted,hdfc_data$y)


## Forecasts




