#loading packages
library(ggplot2)
library(ggeasy)
library(ggpubr)
library(gganimate)
library(gifski)
library(transformr)
library(scales)
library(readxl)
library(zoo)
library(xts)

#loading Oxford government response data
Stringency <- read_excel("OxCGRT_timeseries_all.xlsx", sheet = "stringency_index")
ContainmentH <- read_excel("OxCGRT_timeseries_all.xlsx", sheet = "containment_health_index")

#creating time series objects for government response data
ZARstringency = ts(as.numeric(Stringency[which(Stringency$country_code=="ZAF"),25:458]), start = c(2020,23), frequency = 365)
ZARContainmentH = ts(as.numeric(ContainmentH[which(ContainmentH$country_code=="ZAF"),25:458]), start = c(2020,23), frequency = 365)

#loading covid case data
Cases = read.csv("time_series_covid19_confirmed_global.csv")

#creating time series objects for covid case data
ZARCases = ts(as.numeric(Cases[which(Cases$Country.Region=="South Africa"),5:439]), start = c(2020,22), frequency = 365)
DailyCases = diff(ZARCases)

#creating the date series for ggplot
Days = as.Date(colnames(Stringency[,25:458]), "%d%b%Y")
data = data.frame(cbind(as.Date(Days),DailyCases,ZARstringency, ZARContainmentH))

#creating data frame for alert level dates 
alert_levels <- data.frame(Ref = c("Lv5", "Lv4","Lv3","Lv2", "Lv1", "Lv3*"), vals = c(as.Date("2020/03/26"), as.Date("2020/05/01"), as.Date("2020/06/01"), as.Date("2020/08/18"), as.Date("2020/09/21"), as.Date("2020/12/29")), stringsAsFactors = FALSE)

#creating the main graph
pString <- ggplot(data, aes(x=Days))+
  geom_line( aes(y=DailyCases, color = 'Daily cases'), size = 1)+
  geom_line( aes(y=ZARstringency*200, color = 'Stringency'), size = 1)+
  geom_line(aes(y=ZARContainmentH*200, color = "Containment"), size =  1)+
  scale_y_continuous(
    name = "Daily covid-19 cases\n",
    sec.axis = sec_axis(~./200, name = "Index\n", breaks = seq(0,100,10))
  )+
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month")+
  xlab("Date")+
  geom_vline(mapping = aes(xintercept = vals), colour = "black", data = alert_levels, show.legend = FALSE) +
  geom_text(mapping = aes(x = vals, y = 0.45, label = Ref, hjust = -0.2, vjust = -40), colour="black", data = alert_levels, show.legend = FALSE)+
  scale_colour_manual(values=c("red", "steelblue", "green"), name=NULL)+
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5))+
  labs(colour = "")+
  
  #the code below specifies that the graph shold be drawn out using the Days
  transition_reveal(as.Date(Days))

animate(pString, renderer = gifski_renderer(), end_pause = 15)
anim_save("stringency1.gif")