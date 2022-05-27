library(dplyr)
library(data.table)
library(readr)
library(tidyr)
library(tidyverse)
library("gridExtra")
library(writexl)
library(gridExtra)
install.packages("ggpubr")

# Load in Data

fulldata <- read_csv("Temp_1900-2050.csv",col_names=c("row","lat","lon","db1","db2","db3","db4",
              "db5","db6","db7","db8","db9","db10","db11","db12","db13","db14","db15","db16",
              "db17","db18","db19","db20","db21","db22","db23","db24","db25","db26","db27","db28",
              "db29","db30","db31","db32","db33","db34","db35","db36","db37","db38","db39","db40",
              "db41","db42","db43","db44","db45","db46"),skip=2)

realAMOdata <- read.csv("actualAMO.csv")

# Data Cleaning 

historicaltemp<-fulldata[!(fulldata$lat==1 | fulldata$lat==2 |fulldata$lat==3 |
                             fulldata$lat==4 | fulldata$lat==5 | fulldata$lat==6 |
                             fulldata$lat==7 | fulldata$lat==8 | fulldata$lat==9 |
                             fulldata$lat==10 | fulldata$lat==11 | fulldata$lat==12),]
historicaltemp$year<-rep(1900:2050, each=78852)
historicaltemp$month<-rep(1:12,each=6571,151)

# Data Manipulation 

NAhistoricaltemp <- historicaltemp %>% filter(lat>0)

realhistoricalAMO <- realAMOdata %>% 
  filter(year<=2050) %>%
  filter(year>=1900) %>%
  group_by(year) 

# Calculate Global Anomaly time series (db1)

globaltemp <- historicaltemp %>% filter(lat<0)

globalhisttempdb1 <- globaltemp %>% 
  filter(year<=2014) %>%
  filter(db1!=0)%>%
  group_by(year) %>%
  summarise_at(vars(db1), list(avg_temp = mean))

avgglobaltempdb1 <- mean(globalhisttempdb1$avg_temp)

globalanomaly<-mutate(globalhisttempdb1, globaldiff = (globalhisttempdb1$avg_temp - avgglobaltempdb1))

# Recreate Historical AMO (db1)

histtempdb1 <- NAhistoricaltemp %>% 
  filter(year<=2014) %>%
  filter(db1>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db1), list(avg_temp = mean))

#head(histtempdb1)
#df.wide <- pivot_wider(histtempdb1, names_from = month, values_from = avg_temp) 
#head(df.wide)
#write.table(df.wide , file = "GFDL_AMO.csv")
avgtempdb1 <- mean(histtempdb1$avg_temp)

# Historical AMO (db1 - NOT detrended)

db1histamo<-mutate(histtempdb1, db1diff = histtempdb1$avg_temp - avgtempdb1) %>%
  mutate(histtempdb1, realavgAMO = realhistoricalAMO$average) %>%
  mutate(pos = db1diff >= 0) %>%
  mutate(pos2 = realavgAMO >= 0)

# recreated

ggplot(db1histamo,aes(x=year,y=db1diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")

# real and recreated

ndhistdb1<-ggplot(db1histamo, aes(year)) + 
  geom_col(aes(y = realavgAMO, fill=pos2)) + 
  scale_fill_manual(values = c("royalblue2", "red"), guide = FALSE)+
  geom_line(aes(y = db1diff))+scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("Accepted AMO Index with Recreated AMO overlay ")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+
  xlab("Year")+
  ylab("SST °C")

ndhistdb1

# Historical AMO (db1 - detrended by global anomaly)

db1amo2<-mutate(histtempdb1, db1diff = (histtempdb1$avg_temp - avgtempdb1 - globalanomaly$globaldiff)) %>%
  mutate(histtempdb1, realavgAMO = realhistoricalAMO$average) %>%
  mutate(pos = db1diff >= 0) %>%
  mutate(pos2 = realavgAMO >= 0)

# recreated

ggplot(db1amo2,aes(x=year,y=db1diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("royalblue2", "red2"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index Detrended")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")

# real and recreated

dhistdb1<-ggplot(db1amo2, aes(year,fill=pos2)) + 
  geom_col(aes(y = realavgAMO)) + 
  scale_fill_manual(values = c("royalblue2", "blue","red"), guide = FALSE)+
  geom_line(aes(y = db1diff, fill = "black"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("Recreated Detrended AMO Index with Accepted AMO overlay")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+
  xlab("Year")+
  ylab("SST °C")

dhistdb1

# Comparison grid (real vs. recreated Historical AMO detrended/not detrended)

grid.arrange(ndhistdb1, dhistdb1)


#Checking Script

checkdata <- read_csv("Temp_1958-2012.csv",col_names=c("row","lat","lon","db1","db2","db3","db4",
                                                      "db5","db6","db7","db8","db9","db10","db11","db12","db13","db14","db15","db16",
                                                      "db17","db18","db19","db20","db21","db22","db23","db24","db25","db26","db27","db28",
                                                      "db29","db30","db31","db32","db33","db34","db35","db36","db37","db38","db39","db40",
                                                      "db41","db42","db43","db44","db45","db46"),skip=2)


historicaltemps<-checkdata[!(checkdata$lat==1 | checkdata$lat==2 |checkdata$lat==3 |
                            checkdata$lat==4 | checkdata$lat==5 | checkdata$lat==6 |
                              checkdata$lat==7 | checkdata$lat==8 | checkdata$lat==9 |
                              checkdata$lat==10 | checkdata$lat==11 | checkdata$lat==12),]
historicaltemps$year<-rep(1958:2012, each=78852)
historicaltemps$month<-rep(1:12,each=6571,55)

NAhistoricaltemps <- historicaltemps %>% filter(lat>0)

histtempdb1s <- NAhistoricaltemps %>% 
  filter(db1!=0)%>%
  group_by(year) %>%
  summarise_at(vars(db1), list(avg_temp = mean))

avgtempdb1s <- mean(histtempdb1s$avg_temp)

globaltemps <- historicaltemps %>% filter(lat<0)

globalhisttempdb1s <- globaltemps %>% 
  filter(db1!=0)%>%
  group_by(year) %>%
  summarize(avg_temp = mean(db1))

avgglobaltempdb1s <- mean(globalhisttempdb1s$avg_temp)

globalanomalys<-mutate(globalhisttempdb1s, globaldiff = (globalhisttempdb1s$avg_temp - avgglobaltempdb1s))

# Historical AMO (db1 - NOT detrended)

db1histamos<-mutate(histtempdb1s, db1diff = histtempdb1s$avg_temp - avgtempdb1s)
ggplot(db1histamos,aes(x=year,y=db1diff))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")

### Data Diagnostics

hist1<-ggplot(fulldata, aes(x=db1)) + 
  geom_histogram(color="black", fill="lightblue") +
  ggtitle("Raw GFDL Histogram of SST values")+
  xlab("Depth Bin 1")

hist2<-ggplot(checkdata, aes(x=db1)) + 
  geom_histogram(color="black", fill="lightblue") +
  ggtitle("Raw CESM Histogram of SST values")+
  xlab("Depth Bin 1")

grid.arrange(hist1, hist2)



GFDL_monthly_clean <- historicaltemp %>% 
  filter(year<=2014) %>%
  filter(year>=1900) %>%
  filter(db1!=0)%>%
  filter(db1>0)%>%
  group_by(year,month) %>%
  summarise_at(vars(db1), list(GFDL_avg_temp = mean)) 


CESM_monthly_clean <- historicaltemps %>% 
  filter(db1!=0)%>%
  filter(db1>0)%>%
  group_by(year,month) %>%
  summarise_at(vars(db1), list(CESM_avg_temp = mean))

GFDL_yearly_clean <- historicaltemp %>% 
  filter(year<=2014) %>%
  filter(year>=1900) %>%
  filter(db1!=0)%>%
  filter(db1>0)%>%
  group_by(year) %>%
  summarise_at(vars(db1), list(GFDL_avg_temp = mean)) 

CESM_yearly_clean <- historicaltemps %>% 
  filter(db1!=0)%>%
  filter(db1>0)%>%
  group_by(year) %>%
  summarise_at(vars(db1), list(CESM_avg_temp = mean))


write.table(GFDL_monthly_clean , file = "GFDL_monthly_clean.csv")
write.table(CESM_monthly_clean , file = "CESM_monthly_clean.csv")
write.table(GFDL_yearly_clean , file = "GFDL_yearly_clean.csv")
write.table(CESM_yearly_clean , file = "CESM_yearly_clean")

write.table(db1histamo, file="GFDLSST.csv", row.names=FALSE)
write.table(db1amo2, file="GFDLSSTD.csv", row.names=FALSE)

head(db1amo2)
####
####
####

# AMO By Depth Bin 

####
####
####

###  Depth Bin 1

histtempdb1 <- NAhistoricaltemp %>% 
  filter(year>=1960) %>%
  group_by(year) %>%
  summarise_at(vars(db1), list(avg_temp = mean))
avgtempdb1 <- mean(histtempdb1$avg_temp)
db1histamo<-mutate(histtempdb1, db1diff = histtempdb1$avg_temp - avgtempdb1) %>%
  mutate(pos = db1diff >= 0) 
db1<-ggplot(db1histamo,aes(x=year,y=db1diff,fill=pos))+
  geom_col(position = "identity", colour="black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("0 - 5m")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("Temp °C")
db1
?ylim
view(db1histamo)
db1normal<-ggplot(db1histamo,aes(x=year,y=avg_temp))+
  geom_line(position = "identity", colour = "deeppink", size = 0.75)+
  geom_hline(yintercept=avgtempdb1)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("Average Annual SST")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("Temp °C")
db1normal
avgtempdb1

range(db40histamo$db40diff)
0.1083303+0.3277290
### Depth Bin 2 

histtempdb2 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db2>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db2), list(avg_temp = mean))
avgtempdb2 <- mean(histtempdb2$avg_temp)
db2histamo<-mutate(histtempdb2, db2diff = histtempdb2$avg_temp - avgtempdb2) %>%
  mutate(pos = db2diff >= 0) 
db2<-ggplot(db2histamo,aes(x=year,y=db2diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 5-15m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db2

### Depth Bin 3

histtempdb3 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db3>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db3), list(avg_temp = mean))
avgtempdb3 <- mean(histtempdb3$avg_temp)
db3histamo<-mutate(histtempdb3, db3diff = histtempdb3$avg_temp - avgtempdb3) %>%
  mutate(pos = db3diff >= 0) 
db3<-ggplot(db3histamo,aes(x=year,y=db3diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 15-25m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db3

### Depth Bin 4

histtempdb4 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db4>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db4), list(avg_temp = mean))
avgtempdb4 <- mean(histtempdb4$avg_temp)
db4histamo<-mutate(histtempdb4, db4diff = histtempdb4$avg_temp - avgtempdb4) %>%
  mutate(pos = db4diff >= 0) 
db4<-ggplot(db4histamo,aes(x=year,y=db4diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 25-35m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db4

### Depth Bin 5

histtempdb5 <- NAhistoricaltemp %>% 
  filter(year>=1960) %>%
  filter(db5>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db5), list(avg_temp = mean))
avgtempdb5 <- mean(histtempdb5$avg_temp)
db5histamo<-mutate(histtempdb5, db5diff = histtempdb5$avg_temp - avgtempdb5) %>%
  mutate(pos = db5diff >= 0) 
db5<-ggplot(db5histamo,aes(x=year,y=db5diff,fill=pos))+
  geom_col(position = "identity", colour="black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("35 - 45m")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("")
db5

### Depth Bin 6

histtempdb6 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db6>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db6), list(avg_temp = mean))
avgtempdb6 <- mean(histtempdb6$avg_temp)
db6histamo<-mutate(histtempdb6, db6diff = histtempdb6$avg_temp - avgtempdb6) %>%
  mutate(pos = db6diff >= 0) 
db6<-ggplot(db6histamo,aes(x=year,y=db6diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 45-55m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db6

### Depth Bin 7

histtempdb7 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db7>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db7), list(avg_temp = mean))
avgtempdb7 <- mean(histtempdb7$avg_temp)
db7histamo<-mutate(histtempdb7, db7diff = histtempdb7$avg_temp - avgtempdb7) %>%
  mutate(pos = db7diff >= 0) 
db7<-ggplot(db7histamo,aes(x=year,y=db7diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 55-65m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db7

### Depth Bin 8

histtempdb8 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db8>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db8), list(avg_temp = mean))
avgtempdb8 <- mean(histtempdb8$avg_temp)
db8histamo<-mutate(histtempdb8, db8diff = histtempdb8$avg_temp - avgtempdb8) %>%
  mutate(pos = db8diff >= 0) 
db8<-ggplot(db8histamo,aes(x=year,y=db8diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 65-75m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db8

### Depth Bin 9

histtempdb9 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db9>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db9), list(avg_temp = mean))
avgtempdb9 <- mean(histtempdb9$avg_temp)
db9histamo<-mutate(histtempdb9, db9diff = histtempdb9$avg_temp - avgtempdb9) %>%
  mutate(pos = db9diff >= 0) 
db9<-ggplot(db9histamo,aes(x=year,y=db9diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 75-85m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db9

### Depth Bin 10

histtempdb10 <- NAhistoricaltemp %>% 
  filter(year>=1960) %>%
  filter(db10>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db10), list(avg_temp = mean))
avgtempdb10 <- mean(histtempdb10$avg_temp)
db10histamo<-mutate(histtempdb10, db10diff = histtempdb10$avg_temp - avgtempdb10) %>%
  mutate(pos = db10diff >= 0) 
db10<-ggplot(db10histamo,aes(x=year,y=db10diff,fill=pos))+
  geom_col(position = "identity", colour="black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("89 - 95m")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("")
db10

### Depth Bin 11

histtempdb11 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db11>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db11), list(avg_temp = mean))
avgtempdb11 <- mean(histtempdb11$avg_temp)
db11histamo<-mutate(histtempdb11, db11diff = histtempdb11$avg_temp - avgtempdb11) %>%
  mutate(pos = db11diff >= 0) 
db11<-ggplot(db11histamo,aes(x=year,y=db11diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 95-105m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db11

### Depth Bin 12

histtempdb12 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db12>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db12), list(avg_temp = mean))
avgtempdb12 <- mean(histtempdb12$avg_temp)
db12histamo<-mutate(histtempdb12, db12diff = histtempdb12$avg_temp - avgtempdb12) %>%
  mutate(pos = db12diff >= 0) 
db12<-ggplot(db12histamo,aes(x=year,y=db12diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 105-115m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db12

### Depth Bin 13

histtempdb13 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db13>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db13), list(avg_temp = mean))
avgtempdb13 <- mean(histtempdb13$avg_temp)
db13histamo<-mutate(histtempdb13, db13diff = histtempdb13$avg_temp - avgtempdb13) %>%
  mutate(pos = db13diff >= 0) 
db13<-ggplot(db13histamo,aes(x=year,y=db13diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 115-125m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db13

### Depth Bin 14

histtempdb14 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db14>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db14), list(avg_temp = mean))
avgtempdb14 <- mean(histtempdb14$avg_temp)
db14histamo<-mutate(histtempdb14, db14diff = histtempdb14$avg_temp - avgtempdb14) %>%
  mutate(pos = db14diff >= 0) 
db14<-ggplot(db14histamo,aes(x=year,y=db14diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 125-135m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db14

### Depth Bin 15

histtempdb15 <- NAhistoricaltemp %>% 
  filter(year>=1960) %>%
  filter(db15>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db15), list(avg_temp = mean))
avgtempdb15 <- mean(histtempdb15$avg_temp)
db15histamo<-mutate(histtempdb15, db15diff = histtempdb15$avg_temp - avgtempdb15) %>%
  mutate(pos = db15diff >= 0) 
db15<-ggplot(db15histamo,aes(x=year,y=db15diff,fill=pos))+
  geom_col(position = "identity", colour="black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("135 - 145m")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("Temp °C")
db15

### Depth Bin 16

histtempdb16 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db16>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db16), list(avg_temp = mean))
avgtempdb16 <- mean(histtempdb16$avg_temp)
db16histamo<-mutate(histtempdb16, db16diff = histtempdb16$avg_temp - avgtempdb16) %>%
  mutate(pos = db16diff >= 0) 
db16<-ggplot(db16histamo,aes(x=year,y=db16diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 145-155m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db16

### Depth Bin 17

histtempdb17 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db17>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db17), list(avg_temp = mean))
avgtempdb17 <- mean(histtempdb17$avg_temp)
db17histamo<-mutate(histtempdb17, db17diff = histtempdb17$avg_temp - avgtempdb17) %>%
  mutate(pos = db17diff >= 0) 
db17<-ggplot(db17histamo,aes(x=year,y=db17diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 155-165m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db17

### Depth Bin 18

histtempdb18 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db18>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db18), list(avg_temp = mean))
avgtempdb18 <- mean(histtempdb18$avg_temp)
db18histamo<-mutate(histtempdb18, db18diff = histtempdb18$avg_temp - avgtempdb18) %>%
  mutate(pos = db18diff >= 0) 
db18<-ggplot(db18histamo,aes(x=year,y=db18diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 165-175m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db18

### Depth Bin 19

histtempdb19 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db19>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db19), list(avg_temp = mean))
avgtempdb19 <- mean(histtempdb19$avg_temp)
db19histamo<-mutate(histtempdb19, db19diff = histtempdb19$avg_temp - avgtempdb19) %>%
  mutate(pos = db19diff >= 0) 
db19<-ggplot(db19histamo,aes(x=year,y=db19diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 175-186m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db19

### Depth Bin 20

histtempdb20 <- NAhistoricaltemp %>% 
  filter(year>=1960) %>%
  filter(db20>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db20), list(avg_temp = mean))
avgtempdb20 <- mean(histtempdb20$avg_temp)
db20histamo<-mutate(histtempdb20, db20diff = histtempdb20$avg_temp - avgtempdb20) %>%
  mutate(pos = db20diff >= 0) 
db20<-ggplot(db20histamo,aes(x=year,y=db20diff,fill=pos))+
  geom_col(position = "identity", colour="black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("186 - 197m")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("")
db20

### Depth Bin 21

histtempdb21 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db21>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db21), list(avg_temp = mean))
avgtempdb21 <- mean(histtempdb21$avg_temp)
db21histamo<-mutate(histtempdb21, db21diff = histtempdb21$avg_temp - avgtempdb21) %>%
  mutate(pos = db21diff >= 0) 
db21<-ggplot(db21histamo,aes(x=year,y=db21diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 197-209m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db21

### Depth Bin 22

histtempdb22 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db22>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db22), list(avg_temp = mean))
avgtempdb22 <- mean(histtempdb22$avg_temp)
db22histamo<-mutate(histtempdb22, db22diff = histtempdb22$avg_temp - avgtempdb22) %>%
  mutate(pos = db22diff >= 0) 
db22<-ggplot(db22histamo,aes(x=year,y=db22diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 209-222m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db22

### Depth Bin 23

histtempdb23 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db23>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db23), list(avg_temp = mean))
avgtempdb23 <- mean(histtempdb23$avg_temp)
db23histamo<-mutate(histtempdb23, db23diff = histtempdb23$avg_temp - avgtempdb23) %>%
  mutate(pos = db23diff >= 0) 
db23<-ggplot(db23histamo,aes(x=year,y=db23diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 222-236m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db23

### Depth Bin 24

histtempdb24 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db24>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db24), list(avg_temp = mean))
avgtempdb24 <- mean(histtempdb24$avg_temp)
db24histamo<-mutate(histtempdb24, db24diff = histtempdb24$avg_temp - avgtempdb24) %>%
  mutate(pos = db24diff >= 0) 
db24<-ggplot(db24histamo,aes(x=year,y=db24diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 236-251m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db24

### Depth Bin 25

histtempdb25 <- NAhistoricaltemp %>% 
  filter(year>=1960) %>%
  filter(db25>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db25), list(avg_temp = mean))
avgtempdb25 <- mean(histtempdb25$avg_temp)
db25histamo<-mutate(histtempdb25, db25diff = histtempdb25$avg_temp - avgtempdb25) %>%
  mutate(pos = db25diff >= 0) 
db25<-ggplot(db25histamo,aes(x=year,y=db25diff,fill=pos))+
  geom_col(position = "identity", colour="black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("251 - 267m")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("")
db25

### Depth Bin 26

histtempdb26 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db26>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db26), list(avg_temp = mean))
avgtempdb26 <- mean(histtempdb26$avg_temp)
db26histamo<-mutate(histtempdb26, db26diff = histtempdb26$avg_temp - avgtempdb26) %>%
  mutate(pos = db26diff >= 0) 
db26<-ggplot(db26histamo,aes(x=year,y=db26diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 267-285m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db26

### Depth Bin 27

histtempdb27 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db27>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db27), list(avg_temp = mean))
avgtempdb27 <- mean(histtempdb27$avg_temp)
db27histamo<-mutate(histtempdb27, db27diff = histtempdb27$avg_temp - avgtempdb27) %>%
  mutate(pos = db27diff >= 0) 
db27<-ggplot(db27histamo,aes(x=year,y=db27diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 285-305m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db27

### Depth Bin 28

histtempdb28 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db28>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db28), list(avg_temp = mean))
avgtempdb28 <- mean(histtempdb28$avg_temp)
db28histamo<-mutate(histtempdb28, db28diff = histtempdb28$avg_temp - avgtempdb28) %>%
  mutate(pos = db28diff >= 0) 
db28<-ggplot(db28histamo,aes(x=year,y=db28diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 305-326m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db28

### Depth Bin 29

histtempdb29 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db29>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db29), list(avg_temp = mean))
avgtempdb29 <- mean(histtempdb29$avg_temp)
db29histamo<-mutate(histtempdb29, db29diff = histtempdb29$avg_temp - avgtempdb29) %>%
  mutate(pos = db29diff >= 0) 
db29<-ggplot(db29histamo,aes(x=year,y=db29diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 326-351m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db29

### Depth Bin 30

histtempdb30 <- NAhistoricaltemp %>% 
  filter(year>=1960) %>%
  filter(db30>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db30), list(avg_temp = mean))
avgtempdb30 <- mean(histtempdb30$avg_temp)
db30histamo<-mutate(histtempdb30, db30diff = histtempdb30$avg_temp - avgtempdb30) %>%
  mutate(pos = db30diff >= 0) 
db30<-ggplot(db30histamo,aes(x=year,y=db30diff,fill=pos))+
  geom_col(position = "identity", colour="black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("351 - 378m")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("Temp °C")
db30

### Depth Bin 31

histtempdb31 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db31>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db31), list(avg_temp = mean))
avgtempdb31 <- mean(histtempdb31$avg_temp)
db31histamo<-mutate(histtempdb31, db31diff = histtempdb31$avg_temp - avgtempdb31) %>%
  mutate(pos = db31diff >= 0) 
db31<-ggplot(db31histamo,aes(x=year,y=db31diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 378-408m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db31

### Depth Bin 32

histtempdb32 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db32>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db32), list(avg_temp = mean))
avgtempdb32 <- mean(histtempdb32$avg_temp)
db32histamo<-mutate(histtempdb32, db32diff = histtempdb32$avg_temp - avgtempdb32) %>%
  mutate(pos = db32diff >= 0) 
db32<-ggplot(db32histamo,aes(x=year,y=db32diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 408-443m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db32

### Depth Bin 33

histtempdb33 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db33>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db33), list(avg_temp = mean))
avgtempdb33 <- mean(histtempdb33$avg_temp)
db33histamo<-mutate(histtempdb33, db33diff = histtempdb33$avg_temp - avgtempdb33) %>%
  mutate(pos = db33diff >= 0) 
db33<-ggplot(db33histamo,aes(x=year,y=db33diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 443-482m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db33

### Depth Bin 34

histtempdb34 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db34>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db34), list(avg_temp = mean))
avgtempdb34 <- mean(histtempdb34$avg_temp)
db34histamo<-mutate(histtempdb34, db34diff = histtempdb34$avg_temp - avgtempdb34) %>%
  mutate(pos = db34diff >= 0) 
db34<-ggplot(db34histamo,aes(x=year,y=db34diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 482-527m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db34

### Depth Bin 35

histtempdb35 <- NAhistoricaltemp %>% 
  filter(year>=1960) %>%
  filter(db35>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db35), list(avg_temp = mean))
avgtempdb35 <- mean(histtempdb35$avg_temp)
db35histamo<-mutate(histtempdb35, db35diff = histtempdb35$avg_temp - avgtempdb35) %>%
  mutate(pos = db35diff >= 0) 
db35<-ggplot(db35histamo,aes(x=year,y=db35diff,fill=pos))+
  geom_col(position = "identity", colour="black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("527 - 579m")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("")
db35

### Depth Bin 36

histtempdb36 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db36>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db36), list(avg_temp = mean))
avgtempdb36 <- mean(histtempdb36$avg_temp)
db36histamo<-mutate(histtempdb36, db36diff = histtempdb36$avg_temp - avgtempdb36) %>%
  mutate(pos = db36diff >= 0) 
db36<-ggplot(db36histamo,aes(x=year,y=db36diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 579-638m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db36

### Depth Bin 37

histtempdb37 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db37>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db37), list(avg_temp = mean))
avgtempdb37 <- mean(histtempdb37$avg_temp)
db37histamo<-mutate(histtempdb37, db37diff = histtempdb37$avg_temp - avgtempdb37) %>%
  mutate(pos = db37diff >= 0) 
db37<-ggplot(db37histamo,aes(x=year,y=db37diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 638-707m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db37

### Depth Bin 38

histtempdb38 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db38>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db38), list(avg_temp = mean))
avgtempdb38 <- mean(histtempdb38$avg_temp)
db38histamo<-mutate(histtempdb38, db38diff = histtempdb38$avg_temp - avgtempdb38) %>%
  mutate(pos = db38diff >= 0) 
db38<-ggplot(db38histamo,aes(x=year,y=db38diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 707-787m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db38

### Depth Bin 39

histtempdb39 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db39>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db39), list(avg_temp = mean))
avgtempdb39 <- mean(histtempdb39$avg_temp)
db39histamo<-mutate(histtempdb39, db39diff = histtempdb39$avg_temp - avgtempdb39) %>%
  mutate(pos = db39diff >= 0) 
db39<-ggplot(db39histamo,aes(x=year,y=db39diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 787-878m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db39

### Depth Bin 40

histtempdb40 <- NAhistoricaltemp %>% 
  filter(year>=1960) %>%
  filter(db40>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db40), list(avg_temp = mean))
avgtempdb40 <- mean(histtempdb40$avg_temp)
db40histamo<-mutate(histtempdb40, db40diff = histtempdb40$avg_temp - avgtempdb40) %>%
  mutate(pos = db40diff >= 0) 
db40<-ggplot(db40histamo,aes(x=year,y=db40diff,fill=pos))+
  geom_col(position = "identity", colour="black", size=0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("878 - 984m")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("")
db40

db40histamo
write.table(db1histamo, file="1.csv", row.names=FALSE)
write.table(db5histamo, file="2.csv", row.names=FALSE)
write.table(db10histamo, file="3.csv", row.names=FALSE)
write.table(db15histamo, file="4.csv", row.names=FALSE)
write.table(db20histamo, file="5.csv", row.names=FALSE)
write.table(db25histamo, file="6.csv", row.names=FALSE)
write.table(db30histamo, file="7.csv", row.names=FALSE)
write.table(db35histamo, file="8.csv", row.names=FALSE)
write.table(db40histamo, file="9.csv", row.names=FALSE)

### Depth Bin 41

histtempdb41 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db41>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db41), list(avg_temp = mean))
avgtempdb41 <- mean(histtempdb41$avg_temp)
db41histamo<-mutate(histtempdb41, db41diff = histtempdb41$avg_temp - avgtempdb41) %>%
  mutate(pos = db41diff >= 0) 
db41<-ggplot(db41histamo,aes(x=year,y=db41diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 984-1106m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db41

### Depth Bin 42

histtempdb42 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db42>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db42), list(avg_temp = mean))
avgtempdb42 <- mean(histtempdb42$avg_temp)
db42histamo<-mutate(histtempdb42, db42diff = histtempdb42$avg_temp - avgtempdb42) %>%
  mutate(pos = db42diff >= 0) 
db42<-ggplot(db42histamo,aes(x=year,y=db42diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 1106-1244m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db42

### Depth Bin 43

histtempdb43 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db43>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db43), list(avg_temp = mean))
avgtempdb43 <- mean(histtempdb43$avg_temp)
db43histamo<-mutate(histtempdb43, db43diff = histtempdb43$avg_temp - avgtempdb43) %>%
  mutate(pos = db43diff >= 0) 
db43<-ggplot(db43histamo,aes(x=year,y=db43diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 1244-1400m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db43

### Depth Bin 44

histtempdb44 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db44>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db44), list(avg_temp = mean))
avgtempdb44 <- mean(histtempdb44$avg_temp)
db44histamo<-mutate(histtempdb44, db44diff = histtempdb44$avg_temp - avgtempdb44) %>%
  mutate(pos = db44diff >= 0) 
db44<-ggplot(db44histamo,aes(x=year,y=db44diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 1400-1573m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db44

### Depth Bin 45

histtempdb45 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db45>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db45), list(avg_temp = mean))
avgtempdb45 <- mean(histtempdb45$avg_temp)
db45histamo<-mutate(histtempdb45, db45diff = histtempdb45$avg_temp - avgtempdb45) %>%
  mutate(pos = db45diff >= 0) 
db45<-ggplot(db45histamo,aes(x=year,y=db45diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 1573-1764m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")+
  ylim(-0.50,0.60)
db45

### Depth Bin 46

histtempdb46 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db46>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db46), list(avg_temp = mean))
avgtempdb46 <- mean(histtempdb46$avg_temp)
db46histamo<-mutate(histtempdb46, db46diff = histtempdb46$avg_temp - avgtempdb46) %>%
  mutate(pos = db46diff >= 0) 
db46<-ggplot(db46histamo,aes(x=year,y=db46diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 1764-1968m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")
db46

### Comparing Depth Bins

grid.arrange(db1,db2,db3,db4,db5,db6,db7,db8,db9,db10,db11,db12,db13,db14,db15,db16,
             db17,db18,db19,db20,db21,db22,db23,db24,db25,db26,db27,db28,
             db29,db30,db31,db32,db33,db34,db35,db36,db37,db38,db39,db40,
             db41,db42,db43,db44,db45,db46,ncol=6)

grid.arrange(db1,db5,db10,db15,db20,db25,db30,db35,db40)


# Map different depth bins in warm, cold and neutral periods - what to look for?
# Regime Shift detection / Software Comparison - Whats the goal?
# Timeline? Detrended into future
# Project to 2050 -> Maps 
#############################################################3

##Regime Shift Detection

install.packages("rshift")
library(rshift)
?rshift
vignette("STARSmanual")

histtempdb1 <- NAhistoricaltemp %>%
  filter(year<=2022) %>%
  filter(db1>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db1), list(avg_temp = mean))
avgtempdb1 <- mean(histtempdb1$avg_temp)
db1histamo<-mutate(histtempdb1, db1diff = histtempdb1$avg_temp - avgtempdb1) %>%
  mutate(pos = db1diff >= 0) 

Rodionov(db1histamo, "db1diff", "year", 10)
RSI_GFDL <- Rodionov(db1histamo, "db1diff", "year", 10, merge = TRUE)
RSI_graph(RSI_GFDL, "db1diff", "year", "RSI")

histtempdb35 <- NAhistoricaltemp %>%
  filter(year<=2022) %>%
  filter(db35>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db35), list(avg_temp = mean))
avgtempdb35 <- mean(histtempdb35$avg_temp)
db35histamo<-mutate(histtempdb35, db35diff = histtempdb35$avg_temp - avgtempdb35) %>%
  mutate(pos = db35diff >= 0)

Rodionov(db35histamo, "db35diff", "year", 5)
RSI_GFDL2 <- Rodionov(db35histamo, "db35diff", "year", 5, merge = TRUE)
RSI_graph(RSI_GFDL2, "db35diff", "year", "RSI")

############
############
#############

#Detrended AMO Plots by Depth bind

###########
###########
###########

globaltemp <- historicaltemp %>% filter(lat<0)

### Depth Bin 1 Detrended

globalhisttempdb1 <- globaltemp %>% 
  filter(year>=1960) %>%
  filter(db1>=0)%>%
  group_by(year) %>%
  summarise_at(vars(db1), list(avg_temp = mean))
histtempdb1 <- NAhistoricaltemp %>% 
  filter(year>=1960) %>%
  filter(db1>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db1), list(avg_temp = mean))
avgglobaltempdb1 <- mean(globalhisttempdb1$avg_temp)
avgtempdb1 <- mean(histtempdb1$avg_temp)
globalanomalydb1<-mutate(globalhisttempdb1, globaldiff = (globalhisttempdb1$avg_temp - avgglobaltempdb1))
db1amo2<-mutate(histtempdb1, db1diff = (histtempdb1$avg_temp - avgtempdb1 - globalanomalydb1$globaldiff)) %>%
  mutate(pos = db1diff >= 0) 

dedb1<-ggplot(db1amo2,aes(x=year,y=db1diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 0-5m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("Temp °C")+
  ylim(-0.50,0.60)
dedb1

### Depth Bin 5 Detrended

globalhisttempdb5 <- globaltemp %>% 
  filter(year<=2022) %>%
  filter(db5>=0)%>%
  group_by(year) %>%
  summarise_at(vars(db5), list(avg_temp = mean))
histtempdb5 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db5>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db5), list(avg_temp = mean))
avgglobaltempdb5 <- mean(globalhisttempdb5$avg_temp)
avgtempdb5 <- mean(histtempdb5$avg_temp)
globalanomalydb5<-mutate(globalhisttempdb5, globaldiff = (globalhisttempdb5$avg_temp - avgglobaltempdb5))
db5amo2<-mutate(histtempdb5, db5diff = (histtempdb5$avg_temp - avgtempdb5 - globalanomalydb5$globaldiff)) %>%
  mutate(pos = db5diff >= 0) 

dedb5<-ggplot(db5amo2,aes(x=year,y=db5diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 35-45m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")+
  ylim(-0.50,0.60)
dedb5

### Depth Bin 10 Detrended

globalhisttempdb10 <- globaltemp %>% 
  filter(year<=2022) %>%
  filter(db10>=0)%>%
  group_by(year) %>%
  summarise_at(vars(db10), list(avg_temp = mean))
histtempdb10 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db10>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db10), list(avg_temp = mean))
avgglobaltempdb10 <- mean(globalhisttempdb10$avg_temp)
avgtempdb10 <- mean(histtempdb10$avg_temp)
globalanomalydb10<-mutate(globalhisttempdb10, globaldiff = (globalhisttempdb10$avg_temp - avgglobaltempdb10))
db10amo2<-mutate(histtempdb10, db10diff = (histtempdb10$avg_temp - avgtempdb10 - globalanomalydb10$globaldiff)) %>%
  mutate(pos = db10diff >= 0) 

dedb10<-ggplot(db10amo2,aes(x=year,y=db10diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 85-95m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")+
  ylim(-0.50,0.60)
dedb10

### Depth Bin 15 Detrended

globalhisttempdb15 <- globaltemp %>% 
  filter(year<=2022) %>%
  filter(db15>=0)%>%
  group_by(year) %>%
  summarise_at(vars(db15), list(avg_temp = mean))
histtempdb15 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db15>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db15), list(avg_temp = mean))
avgglobaltempdb15 <- mean(globalhisttempdb15$avg_temp)
avgtempdb15 <- mean(histtempdb15$avg_temp)
globalanomalydb15<-mutate(globalhisttempdb15, globaldiff = (globalhisttempdb15$avg_temp - avgglobaltempdb15))
db15amo2<-mutate(histtempdb15, db15diff = (histtempdb15$avg_temp - avgtempdb15 - globalanomalydb15$globaldiff)) %>%
  mutate(pos = db15diff >= 0) 

dedb15<-ggplot(db15amo2,aes(x=year,y=db15diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 135-145m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")+
  ylim(-0.50,0.60)
dedb15

### Depth Bin 20 Detrended

globalhisttempdb20 <- globaltemp %>% 
  filter(year<=2022) %>%
  filter(db20>=0)%>%
  group_by(year) %>%
  summarise_at(vars(db20), list(avg_temp = mean))
histtempdb20 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db20>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db20), list(avg_temp = mean))
avgglobaltempdb20 <- mean(globalhisttempdb20$avg_temp)
avgtempdb20 <- mean(histtempdb20$avg_temp)
globalanomalydb20<-mutate(globalhisttempdb20, globaldiff = (globalhisttempdb20$avg_temp - avgglobaltempdb20))
db20amo2<-mutate(histtempdb20, db20diff = (histtempdb20$avg_temp - avgtempdb20 - globalanomalydb20$globaldiff)) %>%
  mutate(pos = db20diff >= 0) 

dedb20<-ggplot(db20amo2,aes(x=year,y=db20diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 186-197m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")+
  ylim(-0.50,0.60)
dedb20

### Depth Bin 25 Detrended

globalhisttempdb25 <- globaltemp %>% 
  filter(year<=2022) %>%
  filter(db25>=0)%>%
  group_by(year) %>%
  summarise_at(vars(db25), list(avg_temp = mean))
histtempdb25 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db25>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db25), list(avg_temp = mean))
avgglobaltempdb25 <- mean(globalhisttempdb25$avg_temp)
avgtempdb25 <- mean(histtempdb25$avg_temp)
globalanomalydb25<-mutate(globalhisttempdb25, globaldiff = (globalhisttempdb25$avg_temp - avgglobaltempdb25))
db25amo2<-mutate(histtempdb25, db25diff = (histtempdb25$avg_temp - avgtempdb25 - globalanomalydb25$globaldiff)) %>%
  mutate(pos = db25diff >= 0) 

dedb25<-ggplot(db25amo2,aes(x=year,y=db25diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 251-267m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")+
  ylim(-0.50,0.60)
dedb25

### Depth Bin 30 Detrended

globalhisttempdb30 <- globaltemp %>% 
  filter(year<=2022) %>%
  filter(db30>=0)%>%
  group_by(year) %>%
  summarise_at(vars(db30), list(avg_temp = mean))
histtempdb30 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db30>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db30), list(avg_temp = mean))
avgglobaltempdb30 <- mean(globalhisttempdb30$avg_temp)
avgtempdb30 <- mean(histtempdb30$avg_temp)
globalanomalydb30<-mutate(globalhisttempdb30, globaldiff = (globalhisttempdb30$avg_temp - avgglobaltempdb30))
db30amo2<-mutate(histtempdb30, db30diff = (histtempdb30$avg_temp - avgtempdb30 - globalanomalydb30$globaldiff)) %>%
  mutate(pos = db30diff >= 0) 

dedb30<-ggplot(db30amo2,aes(x=year,y=db30diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 351-378m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")+
  ylim(-0.50,0.60)
dedb30

### Depth Bin 35 Detrended

globalhisttempdb35 <- globaltemp %>% 
  filter(year<=2022) %>%
  filter(db35>=0)%>%
  group_by(year) %>%
  summarise_at(vars(db35), list(avg_temp = mean))
histtempdb35 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db35>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db35), list(avg_temp = mean))
avgglobaltempdb35 <- mean(globalhisttempdb35$avg_temp)
avgtempdb35 <- mean(histtempdb35$avg_temp)
globalanomalydb35<-mutate(globalhisttempdb35, globaldiff = (globalhisttempdb35$avg_temp - avgglobaltempdb35))
db35amo2<-mutate(histtempdb35, db35diff = (histtempdb35$avg_temp - avgtempdb35 - globalanomalydb35$globaldiff)) %>%
  mutate(pos = db35diff >= 0) 

dedb35<-ggplot(db35amo2,aes(x=year,y=db35diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 527-579m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")+
  ylim(-0.50,0.60)
dedb35

### Depth Bin 40 Detrended

globalhisttempdb40 <- globaltemp %>% 
  filter(year<=2022) %>%
  filter(db40>=0)%>%
  group_by(year) %>%
  summarise_at(vars(db40), list(avg_temp = mean))
histtempdb40 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db40>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db40), list(avg_temp = mean))
avgglobaltempdb40 <- mean(globalhisttempdb40$avg_temp)
avgtempdb40 <- mean(histtempdb40$avg_temp)
globalanomalydb40<-mutate(globalhisttempdb40, globaldiff = (globalhisttempdb40$avg_temp - avgglobaltempdb40))
db40amo2<-mutate(histtempdb40, db40diff = (histtempdb40$avg_temp - avgtempdb40 - globalanomalydb40$globaldiff)) %>%
  mutate(pos = db40diff >= 0) 

dedb40<-ggplot(db40amo2,aes(x=year,y=db40diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 878-984m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")+
  ylim(-0.50,0.60)
dedb40

### Depth Bin 45 Detrended

globalhisttempdb45 <- globaltemp %>% 
  filter(year<=2022) %>%
  filter(db45>=0)%>%
  group_by(year) %>%
  summarise_at(vars(db45), list(avg_temp = mean))
histtempdb45 <- NAhistoricaltemp %>% 
  filter(year<=2022) %>%
  filter(db45>=1)%>%
  group_by(year) %>%
  summarise_at(vars(db45), list(avg_temp = mean))
avgglobaltempdb45 <- mean(globalhisttempdb45$avg_temp)
avgtempdb45 <- mean(histtempdb45$avg_temp)
globalanomalydb45<-mutate(globalhisttempdb45, globaldiff = (globalhisttempdb45$avg_temp - avgglobaltempdb45))
db45amo2<-mutate(histtempdb45, db45diff = (histtempdb45$avg_temp - avgtempdb45 - globalanomalydb45$globaldiff)) %>%
  mutate(pos = db45diff >= 0) 

dedb45<-ggplot(db45amo2,aes(x=year,y=db45diff,fill=pos))+
  geom_col(position = "identity", colour = "black", size = 0.25)+
  scale_fill_manual(values = c("blue", "red"), guide = FALSE)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  ggtitle("AMO Index (Depth Range 1573-1764m)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("SST °C")+
  ylim(-0.50,0.60)
dedb45

###

grid.arrange(dedb1,dedb5,dedb10,dedb15,dedb20,dedb25,dedb30,dedb35,dedb40,dedb45,top=("DETRENDED AMO"))

##################
##################
##################

# Mapping the AMO in Warm and Cool Phases by Depth

#######
#######
#######

library(gsw)
library(oce)
library(ocedata)
library(fields)
library(rgdal)
library(rworldmap)
library(raster)

# Warm Phase - 2013 
# Cool Phase - 1965
# Map at Depths of SST, 200m, and 900m 

### Manipulate into Dataframes

Map2050db <- NAhistoricaltemp %>% 
  filter(year==2050) %>%
  filter(db1>=1) %>%
  filter(db21>=1) %>%
  filter(db39>=1)
Map2050 <- Map2050db[ , c("lat", "lon", "db1", "db21", "db39")]

Map1965db <- NAhistoricaltemp %>% 
  filter(year==1965) %>%
  filter(db1>=1) %>%
  filter(db21>=1) %>%
  filter(db39>=1)
Map1965 <- Map1965db[ , c("lat", "lon", "db1","db21", "db39")]

write.table(Map2013, file = "Map2013.csv",row.names=FALSE)
write.table(Map1965, file = "Map1965.csv",row.names=FALSE)



### Plotting 

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(ggspatial)

my_breaks <- seq(0,30,5)
rng<- range(Map2050$db1)

#### Warm Phase Db1

mapw1<-ggplot() + geom_polygon(data = map_data("world"), aes(x=long, y = lat, group=group)) + 
  geom_raster(data = Map2050, aes(x=lon, y = lat, fill = db1))+
  coord_fixed(xlim = c(-100, 20),  ylim = c(0, 55), ratio = 1.3)+
  scale_fill_gradientn("Temp °C", colours=rainbow(5,rev=TRUE),breaks=my_breaks, labels=my_breaks, limits=rng)+
  theme_bw()+ggtitle("0 - 5m")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  xlab("Lon")+
  ylab("Lat")
mapw1

#### Warm Phase Db21

mapw21<-ggplot() + geom_polygon(data = map_data("world"), aes(x=long, y = lat, group=group)) + 
  geom_raster(data = Map2050, aes(x=lon, y = lat, fill = db21))+
  coord_fixed(xlim = c(-100, 20),  ylim = c(0, 55), ratio = 1.3)+
  scale_fill_gradientn("Temp °C", colours=rainbow(5,rev=TRUE),breaks=my_breaks, labels=my_breaks, limits=rng)+
  theme_bw()+ggtitle("197 - 209m")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  xlab("Lon")+
  ylab("Lat")
mapw21

#### Warm Phase Db39

mapw39<-ggplot() + geom_polygon(data = map_data("world"), aes(x=long, y = lat, group=group)) + 
  geom_raster(data = Map2050, aes(x=lon, y = lat, fill = db39))+
  coord_fixed(xlim = c(-100, 20),  ylim = c(0, 55), ratio = 1.3)+
  scale_fill_gradientn("Temp °C", colours=rainbow(5,rev=TRUE),breaks=my_breaks, labels=my_breaks, limits=rng)+
  theme_bw()+ggtitle("787 - 878m")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  xlab("Lon")+
  ylab("Lat")
mapw39
?scale_fill_gradientn()

#### Cool Phase Db1

mapc1<-ggplot() + geom_polygon(data = map_data("world"), aes(x=long, y = lat, group=group)) + 
  geom_raster(data = Map1965, aes(x=lon, y = lat, fill = db1))+
  coord_fixed(xlim = c(-100, 20),  ylim = c(0, 55), ratio = 1.3)+
  scale_fill_gradientn("Temp °C", colours=rainbow(5,rev=TRUE),breaks=my_breaks, labels=my_breaks, limits=rng)+
  theme_bw()+ggtitle("0 - 5m")+
  theme( plot.title = element_text(hjust = 0.5))+
  xlab("Lon")+
  ylab("Lat")
mapc1

#### Cool Phase Db21

mapc21<-ggplot() + geom_polygon(data = map_data("world"), aes(x=long, y = lat, group=group)) + 
  geom_raster(data = Map1965, aes(x=lon, y = lat, fill = db21))+
  coord_fixed(xlim = c(-100, 20),  ylim = c(0, 55), ratio = 1.3)+
  scale_fill_gradientn("Temp °C", colours=rainbow(5,rev=TRUE),breaks=my_breaks, labels=my_breaks,limits=rng)+
  theme_bw()+ggtitle("197 - 209m")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Lon")+
  ylab("Lat")
mapc21

#### Cool Phase Db39

mapc39<-ggplot() + geom_polygon(data = map_data("world"), aes(x=long, y = lat, group=group)) + 
  geom_raster(data = Map1965, aes(x=lon, y = lat, fill = db39))+
  coord_fixed(xlim = c(-100, 20),  ylim = c(0, 55), ratio = 1.3)+
  scale_fill_gradientn("Temp °C", colours=rainbow(5,rev=TRUE),breaks=my_breaks, labels=my_breaks,limits=rng)+
  theme_bw()+ggtitle("787 - 878m")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Lon")+
  ylab("Lat")
mapc39

### Comparison Grid

grid.arrange(mapw1,mapc1, mapw21,mapc21, mapw39, mapc39,top="Warm Phase (2050)                                                Cool Phase (1965)")

### Bathymetry Plot for Atlantic Ocean

library(ggOceanMaps)

dt <- data.frame(lon = c(-100, -100, 10, 10), lat = c(0, 50, 50, 0))

basemap(data = dt, bathymetry = TRUE) + 
  geom_polygon(data = transform_coord(dt), aes(x = lon, y = lat), color = "red", fill = NA)+ggtitle("Bathymetry of the North Atlantic Ocean")

regime<-read.csv("rsi.csv")

a<-ggplot(regime,aes(x=depth,y=year))+
  geom_smooth(method="loess", colour="deeppink")+
  geom_point()+
  ggtitle("Timing of Cool to Warm Phase Transition")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Depth (m)")+
  ylab("Year of Regime Shift")+
  xlim(0,931)

b<-ggplot(regime,aes(x=depth,y=rsi))+
  geom_col(fill="black")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Depth (m)")+
  ylab("RSI")

grid.arrange(a,b)

