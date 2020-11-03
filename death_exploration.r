

eud$CODE<-as.factor(eud$CODE)
eud$NUTS3<-as.factor(eud$NUTS3)
eud$DWELLINGS<-as.numeric(eud$DWELLINGS)
eud$HOUSEHOLDS<-as.numeric(eud$HOUSEHOLDS)
eud$COVID19_D <-as.numeric(eud$COVID19_D)
eud$COVID19_CCONF <-as.numeric(eud$COVID19_CCONF)
eud$DURATION_LD <-as.numeric(eud$DURATION_LD)
eud$LAG_LD <-as.numeric(eud$LAG_LD)
eud$NO2 <-as.numeric(eud$NO2)
eud$WIND <-as.numeric(eud$WIND)
eud$TEMP <-as.numeric(eud$TEMP)
eud$NLI <-as.numeric(eud$NLI)
eud$PRESSURE <-as.numeric(eud$PRESSURE)
eud$PRECIPITATION <-as.numeric(eud$PRECIPITATION)
eud$SOLAR_RAD <-as.numeric(eud$SOLAR_RAD)
eud$LAI <-as.numeric(eud$LAI)
eud$PM25 <-as.numeric(eud$PM25)


######  outliers ####### 

dotchart(eud$COVID19_D, pch=20, main="COVID DEATHS")
#eud<-eud[eud$COVID19_D<10000 ,]

dotchart(eud$COVID19_CCONF, pch=20, main="POSITIVE CASES")
#eud<-eud[eud$COVID19_CCONF<20000 ,]

dotchart(eud$POPULATION, pch=20, main="POPULATION") ### We keep NUTS3 less than 2 million
#eud<-eud[eud$POPULATION<2000000,]

dotchart(eud$POP_DENS, pch=20, main="POPULATION DENSITY")
dotchart(eud$P_POP_60, pch=20, main="% > 60 YR")
dotchart(eud$P_MALES, pch=20, main="% MALES")
dotchart(eud$HOUSEHOLDS, pch=20, main="HOUSEHOLDS")
dotchart(eud$DWELLINGS, pch=20, main="DWELLINGS")

dotchart(eud$NO2, pch=20, main="NO2")
dotchart(eud$WIND, pch=20, main="WIND")
dotchart(eud$TEMP, pch=20, main="TEMP")
dotchart(eud$NLI, pch=20, main="NLI")
dotchart(eud$PRESSURE, pch=20, main="PRESSURE")
dotchart(eud$PRECIPITATION, pch=20, main="PRECIPITATION")
dotchart(eud$SOLAR_RAD, pch=20, main="SOLAR RADIATION")
dotchart(eud$LAI, pch=20, main="LEAF AREA INDEX")
dotchart(eud$PM25, pch=20, main="PM2.5")

# Outliers removed OK 

#Lockdown_severity_index_cat<-rep.int(NA, nrow(eud))
#Lockdown_severity_index_cat[which(eud$Lockdown_severity_index<48)]<-"low"
#Lockdown_severity_index_cat[which(eud$Lockdown_severity_index>48 & eud$Lockdown_severity_index<50)]<-"medium"
#Lockdown_severity_index_cat[which(eud$Lockdown_severity_index>50)]<-"high"
#eud<-cbind(eud, Lockdown_severity_index_cat)
#eud$Lockdown_severity_index_cat<-as.factor(eud$Lockdown_severity_index_cat)

#eud$state[eud$Lockdown_severity_index_cat=="high"]
#eud$state[eud$Lockdown_severity_index_cat=="medium"]
#eud$state[eud$Lockdown_severity_index_cat=="low"]

#pairs.panels(cbind(eu_new_dataset[3:5],eu_new_dataset[17],eu_new_dataset[20]), smooth=T, scale=T, ellipses=T)
#pairs.panels(cbind(eu_new_dataset[6:11],eu_new_dataset[17]), smooth=T, scale=T, ellipses=T)
#pairs.panels(eu_new_dataset[10:17], smooth=T, scale=T, ellipses=T)
#pairs.panels(cbind(eu_new_dataset[15:21],eu_new_dataset[10]), smooth=T, scale=T, ellipses=T)
#pairs.panels(cbind(eu_new_dataset[22:28],eu_new_dataset[17]), smooth=T, scale=T, ellipses=T)

# introducing number of nuts per country to tell the model that countries with smaller sample sizes should be weighted less than those with larger sample sizes
# https://stackoverflow.com/questions/56050324/how-to-add-weight-to-variable-for-gam-model

#weight<-rep(NA,nrow(eud))

#weight[which(eud$state=="FR")]<-length(which(eud$state=="FR"))/nrow(eud)
#weight[which(eud$state=="DE")]<-length(which(eud$state=="DE"))/nrow(eud)
#weight[which(eud$state=="NL")]<-length(which(eud$state=="NL"))/nrow(eud)
#weight[which(eud$state=="RO")]<-length(which(eud$state=="RO"))/nrow(eud)
#weight[which(eud$state=="UK")]<-length(which(eud$state=="UK"))/nrow(eud)

#eud<-cbind(eud, weight) 

# Proportion of population by age group