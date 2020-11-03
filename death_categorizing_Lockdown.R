#### categorizing duration and lag

hist(eud$DURATION_LD)
DURATION_LD_CAT<-rep(NA,nrow(eud))
DURATION_LD_CAT[which(eud$DURATION_LD<48)]<-"Low"
DURATION_LD_CAT[which(eud$DURATION_LD>48 & eud$DURATION_LD<=52)]<-"Medium"
DURATION_LD_CAT[which(eud$DURATION_LD>52)]<-"High"
eud<-cbind(eud,DURATION_LD_CAT)
eud$DURATION_LD_CAT<-as.factor(eud$DURATION_LD_CAT)

hist(eud$LAG_LD)
LAG_LD_CAT<-rep(NA,nrow(eud))
LAG_LD_CAT[which(eud$LAG_LD<31)]<-"Low"
LAG_LD_CAT[which(eud$LAG_LD>49)]<-"High"
eud<-cbind(eud,LAG_LD_CAT)
eud$LAG_LD_CAT<-as.factor(eud$LAG_LD_CAT)

summary(eud)
