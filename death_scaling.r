
eud_s<-eud

eud_s$POP_DENS <-   eud$POP_DENS      #  scale(eud$POP_DENS, center = TRUE, scale = TRUE)
eud_s$POPULATION <- eud$POPULATION    # scale(eud$POPULATION, center = TRUE, scale = TRUE)
eud_sP_MALES   <- eud$P_MALES         # scale(eud$P_MALES, center = TRUE, scale = TRUE)
eud_s$P_POP_60 <- eud$P_POP_60        # scale(eud$P_POP_60, center = TRUE, scale = TRUE)

eud_s$HOUSEHOLDS <- scale(eud$HOUSEHOLDS, center = TRUE, scale = TRUE)
eud_s$DWELLINGS <- scale(eud$DWELLINGS, center = TRUE, scale = TRUE)

eud_s$NO2 <- scale(eud$NO2, center = TRUE, scale = TRUE)
eud_s$PM25 <- scale(eud$PM25, center = TRUE, scale = TRUE)
eud_s$NLI <- scale(eud$NLI, center = TRUE, scale = TRUE)
eud_s$PRESSURE <- scale(eud$PRESSURE, center = TRUE, scale = TRUE)
eud_s$PRECIPITATION  <- scale(eud$PRECIPITATION, center = TRUE, scale = TRUE)
eud_s$SOLAR_RAD <- scale(eud$SOLAR_RAD, center = TRUE, scale = TRUE)
eud_s$LAI <- scale(eud$LAI, center = TRUE, scale = TRUE)
eud_s$TEMP <- scale(as.numeric(eud$TEMP), center = TRUE, scale = TRUE)
eud_s$WIND <- scale(as.numeric(eud$WIND), center = TRUE, scale = TRUE)

eud_s$DURATION_LD <- eud$DURATION_LD # scale(as.numeric(eud$DURATION_LD), center = TRUE, scale = TRUE)
eud_s$eud$LAG_LD <- eud$LAG_LD       # scale(as.numeric(eud$LAG_LD), center = TRUE, scale = TRUE)
