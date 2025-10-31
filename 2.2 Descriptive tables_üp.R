###################### Descriptive Tables ###########################

# 1. Full cohort -------------------------------------------------------------
# Age
summary(Resp_0$age)
sd(Resp_0$age)

# Sex
table(Resp_0$sex)

#Disease type
table(Resp_0$disease_type)

# 2. PPS all together ------------------------------------------------------
# Age
summary(PPS_0$age)
sd(PPS_0$age)

# Sex
table(PPS_0$sex)

#Disease type
table(PPS_0$disease_type)

# 3. PPS Distinguish prevalent and incident infection ------------------------


PPS_0$prevalent_infection <- ifelse(PPS_0$cens_infection_status == 1 & 
                                      PPS_0$cens_infection_time < PPS_0$PPS_date,
                                    1,
                                    0)

PPS_0$incident_infection <- ifelse(PPS_0$cens_infection_status == 1 & 
                                     PPS_0$cens_infection_time > PPS_0$PPS_date,
                                   1,
                                   0)

# 4. PPS prevalent infection---------------------------------------------------
prevalent_infection <- PPS_0[PPS_0$prevalent_infection == 1, ]
nrow(prevalent_infection)
summary(prevalent_infection$age)
sd(prevalent_infection$age)
table(prevalent_infection$sex)
table(prevalent_infection$disease_type)

# 5. PPS incident infection--------------------------------------------------
incident_infection <- PPS_0[PPS_0$incident_infection == 1, ]
nrow(incident_infection)
summary(incident_infection$age)
sd(incident_infection$age)
table(incident_infection$sex)
table(incident_infection$disease_type)

# 6. PPS no infection------------------------------------------------------
no_infection<- PPS_0[PPS_0$incident_infection == 0, ]
nrow(no_infection)
summary(no_infection$age)
sd(no_infection$age)
table(no_infection$sex)
table(no_infection$disease_type)
