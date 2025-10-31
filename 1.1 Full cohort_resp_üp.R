###################### Respiratory Infection ##################################
# Cens patients after 90 days ##########
# Infection == 1 only respiratory

library(mvna)
library(etm)
library(survival)
library(cmprsk)

# 1. Access ks data and create death_icu --------------------------------------------
setwd("C:/Users/messuti/Documents/Master thesis R/Russian")
ks_data= read.csv("ks.csv")  # read csv file 
variable.names(ks_data)

#death in icu, wenn outcome==death und ICU stay == hospital stay
death_icu<-1*(ks_data$outcome=="death")*(ks_data$days_in_hospital==ks_data$day_in_ICU)



# 2. Create data frame only respiratory infections -----------------------------
Resp_0_all <- data.frame(id=ks_data$ID,
                     #LOS:
                     ICU_LOS=ks_data$day_in_ICU,
                     #covariates:
                     age=ks_data$age,sex=ks_data$gender,disease_type=ks_data$disease_type,
                     Charlson=ks_data$CHARLSON_FIRST,
                     # Infection == 1 only if respiratory infection == 1
                     infection = ks_data$infection_respiratory,  
                     time_to_infection = ks_data$infection_respiratory_1st_day,
                     end_of_infection = ks_data$infection_respiratory_1st_day + ks_data$infection_respiratory_days,
                     ICU_death=death_icu)



# 3. Type of disease: keep only tumor, vascular and trauma --------------------
#Spinal, congenital and others has too little cases, can´t do my analysis
Resp_0_all <- Resp_0_all[Resp_0_all$disease_type %in% c("tumor", "vascular", "trauma"), ]

# As factor for covariate analysis. Tumor being the reference bc it has the most cases
Resp_0_all$disease_type <- factor(Resp_0_all$disease_type, levels = c("tumor","vascular","trauma"))

#Check
levels(Resp_0_all$disease_type)
table(Resp_0_all$disease_type)

# 4. Sex as factor------------------------------------------------------------
Resp_0_all$sex <- as.factor(Resp_0_all$sex)

# 5. Keep only patients without NA in covariates
#Check complete cases
sum(complete.cases(Resp_0_all[, c("age", "sex", "disease_type", "Charlson")]))
Resp_0 <- Resp_0_all[complete.cases(Resp_0_all[, c("age", "sex", "disease_type", "Charlson")]), ]

# 6. Cens patients -------------------------------------------------------------
#Cens time to infection 
Resp_0$cens_infection_status <- ifelse(Resp_0$infection == 1 &
                                         Resp_0$time_to_infection < 90,
                                       1,
                                       0)
Resp_0$cens_infection_time <- ifelse(Resp_0$cens_infection_status == 1 &
                                       Resp_0$time_to_infection <90,
                                     Resp_0$time_to_infection,
                                     "NA")

Resp_0$cens_infection_time <- as.numeric(Resp_0$cens_infection_time)

Resp_0$end_of_infection <- ifelse(Resp_0$end_of_infection >90, 90, Resp_0$end_of_infection)


#Cens ICU_LOS
Resp_0$cens_ICU_LOS_status <- ifelse(Resp_0$ICU_LOS>90, 0, 1)
Resp_0$cens_ICU_LOS_time <- ifelse(Resp_0$ICU_LOS>90, 90, Resp_0$ICU_LOS)

# 6. Prepare for MSM ------------------------------------------------------------
# Resp_0
Resp_0$entry<-0
Resp_0$exit<- ifelse(Resp_0$cens_infection_status == 0,
                     Resp_0$cens_ICU_LOS_time, # non-infected, cens_ICU_LOS_time
                     Resp_0$cens_infection_time) # infected, cens_infection_time
Resp_0$from<-0
Resp_0$to<- ifelse(Resp_0$cens_infection_status==1, 1, 2)

table(Resp_0$to, useNA = "ifany")

# Resp_1
Resp_1<-Resp_0[Resp_0$cens_infection_status==1,]
Resp_1$entry<-Resp_1$cens_infection_time
Resp_1$exit<-Resp_1$cens_ICU_LOS_time
Resp_1$from<-1
Resp_1$to<-2

# Check transitions
table(Resp_1$from,Resp_1$to,useNA="ifany")

# Check if time
#If patients aquired a HAI the same they they were discharged
dim(Resp_1[Resp_1$entry>=Resp_1$exit,])

#For patients that acquired HAI the same they they were discharged set ICU_exit +0.001
Resp_1$exit[Resp_1$entry>=Resp_1$exit]<-Resp_1$exit[Resp_1$entry>=Resp_1$exit]+0.001

#Resp_etm: rbind both tables
Resp_etm<-rbind(Resp_0,Resp_1)
table(Resp_etm$from,Resp_etm$to,useNA="ifany")

# 7.Simple illnes-death MSM analysis ------------------------------------------------------
#Calculate incidence:
total_patients <- length(unique(Resp_etm$id))
num_infections <- length(unique(Resp_etm$id[Resp_etm$from == 0 & Resp_etm$to == 1]))
num_infections / total_patients # Incidence
num_infections / total_patients * 100 # 100% incidence

#Create matrix
tra.idm <- matrix(FALSE, 3, 3, dimnames = list(c(0, 1, 2), c(0, 1, 2)))
tra.idm[1, 2:3] <- TRUE
tra.idm[2, 3] <- TRUE
tra.idm

#Cumulative hazard function
mvna.idm <- mvna(Resp_etm, c("0", "1", "2"), tra.idm, "cens")
plot(mvna.idm, xlim=c(0,90))

#Transition probabilities using etm package.
etm.idm <- etm(Resp_etm, c("0", "1", "2"), tra.idm, "cens", s = 0)

# Calculate cLOS. 
clos(etm.idm)
#Plot cLOS
plot(clos(etm.idm),
     lwd = 3)     

# 8. Distinguish HR of being discharged death and alive -----------------------
#create time-dependent infection variable:
Resp_etm$infection_td<-0
Resp_etm$infection_td[Resp_etm$from==1]<-1

# Model for discharge (dead or alive)
fit_combined<-coxph(Surv(entry, exit, to == 2 ) ~ infection_td, data = Resp_etm)

# Model for discharge alive (event = to == 2 & ICU_death == 0)
fit_alive <- coxph(Surv(entry, exit, to == 2 & ICU_death == 0) ~ infection_td, data = Resp_etm)

# Model for death (event = to == 2 & ICU_death == 1)
fit_death <- coxph(Surv(entry, exit, to == 2 & ICU_death == 1) ~ infection_td, data = Resp_etm)

summary(fit_combined)
summary(fit_alive)
summary(fit_death)

# 9. Check assumptions for Covariate analysis --------------------------------
#Cumulative incidence function
cif <- cuminc(Resp_etm$exit, Resp_etm$to)
plot(cif,
     xlim = c(0,90))

plot(cif,
     xlim = c(0,15)) # at 1 point in time there is a violation of the PH assumption

# 10. Fit a Proportional Cause_Specific model ---------------------------------------
# Transition 0 to 1
fit_csh_01 <- coxph(Surv(entry, exit, to == 1) ~ age + sex + Charlson + disease_type, data = Resp_etm)
summary(fit_csh_01)

# Transition 0 to 2
fit_csh_02 <- coxph(Surv(entry, exit, to == 2) ~ age + sex + Charlson + disease_type, data = Resp_etm[Resp_etm$from == 0, ])
summary(fit_csh_02)

# Transition 1 to 2
fit_csh_12 <- coxph(Surv(entry, exit, to == 2) ~  age + sex + Charlson + disease_type, 
                    data = Resp_etm[Resp_etm$from == 1, ])
summary(fit_csh_12)

# 11. Fine and Gray model ------------------------------------------------------

# Transition 0 to 1
# Recreate covariate matrix
covariates <- model.matrix(~ age + sex + Charlson + disease_type, data = Resp_etm)[, -1]

# Fit the Fine & Gray model
fit_subdist_01 <- crr(
  ftime = Resp_etm$exit,
  fstatus = Resp_etm$to,
  cov1 = covariates,
  failcode = 1,
  cencode = 0
)

summary(fit_subdist_01)

# Transition 0 to 2
# Filter only patients who start in state 0
data_0 <- Resp_etm[Resp_etm$from == 0, ]

# Create covariate matrix
covariates_0 <- model.matrix(~  age + sex + Charlson + disease_type, data = data_0)[, -1]

# Fit Fine & Gray model: event of interest = 2 (discharge), competing = 1 (infection), censoring = 0
fit_subdist_02 <- crr(
  ftime = data_0$exit,
  fstatus = data_0$to,
  cov1 = covariates_0,
  failcode = 2,
  cencode = 0
)

summary(fit_subdist_02)

### No Fine and gray model for transition 1 to 2 because there is no competing events