########################## PPS mimic ########################################

library(survival)

# 1. Mimic PPS (Staus et al.) ---------------------------------------

#Find the 99% quantile = 90 becuase of censoring
quantile(Resp_0$cens_ICU_LOS_time, probs = 0.99, na.rm = TRUE)

#Assign a number between 0 and 90 to each patient
set.seed(987)
Resp_0$PPS_date <- runif(n = nrow(Resp_0), min = 0, max = 90)

#Sample only patients which ICU_LOS > than PPS_date
PPS_0 <- Resp_0[Resp_0$ICU_LOS > Resp_0$PPS_date, ]

### Define prevalent and incident infection
PPS_0$prevalent_infection <- ifelse(PPS_0$cens_infection_status == 1 & 
                                      PPS_0$cens_infection_time < PPS_0$PPS_date,
                                    1,
                                    0)

PPS_0$incident_infection <- ifelse(PPS_0$cens_infection_status == 1 & 
                                     PPS_0$cens_infection_time > PPS_0$PPS_date,
                                   1,
                                   0)

# 2. Repeat 1. 100 times and plot amount of sampled patients -----------------
# Set seed for reproducibility
set.seed(987)

# Create an empty vector to store the sample sizes
sample_sizes <- numeric(100)

# Repeat the sampling process 100 times
for (i in 1:100) {
  # Generate a random PPS_date for each patient
  PPS_date <- runif(n = nrow(Resp_0), min = 0, max = 90)
  
  # Count how many patients have ICU_LOS > PPS_date
  count <- sum(Resp_0$ICU_LOS > PPS_date, na.rm = TRUE)
  
  # Store the count
  sample_sizes[i] <- count
}

# Visualize the distribution
hist(sample_sizes, breaks = 20,
     main = "Distribution of Sample Sizes",
     xlab = "Number of Patients Sampled")

# 3. Check amount of events per covariate ----------------------------------
table(PPS_0$sex, PPS_0$cens_infection_status)
table(PPS_0$disease_type,PPS_0$cens_infection_status)
table(PPS_0$cens_infection_status)

# 4. Prepare for MSM analysis  ----------------------------------------------
PPS_1<-PPS_0[PPS_0$cens_infection_status==1,]
PPS_1$entry<-PPS_1$time_to_infection
PPS_1$exit<-PPS_1$ICU_LOS
PPS_1$from<-1
PPS_1$to<-2

table(PPS_1$from,PPS_1$to,useNA="ifany")

#check if time is ok:
dim(PPS_1[PPS_1$entry>=PPS_1$exit,])

#-> set ICU_exit +0.001
PPS_1$exit[PPS_1$entry>=PPS_1$exit]<-PPS_1$exit[PPS_1$entry>=PPS_1$exit]+0.001

#rbind both tables
PPS_etm<-rbind(PPS_0,PPS_1)

#Check transitions and type of variables
table(PPS_etm$from,PPS_etm$to,useNA="ifany")
summary(PPS_etm)

# 5. Calculate weights for IPW --------------------------------------------
# To correct the length-biased sampling we weight patients inversely to their days in ICU. 
PPS_etm$weight <- 1/PPS_etm$cens_ICU_LOS_time  

# Analyze the distribution of my weights
hist(PPS_etm$weight, breaks = 30)
summary(PPS_etm$weight)

# 6. Fit proportional cause specific model unweighted and weighted ---------

# Transition 0 -> 1
# unweighted
coxmodel.pp.event1.uw <- coxph(Surv(entry, exit, to == 1) ~  age + sex + Charlson + disease_type, PPS_etm)
summary(coxmodel.pp.event1.uw)

#weighted
coxmodel.pp.event1.w <- coxph(Surv(entry, exit, to == 1) ~ age + sex + Charlson + disease_type + cluster(id), PPS_etm, weights = weight)
summary(coxmodel.pp.event1.w)

cox.zph(coxmodel.pp.event1.w)

# Transition 0 -> 2
# unweighted
coxmodel.pp.event2.uw <- coxph(
  Surv(entry, exit, to == 2) ~ age + sex + Charlson + disease_type,
  data = PPS_etm,
  subset = from == 0
)
summary(coxmodel.pp.event2.uw)

#weighted
coxmodel.pp.event2.w <- coxph(
  Surv(entry, exit, to == 2) ~ age + sex + Charlson + disease_type + cluster(id),
  data = PPS_etm,
  subset = from == 0,
  weights = weight
)
summary(coxmodel.pp.event2.w)

# Transition 1 -> 2
# unweighted
coxmodel.pp.event3.uw <- coxph(
  Surv(entry, exit, to == 2) ~ age + sex + Charlson + disease_type,
  data = PPS_etm,
  subset = from == 1
)
summary(coxmodel.pp.event3.uw)

#weighted
coxmodel.pp.event3.w <- coxph(
  Surv(entry, exit, to == 2) ~ age + sex + Charlson + disease_type + cluster(id),
  data = PPS_etm,
  subset = from == 1,
  weights = weight
)
summary(coxmodel.pp.event3.w)

#Calculate incidence

# These are patients who have a transition from state 0 to state 1
First_Infection_patients <- unique(PPS_etm$id[PPS_etm$from == 0 & PPS_etm$to == 1])

# Total number of unique patients in the dataset
total_patients <- length(unique(PPS_etm$id))

# Incidence of pneumonia (proportion of patients who acquired pneumonia)
incidence_First_Infection <- length(First_Infection_patients) / total_patients
per_incidence_First_Infection <- incidence_First_Infection*100 
# Print the results
cat("Number of patients who acquired First_Infection:", length(First_Infection_patients), "\n")
cat("Total number of patients:", total_patients, "\n")
cat("Incidence of First_Infection:", incidence_First_Infection, "\n")
cat("%Incidence of First_Infection:", per_incidence_First_Infection, "%\n")

