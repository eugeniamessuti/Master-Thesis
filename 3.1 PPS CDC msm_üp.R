######## CDC analysis using msm package #########
# Run 1 Full cohort_resp_überprüft until line 83
# Use Resp_0

library(msm)
library(dplyr)

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

# 2. Gather data according to CDC protocol ---------------------------------
CDC_0 <- data.frame(id = PPS_0$id,
                    ICU_LOS = PPS_0$ICU_LOS,
                    age = PPS_0$age,
                    sex = PPS_0$sex,
                    disease_type = PPS_0$disease_type,
                    Charlson = PPS_0$Charlson,
                    prevalent_infection = PPS_0$prevalent_infection, #Here prevalent infection is already censed (see 2.1 line 23)
                    PPS_date = PPS_0$PPS_date,
                    cens_ICU_LOS_time = PPS_0$cens_ICU_LOS_time,
                    cens_ICU_LOS_status = PPS_0$cens_ICU_LOS_status)

#IMPORTANT!!! TIME TO INFECTION IS CENSORED FOR THIS DATA FRAME
CDC_0$cens_time_to_infection <- ifelse (CDC_0$prevalent_infection == 1, PPS_0$cens_infection_time, "NA")
CDC_0$cens_time_to_infection <- as.numeric(CDC_0$cens_time_to_infection)

# 3. Prepare data for msm package -------------------------------------------
#1 row per observation
# the packages needs states to be coded like 1, 2, 3
#State 1: admitted
#State 2: HAI
#State 3: discharged death/alive

#Observation at entry time
CDC_0$time <- 0
CDC_0$state <- 1
CDC_0$obs_type <- 2

#Observation either at infection time (patients infected) or at PPS time (patients not infected)
CDC_1 <- CDC_0
CDC_1$time <- ifelse (CDC_1$prevalent_infection == 1, CDC_1$cens_time_to_infection, CDC_1$PPS_date)
CDC_1$state <- ifelse (CDC_1$prevalent_infection == 1, 2, 1)
CDC_1$obs_type <- 2

#Observation at discharge time
CDC_2 <- CDC_0
CDC_2$time <- CDC_2$ICU_LOS
CDC_2$state <- 3
CDC_2$obs_type <- ifelse(CDC_2$prevalent_infection == 1, 2, 3)

CDC_msm <- rbind(CDC_0, CDC_1, CDC_2)

# Sort the data frame by the 'id' column
CDC_msm <- CDC_msm %>% arrange(id)

statetable.msm(state, id, CDC_msm)

# 4. Code for censored patients ----------------------------------------------
# For patients with an ICU_LOS > 90, I will code one observation at day 90
# that is in the same state as the state before (either 0 or 1)

# Only censored observations will get a 0
CDC_msm$obs_status <- ifelse(CDC_msm$time > 90, 0, 1)

#Cens the time
CDC_msm$time <- ifelse(CDC_msm$obs_status == 0, 90, CDC_msm$time)

#Censored patients with a HAI should have state 2 (same as before)
#For patients without a HAI, then 99 is the cens code because at that point they could be either in state 0 or 1
CDC_msm$state <- ifelse(CDC_msm$obs_status == 0 & CDC_msm$prevalent_infection == 1, 2, 
                        ifelse(CDC_msm$obs_status == 0 & CDC_msm$prevalent_infection == 0, 99,
                               CDC_msm$state))

#For patients without a HAI, change the last obs_type to 1
CDC_msm$obs_type <- ifelse(CDC_msm$obs_status == 0 & CDC_msm$prevalent_infection == 0, 1, CDC_msm$obs_type)

#See patients that were censored and did not have a HAI at day PPS to see how it ended up coding
print(CDC_msm[CDC_msm$obs_status == 0 & CDC_msm$prevalent_infection == 0, ])


# 5. Get an initial transition matrix based on my data -----------------------
tra.msm <- crudeinits.msm(
  formula = state ~ time,
  subject = id,
  data = CDC_msm,
  censor = 99,
  qmatrix = matrix(
    c(0, 1, 1,
      0, 0, 1,
      0, 0, 0),
    nrow = 3, byrow = TRUE, dimnames = list(c(1, 2, 3), c(1, 2, 3)))
)

# 6. Fit the unweighted model --------------------------------------------------
cdc.msm.uw <- msm(
  state ~ time,
  subject = id,
  data = CDC_msm,
  qmatrix = tra.msm,
  obstype = obs_type,
  censor = 99, 
  covariates = ~ age + sex + Charlson + disease_type
)

summary(cdc.msm.uw)
hazard.msm(cdc.msm.uw)
plot(cdc.msm.uw)

# 7. Fit the weighted model ---------------------------------------------------
# To correct the length-biased sampling we weight patients inversely to their days in ICU. 
CDC_msm$weight <- 1/CDC_msm$cens_ICU_LOS_time
summary(CDC_msm$weight)
sum(CDC_msm$weight == 0)#No weight equals to 0

#Fit the model
cdc.msm.w <- msm(
  state ~ time,
  subject = id,
  data = CDC_msm,
  qmatrix = tra.msm,
  obstype = obs_type,
  censor = 99, 
  covariates = ~ age + sex + Charlson + disease_type,
  subject.weights = weight
)

hazard.msm(cdc.msm.w)
plot(cdc.msm.w)
### Problem: Very big CI

# 8. Fit the weighted model + Bootstraping for CI ------------------------------
set.seed(1298)
boot.haz.1000 <- boot.msm(
  cdc.msm.w,
  stat = function(x) hazard.msm(x),  # apply hazard ratio calculation on each bootstrap model
  B = 1000,                      # number of bootstrap samples (increase if needed                         # parallelisation (optional)
  remove.errors = TRUE
)

# 9. Exctract HR and CI manually (no build in function) ------------------------
# --- Step 1: Point estimates from the original model ---
h0 <- hazard.msm(cdc.msm.w)

HR_point <- unlist(lapply(names(h0), function(cov) {
  tab <- h0[[cov]]
  vals <- as.numeric(tab[, "HR"])   # <-- corrected column name
  names(vals) <- paste0(rownames(tab), " (", cov, ")")
  vals
}))

# --- Step 2: Function to flatten hazard.msm output in bootstrap ---
flatten_hazard <- function(h) {
  if (inherits(h, "try-error") || is.null(h)) return(rep(NA, length(HR_point)))
  unlist(lapply(names(h0), function(cov) {
    if (!is.null(h[[cov]])) {
      as.numeric(h[[cov]][, "HR"])   # <-- corrected column name
    } else {
      rep(NA, nrow(h0[[cov]]))
    }
  }))
}

# --- Step 3: Apply to bootstrap output ---
HR_mat <- t(sapply(boot.haz.1000, flatten_hazard))
colnames(HR_mat) <- names(HR_point)

# --- Step 4: Summaries ---
HR_SE <- apply(HR_mat, 2, sd, na.rm = TRUE)
HR_CI <- t(apply(HR_mat, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE)))

summary_table <- data.frame(
  Transition_Covariate = names(HR_point),
  HR = HR_point,
  SE_boot = HR_SE,
  CI_lower = HR_CI[, 1],
  CI_upper = HR_CI[, 2],
  row.names = NULL
)

print(summary_table)

