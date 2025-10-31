####### Cumulative Hazards plots extracting manually the Hazards #############
# Run 1.1 Full_cohort until line 83
# Run 2.1 Until line 26
# Run 3.1 Until line 57


library(survival)
library(mstate)
library(dplyr)
library(ggplot2)


# Full cohort --------------------------------------------------------

# 1. Prepare the data in a long format
Resp_mstate_0 <- Resp_0

#### IMPORTANTE!!!!
# Si el paciente no tiene la infección, entonces tiene que llamarse infinito en vez de NA el time to infection
Resp_mstate_0$cens_infection_time[is.na(Resp_mstate_0$cens_infection_time)] <- Inf

summary(Resp_mstate_0)

#cens patients
Resp_mstate_0$cens_ICU_LOS_status <- ifelse(Resp_mstate_0$ICU_LOS>90, 0, 1)
Resp_mstate_0$cens_ICU_LOS_time <- ifelse(Resp_mstate_0$ICU_LOS>90, 90, Resp_mstate_0$ICU_LOS)

tmat <- trans.illdeath(names = c("Admited", "HAI", "Discharged/Death"))
tmat

Resp_mstate <- msprep(
  time = c(NA, "cens_infection_time", "cens_ICU_LOS_time"),
  status = c(NA, "cens_infection_status", "cens_ICU_LOS_status"),
  data = Resp_mstate_0,
  trans = tmat,
  keep = c("id", "ICU_LOS", "age", "Charlson", "cens_infection_status", "ICU_Death", "cens_infection_time")
)
#summary number of events
events(Resp_mstate)

# check time
#check if time is ok:
dim(Resp_mstate[Resp_mstate$Tstart>=Resp_mstate$Tstop,])

Resp_mstate$Tstop[Resp_mstate$Tstart>=Resp_mstate$Tstop]<-Resp_mstate$Tstop[Resp_mstate$Tstart>=Resp_mstate$Tstop]+0.001

dim(Resp_mstate[Resp_mstate$Tstart>=Resp_mstate$Tstop,])

# 2. Cox model estimation
Resp_surv <- coxph(
  Surv(Tstart, Tstop, status) ~ strata(trans),
  data = Resp_mstate
)

# 3. Extract the baseline cumulative hazards using basehaz
Full_Cohort_CH <- basehaz(Resp_surv, centered = FALSE)

# 4. Prepare the data for plotting and add descriptive labels
Full_Cohort_CH <- Full_Cohort_CH %>%
  rename(
    time = time,
    cumulative_hazard = hazard,
    Transition_ID = strata # 'strata' holds the transition ID from the Cox fit
  ) %>%
  # Map the Transition ID (e.g., "trans=1") to a descriptive label
  mutate(
    # You will need to adjust these labels (1, 2, 3) to match your tmat transitions
    Transition = case_when(
      Transition_ID == "trans=1" ~ "Admitted (0) -> HAI (1)",
      Transition_ID == "trans=2" ~ "Admitted (0) -> Discharged (2)",
      Transition_ID == "trans=3" ~ "HAI (1) -> Discharged (2)",
      TRUE ~ Transition_ID
    )
  )

# 5. Plot the Weighted Cumulative Hazard Functions
ggplot(Full_Cohort_CH, aes(x = time, y = cumulative_hazard, color = Transition)) +
  geom_step(linewidth = 1) + # Use geom_step for the Nelson-Aalen look
  labs(
    title = "Cumulative Hazard Function Full Cohort",
    x = "Time",
    y = "Cumulative Hazard H(t)",
    color = "Multistate Transition"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



# PPS unweighted --------------------------------------------------------

# 1. Prepare the data in a long format
PPS_mstate_0 <- PPS_0

#### IMPORTANTE!!!!
# Si el paciente no tiene la infección, entonces tiene que llamarse infinito en vez de NA el time to infection
PPS_mstate_0$cens_infection_time[is.na(PPS_mstate_0$cens_infection_time)] <- Inf

summary(PPS_mstate_0)

tmat <- trans.illdeath(names = c("Admited", "HAI", "Discharged/Death"))
tmat

PPS_mstate <- msprep(
  time = c(NA, "cens_infection_time", "cens_ICU_LOS_time"),
  status = c(NA, "cens_infection_status", "cens_ICU_LOS_status"),
  data = PPS_mstate_0,
  trans = tmat,
  keep = c("id", "ICU_LOS", "age", "Charlson", "cens_infection_status", "ICU_death", "cens_infection_time", "cens_ICU_LOS_time")
)
#summary number of events
events(PPS_mstate)

# check time
#check if time is ok:
dim(PPS_mstate[PPS_mstate$Tstart>=PPS_mstate$Tstop,])

PPS_mstate$Tstop[PPS_mstate$Tstart>=PPS_mstate$Tstop]<-PPS_mstate$Tstop[PPS_mstate$Tstart>=PPS_mstate$Tstop]+0.001

dim(PPS_mstate[PPS_mstate$Tstart>=PPS_mstate$Tstop,])

# 2. Cox model estimation unweighted
PPS_Surv_uw <- coxph(
  Surv(Tstart, Tstop, status) ~ strata(trans),
  data = PPS_mstate
)

# 3. Extract the baseline cumulative hazards using basehaz
PPS_uw_CH <- basehaz(PPS_Surv_uw, centered = FALSE)

# 4. Prepare the data for plotting and add descriptive labels
PPS_uw_CH <- PPS_uw_CH %>%
  rename(
    time = time,
    cumulative_hazard = hazard,
    Transition_ID = strata # 'strata' holds the transition ID from the Cox fit
  ) %>%
  # Map the Transition ID (e.g., "trans=1") to a descriptive label
  mutate(
    # You will need to adjust these labels (1, 2, 3) to match your tmat transitions
    Transition = case_when(
      Transition_ID == "trans=1" ~ "Admitted (0) -> HAI (1)",
      Transition_ID == "trans=2" ~ "Admitted (0) -> Discharged (2)",
      Transition_ID == "trans=3" ~ "HAI (1) -> Discharged (2)",
      TRUE ~ Transition_ID
    )
  )

# 5. Plot the Weighted Cumulative Hazard Functions
ggplot(PPS_uw_CH, aes(x = time, y = cumulative_hazard, color = Transition)) +
  geom_step(linewidth = 1) + # Use geom_step for the Nelson-Aalen look
  labs(
    title = "Cumulative Hazard Function PPS unweighted",
    x = "Time",
    y = "Cumulative Hazard H(t)",
    color = "Multistate Transition"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")




# PPS weighted ----------------------------------------------------------------

# 1. Prepare data
PPS_mstate$weights <-  1/PPS_mstate$cens_ICU_LOS_time

# 2. Cox model estimation weighted
PPS_Surv_w <- coxph(
  Surv(Tstart, Tstop, status) ~ strata(trans),
  data = PPS_mstate,
  weights = weights
)


# 3. Extract the weighted baseline cumulative hazards using basehaz
PPS_w_CH <- basehaz(PPS_Surv_w, centered = FALSE)

# 4. Prepare the data for plotting and add descriptive labels
PPS_w_CH <- PPS_w_CH %>%
  rename(
    time = time,
    cumulative_hazard = hazard,
    Transition_ID = strata # 'strata' holds the transition ID from the Cox fit
  ) %>%
  # Map the Transition ID (e.g., "trans=1") to a descriptive label
  mutate(
    # You will need to adjust these labels (1, 2, 3) to match your tmat transitions
    Transition = case_when(
      Transition_ID == "trans=1" ~ "Admitted (0) -> HAI (1)",
      Transition_ID == "trans=2" ~ "Admitted (0) -> Discharged (2)",
      Transition_ID == "trans=3" ~ "HAI (1) -> Discharged (2)",
      TRUE ~ Transition_ID
    )
  )

# 5. Plot the Weighted Cumulative Hazard Functions
ggplot(PPS_w_CH, aes(x = time, y = cumulative_hazard, color = Transition)) +
  geom_step(linewidth = 1) + # Use geom_step for the Nelson-Aalen look
  labs(
    title = "Cumulative Hazard Function PPS weighted",
    x = "Time",
    y = "Cumulative Hazard H(t)",
    color = "Multistate Transition"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# CDC protocol unweighted ----------------------------------------------------------------

# 1. Prepare the data in a long format
CDC_mstate_0 <- CDC_0

#### IMPORTANTE!!!!
# Si el paciente no tiene la infección, entonces tiene que llamarse infinito en vez de NA el time to infection
CDC_mstate_0$cens_time_to_infection[is.na(CDC_mstate_0$cens_time_to_infection)] <- Inf

summary(CDC_mstate_0)

#cens patients
CDC_mstate_0$cens_ICU_LOS_status <- ifelse(CDC_mstate_0$ICU_LOS>90, 0, 1)
CDC_mstate_0$cens_ICU_LOS_time <- ifelse(CDC_mstate_0$ICU_LOS>90, 90, CDC_mstate_0$ICU_LOS)


tmat <- trans.illdeath(names = c("Admited", "HAI", "Discharged/Death"))
tmat

CDC_mstate <- msprep(
  time = c(NA, "cens_time_to_infection", "cens_ICU_LOS_time"),
  status = c(NA, "prevalent_infection", "cens_ICU_LOS_status"), #Here prevalent infection is already censed (see 2.1 line 26)
  data = CDC_mstate_0,
  trans = tmat,
  keep = c("id", "ICU_LOS", "age", "Charlson", "prevalent_infection", "cens_time_to_infection", "cens_ICU_LOS_time")
)
#summary number of events
events(CDC_mstate)

# check time
#check if time is ok:
dim(CDC_mstate[CDC_mstate$Tstart>=CDC_mstate$Tstop,])

CDC_mstate$Tstop[CDC_mstate$Tstart>=CDC_mstate$Tstop]<-CDC_mstate$Tstop[CDC_mstate$Tstart>=CDC_mstate$Tstop]+0.001

dim(CDC_mstate[CDC_mstate$Tstart>=CDC_mstate$Tstop,])

# 2. Cox model estimation unweighted
CDC_Surv_uw <- coxph(
  Surv(Tstart, Tstop, status) ~ strata(trans),
  data = CDC_mstate
)

# 3. Extract the baseline cumulative hazards using basehaz
CDC_uw_CH <- basehaz(CDC_Surv_uw, centered = FALSE)

# 4. Prepare the data for plotting and add descriptive labels
CDC_uw_CH <- CDC_uw_CH %>%
  rename(
    time = time,
    cumulative_hazard = hazard,
    Transition_ID = strata # 'strata' holds the transition ID from the Cox fit
  ) %>%
  # Map the Transition ID (e.g., "trans=1") to a descriptive label
  mutate(
    # You will need to adjust these labels (1, 2, 3) to match your tmat transitions
    Transition = case_when(
      Transition_ID == "trans=1" ~ "Admitted (0) -> HAI (1)",
      Transition_ID == "trans=2" ~ "Admitted (0) -> Discharged (2)",
      Transition_ID == "trans=3" ~ "HAI (1) -> Discharged (2)",
      TRUE ~ Transition_ID
    )
  )

# 5. Plot the Weighted Cumulative Hazard Functions
ggplot(CDC_uw_CH, aes(x = time, y = cumulative_hazard, color = Transition)) +
  geom_step(linewidth = 1) + # Use geom_step for the Nelson-Aalen look
  labs(
    title = "Cumulative Hazard Function CDC unweighted",
    x = "Time",
    y = "Cumulative Hazard H(t)",
    color = "Multistate Transition"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# CDC protocol weighted ----------------------------------------------------------------

# 1. Prepare data
CDC_mstate$weights <-  1/CDC_mstate$cens_ICU_LOS_time


# 2. Cox model estimation weighted
CDC_Surv_w <- coxph(
  Surv(Tstart, Tstop, status) ~ strata(trans),
  data = CDC_mstate,
  weights = weights
)


# 3. Extract the weighted baseline cumulative hazards using basehaz
CDC_w_CH <- basehaz(CDC_Surv_w, centered = FALSE)

# 4. Prepare the data for plotting and add descriptive labels
CDC_w_CH <- CDC_w_CH %>%
  rename(
    time = time,
    cumulative_hazard = hazard,
    Transition_ID = strata # 'strata' holds the transition ID from the Cox fit
  ) %>%
  # Map the Transition ID (e.g., "trans=1") to a descriptive label
  mutate(
    # You will need to adjust these labels (1, 2, 3) to match your tmat transitions
    Transition = case_when(
      Transition_ID == "trans=1" ~ "Admitted (0) -> HAI (1)",
      Transition_ID == "trans=2" ~ "Admitted (0) -> Discharged (2)",
      Transition_ID == "trans=3" ~ "HAI (1) -> Discharged (2)",
      TRUE ~ Transition_ID
    )
  )

# 5. Plot the Weighted Cumulative Hazard Functions
ggplot(CDC_w_CH, aes(x = time, y = cumulative_hazard, color = Transition)) +
  geom_step(linewidth = 1) + # Use geom_step for the Nelson-Aalen look
  labs(
    title = "Cumulative Hazard Function CDC weighted",
    x = "Time",
    y = "Cumulative Hazard H(t)",
    color = "Multistate Transition"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

