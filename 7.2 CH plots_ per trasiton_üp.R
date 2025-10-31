# Run all of 7.1
library(dplyr)
library(ggplot2)

########### Full cohort, PPS uw, PPS w #######################################
# Full Cohort, PPS uw and PP w transition 0 -> 1 ------------------------------
# 1. Filter and Label Data for the "Healthy (0) -> Illness (1)" Transition
# This transition corresponds to Transition_ID == "trans=1" based on your code.

# Full Cohort Protocol
Full_CH_1 <- Full_Cohort_CH %>%
  filter(Transition_ID == "trans=1") %>%
  mutate(Protocol = "Full Cohort")

# PPS Unweighted Protocol
PPS_uw_CH_1 <- PPS_uw_CH %>%
  filter(Transition_ID == "trans=1") %>%
  mutate(Protocol = "PPS Unweighted")

# PPS Weighted Protocol
PPS_w_CH_1 <- PPS_w_CH %>%
  filter(Transition_ID == "trans=1") %>%
  mutate(Protocol = "PPS Weighted")

# 2. Combine the Data Frames
Combined_CH_1 <- bind_rows(Full_CH_1, PPS_uw_CH_1, PPS_w_CH_1)

# 3. Plot the Comparative Cumulative Hazard Functions
ggplot(Combined_CH_1, aes(x = time, y = cumulative_hazard, color = Protocol)) +
  geom_step(linewidth = 1.2) +
  labs(
    title = "Comparison of Cumulative Hazard: Admitted (0) → HAI (1)",
    subtitle = "Full Cohort vs. PPS Unweighted vs. PPS Weighted",
    x = "Time",
    y = "Cumulative Hazard H(t)",
    color = "Estimation Protocol"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Full Cohort, PPS uw and PP w transition 0 -> 2 ------------------------------

# Full Cohort Protocol
Full_CH_2 <- Full_Cohort_CH %>%
  filter(Transition_ID == "trans=2") %>%
  mutate(Protocol = "Full Cohort")

# PPS Unweighted Protocol
PPS_uw_CH_2 <- PPS_uw_CH %>%
  filter(Transition_ID == "trans=2") %>%
  mutate(Protocol = "PPS Unweighted")

# PPS Weighted Protocol
PPS_w_CH_2 <- PPS_w_CH %>%
  filter(Transition_ID == "trans=2") %>%
  mutate(Protocol = "PPS Weighted")

# 2. Combine the Data Frames
Combined_CH_2 <- bind_rows(Full_CH_2, PPS_uw_CH_2, PPS_w_CH_2)

# 3. Plot the Comparative Cumulative Hazard Functions
ggplot(Combined_CH_2, aes(x = time, y = cumulative_hazard, color = Protocol)) +
  geom_step(linewidth = 1.2) +
  labs(
    title = "Comparison of Cumulative Hazard: Admitted (0) → Discharged (2)",
    subtitle = "Full Cohort vs. PPS Unweighted vs. PPS Weighted",
    x = "Time (days)",
    y = "Cumulative Hazard H(t)",
    color = "Estimation Protocol"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Full Cohort, PPS uw and PP w transition 1 -> 2 ------------------------------

# Full Cohort Protocol
Full_CH_3 <- Full_Cohort_CH %>%
  filter(Transition_ID == "trans=3") %>%
  mutate(Protocol = "Full Cohort")

# PPS Unweighted Protocol
PPS_uw_CH_3 <- PPS_uw_CH %>%
  filter(Transition_ID == "trans=3") %>%
  mutate(Protocol = "PPS Unweighted")

# PPS Weighted Protocol
PPS_w_CH_3 <- PPS_w_CH %>%
  filter(Transition_ID == "trans=3") %>%
  mutate(Protocol = "PPS Weighted")

# 2. Combine the Data Frames
Combined_CH_3 <- bind_rows(Full_CH_3, PPS_uw_CH_3, PPS_w_CH_3)

# 3. Plot the Comparative Cumulative Hazard Functions
ggplot(Combined_CH_3, aes(x = time, y = cumulative_hazard, color = Protocol)) +
  geom_step(linewidth = 1.2) +
  labs(
    title = "Comparison of Cumulative Hazard: HAI (1) → Discharged (2)",
    subtitle = "Full Cohort vs. PPS Unweighted vs. PPS Weighted",
    x = "Time (days)",
    y = "Cumulative Hazard H(t)",
    color = "Estimation Protocol"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


########### Full cohort, CDC uw, CDC w #######################################
# Full Cohort, PPS uw and PP w transition 0 -> 1 ------------------------------
# 1. Filter and Label Data for the "Healthy (0) -> Illness (1)" Transition
# This transition corresponds to Transition_ID == "trans=1" based on your code.

# Full Cohort Protocol
Full_CH_1 <- Full_Cohort_CH %>%
  filter(Transition_ID == "trans=1") %>%
  mutate(Protocol = "Full Cohort")

# PPS Unweighted Protocol
CDC_uw_CH_1 <- CDC_uw_CH %>%
  filter(Transition_ID == "trans=1") %>%
  mutate(Protocol = "PPS Unweighted")

# PPS Weighted Protocol
CDC_w_CH_1 <- CDC_w_CH %>%
  filter(Transition_ID == "trans=1") %>%
  mutate(Protocol = "PPS Weighted")

# 2. Combine the Data Frames
Combined_CDC_CH_1 <- bind_rows(Full_CH_1, CDC_uw_CH_1, CDC_w_CH_1)

# 3. Plot the Comparative Cumulative Hazard Functions
ggplot(Combined_CDC_CH_1, aes(x = time, y = cumulative_hazard, color = Protocol)) +
  geom_step(linewidth = 1.2) +
  labs(
    title = "Comparison of Cumulative Hazard: Admitted (0) → HAI (1)",
    subtitle = "Full Cohort vs. CDC Unweighted vs. CDC Weighted",
    x = "Time",
    y = "Cumulative Hazard H(t)",
    color = "Estimation Protocol"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Full Cohort, PPS uw and PP w transition 0 -> 2 ------------------------------

# Full Cohort Protocol
Full_CH_2 <- Full_Cohort_CH %>%
  filter(Transition_ID == "trans=2") %>%
  mutate(Protocol = "Full Cohort")

# PPS Unweighted Protocol
CDC_uw_CH_2 <- CDC_uw_CH %>%
  filter(Transition_ID == "trans=2") %>%
  mutate(Protocol = "PPS Unweighted")

# PPS Weighted Protocol
CDC_w_CH_2 <- CDC_w_CH %>%
  filter(Transition_ID == "trans=2") %>%
  mutate(Protocol = "PPS Weighted")

# 2. Combine the Data Frames
Combined_CDC_CH_2 <- bind_rows(Full_CH_2, CDC_uw_CH_2, CDC_w_CH_2)

# 3. Plot the Comparative Cumulative Hazard Functions
ggplot(Combined_CDC_CH_2, aes(x = time, y = cumulative_hazard, color = Protocol)) +
  geom_step(linewidth = 1.2) +
  labs(
    title = "Comparison of Cumulative Hazard: Admitted (0) → Discharged (2)",
    subtitle = "Full Cohort vs. CDC Unweighted vs. CDC Weighted",
    x = "Time (days)",
    y = "Cumulative Hazard H(t)",
    color = "Estimation Protocol"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Full Cohort, PPS uw and PP w transition 1 -> 2 ------------------------------

# Full Cohort Protocol
Full_CH_3 <- Full_Cohort_CH %>%
  filter(Transition_ID == "trans=3") %>%
  mutate(Protocol = "Full Cohort")

# PPS Unweighted Protocol
CDC_uw_CH_3 <- CDC_uw_CH %>%
  filter(Transition_ID == "trans=3") %>%
  mutate(Protocol = "PPS Unweighted")

# PPS Weighted Protocol
CDC_w_CH_3 <- CDC_w_CH %>%
  filter(Transition_ID == "trans=3") %>%
  mutate(Protocol = "PPS Weighted")

# 2. Combine the Data Frames
Combined_CDC_CH_3 <- bind_rows(Full_CH_3, CDC_uw_CH_3, CDC_w_CH_3)

# 3. Plot the Comparative Cumulative Hazard Functions
ggplot(Combined_CDC_CH_3, aes(x = time, y = cumulative_hazard, color = Protocol)) +
  geom_step(linewidth = 1.2) +
  labs(
    title = "Comparison of Cumulative Hazard: HAI (1) → Discharged (2)",
    subtitle = "Full Cohort vs. CDC Unweighted vs. CDC Weighted",
    x = "Time (days)",
    y = "Cumulative Hazard H(t)",
    color = "Estimation Protocol"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

