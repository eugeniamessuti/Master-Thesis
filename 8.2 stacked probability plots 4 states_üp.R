########### STACKED PROBABILITY PLOTS 4 STATES ###############################
# Run 7.1 CH plots_per protocol
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

# Full cohort --------------------------------------------------------

# --- 1. Extract each transition's cumulative hazard ------------------
H12 <- Full_Cohort_CH %>% filter(Transition_ID == "trans=1") %>%
  select(time, H12 = cumulative_hazard)
H13 <- Full_Cohort_CH %>% filter(Transition_ID == "trans=2") %>%
  select(time, H13 = cumulative_hazard)
H24 <- Full_Cohort_CH %>% filter(Transition_ID == "trans=3") %>%
  select(time, H24 = cumulative_hazard)

# --- 2. Merge and fill missing values --------------------------------
haz_all <- full_join(H12, H13, by = "time") %>%
  full_join(H24, by = "time") %>%
  arrange(time) %>%
  mutate(across(starts_with("H"), ~ zoo::na.locf(., na.rm = FALSE))) %>%
  mutate(across(starts_with("H"), ~ replace_na(., 0)))

# --- 3. Compute hazard increments ------------------------------------
haz_all <- haz_all %>%
  mutate(
    dH12 = c(H12[1], diff(H12)),
    dH13 = c(H13[1], diff(H13)),
    dH24 = c(H24[1], diff(H24))
  )

# --- 4. Initialize probabilities -------------------------------------
n <- nrow(haz_all)
p <- matrix(0, n, 4)  # 4 states
p[1, ] <- c(1, 0, 0, 0)  # everyone starts in Admitted

# --- 5. Iterate over time to update probabilities --------------------
for (i in 2:n) {
  h12 <- haz_all$dH12[i]
  h13 <- haz_all$dH13[i]
  h24 <- haz_all$dH24[i]
  
  # state 1: admitted
  p[i, 1] <- p[i-1, 1] * (1 - (h12 + h13))
  
  # state 2: HAI
  p[i, 2] <- p[i-1, 2] * (1 - h24) + p[i-1, 1] * h12
  
  # state 3: discharged/death w/o HAI (absorbing)
  p[i, 3] <- p[i-1, 3] + p[i-1, 1] * h13
  
  # state 4: discharged/death after HAI (absorbing)
  p[i, 4] <- p[i-1, 4] + p[i-1, 2] * h24
}

# --- 6. Tidy the probabilities ---------------------------------------
prob_df <- data.frame(
  time = haz_all$time,
  Admitted = p[, 1],
  HAI = p[, 2],
  Discharged_no_HAI = p[, 3],
  Discharged_post_HAI = p[, 4]
) %>%
  pivot_longer(-time, names_to = "State", values_to = "Probability")


# --- 7. Change the order ---------------------------------------
prob_df$State <- factor(
  prob_df$State,
  levels = c(
    "Discharged_no_HAI",               # 1. Bottom
    "Admitted", # 2. Second from bottom
    "Discharged_post_HAI",          # 3. Third from bottom
    "HAI"  # 4. Top
  )
)

# --- 8. Plot  ---------------------------------------------------------------

ggplot(prob_df, aes(x = time, y = Probability, fill = State)) +
  geom_area(alpha = 0.85, color = "white") +
  scale_fill_manual(values = c(
    "Admitted" = "deeppink3" ,
    "HAI" = "khaki1",
    "Discharged_no_HAI" = "cornflowerblue" ,
    "Discharged_post_HAI" = "darkolivegreen4"
  )) +
  labs(
    title = "Stacked Probability Plot: Full Cohort",
    x = "Days Since ICU Admission",
    y = "Probability of Being in State"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

###############################################################################
# PPS unweighted --------------------------------------------------------

# --- 1. Extract each transition's cumulative hazard ------------------
H12 <- PPS_uw_CH %>% filter(Transition_ID == "trans=1") %>%
  select(time, H12 = cumulative_hazard)
H13 <- PPS_uw_CH %>% filter(Transition_ID == "trans=2") %>%
  select(time, H13 = cumulative_hazard)
H24 <- PPS_uw_CH %>% filter(Transition_ID == "trans=3") %>%
  select(time, H24 = cumulative_hazard)

# --- 2. Merge and fill missing values --------------------------------
haz_all <- full_join(H12, H13, by = "time") %>%
  full_join(H24, by = "time") %>%
  arrange(time) %>%
  mutate(across(starts_with("H"), ~ zoo::na.locf(., na.rm = FALSE))) %>%
  mutate(across(starts_with("H"), ~ replace_na(., 0)))

# --- 3. Compute hazard increments ------------------------------------
haz_all <- haz_all %>%
  mutate(
    dH12 = c(H12[1], diff(H12)),
    dH13 = c(H13[1], diff(H13)),
    dH24 = c(H24[1], diff(H24))
  )

# --- 4. Initialize probabilities -------------------------------------
n <- nrow(haz_all)
p <- matrix(0, n, 4)  # 4 states
p[1, ] <- c(1, 0, 0, 0)  # everyone starts in Admitted

# --- 5. Iterate over time to update probabilities --------------------
for (i in 2:n) {
  h12 <- haz_all$dH12[i]
  h13 <- haz_all$dH13[i]
  h24 <- haz_all$dH24[i]
  
  # state 1: admitted
  p[i, 1] <- p[i-1, 1] * (1 - (h12 + h13))
  
  # state 2: HAI
  p[i, 2] <- p[i-1, 2] * (1 - h24) + p[i-1, 1] * h12
  
  # state 3: discharged/death w/o HAI (absorbing)
  p[i, 3] <- p[i-1, 3] + p[i-1, 1] * h13
  
  # state 4: discharged/death after HAI (absorbing)
  p[i, 4] <- p[i-1, 4] + p[i-1, 2] * h24
}

# --- 6. Tidy the probabilities ---------------------------------------
prob_df <- data.frame(
  time = haz_all$time,
  Admitted = p[, 1],
  HAI = p[, 2],
  Discharged_no_HAI = p[, 3],
  Discharged_post_HAI = p[, 4]
) %>%
  pivot_longer(-time, names_to = "State", values_to = "Probability")


# --- 7. Change the order ---------------------------------------
prob_df$State <- factor(
  prob_df$State,
  levels = c(
    "Discharged_no_HAI",               # 1. Bottom
    "Admitted", # 2. Second from bottom
    "Discharged_post_HAI",          # 3. Third from bottom
    "HAI"  # 4. Top
  )
)

# --- 8. Plot  ---------------------------------------------------------------

ggplot(prob_df, aes(x = time, y = Probability, fill = State)) +
  geom_area(alpha = 0.85, color = "white") +
  scale_fill_manual(values = c(
    "Admitted" = "deeppink3" ,
    "HAI" = "khaki1",
    "Discharged_no_HAI" = "cornflowerblue" ,
    "Discharged_post_HAI" = "darkolivegreen4"
  )) +
  labs(
    title = "Stacked Probability Plot: PPS unweighted",
    x = "Days Since ICU Admission",
    y = "Probability of Being in State"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

###############################################################################
# PPS weighted --------------------------------------------------------

# --- 1. Extract each transition's cumulative hazard ------------------
H12 <- PPS_w_CH %>% filter(Transition_ID == "trans=1") %>%
  select(time, H12 = cumulative_hazard)
H13 <- PPS_w_CH %>% filter(Transition_ID == "trans=2") %>%
  select(time, H13 = cumulative_hazard)
H24 <- PPS_w_CH %>% filter(Transition_ID == "trans=3") %>%
  select(time, H24 = cumulative_hazard)

# --- 2. Merge and fill missing values --------------------------------
haz_all <- full_join(H12, H13, by = "time") %>%
  full_join(H24, by = "time") %>%
  arrange(time) %>%
  mutate(across(starts_with("H"), ~ zoo::na.locf(., na.rm = FALSE))) %>%
  mutate(across(starts_with("H"), ~ replace_na(., 0)))

# --- 3. Compute hazard increments ------------------------------------
haz_all <- haz_all %>%
  mutate(
    dH12 = c(H12[1], diff(H12)),
    dH13 = c(H13[1], diff(H13)),
    dH24 = c(H24[1], diff(H24))
  )

# --- 4. Initialize probabilities -------------------------------------
n <- nrow(haz_all)
p <- matrix(0, n, 4)  # 4 states
p[1, ] <- c(1, 0, 0, 0)  # everyone starts in Admitted

# --- 5. Iterate over time to update probabilities --------------------
for (i in 2:n) {
  h12 <- haz_all$dH12[i]
  h13 <- haz_all$dH13[i]
  h24 <- haz_all$dH24[i]
  
  # state 1: admitted
  p[i, 1] <- p[i-1, 1] * (1 - (h12 + h13))
  
  # state 2: HAI
  p[i, 2] <- p[i-1, 2] * (1 - h24) + p[i-1, 1] * h12
  
  # state 3: discharged/death w/o HAI (absorbing)
  p[i, 3] <- p[i-1, 3] + p[i-1, 1] * h13
  
  # state 4: discharged/death after HAI (absorbing)
  p[i, 4] <- p[i-1, 4] + p[i-1, 2] * h24
}

# --- 6. Tidy the probabilities ---------------------------------------
prob_df <- data.frame(
  time = haz_all$time,
  Admitted = p[, 1],
  HAI = p[, 2],
  Discharged_no_HAI = p[, 3],
  Discharged_post_HAI = p[, 4]
) %>%
  pivot_longer(-time, names_to = "State", values_to = "Probability")


# --- 7. Change the order ---------------------------------------
prob_df$State <- factor(
  prob_df$State,
  levels = c(
    "Discharged_no_HAI",               # 1. Bottom
    "Admitted", # 2. Second from bottom
    "Discharged_post_HAI",          # 3. Third from bottom
    "HAI"  # 4. Top
  )
)

# --- 8. Plot  ---------------------------------------------------------------

ggplot(prob_df, aes(x = time, y = Probability, fill = State)) +
  geom_area(alpha = 0.85, color = "white") +
  scale_fill_manual(values = c(
    "Admitted" = "deeppink3" ,
    "HAI" = "khaki1",
    "Discharged_no_HAI" = "cornflowerblue" ,
    "Discharged_post_HAI" = "darkolivegreen4"
  )) +
  labs(
    title = "Stacked Probability Plot: PPS weighted",
    x = "Days Since ICU Admission",
    y = "Probability of Being in State"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")


###############################################################################
# CDC unweighted --------------------------------------------------------

# --- 1. Extract each transition's cumulative hazard ------------------
H12 <- CDC_uw_CH %>% filter(Transition_ID == "trans=1") %>%
  select(time, H12 = cumulative_hazard)
H13 <- CDC_uw_CH %>% filter(Transition_ID == "trans=2") %>%
  select(time, H13 = cumulative_hazard)
H24 <- CDC_uw_CH %>% filter(Transition_ID == "trans=3") %>%
  select(time, H24 = cumulative_hazard)

# --- 2. Merge and fill missing values --------------------------------
haz_all <- full_join(H12, H13, by = "time") %>%
  full_join(H24, by = "time") %>%
  arrange(time) %>%
  mutate(across(starts_with("H"), ~ zoo::na.locf(., na.rm = FALSE))) %>%
  mutate(across(starts_with("H"), ~ replace_na(., 0)))

# --- 3. Compute hazard increments ------------------------------------
haz_all <- haz_all %>%
  mutate(
    dH12 = c(H12[1], diff(H12)),
    dH13 = c(H13[1], diff(H13)),
    dH24 = c(H24[1], diff(H24))
  )

# --- 4. Initialize probabilities -------------------------------------
n <- nrow(haz_all)
p <- matrix(0, n, 4)  # 4 states
p[1, ] <- c(1, 0, 0, 0)  # everyone starts in Admitted

# --- 5. Iterate over time to update probabilities --------------------
for (i in 2:n) {
  h12 <- haz_all$dH12[i]
  h13 <- haz_all$dH13[i]
  h24 <- haz_all$dH24[i]
  
  # state 1: admitted
  p[i, 1] <- p[i-1, 1] * (1 - (h12 + h13))
  
  # state 2: HAI
  p[i, 2] <- p[i-1, 2] * (1 - h24) + p[i-1, 1] * h12
  
  # state 3: discharged/death w/o HAI (absorbing)
  p[i, 3] <- p[i-1, 3] + p[i-1, 1] * h13
  
  # state 4: discharged/death after HAI (absorbing)
  p[i, 4] <- p[i-1, 4] + p[i-1, 2] * h24
}

# --- 6. Tidy the probabilities ---------------------------------------
prob_df <- data.frame(
  time = haz_all$time,
  Admitted = p[, 1],
  HAI = p[, 2],
  Discharged_no_HAI = p[, 3],
  Discharged_post_HAI = p[, 4]
) %>%
  pivot_longer(-time, names_to = "State", values_to = "Probability")


# --- 7. Change the order ---------------------------------------
prob_df$State <- factor(
  prob_df$State,
  levels = c(
    "Discharged_no_HAI",               # 1. Bottom
    "Admitted", # 2. Second from bottom
    "Discharged_post_HAI",          # 3. Third from bottom
    "HAI"  # 4. Top
  )
)

# --- 8. Plot  ---------------------------------------------------------------

ggplot(prob_df, aes(x = time, y = Probability, fill = State)) +
  geom_area(alpha = 0.85, color = "white") +
  scale_fill_manual(values = c(
    "Admitted" = "deeppink3" ,
    "HAI" = "khaki1",
    "Discharged_no_HAI" = "cornflowerblue" ,
    "Discharged_post_HAI" = "darkolivegreen4"
  )) +
  labs(
    title = "Stacked Probability Plot: CDC unweighted",
    x = "Days Since ICU Admission",
    y = "Probability of Being in State"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

###############################################################################
# CDC weighted --------------------------------------------------------

# --- 1. Extract each transition's cumulative hazard ------------------
H12 <- CDC_w_CH %>% filter(Transition_ID == "trans=1") %>%
  select(time, H12 = cumulative_hazard)
H13 <- CDC_w_CH %>% filter(Transition_ID == "trans=2") %>%
  select(time, H13 = cumulative_hazard)
H24 <- CDC_w_CH %>% filter(Transition_ID == "trans=3") %>%
  select(time, H24 = cumulative_hazard)

# --- 2. Merge and fill missing values --------------------------------
haz_all <- full_join(H12, H13, by = "time") %>%
  full_join(H24, by = "time") %>%
  arrange(time) %>%
  mutate(across(starts_with("H"), ~ zoo::na.locf(., na.rm = FALSE))) %>%
  mutate(across(starts_with("H"), ~ replace_na(., 0)))

# --- 3. Compute hazard increments ------------------------------------
haz_all <- haz_all %>%
  mutate(
    dH12 = c(H12[1], diff(H12)),
    dH13 = c(H13[1], diff(H13)),
    dH24 = c(H24[1], diff(H24))
  )

# --- 4. Initialize probabilities -------------------------------------
n <- nrow(haz_all)
p <- matrix(0, n, 4)  # 4 states
p[1, ] <- c(1, 0, 0, 0)  # everyone starts in Admitted

# --- 5. Iterate over time to update probabilities --------------------
for (i in 2:n) {
  h12 <- haz_all$dH12[i]
  h13 <- haz_all$dH13[i]
  h24 <- haz_all$dH24[i]
  
  # state 1: admitted
  p[i, 1] <- p[i-1, 1] * (1 - (h12 + h13))
  
  # state 2: HAI
  p[i, 2] <- p[i-1, 2] * (1 - h24) + p[i-1, 1] * h12
  
  # state 3: discharged/death w/o HAI (absorbing)
  p[i, 3] <- p[i-1, 3] + p[i-1, 1] * h13
  
  # state 4: discharged/death after HAI (absorbing)
  p[i, 4] <- p[i-1, 4] + p[i-1, 2] * h24
}

# --- 6. Tidy the probabilities ---------------------------------------
prob_df <- data.frame(
  time = haz_all$time,
  Admitted = p[, 1],
  HAI = p[, 2],
  Discharged_no_HAI = p[, 3],
  Discharged_post_HAI = p[, 4]
) %>%
  pivot_longer(-time, names_to = "State", values_to = "Probability")


# --- 7. Change the order ---------------------------------------
prob_df$State <- factor(
  prob_df$State,
  levels = c(
    "Discharged_no_HAI",               # 1. Bottom
    "Admitted", # 2. Second from bottom
    "Discharged_post_HAI",          # 3. Third from bottom
    "HAI"  # 4. Top
  )
)

# --- 8. Plot  ---------------------------------------------------------------

ggplot(prob_df, aes(x = time, y = Probability, fill = State)) +
  geom_area(alpha = 0.85, color = "white") +
  scale_fill_manual(values = c(
    "Admitted" = "deeppink3" ,
    "HAI" = "khaki1",
    "Discharged_no_HAI" = "cornflowerblue" ,
    "Discharged_post_HAI" = "darkolivegreen4"
  )) +
  labs(
    title = "Stacked Probability Plot: CDC weighted",
    x = "Days Since ICU Admission",
    y = "Probability of Being in State"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
