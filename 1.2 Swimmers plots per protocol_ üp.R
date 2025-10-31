#### Swimmers plot for each protocol
### Run 1.1 Full cohort_resp_überprüft until line 86

library(ggplot2)
library(dplyr)

# 1. Calculate weights ---------------------------------------------------------

#Find the 99% quantile = 90 becuase of censoring
quantile(Resp_0$cens_ICU_LOS_time, probs = 0.99, na.rm = TRUE)

#Assign a number between 0 and 90 to each patient
set.seed(987)
Resp_0$PPS_date <- runif(n = nrow(Resp_0), min = 0, max = 90)


#2. Define prevalent and incident infection ------------------------------------
Resp_0$prevalent_infection <- ifelse(Resp_0$cens_infection_status == 1 & 
                                       Resp_0$cens_infection_time < Resp_0$PPS_date,
                                     1,
                                     0)

Resp_0$incident_infection <- ifelse(Resp_0$cens_infection_status == 1 & 
                                      Resp_0$cens_infection_time > Resp_0$PPS_date,
                                    1,
                                    0)

Resp_0$sampled <- ifelse(Resp_0$PPS_date < Resp_0$cens_ICU_LOS_time,
                         1,
                         0)

# 3.Subset 50 patients from Resp_0 ---------------------------------------------
set.seed(95)
Swimm_CDC_75 <- Resp_0 %>% slice_sample(n = 50)

# Order patients by ICU_LOS (descending)
Swimm_CDC_75 <- Swimm_CDC_75 %>%
  arrange(ICU_LOS) %>%
  mutate(patient_id = factor(row_number(), levels = row_number()))


###############################################################################
################# SWIMMERS PLOT FULL COHORT ################


ggplot(Swimm_CDC_75) +
  
  # For patients with infection == 0 and a PPS_date within their ICU stay,
  # This segment is now mapped to 'Non infected' for the COLOR legend.
  geom_segment(
    aes(
      x = 0,
      xend = cens_ICU_LOS_time,
      y = patient_id,
      yend = patient_id,
      # Mapping to color to create the legend item.
      color = "Non infected" 
    ),
    linewidth = 2
  ) +
  
  # Infection segments.
  # This segment is mapped to 'Infected' for the color legend.
  geom_segment(
    data = subset(Swimm_CDC_75, infection == 1),
    aes(
      x = time_to_infection,
      xend = cens_ICU_LOS_time,
      y = patient_id,
      yend = patient_id,
      # Mapping to color to create the legend item.
      color = "Infected"
    ),
    linewidth = 2,
    linetype = "solid" # Explicitly set to solid
  ) +
  
  # Correct manual color scale to handle all three "Status" labels.
  scale_color_manual(
    name = "Status",
    values = c(
      "Infected" = "coral2",
      "Non infected" = "darkgray"
    )
  )+
  
  # Black dot for censoring at 90 days.
  geom_point(
    data = subset(Swimm_CDC_75, cens_ICU_LOS_status == 0),
    aes(x = 90, y = patient_id, shape = "Censored (90d)"),
    size = 2,
    color = "black",
    stroke = 2
  ) +
  
  # Red cross for ICU death.
  geom_point(
    data = subset(Swimm_CDC_75, ICU_death == 1),
    aes(x = ICU_LOS, y = patient_id, shape = "ICU death"),
    size = 2,
    color = "darkolivegreen3",
    stroke = 2
  ) +
  
  # Set x-axis limits and labels.
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 10)) +
  labs(x = "time from admission (days)", y = "patient") +
  
  # Theme and styling.
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  ) + 
  
  # A single guides() function is used to coordinate all legend aesthetics.
  guides(
    shape = guide_legend(override.aes = list(
      color = c("black", "darkolivegreen3")
    ))
  )

###############################################################################
################# SWIMMERS PLOT MIMC PPS 1 ################
##No transparency


ggplot(Swimm_CDC_75) +
  
  ####### FOR SAMPLED PATIENTS; DARK LINES  
  # For patients with infection == 0 and a PPS_date within their ICU stay,
  # This segment is now mapped to 'Non infected' for the COLOR legend.
  geom_segment(
    aes(
      x = 0,
      xend = cens_ICU_LOS_time,
      y = patient_id,
      yend = patient_id,
      # Mapping to color to create the legend item.
      color = "Non infected" 
    ),
    linewidth = 2
  ) +
  
  # Infection segments.
  # This segment is mapped to 'Infected' for the color legend.
  geom_segment( 
    data = subset(Swimm_CDC_75, prevalent_infection == 1 ),
    aes(
      x = time_to_infection,
      xend = cens_ICU_LOS_time,
      y = patient_id,
      yend = patient_id,
      # Mapping to color to create the legend item.
      color = "Prevalent Infection"
    ),
    linewidth = 2,
    linetype = "solid" # Explicitly set to solid
  ) +
  
  # For incident infection
  
  geom_segment(
    data = subset(Swimm_CDC_75, incident_infection == 1 ),
    aes(
      x = time_to_infection,
      xend = cens_ICU_LOS_time,
      y = patient_id,
      yend = patient_id,
      # Mapping to color to create the legend item.
      color = "Incident Infection"
    ),
    linewidth = 2,
    linetype = "solid" # Explicitly set to solid
  ) +
  
  # Correct manual color scale to handle all three "Status" labels.
  scale_color_manual(
    name = "Status",
    values = c(
      "Prevalent Infection" = "coral2",
      "Incident Infection" = "purple",
      "Non infected" = "darkgray"
    )
  ) +
  
  # Black dot for censoring at 90 days.
  geom_point(
    data = subset(Swimm_CDC_75, cens_ICU_LOS_status == 0),
    aes(x = 90, y = patient_id, shape = "Censored (90d)"),
    size = 2,
    color = "black",
    stroke = 2
  ) +
  
  # Red cross for ICU death.
  geom_point(
    data = subset(Swimm_CDC_75, ICU_death == 1),
    aes(x = ICU_LOS, y = patient_id, shape = "ICU death"),
    size = 2,
    color = "darkolivegreen3",
    stroke = 2
  ) +
  
  
  # Add a vertical blue line for each patient at their PPS_date.
  # This segment is mapped to 'PPS date' for the linetype legend.
  geom_segment(
    data = subset(Swimm_CDC_75, PPS_date < 90),
    aes(x = PPS_date,
        xend = PPS_date + 0.3,
        y = patient_id,
        yend = patient_id,
        # Mapping to linetype to create the legend item.
        linetype = "PPS date"
    ),
    color = "blue", # Fixed color
    linewidth = 3
  ) +
  
  # The linetype scale now has a name and values, and the guide is handled separately.
  scale_linetype_manual(
    name = "",
    values = c("PPS date" = "dashed")
  ) +
  
  # Set x-axis limits and labels.
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 10)) +
  labs(x = "time from admission (days)", y = "patient") +
  
  # Theme and styling.
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  ) + 
  
  # A single guides() function is used to coordinate all legend aesthetics.
  guides(
    linetype = guide_legend(override.aes = list(
      color = "blue", 
      size = 2, 
      alpha = 1
    )),
    shape = guide_legend(override.aes = list(
      color = c("black", "darkolivegreen3")
    ))
  )

###############################################################################
################# SWIMMERS PLOT MIMC PPS 2 ################
#With transparency




ggplot(Swimm_CDC_75) +

  ####### FOR SAMPLED PATIENTS; DARK LINES  
  # For patients with infection == 0 and a PPS_date within their ICU stay,
  # This segment is now mapped to 'Non infected' for the COLOR legend.
  geom_segment(
    aes(
      x = 0,
      xend = cens_ICU_LOS_time,
      y = patient_id,
      yend = patient_id,
      # Mapping to color to create the legend item.
      color = "Non infected" ,
      alpha = ifelse(sampled == 0, 0.9, 1)
    ),
    linewidth = 2
  ) +
  
  # Infection segments.
  # This segment is mapped to 'Infected' for the color legend.
  geom_segment( 
    data = subset(Swimm_CDC_75, prevalent_infection == 1 ),
    aes(
      x = time_to_infection,
      xend = cens_ICU_LOS_time,
      y = patient_id,
      yend = patient_id,
      # Mapping to color to create the legend item.
      color = "Prevalent Infection",
      alpha = ifelse(sampled == 0, 0.9, 1)
    ),
    linewidth = 2,
    linetype = "solid" # Explicitly set to solid
  ) +
  
  # For incident infection
  
  geom_segment(
    data = subset(Swimm_CDC_75, incident_infection == 1 ),
    aes(
      x = time_to_infection,
      xend = ICU_LOS,
      y = patient_id,
      yend = patient_id,
      # Mapping to color to create the legend item.
      color = "Incident Infection",
      alpha = ifelse(sampled == 0, 0.9, 1) # Transparency
    ),
    linewidth = 2,
    linetype = "solid" # Explicitly set to solid
  ) +
  
  # Correct manual color scale to handle all three "Status" labels.
  scale_color_manual(
    name = "Status",
    values = c(
      "Prevalent Infection" = "coral2",
      "Incident Infection" = "purple",
      "Non infected" = "darkgray"
    )
  ) +
  
  # Black dot for censoring at 90 days.
  geom_point(
    data = subset(Swimm_CDC_75, cens_ICU_LOS_status == 0),
    aes(x = 90, y = patient_id, shape = "Censored (90d)"),
    size = 2,
    color = "black",
    stroke = 2
  ) +
  
  # Red cross for ICU death.
  geom_point(
    data = subset(Swimm_CDC_75, ICU_death == 1),
    aes(x = ICU_LOS, y = patient_id, shape = "ICU death"),
    size = 2,
    color = "darkolivegreen3",
    stroke = 2
  ) +
  
  
  # Add a vertical blue line for each patient at their PPS_date.
  # This segment is mapped to 'PPS date' for the linetype legend.
  geom_segment(
    data = subset(Swimm_CDC_75, PPS_date < 90),
    aes(x = PPS_date,
        xend = PPS_date + 0.3,
        y = patient_id,
        yend = patient_id,
        # Mapping to linetype to create the legend item.
        linetype = "PPS date"
    ),
    color = "blue", # Fixed color
    linewidth = 3
  ) +
  
  # The linetype scale now has a name and values, and the guide is handled separately.
  scale_linetype_manual(
    name = "",
    values = c("PPS date" = "dashed")
  ) +
  
  # Set x-axis limits and labels.
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 10)) +
  labs(x = "time from admission (days)", y = "patient") +
  
  # Theme and styling.
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  ) + 
  
  # A single guides() function is used to coordinate all legend aesthetics.
  guides(
    linetype = guide_legend(override.aes = list(
      color = "blue", 
      size = 2, 
      alpha = 1
    )),
    shape = guide_legend(override.aes = list(
      color = c("black", "darkolivegreen3")
    ))
  )



###############################################################################
################# SWIMMERS PLOT PPS CDC ################

################# SWIMMERS PLOT FOR A RANDOM SUBSET OF 75 PATIENTS ################

ggplot(Swimm_CDC_75) +
  
  # For patients with infection == 0 and a PPS_date within their ICU stay,
  # This segment is now mapped to 'Non infected' for the COLOR legend.
  geom_segment(
    aes(
      x = 0,
      xend = ifelse(
        prevalent_infection == 0 & sampled == 1,
        PPS_date,
        cens_ICU_LOS_time
      ),
      y = patient_id,
      yend = patient_id,
      # Mapping to color to create the legend item.
      color = "Non infected",
      alpha = ifelse(sampled == 0, 0.9, 1)
    ),
    linewidth = 2
  ) +
  
  # Infection segments.
  # This segment is mapped to 'Infected' for the color legend.
  geom_segment(
    data = subset(Swimm_CDC_75, prevalent_infection == 1),
    aes(
      x = time_to_infection,
      xend = cens_ICU_LOS_time,
      y = patient_id,
      yend = patient_id,
      # Mapping to color to create the legend item.
      color = "Infected",
      alpha = ifelse(sampled == 0, 0.9, 1)
    ),
    linewidth = 2,
    linetype = "solid" # Explicitly set to solid
  ) +
  
  # Add a lightgrey, transparent segment for patients with infection == 0.
  # This segment is now mapped to 'Infection Status Unknown' for the COLOR legend.
  geom_segment(
    data = subset(Swimm_CDC_75, prevalent_infection == 0 & sampled == 1),
    aes(x = PPS_date,
        xend = cens_ICU_LOS_time,
        y = patient_id,
        yend = patient_id,
        # Mapping to color to create the legend item.
        color = "Infection Status Unknown",
        alpha = ifelse(sampled == 0, 0.9, 1)
    ),
    linewidth = 2
  ) +
  
  # Correct manual color scale to handle all three "Status" labels.
  scale_color_manual(
    name = "Status",
    values = c(
      "Infected" = "coral2",
      "Non infected" = "darkgray",
      "Infection Status Unknown" = "lightgray"
    )
  ) +
  
  # Black dot for censoring at 90 days.
  geom_point(
    data = subset(Swimm_CDC_75, cens_ICU_LOS_status == 0),
    aes(x = 90, y = patient_id, shape = "Censored (90d)"),
    size = 2,
    color = "black",
    stroke = 2
  ) +

  
  # Add a vertical blue line for each patient at their PPS_date.
  # This segment is mapped to 'PPS date' for the linetype legend.
  geom_segment(
    aes(x = PPS_date,
        xend = PPS_date + 0.3,
        y = patient_id,
        yend = patient_id,
        # Mapping to linetype to create the legend item.
        linetype = "PPS date"
    ),
    color = "blue", # Fixed color
    linewidth = 3
  ) +
  
  # The linetype scale now has a name and values, and the guide is handled separately.
  scale_linetype_manual(
    name = "",
    values = c("PPS date" = "dashed")
  ) +
  
  # Set x-axis limits and labels.
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 10)) +
  labs(x = "time from admission (days)", y = "patient") +
  
  # Theme and styling.
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  ) + 
  
  # A single guides() function is used to coordinate all legend aesthetics.
  guides(
    linetype = guide_legend(override.aes = list(
      color = "blue", 
      size = 2, 
      alpha = 1
    )),
    shape = guide_legend(override.aes = list(
      color = c("black")
    ))
  )



###############################################################################
#################  SWIMMERS PLOT PPS EPIC##################### ################


ggplot(Swimm_CDC_75) +
  
  # 1. BASE SEGMENT: Non-Infected (Dark Gray)
  # The length of this segment now stops at PPS_date if the patient is sampled AND prevalent_infected.
  geom_segment(
    aes(
      x = 0,
      xend = ifelse(
        prevalent_infection == 0 & sampled == 1,
        PPS_date,
        cens_ICU_LOS_time
      ),
      y = patient_id,
      yend = patient_id,
      color = "Non infected",
      alpha = ifelse(sampled == 0, 0.9, 1)
    ),
    linewidth = 2
  ) +
  
  # 2. **NEW SEGMENT:** Pre-PPS Infection Status Unknown (Light Gray)
  # This covers the period from admission (0) to PPS_date for PREVALENTLY infected and sampled patients.
  geom_segment(
    data = subset(Swimm_CDC_75, prevalent_infection == 1 & sampled == 1),
    aes(
      x = 0,
      xend = PPS_date,
      y = patient_id,
      yend = patient_id,
      color = "Infection Status Unknown",
      alpha = ifelse(sampled == 0, 0.9, 1)
    ),
    linewidth = 2,
    linetype = "solid"
  ) +
  
  # 3. **MODIFIED SEGMENT:** Confirmed Infected (Coral2)
  # For prevalent infection (== 1), the segment now starts at PPS_date (Change #2).
  geom_segment(
    data = subset(Swimm_CDC_75, prevalent_infection == 1),
    aes(
      # CHANGED: Starting point is PPS_date if sampled, otherwise it's time_to_infection
      x = ifelse(sampled == 1, PPS_date, time_to_infection),
      xend = cens_ICU_LOS_time,
      y = patient_id,
      yend = patient_id,
      color = "Infected",
      alpha = ifelse(sampled == 0, 0.9, 1)
    ),
    linewidth = 2,
    linetype = "solid"
  ) +
  
  # 4. Light Gray UNKNOWN segment for Non-infected patients (Original segment)
  # This remains to show the period after PPS_date where status is unknown for non-infected sampled patients.
  geom_segment(
    data = subset(Swimm_CDC_75, prevalent_infection == 0 & sampled == 1),
    aes(
      x = PPS_date,
      xend = cens_ICU_LOS_time,
      y = patient_id,
      yend = patient_id,
      color = "Infection Status Unknown",
      alpha = ifelse(sampled == 0, 0.9, 1)
    ),
    linewidth = 2
  ) +
  
  # Correct manual color scale (No changes needed here)
  scale_color_manual(
    name = "Status",
    values = c(
      "Infected" = "coral2",
      "Non infected" = "darkgray",
      "Infection Status Unknown" = "lightgray"
    )
  ) +
  
  # Black dot for censoring at 90 days. (No changes)
  geom_point(
    data = subset(Swimm_CDC_75, cens_ICU_LOS_status == 0),
    aes(x = 90, y = patient_id, shape = "Censored (90d)"),
    size = 2,
    color = "black",
    stroke = 2
  ) +
  
  # Add a vertical blue line for each patient at their PPS_date. (No changes)
  geom_segment(
    aes(x = PPS_date,
        xend = PPS_date + 0.3,
        y = patient_id,
        yend = patient_id,
        linetype = "PPS date"
    ),
    color = "blue",
    linewidth = 3
  ) +
  
  # The linetype scale (No changes)
  scale_linetype_manual(
    name = "",
    values = c("PPS date" = "dashed")
  ) +
  
  # Set x-axis limits and labels. (No changes)
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 10)) +
  labs(x = "time from admission (days)", y = "patient") +
  
  # Theme and styling. (No changes)
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  ) +
  
  # A single guides() function is used to coordinate all legend aesthetics. (No changes)
  guides(
    linetype = guide_legend(override.aes = list(
      color = "blue",
      size = 2,
      alpha = 1
    )),
    shape = guide_legend(override.aes = list(
      color = c("black")
    ))
  )

