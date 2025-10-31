######### Tornado plot for CDC protocol ####################################

library(ggplot2)
library(dplyr)

# 1.  Subset 50 patients from PPS_0 ------------------------------------------
set.seed(39)
Swimm_CDC_75 <- PPS_0 %>% slice_sample(n = 50)

# Order patients by ICU_LOS (descending)
Swimm_CDC_75 <- Swimm_CDC_75 %>%
  arrange(ICU_LOS) %>%
  mutate(patient_id = factor(row_number(), levels = row_number()))

# 2. Define start and end of the plot ------------------------------------------
Tornado <- Swimm_CDC_75
Tornado$start <- 0 - Tornado$PPS_date
Tornado$end <- Tornado$cens_ICU_LOS_time- Tornado$PPS_date
Tornado$infection_time_PPS <- Tornado$cens_infection_time - Tornado$PPS_date


# 3. Plot ---------------------------------------------------------------------
ggplot(Tornado) +
  #Segments per patient
  geom_segment(aes(x = start, xend = end, y = patient_id, yend = patient_id),
               color = "darkgray",
               size = 3) +
  
  #Infected patients infection status know
  geom_segment(data = dplyr::filter(Tornado, end > 0, prevalent_infection == 1),
               aes(x = 0, xend = end, y = patient_id, yend = patient_id),
               color = "darkgray",
               size = 3) +
  
  #non infected patients status after PPS unknown
  geom_segment(data = dplyr::filter(Tornado, end > 0, prevalent_infection == 0),
               aes(x = 0, xend = end, y = patient_id, yend = patient_id),
               color = "lightgray",
               size = 3) +
  
  #Black line time 0
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.8) +
  
  #Point at infection time
  geom_point(data = dplyr::filter(Tornado, prevalent_infection == 1),
             aes(x = infection_time_PPS, y = patient_id),
             color = "black",
             size = 2, # Adjust size as needed
             shape = 19) + # shape = 19 for a filled circle
  
  #x axis
  scale_x_continuous(name = "days from PPS", # Set the x-axis label
                     limits = c(-120, 80), 
                     breaks = seq(-120, 80, by = 10)) +
  
  labs(y = "patient") +
  
  
  # White background and labels bigger
  theme(
    panel.background = element_rect(fill = "white", colour = NA), # Sets the plotting area background to white
    plot.background = element_rect(fill = "white", colour = NA), # Sets the overall plot background to white
    axis.title.x = element_text(size = 14), # Increase x-axis title font size
    axis.title.y = element_text(size = 14), # Increase y-axis title font size
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
