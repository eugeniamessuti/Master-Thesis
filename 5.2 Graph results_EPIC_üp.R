########## Plots results EPIC ##########################################

library(readxl)
library(dplyr)
library(ggplot2)
library(scales)


setwd("C:/Users/messuti/Documents/Reports/4 Überpruft Ergebnise/Excel Tables Final Results/2 EPIC")


# 1. Admited to HAI ------------------------------------------------------------

# 1) Read file

Results_Adm_to_HAI_EPIC <- read_excel("5.2.1 Results EPIC _ Adm to HAI.xlsx", sheet = 1)



# 2) Convert columns into factors
Results_Adm_to_HAI_EPIC2 <- Results_Adm_to_HAI_EPIC %>%
  mutate(
    HR = as.numeric(gsub(",", ".", HR)),
    Lower_CI = as.numeric(gsub(",", ".", Lower_CI)),
    Upper_CI = as.numeric(gsub(",", ".", Upper_CI)),
    Method = as.numeric(Method),
    Significant = as.factor(Significant)
  )

# 3) Prepare tags and order per factor
method_labs_Results_Adm_to_HAI_EPIC <- Results_Adm_to_HAI_EPIC2 %>%
  select(Method, Method_Name) %>%
  distinct() %>%
  arrange(Method)

# Build a vector for my tags
max_method <- max(Results_Adm_to_HAI_EPIC2$Method, na.rm = TRUE)
method_labels <- as.character(1:max_method)
if(nrow(method_labs_Results_Adm_to_HAI_EPIC) >= 1){
  idx <- as.integer(method_labs_Results_Adm_to_HAI_EPIC$Method)
  method_labels[idx] <- method_labs_Results_Adm_to_HAI_EPIC$Method_Name
}

# 3b) Invert the order of the methods
Results_Adm_to_HAI_EPIC2 <- Results_Adm_to_HAI_EPIC2 %>%
  mutate(Method = factor(Method, levels = rev(sort(unique(as.numeric(Method))))))

method_labels <- rev(method_labels)


# 4) Create a data frame for vertical lines = HR full cohort
vline_data_EPIC <- Results_Adm_to_HAI_EPIC2 %>%
  filter(Method == 1) %>% 
  select(Covariate, HR) %>%
  rename(HR_Method1 = HR) 

# Nota: Eliminamos los pasos 4 y 5 de límites personalizados (axis_limits y left_join)


# 5) Plot
p_epic_adm_hai <- ggplot(Results_Adm_to_HAI_EPIC2, aes(x = HR, y = Method, color = Significant)) +
  
  # Confidence intervals
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.18, linewidth = 2) +
  geom_point(size = 5) +
  
  # Reference line (HR = 1)
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  
  #  Reference line (HR= Fullcohort) ***
  geom_vline(data = vline_data_EPIC, 
             aes(xintercept = HR_Method1), 
             color = "red", 
             linewidth = 0.8) +
  
  # Facet
  facet_wrap(~ Covariate, scales = "free_x", ncol = 2) +
  
  # log scale
  scale_x_log10(
    breaks = scales::pretty_breaks(n = 5),
    minor_breaks = NULL 
  ) +
  
  scale_y_discrete(labels = method_labels) +
  scale_color_manual(values = c("0" = "steelblue", "1" = "firebrick"),
                     labels = c("0" = "Not significant", "1" = "Significant"),
                     name = "Significance") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "gray80", size = 0.4),
    panel.grid.minor = element_line(color = "gray90", size = 0.2),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )+
  # Titles
  labs(
    x = "Hazard Ratio (95% CI) [Log Scale]",
    y = "Method",
    title = "Hazard Ratios across Methods by Covariate",
    subtitle = "Transition: Admitted → HAI (EPIC)"
  )

# Show plot
print(p_epic_adm_hai)



# 2. Admitted to discharged ----------------------------------------------------

# 1) Read the Excel file
Results_Adm_to_Disch_EPIC <- read_excel("5.2.2 Results EPIC_ Adm to Disch.xlsx", sheet = 1)

# 2) Convert numeric columns (handling comma as decimal separator)
Results_Adm_to_Disch_EPIC2 <- Results_Adm_to_Disch_EPIC %>%
  mutate(
    HR = as.numeric(gsub(",", ".", HR)),
    Lower_CI = as.numeric(gsub(",", ".", Lower_CI)),
    Upper_CI = as.numeric(gsub(",", ".", Upper_CI)),
    Method = as.numeric(Method),
    Significant = as.factor(Significant)
  )

# 3) Prepare method labels
method_labs_Results_Adm_to_Disch_EPIC <- Results_Adm_to_Disch_EPIC2 %>%
  select(Method, Method_Name) %>%
  distinct() %>%
  arrange(Method)

# Build ordered label vector
max_method <- max(Results_Adm_to_Disch_EPIC2$Method, na.rm = TRUE)
method_labels <- as.character(1:max_method)
if(nrow(method_labs_Results_Adm_to_Disch_EPIC) >= 1){
  idx <- as.integer(method_labs_Results_Adm_to_Disch_EPIC$Method)
  method_labels[idx] <- method_labs_Results_Adm_to_Disch_EPIC$Method_Name
}

# 3b) Invert the order of methods (from 6 to 1)
Results_Adm_to_Disch_EPIC2 <- Results_Adm_to_Disch_EPIC2 %>%
  mutate(Method = factor(Method, levels = rev(sort(unique(as.numeric(Method))))))

method_labels <- rev(method_labels)


# 4) *** KEY STEP: Create data frame for vertical lines (HR Method 1) ***
vline_data_EPIC_Disch <- Results_Adm_to_Disch_EPIC2 %>%
  filter(Method == 1) %>% 
  select(Covariate, HR) %>%
  rename(HR_Method1 = HR) 

# Note: Manual axis limit steps (4 & 5) are removed.


# 5) Final Plot
p_epic_adm_disch <- ggplot(Results_Adm_to_Disch_EPIC2, aes(x = HR, y = Method, color = Significant)) +
  
  # Confidence Intervals
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.18, linewidth = 2) +
  geom_point(size = 5) +
  
  # Main reference line (HR = 1)
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  
  # Method 1 HR line (specific to each facet)
  geom_vline(data = vline_data_EPIC_Disch, 
             aes(xintercept = HR_Method1), 
             color = "red", 
             linewidth = 0.8) +
  
  # Facets (using free_x)
  facet_wrap(~ Covariate, scales = "free_x", ncol = 2) +
  
  # *** LOGARITHMIC X-SCALE ***
  scale_x_log10(
    breaks = scales::pretty_breaks(n = 5),
    minor_breaks = NULL 
  ) +
  
  scale_y_discrete(labels = method_labels) +
  scale_color_manual(values = c("0" = "steelblue", "1" = "firebrick"),
                     labels = c("0" = "Not significant", "1" = "Significant"),
                     name = "Significance") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "gray80", size = 0.4),
    panel.grid.minor = element_line(color = "gray90", size = 0.2),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )+
  # Titles in ENGLISH
  labs(
    x = "Hazard Ratio (95% CI) [Log Scale]",
    y = "Method",
    title = "Hazard Ratios across Methods by Covariate",
    subtitle = "Transition: Admitted → Discharged (EPIC)"
  )

# Display the plot
print(p_epic_adm_disch)




# 3. HAI to discharged --------------------------------------------------------


# 1) Read the Excel file
Results_HAI_to_Disch_EPIC <- read_excel("5.2.3 Results EPIC_ HAI to Disch.xlsx", sheet = 1)

# 2) Convert numeric columns and factor (handling comma as decimal separator)
Results_HAI_to_Disch_EPIC2 <- Results_HAI_to_Disch_EPIC %>%
  mutate(
    HR = as.numeric(gsub(",", ".", HR)),
    Lower_CI = as.numeric(gsub(",", ".", Lower_CI)),
    Upper_CI = as.numeric(gsub(",", ".", Upper_CI)),
    Method = as.numeric(Method),
    Significant = as.factor(Significant)
  )

# 3) Prepare method labels
method_labs_Results_HAI_to_Disch_EPIC <- Results_HAI_to_Disch_EPIC2 %>%
  select(Method, Method_Name) %>%
  distinct() %>%
  arrange(Method)

# Build ordered label vector
max_method <- max(Results_HAI_to_Disch_EPIC2$Method, na.rm = TRUE)
method_labels <- as.character(1:max_method)
if(nrow(method_labs_Results_HAI_to_Disch_EPIC) >= 1){
  idx <- as.integer(method_labs_Results_HAI_to_Disch_EPIC$Method)
  method_labels[idx] <- method_labs_Results_HAI_to_Disch_EPIC$Method_Name
}

# 3b) Invert the order of methods
Results_HAI_to_Disch_EPIC2 <- Results_HAI_to_Disch_EPIC2 %>%
  mutate(Method = factor(Method, levels = rev(sort(unique(as.numeric(Method))))))

method_labels <- rev(method_labels)


# 4) *** KEY STEP: Create data frame for vertical lines (HR Method 1) ***
vline_data_EPIC_HAI_Disch <- Results_HAI_to_Disch_EPIC2 %>%
  filter(Method == 1) %>% 
  select(Covariate, HR) %>%
  rename(HR_Method1 = HR) 

# Note: Manual axis limit steps (4 & 5) are removed.


# 5) Final Plot
p_epic_hai_disch <- ggplot(Results_HAI_to_Disch_EPIC2, aes(x = HR, y = Method, color = Significant)) +
  
  # Confidence Intervals
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.18, linewidth = 2) +
  geom_point(size = 5) +
  
  # Main reference line (HR = 1)
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  
  # Method 1 HR line (specific to each facet)
  geom_vline(data = vline_data_EPIC_HAI_Disch, 
             aes(xintercept = HR_Method1), 
             color = "red", 
             linewidth = 0.8) +
  
  # Facets (using free_x)
  facet_wrap(~ Covariate, scales = "free_x", ncol = 2) +
  
  # *** LOGARITHMIC X-SCALE ***
  scale_x_log10(
    breaks = scales::pretty_breaks(n = 5),
    minor_breaks = NULL 
  ) +
  
  scale_y_discrete(labels = method_labels) +
  scale_color_manual(values = c("0" = "steelblue", "1" = "firebrick"),
                     labels = c("0" = "Not significant", "1" = "Significant"),
                     name = "Significance") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "gray80", size = 0.4),
    panel.grid.minor = element_line(color = "gray90", size = 0.2),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )+
  # Titles in ENGLISH
  labs(
    x = "Hazard Ratio (95% CI) [Log Scale]",
    y = "Method",
    title = "Hazard Ratios across Methods by Covariate",
    subtitle = "Transition: HAI → Discharged (EPIC)"
  )

# Display the plot
print(p_epic_hai_disch)




############### PLOTS ONLY EPIC IPW WITH AND WITHOUT BOOTSTRAPPING ###########

######################### ADM -> HAI ##############################
# 1) Leer el Excel (Asegúrate de que el archivo esté en la carpeta de trabajo)
Results_Adm_to_HAI <- read_excel("5.2.4 Results EPIC _ Adm to HAI - 5 y 6.xlsx", sheet = 1)

# 2) Convertir columnas numéricas y de factor
Results_Adm_to_HAI2 <- Results_Adm_to_HAI %>%
  mutate(
    HR = as.numeric(gsub(",", ".", HR)),
    Lower_CI = as.numeric(gsub(",", ".", Lower_CI)),
    Upper_CI = as.numeric(gsub(",", ".", Upper_CI)),
    Method = as.numeric(Method),
    Significant = as.factor(Significant)
  )

# 3) Preparar etiquetas de método y ordenar por Factor
method_labs_Results_Adm_to_HAI <- Results_Adm_to_HAI2 %>%
  select(Method, Method_Name) %>%
  distinct() %>%
  arrange(Method)

# Construir vector de etiquetas en orden
max_method <- max(Results_Adm_to_HAI2$Method, na.rm = TRUE)
method_labels <- as.character(1:max_method)
if(nrow(method_labs_Results_Adm_to_HAI) >= 1){
  idx <- as.integer(method_labs_Results_Adm_to_HAI$Method)
  method_labels[idx] <- method_labs_Results_Adm_to_HAI$Method_Name
}

# 3b) Invertir el orden de los métodos
Results_Adm_to_HAI2 <- Results_Adm_to_HAI2 %>%
  mutate(Method = factor(Method, levels = rev(sort(unique(as.numeric(Method))))))

method_labels <- rev(method_labels)


# 4) *** NUEVO PASO: Crear el data frame para las líneas verticales ***
vline_data <- Results_Adm_to_HAI2 %>%
  filter(Method == 5) %>% # Filtra solo el Método 1
  select(Covariate, HR) %>%
  rename(HR_Method1 = HR) # Renombra la columna para claridad


# 5) Plot
p <- ggplot(Results_Adm_to_HAI2, aes(x = HR, y = Method, color = Significant)) +
  # Usamos Lower_CI
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.18, linewidth = 2) +
  geom_point(size = 5) +
  
  # Línea de referencia principal (HR = 1)
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  
  # *** NUEVA LÍNEA: geom_vline con el data frame auxiliar ***
  geom_vline(data = vline_data, 
             aes(xintercept = HR_Method1), 
             color = "red", 
             linewidth = 0.8) +
  
  facet_wrap(~ Covariate, scales = "free_x", ncol = 2) +
  
  # Escala Logarítmica Base 10
  scale_x_log10(
    breaks = scales::pretty_breaks(n = 5),
    minor_breaks = NULL 
  ) +
  
  scale_y_discrete(labels = method_labels) +
  scale_color_manual(values = c("0" = "steelblue", "1" = "firebrick"),
                     labels = c("0" = "Non significant", "1" = "Significant"),
                     name = "Statistical significance") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "gray80", size = 0.4),
    panel.grid.minor = element_line(color = "gray90", size = 0.2),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )+
  labs(
    x = "Hazard Ratio (95% CI) [Log Scale]",
    y = "Method",
    title = "Hazard Ratios across Methods per Covariate",
    subtitle = "Transition: Admitted → HAI"
  )

# Mostrar el gráfico
print(p)


######################### ADM -> DISCH ##############################
# 1) Leer el Excel (Asegúrate de que el archivo esté en la carpeta de trabajo)
Results_Adm_to_HAI <- read_excel("5.2.5 Results EPIC_ Adm to Disch - 5 y 6.xlsx", sheet = 1)

# 2) Convertir columnas numéricas y de factor
Results_Adm_to_HAI2 <- Results_Adm_to_HAI %>%
  mutate(
    HR = as.numeric(gsub(",", ".", HR)),
    Lower_CI = as.numeric(gsub(",", ".", Lower_CI)),
    Upper_CI = as.numeric(gsub(",", ".", Upper_CI)),
    Method = as.numeric(Method),
    Significant = as.factor(Significant)
  )

# 3) Preparar etiquetas de método y ordenar por Factor
method_labs_Results_Adm_to_HAI <- Results_Adm_to_HAI2 %>%
  select(Method, Method_Name) %>%
  distinct() %>%
  arrange(Method)

# Construir vector de etiquetas en orden
max_method <- max(Results_Adm_to_HAI2$Method, na.rm = TRUE)
method_labels <- as.character(1:max_method)
if(nrow(method_labs_Results_Adm_to_HAI) >= 1){
  idx <- as.integer(method_labs_Results_Adm_to_HAI$Method)
  method_labels[idx] <- method_labs_Results_Adm_to_HAI$Method_Name
}

# 3b) Invertir el orden de los métodos
Results_Adm_to_HAI2 <- Results_Adm_to_HAI2 %>%
  mutate(Method = factor(Method, levels = rev(sort(unique(as.numeric(Method))))))

method_labels <- rev(method_labels)


# 4) *** NUEVO PASO: Crear el data frame para las líneas verticales ***
vline_data <- Results_Adm_to_HAI2 %>%
  filter(Method == 5) %>% # Filtra solo el Método 1
  select(Covariate, HR) %>%
  rename(HR_Method1 = HR) # Renombra la columna para claridad


# 5) Plot
p <- ggplot(Results_Adm_to_HAI2, aes(x = HR, y = Method, color = Significant)) +
  # Usamos Lower_CI
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.18, linewidth = 2) +
  geom_point(size = 5) +
  
  # Línea de referencia principal (HR = 1)
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  
  # *** NUEVA LÍNEA: geom_vline con el data frame auxiliar ***
  geom_vline(data = vline_data, 
             aes(xintercept = HR_Method1), 
             color = "red", 
             linewidth = 0.8) +
  
  facet_wrap(~ Covariate, scales = "free_x", ncol = 2) +
  
  # Escala Logarítmica Base 10
  scale_x_log10(
    breaks = scales::pretty_breaks(n = 5),
    minor_breaks = NULL 
  ) +
  
  scale_y_discrete(labels = method_labels) +
  scale_color_manual(values = c("0" = "steelblue", "1" = "firebrick"),
                     labels = c("0" = "Non significant", "1" = "Significant"),
                     name = "Statistical significance") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "gray80", size = 0.4),
    panel.grid.minor = element_line(color = "gray90", size = 0.2),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )+
  labs(
    x = "Hazard Ratio (95% CI) [Log Scale]",
    y = "Method",
    title = "Hazard Ratios across Methods per Covariate",
    subtitle = "Transition: Admitted → Discharged"
  )

# Mostrar el gráfico
print(p)


######################### HAI -> DISCH ##############################
# 1) Leer el Excel (Asegúrate de que el archivo esté en la carpeta de trabajo)
Results_Adm_to_HAI <- read_excel("5.2.6 Results EPIC_ HAI to Disch - 5 y 6.xlsx", sheet = 1)

# 2) Convertir columnas numéricas y de factor
Results_Adm_to_HAI2 <- Results_Adm_to_HAI %>%
  mutate(
    HR = as.numeric(gsub(",", ".", HR)),
    Lower_CI = as.numeric(gsub(",", ".", Lower_CI)),
    Upper_CI = as.numeric(gsub(",", ".", Upper_CI)),
    Method = as.numeric(Method),
    Significant = as.factor(Significant)
  )

# 3) Preparar etiquetas de método y ordenar por Factor
method_labs_Results_Adm_to_HAI <- Results_Adm_to_HAI2 %>%
  select(Method, Method_Name) %>%
  distinct() %>%
  arrange(Method)

# Construir vector de etiquetas en orden
max_method <- max(Results_Adm_to_HAI2$Method, na.rm = TRUE)
method_labels <- as.character(1:max_method)
if(nrow(method_labs_Results_Adm_to_HAI) >= 1){
  idx <- as.integer(method_labs_Results_Adm_to_HAI$Method)
  method_labels[idx] <- method_labs_Results_Adm_to_HAI$Method_Name
}

# 3b) Invertir el orden de los métodos
Results_Adm_to_HAI2 <- Results_Adm_to_HAI2 %>%
  mutate(Method = factor(Method, levels = rev(sort(unique(as.numeric(Method))))))

method_labels <- rev(method_labels)


# 4) *** NUEVO PASO: Crear el data frame para las líneas verticales ***
vline_data <- Results_Adm_to_HAI2 %>%
  filter(Method == 5) %>% # Filtra solo el Método 1
  select(Covariate, HR) %>%
  rename(HR_Method1 = HR) # Renombra la columna para claridad


# 5) Plot
p <- ggplot(Results_Adm_to_HAI2, aes(x = HR, y = Method, color = Significant)) +
  # Usamos Lower_CI
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.18, linewidth = 2) +
  geom_point(size = 5) +
  
  # Línea de referencia principal (HR = 1)
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  
  # *** NUEVA LÍNEA: geom_vline con el data frame auxiliar ***
  geom_vline(data = vline_data, 
             aes(xintercept = HR_Method1), 
             color = "red", 
             linewidth = 0.8) +
  
  facet_wrap(~ Covariate, scales = "free_x", ncol = 2) +
  
  # Escala Logarítmica Base 10
  scale_x_log10(
    breaks = scales::pretty_breaks(n = 5),
    minor_breaks = NULL 
  ) +
  
  scale_y_discrete(labels = method_labels) +
  scale_color_manual(values = c("0" = "steelblue", "1" = "firebrick"),
                     labels = c("0" = "Non significant", "1" = "Significant"),
                     name = "Statistical significance") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "gray80", size = 0.4),
    panel.grid.minor = element_line(color = "gray90", size = 0.2),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )+
  labs(
    x = "Hazard Ratio (95% CI) [Log Scale]",
    y = "Method",
    title = "Hazard Ratios across Methods per Covariate",
    subtitle = "Transition: HAI → Discharged"
  )

# Mostrar el gráfico
print(p)

