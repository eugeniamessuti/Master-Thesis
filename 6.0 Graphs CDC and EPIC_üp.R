##### Graphs EPIC and CDC together #######

library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

setwd("C:/Users/messuti/Documents/Reports/4 Überpruft Ergebnise/Excel Tables Final Results/3 CDC + EPIC compare")


########## PLOT ADM TO HAI (Versión Final Log Scale con V-Line) #############

# 1) Leer el Excel (Asegúrate de que el archivo esté en la carpeta de trabajo)
Results_Adm_to_HAI <- read_excel("6.1 EPIC and CDC _ Adm to HAI.xlsx", sheet = 1)

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
  filter(Method == 1) %>% # Filtra solo el Método 1
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


########## PLOT ADM TO DISCHARGED (Final Log Scale with V-Line) #############

# 1) Leer el Excel
Results_Adm_to_Disch <- read_excel("6.2 EPIC and CDC_Adm to Disch.xlsx", sheet = 1)

# 2) Convertir columnas numéricas y de factor
Results_Adm_to_Disch2 <- Results_Adm_to_Disch %>%
  mutate(
    HR = as.numeric(gsub(",", ".", HR)),
    Lower_CI = as.numeric(gsub(",", ".", Lower_CI)),
    Upper_CI = as.numeric(gsub(",", ".", Upper_CI)),
    Method = as.numeric(Method),
    Significant = as.factor(Significant)
  )


# 3) Preparar etiquetas de método y ordenar por Factor
method_labs_Results_Adm_to_Disch <- Results_Adm_to_Disch2 %>%
  select(Method, Method_Name) %>%
  distinct() %>%
  arrange(Method)

# Construir vector de etiquetas en orden
max_method <- max(Results_Adm_to_Disch2$Method, na.rm = TRUE)
method_labels <- as.character(1:max_method)
if(nrow(method_labs_Results_Adm_to_Disch) >= 1){
  idx <- as.integer(method_labs_Results_Adm_to_Disch$Method)
  method_labels[idx] <- method_labs_Results_Adm_to_Disch$Method_Name
}

# 3b) Invertir el orden de los métodos
Results_Adm_to_Disch2 <- Results_Adm_to_Disch2 %>%
  mutate(Method = factor(Method, levels = rev(sort(unique(as.numeric(Method))))))

method_labels <- rev(method_labels)


# 4) *** PASO CLAVE: Crear el data frame para las líneas verticales (HR Método 1) ***
vline_data_disch <- Results_Adm_to_Disch2 %>%
  filter(Method == 1) %>% 
  select(Covariate, HR) %>%
  rename(HR_Method1 = HR) 


# 5) Plot
p_disch <- ggplot(Results_Adm_to_Disch2, aes(x = HR, y = Method, color = Significant)) +
  
  # Intervalos de Confianza
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.18, linewidth = 2) +
  geom_point(size = 5) +
  
  # Línea de referencia principal (HR = 1)
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  
  # Línea HR del Método 1 (específica para cada faceta)
  geom_vline(data = vline_data_disch, 
             aes(xintercept = HR_Method1), 
             color = "red", 
             linewidth = 0.8) +
  
  # Facetas (se usa scales = "free_x" y se omiten geom_blank/axis_limits)
  facet_wrap(~ Covariate, scales = "free_x", ncol = 2) +
  
  # *** ESCALA LOGARÍTMICA BASE 10 ***
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

# mostrar
print(p_disch)


########## PLOT HAI TO DISCH (Final Log Scale con V-Line) #############

# 1) Leer el Excel
Results_HAI_to_Disch <- read_excel("6.3 EPIC and CDC_HAI to Disch.xlsx", sheet = 1)

# 2) Convertir columnas numéricas y de factor
Results_HAI_to_Disch2 <- Results_HAI_to_Disch %>%
  mutate(
    HR = as.numeric(gsub(",", ".", HR)),
    Lower_CI = as.numeric(gsub(",", ".", Lower_CI)),
    Upper_CI = as.numeric(gsub(",", ".", Upper_CI)),
    Method = as.numeric(Method),
    Significant = as.factor(Significant)
  )

# 3) Preparar etiquetas de método y ordenar por Factor
method_labs_Results_HAI_to_Disch <- Results_HAI_to_Disch2 %>%
  select(Method, Method_Name) %>%
  distinct() %>%
  arrange(Method)

# Construir vector de etiquetas en orden
max_method <- max(Results_HAI_to_Disch2$Method, na.rm = TRUE)
method_labels <- as.character(1:max_method)
if(nrow(method_labs_Results_HAI_to_Disch) >= 1){
  idx <- as.integer(method_labs_Results_HAI_to_Disch$Method)
  method_labels[idx] <- method_labs_Results_HAI_to_Disch$Method_Name
}

# 3b) Invertir el orden de los métodos
Results_HAI_to_Disch2 <- Results_HAI_to_Disch2 %>%
  mutate(Method = factor(Method, levels = rev(sort(unique(as.numeric(Method))))))

method_labels <- rev(method_labels)


# 4) *** PASO CLAVE: Crear el data frame para las líneas verticales (HR Método 1) ***
vline_data_disch_hai <- Results_HAI_to_Disch2 %>%
  filter(Method == 1) %>% 
  select(Covariate, HR) %>%
  rename(HR_Method1 = HR) 


# 5) Plot
p_hai_disch <- ggplot(Results_HAI_to_Disch2, aes(x = HR, y = Method, color = Significant)) +
  
  # Intervalos de Confianza
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.18, , linewidth = 2) +
  geom_point(size = 5) +
  
  # Línea de referencia principal (HR = 1)
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  
  # Línea HR del Método 1 (específica para cada faceta)
  geom_vline(data = vline_data_disch_hai, 
             aes(xintercept = HR_Method1), 
             color = "red", 
             linewidth = 0.8) +
  
  # Facetas (free_x y eliminación de límites personalizados)
  facet_wrap(~ Covariate, scales = "free_x", ncol = 2) +
  
  # *** ESCALA LOGARÍTMICA BASE 10 ***
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

# mostrar
print(p_hai_disch)
