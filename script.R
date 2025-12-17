#PAQUETES QUE VAYAMOS A UTILIZAR
library(tidyverse)
library(readr)
library(ggplot2)
library(ggthemes)
library(gt)
library(gtExtras)
library(dplyr)
library(plotly)
library(eurostat)
library(sf)
library(forcats)
library(sf)
library(mapSpain)
library(eurostat)
library(forecast)
library(tseries)
library(patchwork)



ruta_ccaa_inflacion <- "./datos/ccaa_inflacion.csv"

ccaa_inflacion <- rio::import(ruta_ccaa_inflacion)

str(ccaa_inflacion)

ccaa_inflacion <- ccaa_inflacion %>% drop_na()

ccaa_inflacion_an <- ccaa_inflacion %>% 
  select(c(2, 3, 4, 5)) %>% 
  rename(periodo = Periodo, indice= c(1))

ccaa_inflacion_an <- ccaa_inflacion_an %>% 
  mutate(valor = gsub(",", ".", Total)) 

ccaa_inflacion_an <- ccaa_inflacion_an %>% mutate(periodo = gsub("M12", "", periodo),
                                                  periodo = gsub("M11", "", periodo),
                                                  periodo = gsub("M10", "", periodo),
                                                  periodo = gsub("M09", "", periodo),
                                                  periodo = gsub("M08", "", periodo),
                                                  periodo = gsub("M07", "", periodo),
                                                  periodo = gsub("M06", "", periodo),
                                                  periodo = gsub("M05", "", periodo),
                                                  periodo = gsub("M04", "", periodo),
                                                  periodo = gsub("M03", "", periodo),
                                                  periodo = gsub("M02", "", periodo),
                                                  periodo = gsub("M01", "", periodo),
                                                  indice = str_replace(indice, ".*Variaci.n anual.*", "Var_anual"))

ccaa_inflacion_an <- ccaa_inflacion_an %>% rename(ccaa = c(2)) %>% 
  filter(ccaa %in% "Nacional")

ccaa_inflacion_an <- ccaa_inflacion_an %>% filter(indice %in% "Var_anual")

#Cambiamos los nombres, seleccionamos aquellas columnas que deseamos mantener y cambiamos la notación decimal.
ccaa_inflacion <- ccaa_inflacion %>% 
  rename(indice = c(2), ccaa = c(3), periodo = c(4), valor = c(5)) %>%
  select(c(2,3,4,5)) %>% 
  mutate(valor = gsub(",", ".", valor))

ruta_ccaa_inflacion_subyacente <- "./datos/inflacion_subyacente.csv"

inflacion_subyacente <- rio::import(ruta_ccaa_inflacion_subyacente)

str(inflacion_subyacente)

inflacion_subyacente <- inflacion_subyacente %>%  drop_na()

inflacion_subyacente_an <- inflacion_subyacente %>% rename(indice = c(1), ccaa = c(2), periodo = PERIODO, valor = VALOR) %>% 
  mutate(indice = gsub("Tipo de dato", "Var_anual_sub", indice),
         ccaa = gsub("Variación anual", "Nacional", ccaa),
         valor = gsub(",", ".", valor))

#-------


inflacion_subyacente_serie <- inflacion_subyacente %>% rename(indice = c(1), ccaa = c(2), periodo = PERIODO, valor = VALOR) %>% 
  mutate(indice = gsub("Tipo de dato", "Var_anual_sub", indice),
         ccaa = gsub("Variación anual", "Nacional", ccaa),
         valor = gsub(",", ".", valor))

inflacion_subyacente_serie <- inflacion_subyacente_serie %>% select(c(1,2,7,8))

inflacion_serie <- ccaa_inflacion %>% 
  mutate(indice = str_replace(indice, ".*Variaci.n anual.*", "Var_anual")) %>% 
  filter(indice %in% "Var_anual") %>% 
  filter(ccaa %in% "Nacional")


merge_serie <- full_join(inflacion_serie, inflacion_subyacente_serie) 

str(inflacion_serie)
str(inflacion_subyacente_serie)

merge_serie <- merge_serie %>% pivot_wider(names_from = indice, values_from = valor) %>% 
  mutate(Var_anual = as.numeric(Var_anual))


merge_serie <- merge_serie %>%
  mutate(fecha = as.Date(paste0(gsub("M", "-", periodo), "-01")), 
         Var_anual = as.numeric(Var_anual),
         Var_anual_sub = as.numeric(Var_anual_sub)
  ) %>%
  arrange(fecha)

ts_cpi <- ts(merge_serie$Var_anual, start = c(2002, 1), frequency = 12)
ts_core <- ts(merge_serie$Var_anual_sub, start = c(2002, 1), frequency = 12)

autoplot(ts_cpi, series = "Inflación General") +
  autolayer(ts_core, series = "Inflación Subyacente") +
  ggtitle("Tasa anual inflación España") +
  ylab("% Cambio") +
  theme_minimal()

model_cpi <- auto.arima(ts_cpi, seasonal = TRUE) 

summary(model_cpi)

checkresiduals(model_cpi)

forecast_cpi <- forecast(model_cpi, h = 12)

plot(forecast_cpi, main = "Predicción: Inflación General")


model_core <- auto.arima(ts_core, seasonal = TRUE)
summary(model_core)

checkresiduals(model_core)

forecast_core <- forecast(model_core, h = 12)
plot(forecast_core, main = "Predicción: Inflación subyacente")


print(forecast_cpi)
print(forecast_core)

results_table <- data.frame(
  Month = 1:12, 
  General_CPI = as.numeric(forecast_cpi$mean),
  Core_Inflation = as.numeric(forecast_core$mean))

results_table

autoplot(forecast_cpi, 
         ts.colour = "#2C3E50",      
         predict.colour = "#E31A1C",
         conf.int.fill = "#A6CEE3",   
         lwd = 1) +                         
  coord_cartesian(xlim = c(2020, 2026.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") + 
  geom_vline(xintercept = 2025, linetype = "dotted", color = "gray50") + 
  labs(
    title = "Proyección de la Inflación General (CPI)",
    subtitle = "Predicción ARIMA(0,1,1)(0,0,1)[12] con intervalos de confianza (80% y 95%)",
    x = NULL, 
    y = "Tasa de Variación Anual (%)",
    caption = "Fuente: Elaboración propia a partir de datos del INE."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0), # Alineado izquierda
    plot.subtitle = element_text(size = 11, color = "gray40", margin = margin(b = 15)),
    plot.caption = element_text(size = 8, color = "gray60", margin = margin(t = 10)),
    axis.text = element_text(size = 10, color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),   
    panel.grid.major.x = element_blank(), 
    axis.line.x = element_line(color = "black"))

tabla_sencilla <- as.data.frame(forecast_cpi)

tabla_sencilla$Fecha <- row.names(tabla_sencilla)

tabla_sencilla <- tabla_sencilla[, c(6, 1:5)]

tabla_sencilla <- tabla_sencilla %>% 
  gt() %>%
  tab_header(title = "Predicción Numérica IPC (2025-2026)") %>%
  fmt_number(columns = 2:6, decimals = 2)

tabla_sencilla
