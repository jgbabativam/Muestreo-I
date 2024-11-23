
##### En esta tarea se realiza la Expansión y estimación
rm(list = ls())

library(pacman)

p_load(tidyverse, janitor,
       readxl, writexl,
       survey, srvyr)



##### Lectura de archivos

marco <- read_excel("data/Ejemplo Hogares - Marco Muestral.xls")
muestra <- read_excel("output/Muestra Seleccionada.xlsx")
encuestas <- read_excel("data/Dominio Hogares.xls", sheet = "Hoja1")

glimpse(muestra)
glimpse(encuestas)

###---- Cruce de encuestas con la muestra

anti_join(muestra, encuestas, by = "Hogar")
anti_join(encuestas, muestra, by = "Hogar")

N <- nrow(marco)
n <- nrow(muestra)
  
variables <- paste0("P", 2:7)
encu <- encuestas |> 
        left_join(muestra, by = "Hogar") |> 
        rowwise() |> 
        mutate(gasto_tot = sum(c(P2, P3, P4, P5, P6, P7))) |> 
        mutate(FEXP = N/n) |> 
        mutate(NI = N) |> 
        mutate(P1 = factor(P1, levels = 1:3, labels = c("< 2SMMLV", "De 2 a 4 SMMLV", ">4 SMMLV")))

class(encu)
###----- definición del objeto de diseño


dsg <- encu |> 
       as_survey_design(ids = 1,
                        fpc = NI,
                        weights = FEXP,
                        nest = T)

(total <- dsg |> 
          group_by(P1) |> 
          summarise(vivienda = survey_total(P2, na.rm = T, vartype = "cv"),
                    alimentacion = survey_total(P3, na.rm = T, vartype = "cv"),
                    educacion = survey_total(P4, na.rm = T, vartype = "cv"),
                    transporte = survey_total(P5, na.rm = T, vartype = "cv"),
                    esparcimiento = survey_total(P6, na.rm = T, vartype = "cv"),
                    otros = survey_total(P7, na.rm = T, vartype = "cv"),
                    total_gasto = survey_total(gasto_tot, na.rm = T, vartype = "cv")
                    )
  )


t1 <- dsg |> 
      summarise(vivienda = survey_total(P2, na.rm = T, vartype = "cv"),
                alimentacion = survey_total(P3, na.rm = T, vartype = "cv"),
                educacion = survey_total(P4, na.rm = T, vartype = "cv"),
                transporte = survey_total(P5, na.rm = T, vartype = "cv"),
                esparcimiento = survey_total(P6, na.rm = T, vartype = "cv"),
                otros = survey_total(P7, na.rm = T, vartype = "cv"),
                total_gasto = survey_total(gasto_tot, na.rm = T, vartype = "cv")
      ) |> 
      mutate(P1 = "Total")


salida <- bind_rows(total, t1) |> 
          pivot_longer(cols = - P1, names_to = "gasto", values_to = "estimacion") |> 
          pivot_wider(names_from = P1, values_from = estimacion)

