rm(list = ls())

library(pacman)
p_load(tidyverse, survey, srvyr, remotes, haven, srvyrexploR, skimr,
       readxl, writexl)

censo <- "https://github.com/jgbabativam/Muestreo-I/raw/main/datos/agpop.sav"
encuesta <- "https://github.com/jgbabativam/Muestreo-I/raw/main/datos/Agrsrs.sav"

marco <- read_sav(censo)
datos <- read_sav(encuesta)


datos$FEXP <- nrow(marco)/nrow(datos)

dsg <- datos |> 
       mutate(NI = nrow(marco)) |> 
       as_survey_design(ids = 1,
                        fpc = NI,
                        weights = FEXP,
                        nest = T)
class(dsg)



(estimacion <- dsg |> 
              summarise(Total = survey_total(acres92, vartype = c("se", "cv", "ci")),
                        Promedio = survey_mean(acres92, vartype = c("se", "cv", "ci"))))

ty <- sum(marco$acres92)
ybU <- mean(marco$acres92)
