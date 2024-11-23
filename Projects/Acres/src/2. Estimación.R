rm(list = ls())

options(scipen = 9999)
library(pacman)
p_load(tidyverse, survey, srvyr, remotes, haven, srvyrexploR, skimr,
       readxl, writexl, skimr, DataExplorer)

censo <- "https://github.com/jgbabativam/Muestreo-I/raw/main/datos/agpop.sav"
encuesta <- "https://github.com/jgbabativam/Muestreo-I/raw/main/datos/Agrsrs.sav"

marco <- read_sav(censo)
datos <- read_sav(encuesta)

skim(datos)
glimpse(datos)

table(datos$Region)
table(as_factor(datos$Region))


datos <- datos |> 
         mutate(NI = nrow(marco)) |>  
         mutate(dominio = factor(ifelse(acres92 < 200000, 1, 0),
                                 levels = c(0,1), 
                                 labels = c("Más de 200mil acres", "Menos de 200mil acres"))
         )

table(datos$dominio, useNA = "a")

#create_report(datos)

datos$FEXP <- nrow(marco)/nrow(datos)

write_xlsx(datos, "output/encuesta.xlsx")

dsg <- datos |> 
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


(estimaprop <- dsg |> 
              group_by(dominio) |> 
              summarise(prop = survey_prop(vartype = c("se", "ci"), level = 0.95)))


(estimacion <- dsg |> 
               group_by(Region) |> 
               summarise(Total = survey_total(acres92, vartype = c("se", "cv", "ci")),
                         Promedio = survey_mean(acres92, vartype = c("se", "cv", "ci"))))


marco |> 
  group_by(Region) |> 
  summarise(ty = sum(acres92))


marco |> 
  count(Region, name = "Nh") |> 
  left_join( datos |> 
  count(Region, name = "nh")) |> 
  mutate(PropU = Nh/sum(Nh),
         PropS = nh/sum(nh))

