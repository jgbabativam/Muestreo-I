########################## MUESTREO ESTRATIFICADO
rm(list = ls())
library(pacman)
p_load(tidyverse, survey, srvyr,  haven, skimr,
       readxl, writexl, samplesize4surveys, janitor)

censo <- "https://github.com/jgbabativam/Muestreo-I/raw/main/datos/agpop.sav"
encuesta <- "https://github.com/jgbabativam/Muestreo-I/raw/main/datos/AgStrat.sav"

marco <- read_sav(censo)
#datos <- read_sav(encuesta)

#glimpse(datos)

### Tamaño de la muestra para lograr Error máximo el 7%

nh <- marco |> 
      count(Region, name = "Nh") |> 
      mutate(nh = ss4p(N = Nh, P = 0.5, error = "me", delta = 0.07))
  
sum(nh$nh)
####============ Coordinado Negativo por estrato

set.seed(12456)
marco_ord <- marco |> 
             mutate(aleatorio = runif(n = nrow(marco))) |> 
             arrange(Region, aleatorio) |> 
             group_by(Region) |> 
             mutate(consec = 1:n()) |> 
             ungroup() 

muestra <- marco_ord |> 
           left_join(nh, by = "Region") |> 
           filter(consec <= nh) 
         

glimpse(muestra)
###=============== Factores de expansion.

encuestas <- muestra |> 
             mutate(FEXP = Nh/nh) 

write_xlsx(encuestas, "encuestas_ESTMAS.xlsx")

##### Estimación 

dsg <- encuestas |> 
       as_survey_design(ids = 1,
                        strata = Region,
                        fpc = Nh,
                        weights = FEXP,
                        nest = T)


estimacion <- dsg |> 
              group_by(Region) |> 
              summarise(promedio = survey_mean(acres92, 
                                               vartype = "cv"))








