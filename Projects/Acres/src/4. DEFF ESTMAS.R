rm(list = ls())

options(scipen = 9999)
library(pacman)
p_load(tidyverse, survey, srvyr, remotes, haven, srvyrexploR, skimr,
       readxl, writexl, skimr, DataExplorer, TeachingSampling)

censo <- "https://github.com/jgbabativam/Muestreo-I/raw/main/datos/agpop.sav"
encuesta <- "https://github.com/jgbabativam/Muestreo-I/raw/main/datos/Agrsrs.sav"

marco <- read_sav(censo)
dominios <- read_sav(encuesta)

glimpse(marco)
#### Muestra ESTMAS

nh <- marco |> 
      count(Region, name = "Nh") |> 
      bind_cols(nh = c(103, 21, 135, 41))

{set.seed(123456)
ind <- S.STSI(marco$Region, nh$Nh, nh$nh)
muestra <- marco[ind,]
}

datos <- muestra |> 
         left_join(nh, by = "Region") |> 
         mutate(FEXP = Nh/nh)


dsg <- datos |> 
       as_survey_design(ids = 1,
                        strata = Region,
                        fpc = Nh,
                        weights = FEXP,
                        nest = T)

(estima <- dsg |> 
           group_by(Region) |> 
           summarise(total = survey_total(acres92, vartype = "cv")) |> 
           bind_rows(
            dsg |> 
            summarise(total = survey_total(acres92, vartype = "cv")) |> 
            mutate(Region = 5)  
           ))


(estima <- dsg |> 
    group_by(Region) |> 
    summarise(total = survey_total(acres92, vartype = c("se", "cv"))) |> 
    bind_rows(
      dsg |> 
        summarise(total = survey_total(acres92, vartype = c("se","cv"))) |> 
        mutate(Region = 5)  
    ))


# Objeto de diseño en la estrategia por dominios

N <- nrow(marco)
n <- nrow(dominios)

dom <- dominios |> 
  mutate(FEXP = N/n,
         NI = N)

dsg_dom <- dom |> 
           as_survey_design(ids = 1,
                            fpc = NI,
                            weights = FEXP,
                            nest = T)


(estima <- dsg_dom |> 
    summarise(total = survey_total(acres92, vartype = c("se","cv")))
    )

(DEFF = 69489057^2/58169381^2)

### Tamaño de Muestra - ESTMAS

(tamMue <- samplesize4surveys::ss4p(N = N,
                                   P = 0.5,
                                   error = "me",
                                   delta = 0.05,
                                   DEFF = 1.43))

### Tamaño de Muestra - MAS

(tamMue <- samplesize4surveys::ss4p(N = N,
                                    P = 0.5,
                                    error = "me",
                                    delta = 0.05))



sd <- marco |> 
  group_by(Region) |> 
  summarise(sd = sd(acres87),
            tx = sum(acres87))

nh |> 
  mutate(nProp = round(300 * Nh/sum(Nh))) |> 
  left_join(sd) |> 
  mutate(n_neyman = round(300 * (Nh*sd/sum(Nh*sd))),
         n_afTot = round(300 * tx/sum(tx)))


####------------ Ejemplo Afijación de potencia


ciudades <- tribble(
  ~"Ciudad",  ~"poblacion",
  "Bogotá", 7000000,
  "Medellin", 2000000,
  "Cali", 1300000,
  "Barranquilla", 1100000,
  "Zipaquira", 180000,
  "Chigorodó", 60000
)

n <- 1500

potencia <- 0.4

ciudades |> 
  mutate(nCiudad = round(n * poblacion^potencia/sum(poblacion^potencia))) 
  


