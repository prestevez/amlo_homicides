library(tidyverse)
library(readxl)
library(xkcd)

incidencia <- read_excel("IncidenciaDelictiva_FueroComun_Estatal_1997-2017 jul18.xlsx")

incidencia %>%
    filter(MODALIDAD == "HOMICIDIOS" & TIPO == "DOLOSOS") -> homicidios

names(homicidios)[7:18] -> mths

homicidios$Anual <- rowSums(homicidios[mths], na.rm = TRUE)

homicidios %>%
    mutate(Year = AÑO) %>%
    group_by(Year) %>%
    summarise(Homicides = sum(Anual)) -> homicidiosts

homicidiosts[21,2] * 1.15

homicidiosts[22,] <- c(2018, round(homicidiosts[21,2] * 1.15))

homicidiosts

homs_fcast <- data_frame(Year = c(2021, 2024), 
                         Homicides = c(29113*.6, 5200),
                         type = "Forecast")

homicidiosts %>%
    mutate(type = "Observed") %>%
    bind_rows(homs_fcast) -> homicidiosts

homicidiosts[22,3] <- "Forecast"

homicidiosts %>%
    mutate(type = factor(type)) -> homicidiosts

homicidiosts$type <- relevel(homicidiosts$type, ref = "Observed")

rbind(homicidiosts, c(2018, 29113, "Observed")) -> homicidiosts

levels(homicidiosts$type) <- c("Observed", "AMLO's Goal")

homicidiosts %>%
    mutate(Year = as.numeric(Year),
           Homicides = as.numeric(Homicides)) %>%
    ggplot(aes(Year, Homicides, linetype = type)) +
    geom_line(colour = "red") + 
    scale_x_continuous(breaks = 1997:2024) + 
    theme(plot.margin=unit(c(1,1,1,1),"cm")) +
    theme_xkcd() +
    scale_linetype_discrete("") -> p 

ggsave(filename = "../figures/homicides_amloverse.png", plot = p, 
       width = 32.5, height = 18.3, units = "cm")
