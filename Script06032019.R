library(tidyverse)

setwd("C:/Users/hrcaula/Desktop/CursoR2019-master/")

Micro = read.delim("Tablas/Micro.csv", sep = ",")

Micro = Micro %>% 
  separate(Fec.Nacimiento, c("Nac.dia","Nac.mes","Nac.anyo"), remove = FALSE) %>%
  separate(Fec.Entrada, c("Ent.dia","Ent.mes","Ent.anyo"), remove = FALSE) %>%
  separate(Fec.Resultado, c("Res.dia","Res.mes","Res.anyo"), remove = FALSE)

Micro = Micro %>% 
  mutate(Ent.anyo = as.numeric(Ent.anyo), Nac.anyo = as.numeric(Nac.anyo)) %>%
  mutate(Edad = Ent.anyo - Nac.anyo)

Micro = Micro %>% 
  mutate(Ent.mes = as.numeric(Ent.mes), Nac.mes = as.numeric(Nac.mes))
  

Micro %>% 
  filter(Edad >=0) %>% 
  filter(Edad <= 120) %>%
  ggplot(aes(x =Edad)) + geom_histogram()

Micro %>% 
  filter(Edad >=0) %>% 
  filter(Edad <= 120) %>%
  ggplot(aes(y =Edad)) + geom_boxplot()

1.2
Micro %>%  
  filter(Edad >=0) %>% 
  filter(Edad <= 120) %>%
  filter(grepl("MEDICINA INTERNA",Des.Servicio)) %>% 
  filter(Ent.anyo == 2006) %>% ggplot(aes(x=Edad)) + geom_histogram()


## Dos Variables (discreto-continuo)

Micro %>% filter(Edad >=0) %>% 
  filter(Edad <= 120) %>%
  ggplot(aes(x=Des.Analisis, y = Edad)) + geom_boxplot()

# Ejercicio 2.1

Micro %>% 
  filter(grepl("U.V.I.", Des.Servicio)) %>% 
  group_by(Des.Servicio,Ent.anyo) %>%
  summarise(cases = n()) %>% 
  ggplot(aes(x = Des.Servicio, y = cases, fill =Des.Servicio)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Ejercicio 2.2

Micro %>% 
  filter(grepl("PED", Des.Servicio)) %>%
  filter(Des.Servicio != "ORTOPEDIA ADULTO H") %>%
  group_by(Des.Servicio,Des.Microorganismo) %>% 
  summarise(casos = n()) %>% 
  filter(casos >9) %>% 
  ggplot(aes(x=Des.Servicio, y=casos, fill=Des.Microorganismo)) + 
  geom_col(position = "fill") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Ejercicio 2.3  

Micro %>%
  group_by(Ent.anyo,Ent.mes) %>%
  summarise(casos = n()) %>%
  ggplot(aes(x=as.factor(Ent.mes), y=casos)) +geom_boxplot() + xlab("Mes")



