library(tidyverse)

setwd("C:/Users/hrcaula/Desktop/CursoR2019-master/")

Micro = read.delim("Tablas/Micro.csv", sep = ",")

View(filter(Micro, Cod.Servicio == "UCAP")) ## La forma Ortodoxa

Micro %>% filter(Cod.Servicio == "UCAP") %>% View() ## La forma "tidyverse"

Micro %>%  filter(Cod.Servicio == "UCAP" & Cod.Microorganismo == "11") %>% View()

Micro %>% filter(Cod.Servicio == "UCAP" | Cod.Servicio == "UPED") %>% View()

Micro %>% 
  filter(Cod.Servicio == "UCAP" | Cod.Servicio == "UPED") %>%
  filter(Cod.Microorganismo == "11") %>%
  View()

Micro %>% 
  filter(Cod.Servicio == "UCAP" | Cod.Servicio == "UPED") %>%
  filter(Cod.Microorganismo != "11") %>%
  View()

# Ejercicio 1.1
 Micro %>%filter(Des.Servicio == "MEDICINA URGENCIAS")

 # Ejercicio 1.2
 Micro %>% filter(!is.na(Des.Diagnostico))
 Micro %>% filter(Des.Diagnostico != "")

 # Ejercicio 1.3
 Micro %>% filter(Cod.Microorganismo == "1000") %>% filter(Des.Servicio == "HEMATOLOGIA")
 Micro %>% filter(Cod.Microorganismo == "1000" & Des.Servicio == "HEMATOLOGIA")
 
 #Ejercicio 1.4
  Micro %>% filter(Des.Analisis == "CULTIVO LARGA INCUBACION") %>% filter(Des.Servicio == "INFECCIOSAS H")

## Busqueda con patrones

Micro %>% filter(grepl("U.V.I.",Des.Servicio)) %>% View()
Micro %>% filter(!grepl("U.V.I.",Des.Servicio)) %>% View()

# Ejercicio 1.5


### SELECT

Micro %>% select(-newNHistoria, -Box) %>% View()

Micro %>% select(1:4) %>% View()
Micro %>% select(c(1,5,8)) %>% View()
Micro %>% select(Cod.Servicio:Box, newNHistoria) %>% View()
Micro %>% select(contains("Des.")) %>%  select(ends_with("ganismo")) %>% View()

### unite() & separate()

Micro %>% 
  separate(Fec.Nacimiento,c("DiaNac","MesNac","AnyoNac"),sep = "/") %>% 
  View()

Micro %>%
  separate(Fec.Nacimiento,c("DiaNac","MesNac","AnyoNac"),sep = "/",remove = FALSE) %>% 
  View()

Micro %>% unite(NewColumn,Fec.Entrada,Fec.Resultado, sep = "-") %>% View()


# Ejercicio 2.1

Micro %>% select(newNHistoria,Fec.Nacimiento,Fec.Entrada)

# Ejercicio 2.2

Micro %>% select(-NMuestra,-Habitacion,-Box)


### GROUP_BY + SUMMARISE

Micro %>%   ### Creamos una nueva tabla
  group_by(Des.Servicio, Des.Analisis) %>% 
  summarise(cases = n(), cases.dist = n_distinct(newNHistoria)) %>% 
  View()


Micro %>%  ### Modificar nuestra tabla (añadiendo nuevas columnas)
  group_by(Des.Servicio, Des.Analisis) %>% 
  mutate(cases = n(), cases.dist = n_distinct(newNHistoria)) %>% 
  View()


Micro %>% group_by(newNHistoria) %>% ### Numero de muestras que se han hecho a cada paciente.
  summarise(veces = n()) %>% 
  filter(veces >= 20) %>% View()


#3.2
Micro %>% group_by(Des.Servicio) %>% summarise(casos=n()) %>% View()

#3.3
Micro %>%  group_by(Des.Servicio, Des.Microorganismo) %>% 
  summarise(casos=n()) %>% 
  View()

#3.4
Micro %>% 
  group_by(Des.Servicio) %>% 
  mutate(casos=n()) %>% 
  View()

#3.5
Micro %>% 
  separate(Fec.Entrada, c("diaEntrada", "mesEntrada", "anyoEntrada"), sep="/") %>% 
  group_by(anyoEntrada) %>% 
  mutate(casos=n()) %>% 
  View()

#3.6
Micro %>% 
  filter(Des.Microorganismo!="Staphylococcus coagulasa (-).") %>% 
  separate(Fec.Entrada, c("diaEntrada", "mesEntrada", "anyoEntrada"), sep="/") %>% 
  group_by(anyoEntrada) %>% 
  mutate(casosTotales=n())%>% 
  ungroup() %>% 
  group_by(anyoEntrada, Des.Microorganismo,casosTotales) %>% 
  summarise(microorgPorAnyo= n()) %>% 
  mutate(percent = microorgPorAnyo/casosTotales) %>% View()


