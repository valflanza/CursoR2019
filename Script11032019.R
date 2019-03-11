library(tidyverse)

setwd("C:/Users/hrcaula/Desktop/CursoR2019-master/")

Micro = read.delim("Tablas/Micro.csv", sep = ",")


## Filter y Select

Micro %>% 
  filter(Des.Microorganismo == "Escherichia coli.") %>% 
  filter(grepl("U.V.I.",Des.Servicio)) %>%
  select(newNHistoria:Planta)%>% 
  View()

## distinct() y count()

Micro %>% 
  filter(Des.Microorganismo == "Escherichia coli.") %>% 
  filter(grepl("U.V.I.",Des.Servicio)) %>%
  select(Des.Servicio,Des.Microorganismo)%>%
  distinct() %>%
  count()

## group_by() + summarise() o muatate()

Micro %>% 
  filter(Des.Microorganismo == "Escherichia coli.") %>% 
  filter(grepl("U.V.I.",Des.Servicio)) %>%
  select(Des.Servicio,Des.Microorganismo)%>%
  group_by(Des.Servicio) %>% 
  mutate(casos = n()) %>% 
  #summarise(casos = n()) %>% 
  View()

## summarise_all
Micro %>% 
  group_by(newNHistoria) %>% 
  summarise_all(first) %>%
  View()

## ggplot

Micro %>% 
  filter(Des.Microorganismo == "Escherichia coli.") %>% 
  filter(grepl("U.V.I.",Des.Servicio)) %>%
  group_by(Des.Servicio,Des.Microorganismo)%>%
  summarise( casos = n()) %>%
  ggplot(aes(x = Des.Servicio, y = casos, fill = Des.Servicio)) + geom_col() +coord_flip()

### Ejercicios

#1.1. 

typing = read.delim("Tablas/Typing.csv", sep = ";")
VF = typing %>% 
  select(Strain,esp:bac43) %>%
  gather(key = VirulenceFactor,value = value,esp:bac43)

typing %>% 
  select(Strain,esp:bac43) %>%
  gather(key = VirulenceFactor,value = value,-Strain) %>%
  group_by(VirulenceFactor) %>% summarise(counts = sum(value)) %>%
  ggplot(aes(x=VirulenceFactor, y = counts)) + geom_col()


#1.3
 Plasmids = typing %>% 
   select(Strain,reppRI1:relpHT.) %>% 
   gather(key = Relaxase, value = Presence, -Strain)

 Plasmids %>% filter(Presence != 0) %>% spread(Strain, Presence, fill = 0) %>% View()
 
 Plasmids %>% 
   filter(Presence != 0) %>% 
   group_by(Relaxase) %>% 
   summarise(counts = n()) %>% 
   ggplot(aes(x = Relaxase, y = counts)) + geom_col()
 
 Plasmids %>% filter(Presence != 0)%>% 
   ggplot(aes(x = Strain, y = Relaxase, fill = Presence)) + geom_tile()
 
 Plasmids %>%  
   filter(Presence != 0)%>% 
   ggplot(aes(x = Strain, y = Relaxase, fill = Presence)) + geom_count()
 
 
 ## JOIN ##
 
TablaNueva = typing %>% 
  select(Strain:Tn1546) %>% 
  inner_join(VF) %>% filter(value != 0)
 
 

## 2.2

Datos = read.delim("Tablas/ResCap/Datos.csv", sep = ",")
TipoSeq = read.delim("Tablas/ResCap/TipoSeq.csv", sep = ",")
Secuenciacion = read.delim("Tablas/ResCap/Secuenciacion.csv", sep = ",")

DatosLong = Datos %>% 
  gather(Sample,value, -Gen,-Group,-DataSet,-DataGroup) %>% 
  separate(Sample,c("Sample","data"), sep = "\\.") %>% 
  spread(data,value) %>%
  inner_join(TipoSeq)

DatosLong %>% 
  filter(reads > 0) %>% 
  group_by(Sample , Seq, DataSet) %>% 
  summarise(counts = n()) %>% 
  ggplot(aes(x = DataSet, y= counts , fill = Seq)) + geom_boxplot()


DatosLong %>% 
  filter(reads > 0) %>% 
  group_by(Sample , Seq, DataSet) %>% 
  summarise(counts = n()) %>% 
  ggplot(aes(x = Seq, y= counts , fill = DataSet)) + geom_boxplot()


DatosLong %>% 
  inner_join(Secuenciacion) %>%
  mutate(norm =  reads / TotalReads) %>% 
  filter(norm >0 ) %>% 
  ggplot(aes(x= Sample, y= norm)) +
  geom_violin() + 
  scale_y_log10()












