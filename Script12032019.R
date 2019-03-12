library(tidyverse)

setwd("C:/Users/hrcaula/Desktop/CursoR2019-master/")

Datos = read.delim("Tablas/ResCap/Datos.csv", sep = ",")
TipoSeq = read.delim("Tablas/ResCap/TipoSeq.csv", sep = ",")
Secuenciacion = read.delim("Tablas/ResCap/Secuenciacion.csv", sep = ",")

DatosLong = Datos %>% 
  gather(Sample,value, -Gen,-Group,-DataSet,-DataGroup) %>% 
  separate(Sample,c("Sample","data"), sep = "\\.") %>% 
  spread(data,value) %>% 
  inner_join(Secuenciacion) %>% 
  inner_join(TipoSeq)


## 1.1
DatosLong = DatosLong %>% 
  mutate(Source = ifelse(grepl("Bichat",Sample),"Human","Pig"))

## 1.2

DatosLong = DatosLong %>% 
  mutate(Sample = gsub("K","",Sample)) %>% 
  mutate(Sample = gsub("pre","",Sample)) %>% 
  mutate(Sample = gsub("post","",Sample))

DatosLong %>% select(Sample,Seq) %>% distinct()


## facet_wrap() y facet_grid()

DatosLong %>% filter(reads > 0) %>% 
  ggplot(aes(x=Sample, y =reads, fill = Seq)) + 
  geom_boxplot() + 
  scale_y_log10() + 
  facet_wrap(Source~DataSet, scales = "free_x")
  
## 2.1

DatosLong %>% filter(reads >0) %>% filter(Coverage <=1) %>% 
  ggplot(aes(x=Coverage)) + 
  geom_histogram() + 
  facet_grid(~Seq)
  

## 2.2 

DatosLong %>% filter(reads >0) %>%
  mutate(RPKM = RPK/TotalReads) %>% 
  ggplot(aes(x=Sample, y =RPKM, fill = Seq)) + 
  geom_boxplot() +
  scale_y_log10()+
  facet_wrap(Source~DataSet, scales = "free_x") + 
  theme_light() + scale_fill_manual(values = distinctColorPalette(10))+
  labs(title = " Ejercicio 2.2", x ="Muestra", y = "Abundancia", caption = "caption")
  
## 2.3

DatosLong %>% 
  mutate(RPKM = RPK* 1e6/ TotalReads) %>% 
  select(Gen,Sample, Seq, Source, RPKM) %>%
  filter(RPKM>0) %>% 
  spread(Seq,RPKM, fill =0) %>% 
  ggplot(aes(x=Conventional, y = SeqCap, color = Source)) +
  geom_point() + scale_y_log10() + scale_x_log10()


# 2.4
DatosLong %>% 
  filter(Uniq > 0) %>% 
  group_by(Sample,DataSet,Source,Seq) %>% 
  summarise(counts = n()) %>% 
  ggplot(aes(x=Seq,y = counts, fill = DataSet)) + 
  geom_boxplot() +
  facet_wrap(~Source)
  
  
  
  
  

