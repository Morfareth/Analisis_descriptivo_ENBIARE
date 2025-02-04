rm(list = ls())
# Año 2021 Encuesta Nacional de Bienestar Reportado (ENBIARE) ----

## Carga de paquetes y archivos ----

library(janitor)
library(pollster)
library(sjlabelled)
library(haven)
library(tidyverse)
library(eph)
library(fmsb)
library(patchwork)
library(pyramid)
library(tidylog)
library(readr)
library(foreign)
library(summarytools)
library(survey)
library(paletteer)

setwd("UBICACIÓN DE ARCHIVOS")

V <- read_csv("TVIVIENDA.csv")
H <- read_csv("THOGAR.csv")
SD <- read_csv("TSDEM.csv")
E <- read_csv("TENBIARE.csv")

# Las dimensiones coinciden con lo estipulado en el diccionario de datos de INEGI: https://www.inegi.org.mx/rnm/index.php/catalog/730/data-dictionary

## Pegado de base de datos ----

names(V)
names(H)
names(SD)
names(E)

## Identificadores

idV <- c("FOLIO", "VIV_SEL", "ENT", "TLOC", "EST_DIS", "UPM_DIS")
idH <- c(idV, "HOGAR")
idSD <- c(idH, "N_REN" )
idE <- c(idSD, "FAC_ELE")

## 

ENB1<-V  |>  
  dplyr::inner_join(H, by=idV)

##

ENB2<-ENB1 |>  
  dplyr::left_join(SD, by=idH) |> 
  dplyr::select(-ends_with(".y")) |> 
  dplyr::rename_at(vars(ends_with(".x")), ~ stringr::str_remove(.x, ".x")) 

dim(ENB1)
dim(ENB2)

### Revisar cuestionario si tiene preguntas similares al cuestionario de otras tablas

intersect(names(ENB2), names(E))

# Base final para trabajar 

ENB <- ENB2 |>  
  left_join(E, by=idSD)

dim(ENB)
names(ENB)

## Remover algunos objetos para mejorar uso de memoria ----

rm(V, H, E, SD)
rm(idH, idSD, idV, idE, ENB1, ENB2)
gc()

## Filtrado de base por edad>17, hispanohablante y alfabeta, como menciona la ficha técnica de la encuesta ----

ENB <- ENB |> 
  filter(EDAD > 17)
ENB <- ENB |> 
  filter(EDAD < 97)
ENB <- ENB |> 
  filter(P4_5 < 3)
ENB <- ENB |> 
  filter(ALFABETISMO < 2)

dim(ENB)

## Guardar bases ----

write_dta(ENB, "ENB.dta")
write.csv(ENB, "ENB.csv")

##

ENB <- read_dta("ENB.dta")

ENB <- ENB |> 
  relocate(SEXO, EDAD, .after = FOLIO)

ENB |> 
  as.matrix() |> 
  na_if("")

## Ponderadores svy ----

a1 <- svydesign(ids = ~UPM_DIS, strata = ~EST_DIS, weights = ~FAC_HOG, nest = T, data = ENB)

## Funciones y valores ----

### Funciones ----

func_ed <- function(x, edad_final = 80, grupo = 5, ...) {
  seq_edades <- seq(15, edad_final, grupo)
  n <- length(seq_edades)
  seq_edades_sup <- c(seq_edades + grupo - 1)[-n]
  labs <-  c(paste0(seq_edades[-n], "-", seq_edades_sup), paste0(seq_edades[n], "+"))
  cut(x, c(seq_edades, 200),  
      labels = labs, right = FALSE, include.lowest = TRUE, ...)
}

func_ed2 <- function(x, edad_final = 75, grupo = 5, ...) {
  seq_edades <- seq(20, edad_final, grupo)
  n <- length(seq_edades)
  seq_edades_sup <- c(seq_edades + grupo - 1)[-n]
  labs <-  c(paste0(seq_edades[-n], "-", seq_edades_sup), paste0(seq_edades[n], "+"))
  cut(x, c(seq_edades, 200),  
      labels = labs, right = FALSE, include.lowest = TRUE, ...)
}


### Codificación de las edades----


ENB <- ENB |> 
  as.data.frame() |> 
  mutate(edadq = rep(1,72689), .before = FOLIO) |> 
  mutate(eda = EDAD, .before = FOLIO) |> 
  as_numeric()
ENB$edadq <- func_ed(ENB$eda, 
                     edad_final = 95, 
                     grupo = 5)

ENB <- ENB |> 
  as.data.frame() |> 
  mutate(edad10 = rep(1,72689), .before = eda)
ENB$edad10 <- func_ed(ENB$eda, 
                      edad_final = 65, 
                      grupo = 10)

ENB <- ENB |> 
  as.data.frame() |> 
  mutate(edad15 = rep(1,72689), .before = eda)
ENB$edad15 <- func_ed2(ENB$eda, 
                       edad_final = 65, 
                       grupo = 15)

### Codificación sexo ----

ENB <- ENB |> 
  arrange(SEXO)
ENB <- ENB |> 
  mutate(sex = c(rep("Hombres",34310), rep("Mujeres", 38379)))



# Volver a correr las funciones y después el ponderador svy después de este paso

### Colores----

colors10 <- paletteer_d("colorBlindness::Blue2Orange10Steps")
colors11 <- paletteer_c("ggthemes::Orange-Blue Diverging", 11)
colors4 <- paletteer_c("ggthemes::Orange-Blue Diverging", 4)

# Data Frames sencillos de variables de interés ----

## Estructura por edad ----

estructura <- svytable(~sex+edadq, a1) |> 
  as.data.frame() |> 
  arrange(sex)
estructuradf <- estructura |> 
  pivot_wider(names_from = sex, values_from = Freq)
rm(estructura)

## Satisfacción del tiempo disponible ----

sattdisp <- svytable(~sex+edadq+PA3_09, a1) |> 
  as.data.frame() |>
  arrange(sex)
sattdispdf <- sattdisp |> 
  pivot_wider(names_from = PA3_09, values_from = Freq) |>
  arrange(sex)
rm(sattdisp)


## Confianza en la gente ----

conf_gente <- svytable(~sex+edadq+PB1_01, a1) |> 
  as.data.frame() |> 
  arrange(sex)
conf_gentedf <- conf_gente |> 
  pivot_wider(names_from = PB1_01, values_from = Freq) |>
  arrange(sex)
rm(conf_gente)

## Confianza en funcionarios públicos ----

conf_funcp <- svytable(~sex+edadq+PB1_12, a1) |> 
  as.data.frame() |> 
  arrange(sex)
conf_funcpdf <- conf_funcp |> 
  pivot_wider(names_from = PB1_12, values_from = Freq) |>
  arrange(sex)
rm(conf_funcp)

## Rechazo laboral ----

rech_lab <- svytable(~sex+edadq+PE6, a1) |> 
  as.data.frame() |> 
  arrange(sex)
rech_labdf <- rech_lab |> 
  pivot_wider(names_from = PE6, values_from = Freq) |>
  arrange(sex)
rm(rech_lab)

## Amenazas o extorsiones ----

amen_extor <- svytable(~sex+edadq+PF4_4, a1) |> 
  as.data.frame() |> 
  arrange(sex)
amen_extordf <- amen_extor |> 
  pivot_wider(names_from = PF4_4, values_from = Freq) |>
  arrange(sex)
rm(amen_extor)

## Nivel socioeconómico ----

niv_socec <- svytable(~sex+edadq+PI2, a1) |> 
  as.data.frame() |> 
  arrange(sex)
niv_socecdf <- niv_socec |> 
  pivot_wider(names_from = PI2, values_from = Freq) |>
  arrange(sex)
rm(niv_socec)

## Opciones educativas ----

op_edu <- svytable(~sex+edadq+PI3, a1) |> 
  as.data.frame() |> 
  arrange(sex)
op_edudf <- op_edu |> 
  pivot_wider(names_from = PI3, values_from = Freq) |>
  arrange(SEXO)
rm(op_edu)

## Opciones laborales ----

op_lab <- svytable(~sex+edadq+PI4, a1) |> 
  as.data.frame() |> 
  arrange(sex)
op_labdf <- op_lab |> 
  pivot_wider(names_from = PI4, values_from = Freq) |>
  arrange(sex)
rm(op_lab)

## Opciones de adquirir un patrimonio ----

op_pat <- svytable(~sex+edadq+PI5, a1) |> 
  as.data.frame() |> 
  arrange(sex)
op_patdf <- op_pat |> 
  pivot_wider(names_from = PI5, values_from = Freq) |>
  arrange(sex)
rm(op_pat)

## Utiliza transporte público ----

t_pub <- svytable(~sex+edadq+PJ3_1, a1) |> 
  as.data.frame() |> 
  arrange(sex)
t_pubdf <- t_pub |> 
  pivot_wider(names_from = PJ3_1, values_from = Freq) |>
  arrange(sex)
rm(t_pub)

# Gráficas ----

## Pirámide de población ----

estructura15 <- svytable(~sex+edad15, a1) |> 
  as.data.frame() |> 
  arrange(sex)
estructura15$Freq <- estructura15$Freq/1000000
estructura15 <- estructura15 |> 
  mutate(population = ifelse(sex == "Hombres", Freq*(-1),Freq*1)) |> 
  select(!"Freq")
pyr1 <- ggplot(estructura15, aes(x = `edad15`,y = `population`, fill=`sex`))+
  geom_bar(data = subset(estructura15, sex == "Mujeres"), stat = "identity", colour="black") +
  geom_bar(data = subset(estructura15, sex == "Hombres"), stat = "identity", colour="black") + 
  coord_flip()+
  scale_y_continuous(labels = paste0(as.character(c(seq(15, 0, by = -5), seq(5, 15, by = 5))))) + 
  labs(title = "Pirámide poblacional México, ENBIARE 2021 
                (edades 20 en adelante)", x = "Edades",
       y = "Población (en millones)")+
  scale_fill_brewer(type = "seq",palette = 18)
pyr1

## Satisfacción tiempo libre ----

### Por sexo ----

sattdisp15 <- svytable(~sex+PA3_09, a1) |> 
  as.data.frame() |>
  arrange(sex) |> 
sattdisp15.t <- sattdisp15 |> 
  pivot_wider(names_from = PA3_09, values_from = Freq) |>
  arrange(sex) |> 
  adorn_totals(where = "col") |> 
  select(sex, Total)

#Total de hombres = 16452447 
#Total de mujeres = 19384359

sattdisp15 <- sattdisp15 |> 
  mutate(perc = ifelse(sex=="Hombres", 100*(Freq/16452447), 100*(Freq/19384359)))
  
ggstd <- ggplot(sattdisp15, aes(x = `sex`, y = `perc`, fill=`PA3_09`))+
  geom_bar(data = subset(sattdisp15), position = "dodge", stat = "identity", colour="black")+
  scale_y_continuous(limits = c(0,30),
                     breaks = seq(0,30, by = 10))+
  labs(title = "Satisfacción del tiempo disponible", x = "Sexo",
       y = "Población (en porcentaje por sexo)")+
  guides(fill = guide_legend(title = "Nivel de 
satisfacción"))+
  scale_fill_manual(values = colors11)


### Por edades y sexo ----

sattdisp15 <- svytable(~sex+edad15+PA3_09, a1) |> 
  as.data.frame() |> 
  arrange(sex)
sattdisp15 <- sattdisp15 |> 
  mutate(perc = ifelse(sex=="Hombres", 100*(Freq/16452447), 100*(Freq/19384359)))
ggstd1 <- ggplot(sattdisp15, aes(x = `edad15`, y = `perc`, fill=`PA3_09`))+
  geom_bar(data = subset(sattdisp15, `sex` == "Hombres"), position = "dodge", stat = "identity", colour="black")+
  scale_y_continuous(limits = c(0,10),
                     breaks = seq(0,10, by = 2))+
  labs(title = "Satisfacción del tiempo disponible, hombres encuestados", x = "Grupos de edad",
       y = "Población (en porcentajes)")+
  guides(fill = guide_legend(title = "Nivel de 
satisfacción"))+
  scale_fill_manual(values = colors11)

ggstd2 <- ggplot(sattdisp15, aes(x = `edad15`, y = `perc`, fill=`PA3_09`))+
  geom_bar(data = subset(sattdisp15, `sex` == "Mujeres"), position = "dodge", stat = "identity", colour="black")+
  scale_y_continuous(limits = c(0,10),
                     breaks = seq(0,10, by = 2))+
  labs(title = "Satisfacción del tiempo disponible, mujeres encuestadas", x = "Grupos de edad",
       y = "Población (en porcentajes)")+
  guides(fill = guide_legend(title = "Nivel de 
satisfacción"))+
  scale_fill_manual(values = colors11)

## Confianza en funcionarios públicos ----

### Por sexo ---- 

conf_funcp15 <- svytable(~sex+PB1_12, a1) |> 
  as.data.frame() |> 
  arrange(sex) |> 
  pivot_wider(names_from = sex, values_from = Freq) |>
  adorn_totals(where = c("row", "col"));conf_funcp15

#Total de hombres = 16452447 
#Total de mujeres = 19384359

conf_funcp15 <- svytable(~sex+PB1_12, a1) |> 
  as.data.frame() |> 
  arrange(sex) |> 
  mutate(perc = ifelse(sex=="Hombres", 100*(Freq/16452447), 100*(Freq/19384359)));conf_funcp15

ggcfp <- ggplot(conf_funcp15, aes(x = `sex`, y = `perc`, fill=`PB1_12`))+
  geom_bar(data = subset(conf_funcp15), position = "dodge", stat = "identity", colour="black")+
  scale_y_continuous(limits = c(0,30),
                     breaks = seq(0,30, by = 10))+
  labs(title = "Confianza en funcionarios públicos", x = "Sexo",
       y = "Población (en porcentajes)")+
  guides(fill = guide_legend(title = "Nivel de 
confianza"))+
  scale_fill_manual(values = colors11); ggcfp

### Por edades y sexo ----

conf_funcp15.2 <- svytable(~sex+edad15+PB1_12, a1) |> 
  as.data.frame() |> 
  arrange(sex) |> 
  mutate(perc = ifelse(sex=="Hombres", 100*(Freq/16452447), 100*(Freq/19384359)));conf_funcp15.2

ggcfp1 <- ggplot(conf_funcp15.2, aes(x = `edad15`, y = `perc`, fill=`PB1_12`))+
  geom_bar(data = subset(conf_funcp15.2, `sex` == "Hombres"), position = "dodge", stat = "identity", colour="black")+
  scale_y_continuous(limits = c(0,10),
                     breaks = seq(0,10, by = 2))+
  labs(title = "Confianza en funcionarios públicos, hombres encuestados", x = "Grupos de edad",
       y = "Población (en porcentajes)")+
  guides(fill = guide_legend(title = "Nivel de 
confianza"))+
  scale_fill_manual(values = colors11);ggcfp1

ggcfp2 <- ggplot(conf_funcp15.2, aes(x = `edad15`, y = `perc`, fill=`PB1_12`))+
  geom_bar(data = subset(conf_funcp15.2, `sex` == "Mujeres"), position = "dodge", stat = "identity", colour="black")+
  scale_y_continuous(limits = c(0,10),
                     breaks = seq(0,10, by = 2))+
  labs(title = "Confianza en funcionarios públicos, mujeres encuestadas", x = "Grupos de edad",
       y = "Población (en porcentajes)")+
  guides(fill = guide_legend(title = "Nivel de 
confianza"))+
  scale_fill_manual(values = colors11);ggcfp2

##Rechazo laboral ----

### Por sexo ----

rech_lab15 <- svytable(~sex+PE6, a1) |> 
  as.data.frame() |> 
  arrange(sex) |> 
  pivot_wider(names_from = sex, values_from = Freq) |> 
  adorn_totals(where = c("row", "col"));rech_lab15


#Total de hombres =  500165
#Total de mujeres =  673689

rech_lab15 <- svytable(~sex+PE6, a1) |> 
  as.data.frame() |> 
  arrange(sex) |> 
  mutate(perc = ifelse(sex=="Hombres", 100*(Freq/500165), 100*(Freq/673689))) |> 
  arrange(PE6) |> 
  mutate(Rechazo = c(rep("Sí", 2), rep("No",2))) |> select(!PE6);rech_lab15

ggrl <- ggplot(rech_lab15, aes(x = `sex`, y = `perc`, fill=`Rechazo`))+
  geom_bar(data = subset(rech_lab15), position = "dodge", stat = "identity", colour="black")+
  scale_y_continuous(limits = c(0,60),
                     breaks = seq(0,60, by = 10))+
  labs(title = "Rechazos laborales en el último año", x = "Sexo",
       y = "Población (en porcentaje por sexo)")+
  guides(fill = guide_legend(title = "¿Sufrió rechazo laboral?"))+
  scale_fill_manual(values = c("orange", "cyan3")); ggrl


# Diagrama circular básico

prop <- c(rech_lab15$Freq)

pie(prop, labels = c("H-Sí", "M-Sí", "H-No", "M-No"), edges=180, border = "black", col= c("orange", "darkorange3", "cyan", "darkturquoise"))



# Diagrama circular levemente mejorado

data <- data.frame(
  values = c(rech_lab15$Freq),
  Etiquetas = c("Hombres-Sí", "Mujeres-Sí", "Hombres-No", "Mujeres-No")
)

data <- data |> 
  arrange(desc(Etiquetas)) |> 
  mutate(prop = values / sum(data$values) *100) |> 
  mutate(ypos = cumsum(prop)*10000)

ggrl_circ <- ggplot(data, aes(x = "", y = `values`, fill=`Etiquetas`))+
  geom_bar(stat="identity", width = 1, colour= "pink")+
  coord_polar("y", start = 0)+
  scale_fill_manual(values = c("orange", "darkorange2", "cyan", "darkturquoise"))+
  labs(title = "Rechazos laborales en el último año")+
  geom_text(aes(y = ypos, label = Etiquetas), color = "black", size=4)+
  theme_void(); ggrl_circ
  
### Por sexo y edad ----

rech_lab15.2 <- svytable(~sex+edad15+PE6, a1) |> 
  as.data.frame() |> 
  arrange(sex) |> 
  mutate(perc = ifelse(sex=="Hombres", 100*(Freq/500165), 100*(Freq/673689))) |> 
  arrange(PE6) |> 
  mutate(Rechazo = c(rep("Sí", 8), rep("No",8))) |> select(!PE6);rech_lab15.2

ggrl2 <- ggplot(rech_lab15.2, aes(x = `edad15`, y = `perc`, fill=`Rechazo`))+
  geom_bar(data = subset(rech_lab15.2, `sex` == "Hombres"), position = "dodge", stat = "identity", colour="black")+
  scale_y_continuous(limits = c(0,30),
                     breaks = seq(0,30, by = 5))+
  labs(title = "Rechazo laboral en el último año, hombres encuestados", x = "Grupos de edad",
       y = "Población (en porcentaje por sexo)")+
  guides(fill = guide_legend(title = "Rechazo"))+
  scale_fill_hue(labels = c("Sí", "No"))+
  scale_fill_brewer(type = "seq", palette = 13); ggrl2

ggrl3 <- ggplot(rech_lab15.2, aes(x = `edad15`, y = `perc`, fill=`Rechazo`))+
  geom_bar(data = subset(rech_lab15.2, `sex` == "Mujeres"), position = "dodge", stat = "identity", colour="black")+
  scale_y_continuous(limits = c(0,30),
                     breaks = seq(0,30, by = 5))+
  labs(title = "Rechazo laboral en el último año, mujeres encuestadas", x = "Grupos de edad",
       y = "Población (en porcentaje por sexo)")+
  guides(fill = guide_legend(title = "Rechazo"))+
  scale_fill_hue(labels = c("Sí", "No"))+
  scale_fill_brewer(type = "seq", palette = 4); ggrl3

## Opciones de adquirir un patrimonio ----

op <- svytable(~sex+PI5, a1) |> 
  as.data.frame() |> 
  arrange(sex)|> 
  pivot_wider(names_from = sex, values_from = Freq) |> 
  adorn_totals(where = c("row", "col")); op

#Total de hombres = 16452447 
#Total de mujeres = 19384359

op <- svytable(~sex+PI5, a1) |> 
  as.data.frame() |> 
  mutate(perc = ifelse(sex=="Hombres", 100*(Freq/16452447), 100*(Freq/19384359))) |> 
  arrange(PI5) |> 
  mutate(Capacidad = c(rep("1. Mayor que sus padres", 2), 
                       rep("2. Similar a sus padres",2),
                       rep("3. Menor que sus padres", 2),
                       rep("4. Aún no lo ha intentado", 2))) |> 
  select(!PI5); op

op2.1 <- svytable(~sex+PI5, a1) |> 
  as.data.frame.matrix()
row.sums <- apply(op2.1[2:4], 1, sum)
sex <- c("Hombres", "Mujeres")
op2.1 <- op2.1 |> 
cbind(sum=row.sums,sex=sex) |> 
  select(sex, "1. Mayor que sus padres" =`1`, "2. No mayor que sus padres" = `sum`); op2.1

op2.1 <- op2.1 |> 
  pivot_longer(!sex,names_to = "Cap", values_to = "Freq");op2.1

op2.1 <- op2.1 |> 
  as.data.frame() |> 
  mutate(perc = ifelse(sex=="Hombres", 100*(Freq/16452447), 100*(Freq/19384359))); op2.1

rm(op3)


ggop <- ggplot(op, aes(x = `sex`, y = `perc`, fill=`Capacidad`))+
  geom_bar(data = subset(op), position = "dodge", stat = "identity", colour="black")+
  scale_y_continuous(limits = c(0,50),
                     breaks = seq(0,50, by = 10))+
  labs(title = "Capacidad de adquirir patrimonio", x = "Sexo",
       y = "Población (en porcentaje por sexo)")+
  guides(fill = guide_legend(title = "Nivel de 
capacidad"))+
  scale_fill_manual(values = rev(colors4)); ggop

ggplot(op2.1, aes(x = `sex`, y = `perc`, fill=`Cap`))+
  geom_bar(data = subset(op2.1), position = "dodge", stat = "identity", colour="black")+
  scale_y_continuous(limits = c(0,70),
                     breaks = seq(0,70, by = 10))+
  labs(title = "Capacidad de adquirir patrimonio", x = "Sexo",
       y = "Población (en porcentaje por sexo)")+
  guides(fill = guide_legend(title = "Nivel de 
capacidad"))+
  scale_fill_manual(values = c("cyan", "darkred"))