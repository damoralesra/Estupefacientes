# Librerias

library(dplyr)
library(tidyr) 
library(stringr)
#library(readr) # Usado para la importación y unión de bases iniciales
#library(purrr) # Usado para la importación y unión de bases iniciales
library(lubridate)
library(ggplot2)
library(pander)
library(plotly)

####################################################################


# Leo los archivos CSV en el directorio.
archivos <- dir(path = "bases", pattern = "*.csv")

# Creo la base de datos con los archivos .CSV
estupefacientes <- archivos %>%
  map_dfr(~ read_csv(file.path(path = "bases", .)))

# Separo la Fecha en Mes-Dia-Año y Hora
# Ordeno las columnas en el orden deseado y elimino la columna "CÓDIGO DANE"
estupefacientes1 <- estupefacientes %>%
  mutate(FECHA = mdy_hms(FECHA)) %>%
  separate(FECHA, c("AÑO", "MES", "DIA_MES"), remove = FALSE) %>%
  select(FECHA, AÑO, MES, DIA_MES, DIA, DEPARTAMENTO, MUNICIPIO, ZONA,
         `CLASE DE SITIO`, `CLASE DE ESTUPEFACIENTE`, `UNIDAD DE MEDIDA`, 
         CANTIDAD, -`CÓDIGO DANE`)


write.csv(estupefacientes1, file = "Estupefacientes.csv", 
          fileEncoding = "UTF-8")


####################################################################

data <- read.csv("Estupefacientes.csv")

str(data)

### Se elimina la columna "X" y se transforma a factor la variables deseadas
### adicional se eliminan los datos faltantes 'NA' de año, depto. También se 
### filtra que la cantidad sea mayor a 1 gramo

datos <- data %>%
  select(-X) %>%
  mutate_if(is.integer, as.factor) %>%
  filter(
    AÑO != " " &
      DEPARTAMENTO != " " &
      CANTIDAD >= 1
    ) %>%
  mutate(
    FECHA = date(FECHA),
    MES = recode(MES,
                 `1` = "ENERO",
                 `2` = "FEBRERO",
                 `3` = "MARZO",
                 `4` = "ABRIL",
                 `5` = "MAYO",
                 `6` = "JUNIO",
                 `7` = "JULIO",
                 `8` = "AGOSTO",
                 `9` = "SEPTIEMBRE",
                 `10` = "OCTUBRE",
                 `11` = "NOVIEMBRE",
                 `12` = "DICIEMBRE")
  )

str(datos)
summary(datos)

# Cambios en incuataciones a través del tiempo, total y tipo de estupefaciente

g <- datos %>%
  group_by(FECHA) %>%
  summarise(total = round(sum(CANTIDAD)/1000, 2)) %>%
  ggplot(aes(x = FECHA, y = total)) +
  geom_line(col = "gray30") +
  labs(x = "Fecha",
       y = "Kilogramos Incautados") +
  theme_minimal()
ggplotly(g)

datos %>%
  group_by(FECHA, CLASE.DE.ESTUPEFACIENTE) %>%
  summarise(total = sum(CANTIDAD)) %>%
  ggplot(aes(x = FECHA, y = total/1000, col = CLASE.DE.ESTUPEFACIENTE)) +
  geom_line() +
  facet_wrap(~CLASE.DE.ESTUPEFACIENTE,
             scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Fecha",
       y = "Kilogramos Incautados")

g <- datos %>%
  filter(CLASE.DE.ESTUPEFACIENTE %in% c("COCAINA", "MARIHUANA")) %>%
  group_by(FECHA, CLASE.DE.ESTUPEFACIENTE) %>%
  summarise(total = sum(CANTIDAD)) %>%
  ggplot(aes(x = FECHA, y = total/1000, col = CLASE.DE.ESTUPEFACIENTE)) +
  geom_line() +
  facet_wrap(~CLASE.DE.ESTUPEFACIENTE) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Fecha",
       y = "Kilogramos Incautados")
ggplotly(g)

# Cantidad de Estupefacientes por zona durante los años

zonas <- datos %>%
  group_by(AÑO, ZONA) %>%
  tally() %>%
  mutate(total = sum(n),
         Porcentaje = n/total * 100) %>%
  select(-total)

zonas1 <- datos %>%
  group_by(AÑO, ZONA) %>%
  summarise(`Total (t)` = sum(CANTIDAD)/1000000)

zonasTotal <- full_join(zonas, zonas1)

pander(zonasTotal,
       justify = "center", style = "rmarkdown")

ggplot(data = zonasTotal,
       aes(x = ZONA, y = Porcentaje, group = AÑO)) +
  geom_bar(aes(fill = AÑO),
           stat = "identity", position = "dodge", color = "gray15") +
  geom_text(aes(label = round(Porcentaje, 2)),
            position = position_dodge(width = 1), vjust = 1.5, size = 5) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

ggplot(data = zonasTotal,
       aes(x = ZONA, y = `Total (t)`)) +
  geom_bar(aes(fill = AÑO), 
           stat = "identity", position = "dodge", color = "gray15") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Sitio en el que se incauto el estupefaciente, cantidad y porcentaje

## 5 primeros sitios de incautación.
sitios <- datos %>%
  group_by(CLASE.DE.SITIO) %>%
  tally() %>%
  mutate(Total = sum(n),
         `Participación %` = round(n/Total * 100, 2)) %>%
  arrange(desc(n)) %>%
  select(-Total) %>%
  top_n(5)

pander(sitios,
       style = "rmarkdown", justify = c("left", "right", "center"),
       caption = "5 Mayores lugares de incautación")

## Tipo de estupefaciente mayormente incautado en el sitio #1

estupe_sitio <- datos %>%
  filter(CLASE.DE.SITIO == "VIAS PUBLICAS") %>%
  group_by(CLASE.DE.ESTUPEFACIENTE) %>%
  tally() %>%
  mutate(
    Total = sum(n),
    `Participación %` = round(n/Total*100, 2)
  ) %>% 
  arrange(desc(n))

cantidad_sitio <- datos %>%
  filter(CLASE.DE.SITIO == "VIAS PUBLICAS") %>%
  group_by(CLASE.DE.ESTUPEFACIENTE) %>%
  summarise(`Cantidad (t)` = round(sum(CANTIDAD)/1000000, 2))

sitio1_estupefaciente <- full_join(estupe_sitio, cantidad_sitio)

pander(sitio1_estupefaciente,
       style = "rmarkdown",
       caption = "Cantidad de estupefacientes incautados en Vías Públicas")

sitio1_estupefaciente %>%
  ggplot(aes(x = CLASE.DE.ESTUPEFACIENTE, y = n)) +
  geom_bar(stat = "identity", colour = 1, fill = "gray78") +
  labs(x = "Clase de Estupefaciente",
       y = "Cantidad de incautaciones",
       title = "Incautaciones en Vías Públicas") +
  theme_minimal()

## Incautaciones en otros sitios

### Carceles e instalaciones de fuerzas militares

datos %>%
  filter(
    CLASE.DE.SITIO %in% c("CARCELES",
                          "INSTALACIONES DEL EJERCITO")
  ) %>%
  group_by(AÑO, CLASE.DE.SITIO) %>%
  summarise(Cantidad = sum(CANTIDAD)/1000) %>%
  ggplot(aes(x = AÑO, y = Cantidad, fill = CLASE.DE.SITIO)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Año",
       y = "Cantidad (kg)",
       title = "Incautaciones",
       fill = "Sitio") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set1")

### Colegios y universidades

datos %>%
  filter(
    CLASE.DE.SITIO %in% c("UNIVERSIDADES",
                          "COLEGIOS, ESCUELAS",
                          "ESCENARIOS DEPORTIVOS")
  ) %>%
  group_by(AÑO, CLASE.DE.SITIO) %>%
  summarise(Cantidad = sum(CANTIDAD)) %>%
  ggplot(aes(x = AÑO, y = Cantidad, fill = CLASE.DE.SITIO)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Año",
       y = "Cantidad (grs)",
       title = "Incautaciones",
       fill = "Sitio") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set1")

# Día de la semana donde más se incauta marihuana.

datos %>%
  filter(CLASE.DE.ESTUPEFACIENTE == "MARIHUANA") %>%
  group_by(DIA) %>%
  tally() %>%
  arrange(desc(n)) %>%
  pander(style = "grid")

# Departamentos donde más droga se incauto

### 5 Departamentos con más incautaciones

datos %>%
  group_by(DEPARTAMENTO) %>%
  tally() %>%
  arrange(desc(n)) %>%
  do(head(., 5)) %>%
  pander(col.names = c("Departamento", "Incautaciones"),
         style = "rmarkdown",
         justify = c("left", "right"))

### Tipo y cantidad de estupefacientes en los anteriores departamentos

depto <- datos %>%
  filter(DEPARTAMENTO %in% c("ANTIOQUIA", "VALLE", "CUNDINAMARCA",
                             "SANTANDER", "CESAR")) %>%
  group_by(DEPARTAMENTO, CLASE.DE.ESTUPEFACIENTE) %>%
  tally() %>%
  arrange(DEPARTAMENTO, desc(n))

deptoCan <- datos %>%
  filter(DEPARTAMENTO %in% c("ANTIOQUIA", "VALLE", "CUNDINAMARCA",
                             "SANTANDER", "CESAR")) %>%
  group_by(DEPARTAMENTO, CLASE.DE.ESTUPEFACIENTE) %>%
  summarise(Cantidad = round(sum(CANTIDAD)/1000, 2))

depto_total <- full_join(depto, deptoCan)

pander(depto_total, style = "rmarkdown", 
       col.names = c("Departamento", "Estupefaciente",
                     "Incautaciones", "Cantidad (kg)"),
       justify = c("left", "left", "right", "right"))

## 15 Municipios de Antioquia donde más incautaciones hubo

datos %>%
  filter(DEPARTAMENTO == "ANTIOQUIA") %>%
  group_by(DEPARTAMENTO, MUNICIPIO) %>%
  tally() %>%
  arrange(desc(n)) %>%
  do(head(., 15)) %>%
  pander(style = "rmarkdown", justify = "left")

# Incautaciones día a día en los primeros 5 municipios CT de mayor incautación

datos %>%
  filter(str_detect(MUNICIPIO, "(CT)")) %>%
  group_by(MUNICIPIO) %>%
  tally() %>%
  arrange(desc(n)) %>%
  do(head(., 5))

g <- datos %>%
  filter(
    MUNICIPIO %in% c("BOGOTÁ D.C. (CT)",
                     "MEDELLÍN (CT)",
                     "CALI (CT)")
  ) %>%
  group_by(FECHA, MUNICIPIO) %>%
  summarise(cantidad = sum(CANTIDAD)/1000) %>%
  ggplot(aes(x = FECHA, y = cantidad, col = MUNICIPIO)) +
  geom_line() +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Fecha",
       y = "Cantidad (kg)",
       col = "Ciudad") +
  facet_grid(MUNICIPIO ~ .,
             scales = "free_y") +
  theme(strip.text.y = element_blank())
ggplotly(g)

# Incautaciones por mes 

datos %>%
  group_by(AÑO, MES) %>%
  tally() %>%
  ggplot(aes(x = MES, y = n, fill = AÑO)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  labs(x = "Mes",
       y = "Incautaciones",
       title = "Incautaciones durante los años 2016 y 2017",
       fill = "Año") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5))

## Tabla resumen

datos %>%
  group_by(MES, AÑO) %>%
  summarise(`Cantidad (t)` = round(sum(CANTIDAD)/1000000, 2)) %>%
  pander(style = "rmarkdown",
         justify = c("left", "center", "center"))

