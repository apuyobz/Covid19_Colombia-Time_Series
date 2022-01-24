library(tidyverse)

covid <- read_csv("../Data/Casos_positivos_de_COVID-19_en_Colombia.csv")
head(covid, 10)

### 1. Composición de las variables:
summary(covid)
str(covid)

# Selección de variables
covid_usar <- covid %>% 
  select("ID de caso", "Fecha de notificación",
                     "Ciudad de ubicación", "Departamento o Distrito",
                     "Edad", "Sexo")
str(covid_usar)

# Existencia de NA´s
any(!complete.cases(covid_usar))
map_dbl(covid_usar, .f = function(x){sum(is.na(x))})

# Reestructuración de variables
library(lubridate)

covid_usar$`Ciudad de ubicación` <- as_factor(covid_usar$`Ciudad de ubicación`)
covid_usar$`Departamento o Distrito` <- as_factor(covid_usar$`Departamento o Distrito`)
covid_usar$`Fecha de notificación` <- lubridate::as_date(covid_usar$`Fecha de notificación`)
str(covid_usar)

# La variable sexo tiene  unos en mayusculas y otras en min
covid_usar$Sexo <- ifelse(covid_usar$Sexo == "f", "F", covid_usar$Sexo)
covid_usar$Sexo <- ifelse(covid_usar$Sexo == "m", "M", covid_usar$Sexo)
covid_usar$Sexo <- as_factor(covid_usar$Sexo)
levels(covid_usar$Sexo)

### 2. Análisis de los contagios:

# Se crea una variable de edad por grupos para mejor análisis
covid_usar <- covid_usar %>% 
  mutate(Edad_grupos = case_when(Edad <= 5 ~ "Primera Infancia",
                                 Edad >= 6 & Edad <= 11 ~ "Infancia",
                                 Edad >= 12 & Edad <= 17 ~ "Adolescencia",
                                 Edad >= 18 & Edad <= 30 ~ "Juventud",
                                 Edad >= 31 & Edad <= 60 ~ "Adultez",
                                 Edad >= 61 ~ "Adulto mayor"))

covid_usar$Edad_grupos <- as.factor(covid_usar$Edad_grupos) 


# Tambien se crea una variable de muerte para su análisis, se acomodan los NA y se convierte a factor
covid_usar <- covid_usar %>% mutate(Muerte = covid$`Fecha de muerte`)
covid_usar$Muerte <- ifelse(is.na(covid_usar$Muerte),
                            "No", "Si")
covid_usar$Muerte <- as_factor(covid_usar$Muerte)
levels(covid_usar$Muerte)

# 2.1. Contagios por ciudades
ciud_contg <- covid_usar %>% 
  group_by(covid_usar$`Ciudad de ubicación`, 
           covid_usar$`Departamento o Distrito`) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count))

# Top 15 de contagios
top_15 <- ciud_contg %>% 
  top_n(count, n = 15) %>% 
  arrange(desc(count))

# Renombro las variables
names(top_15) = c("Ciudad", "Departamento", "Count")

# Gráfico del top
ggplot(data = top_15, aes(x = reorder(Ciudad, desc(Count)), 
                          y = Count, 
                          fill = Ciudad,
                          label = Count)) +
  geom_col() +
  geom_text(size = 2.8, position = position_stack(vjust = 0.7)) +
  labs(x = '',
       y = 'Casos') +
  ggtitle(label = 'Las 15 ciudades con mayor casos de COVID en Colombia') +
  scale_y_continuous(limits = c(0, 300000),
                     labels = c("0", "100.000",
                                "200.000", "300.000")) +
  guides(fill = FALSE)

# 2.2. Contagios por sexo
sex_contg <- covid_usar %>% 
  group_by(Sexo) %>% 
  summarise(cont = n())

# Gráfico de distribución
ggplot(data = sex_contg, aes(x = reorder(Sexo, desc(cont)),
                             y = cont, 
                             fill = Sexo,
                             label = cont)) +
  geom_col() +
  geom_text(size = 2.8, position = position_stack(vjust = 0.7)) +
  labs(x = '',
       y = 'Casos',
       title = "Casos en Mujeres y Hombres") +
  scale_y_continuous(limits = c(0, 500000),
                     labels = c("0", "100.000",
                                "200.000", "300.000",
                                "400.000", "500.000")) +
  guides(fill = FALSE)

# 2.3. Contagio por edades

# Comportamiento de los contagios en las edades
age_contg <- covid_usar %>% 
  group_by(Edad_grupos) %>% 
  summarise(c = n()) %>%
  arrange(desc(c))

# Comportamiento de los muertes en las edades
age_muert <- covid_usar %>% 
  group_by(Edad_grupos, Muerte) %>% 
  summarise(c = n()) %>%
  arrange(desc(c))

# Gráfico de contagios por edades
ggplot(data = age_contg, aes(x = reorder(Edad_grupos, desc(c)),
                             y = c, 
                             fill = Edad_grupos,
                             label = c)) +
  geom_col() +
  geom_text(size = 2.8, position = position_stack(vjust = 0.7)) +
  labs(x = '',
       y = 'Casos',
       title = "Casos por Edades") +
  scale_y_continuous(limits = c(0, 500000),
                     labels = c("0", "100.000",
                                "200.000", "300.000",
                                "400.000", "500.000")) +
  guides(fill = FALSE)

# Gráfico de muertes por edades
age_muert %>% filter(Muerte == "Si") %>%
  ggplot(aes(x = reorder(Edad_grupos, desc(c)),
                             y = c, 
                             fill = Edad_grupos,
                             label = c)) +
  geom_col(position = "dodge") +
  geom_text(size = 2.8, position = position_stack(vjust = 0.7)) +
  labs(x = '',
       y = 'Casos',
       title = "Muertes por Edades") +
  scale_y_continuous(limits = c(0, 25000)) +
  guides(fill = FALSE)


### 3. Comportamiento de los datos a traves del tiempo

# 3.1. Contagios a traves del tiempo
sum_covid <- covid_usar %>% 
  group_by(covid_usar$`Fecha de notificación`) %>%
  summarise(conteo = n()) %>% 
  ungroup %>% 
  mutate(acumulado = cumsum(conteo))

names(sum_covid) = c("Fecha", "Conteo", "Acumulado")

# Gráfico del comportamiento diario
library(ggthemes)

ggplot(data = sum_covid, aes(x = Fecha, y = Conteo)) +
  geom_line() +
  scale_x_date(date_breaks = "1 week", limits = c(as.Date('2020-03-02'), as.Date('2020-10-14'))) +
  theme_hc() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        text = element_text(size = 16)) +
  labs(x = '', 
       y = 'Cantidad casos reportados por día',
       title = "Comportamiento de casos de Covid en Colombia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Gráfico de los casos acumulados
ggplot(data = sum_covid, aes(x = Fecha, y = Acumulado)) +
  geom_line() +
  scale_x_date(date_breaks = "1 week", limits = c(as.Date('2020-03-02'), as.Date('2020-10-14'))) +
  theme_hc() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        text = element_text(size = 16)) +
  labs(x = '', 
       y = 'Cantidad casos reportados totales',
       title = "Comportamiento de casos totalizados de Covid en Colombia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 1000000))


### 4. Serie temporal de la enfermedad y predicción
library(forecast)

# Datos en formato ts
# En el comienzo se pone mes, día y la frecuencia mensual
covid_ts <- ts(sum_covid$Acumulado, start = c(3, 3), frequency = 30)
covid_ts

autoplot(covid_ts)

# Descomposición de la ts
plot(stl(covid_ts, s.window = "period"))
   
# Modelado 
covid_mod <- HoltWinters(covid_ts)
head(covid_mod, 10)

# Comparativo del real vs modelo
plot(covid_mod, col = "blue", col.predicted = "red")

# Error
covid_mod$SSE

# Predicción
covid_pred <- forecast(covid_mod, h = 150)
autoplot(covid_pred) +
  labs(x = 'Meses',
       y = 'Casos',
       title = "Predicción casos totales Nov 2020 - Mar 2021")

# Componentes de los intervalos de confianza
ic_covid_sup <- covid_pred$upper
ic_covid_inf <- covid_pred$lower

