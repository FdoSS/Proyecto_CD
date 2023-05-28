
# Leemos el archivo CSV

datos_or <- read.csv("ESS-Data-Wizard-subset-2023-05-20.csv")



# Visualizamos la estructura de los datos

str(datos_or)


# Extraemos las variables de interés y las guardamos en el dataframe "datos"

if(!require(dplyr)){install.packages('dplyr')
  library(dplyr)
}

datos <- select(datos_or, idno, nwspol, netusoft, netustm, ppltrst, pplfair, pplhlp, hhmmb, gndr, agea, cntry, maritalb, chldhhe, domicil, eduyrs)

#Inspeccionamos las variables

str(datos)

#describe(datos[,2:15])

tabla_frecuencias1 <- table(datos$nwspol)
tabla_frecuencias2 <- table(datos$netusoft)
tabla_frecuencias3 <- table(datos$netustm)
tabla_frecuencias4 <- table(datos$ppltrst)
tabla_frecuencias5 <- table(datos$pplfair)
tabla_frecuencias6 <- table(datos$pplhlp)
tabla_frecuencias7 <- table(datos$hhmmb)
tabla_frecuencias8 <- table(datos$gndr)
tabla_frecuencias9 <- table(datos$agea)
tabla_frecuencias10 <- table(datos$cntry)
tabla_frecuencias11 <- table(datos$maritalb)
tabla_frecuencias12 <- table(datos$chldhhe)
tabla_frecuencias13 <- table(datos$domicil)
tabla_frecuencias14 <- table(datos$eduyrs)

# Recodificamos las variables para eliminar las categorias Not applicable, Refusal, Don't know, No answer en las diferentes variables
 
datos <- datos %>% 
  mutate(nwspol = ifelse(nwspol %in% c(6666, 7777, 8888, 9999), NA, nwspol),
         netustm = ifelse(netustm %in% c(6666, 7777, 8888, 9999), NA, netustm),
         netusoft = ifelse(netusoft %in% c(7, 8, 9), NA, netusoft),
         ppltrst = ifelse(ppltrst %in% c(77, 88, 99), NA, ppltrst),
         pplfair = ifelse(pplfair %in% c(77, 88, 99), NA, pplfair),
         pplhlp = ifelse(pplhlp %in% c(77, 88, 99), NA, pplhlp),
         hhmmb = ifelse(hhmmb %in% c(77, 88, 99), NA, hhmmb),
         gndr = ifelse(gndr %in% c(99), NA, gndr),
         agea = ifelse(agea %in% c(999), NA,agea),
         maritalb = ifelse(maritalb %in% c(77, 88, 99), NA, maritalb),
         chldhhe = ifelse(chldhhe %in% c(6, 7, 8, 9), NA, chldhhe),
         domicil = ifelse(domicil %in% c(77, 88, 99), NA, domicil),
         maritalb = ifelse(maritalb %in% c(7, 8, 9), NA, maritalb),
         eduyrs = ifelse(eduyrs %in% c(77, 88, 99), NA, eduyrs))







# Guardamos como factores las variables categóricas 

## Género

datos$gndr <- factor(datos$gndr, levels = c(1, 2), labels = c("Varones", "Mujeres"))

## País

# Creamos un vector de valores correspondientes
correspondencias_1 <- c("AL" = "Albania", "AT" = "Austria", "BE" = "Belgium", "BG" = "Bulgaria", "CH" = "Switzerland", "CY" = "Cyprus", "CZ" = "Czechia", "DE" = "Germany", "DK" = "Denmark", "EE" = "Estonia", "ES" = "Spain", "FI" = "Finland", "FR" = "France", "GB" = "United Kingdom", "GE" = "Georgia", "GR" = "Greece", "HR" = "Croatia", "HU" = "Hungary", "IE" = "Ireland", "IS" = "Iceland",
 "IL" = "Israel", "IT" = "Italy", "LT" = "Lithuania", "LU" = "Luxembourg", "LV" = "Latvia",
 "ME" = "Montenegro", "MK" = "North Macedonia", "NL" = "Netherlands", "NO" = "Norway",
"PL" = "Poland", "PT" = "Portugal", "RO" = "Romania", "RS" = "Serbia", "RU" = "Russian Federation",
"SE" = "Sweden", "SI" = "Slovenia", "SK" = "Slovakia", "TR" = "Turkey", "UA" = "Ukraine",
"XK" = "Kosovo")

datos$cntry <- factor(datos$cntry, levels = names(correspondencias_1), labels = correspondencias_1)


## Estado civil

correspondencias_2 <- c("1" = "Casado/a", "2" = "Pareja de hecho", "3" = "Separado/a", "4" = "Divorciado/a", "5" = "Viudo/a", "6" = "Soltero/a")

datos$maritalb <- factor(datos$maritalb, levels = names(correspondencias_2), labels = correspondencias_2)

## Niños/as en el hogar

correspondencias_3 <- c("1" = "Con menores en el hogar", "2" = "Sin menores en el hogar")

datos$chldhhe <- factor(datos$chldhhe, levels = names(correspondencias_3), labels = correspondencias_3)


## Lugar de residencia

correspondencias_4 <- c("1" = "Gran ciudad", "2" = "Afueras de una gran ciudad", "3" = "Ciudad pequena", 
                      "4" = "Pueblo rural", "5" = "Granja o casa en el campo")
datos$domicil <- factor(datos$domicil, levels = names(correspondencias_4), labels = correspondencias_4)


# Creamos dos variables adicionales para categorizar la edad y los años de educación

## Grupos de edad

datos$grupo_edad <- cut(datos$agea, breaks = seq(15, 90, 15), labels = c("15-29", "30-44", "45-59", "60-74", "75-90"), include.lowest = TRUE)

datos$grupo_edad <- as.factor(datos$grupo_edad)


## Años de educación


datos$etapa_educativa <- cut(datos$eduyrs, breaks = c(0, 6, 14, Inf),
                             labels = c("E. Primarios", "E. Secundarios",
                                        "E. superiores"),
                             include.lowest = TRUE, right = FALSE)

datos$etapa_educativa <- as.factor(datos$etapa_educativa)


# Examinamos la distrobución de las variables continuas mediante gráficos de densidad 

densidad1 <- density(datos$nwspol, na.rm = TRUE)
plot(densidad1, main = "Gráfico de Densidad - nwspol", xlab = "nwspol", ylab = "Densidad")


if(!require(ggplot2)){install.packages('ggplot2')
  library(ggplot2)
}


ggplot(datos, aes(x = nwspol)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(title = "Distribución de Densidad - nwspol", x = "nwspol", y = "Densidad")

ggplot(datos, aes(x = netustm)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(title = "Distribución de Densidad - netustm", x = "netustm", y = "Densidad")

ggplot(datos, aes(x = ppltrst)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(title = "Distribución de Densidad - ppltrst", x = "ppltrst", y = "Densidad")

ggplot(datos, aes(x = pplfair)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(title = "Distribución de Densidad - pplfair", x = "pplfair", y = "Densidad")

ggplot(datos, aes(x = pplhlp)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(title = "Distribución de Densidad - pplhlp", x = "pplhlp", y = "Densidad")


# Dividir los datos por los niveles de "gndr"
split_data <- split(datos$ppltrst, datos$gndr)

# Crear el histograma para cada nivel de "gndr"
hist(split_data$Varones, col = "blue", main = "Histograma de ppltrst (Varones)", xlab = "ppltrst")
hist(split_data$Mujeres, col = "pink", main = "Histograma de ppltrst (Mujeres)", xlab = "ppltrst")

hist(split_data$Mujeres, col = "pink", main = "Histograma de ppltrst (Mujeres)", xlab = "ppltrst", add = TRUE)

# Grabados datos depurados

write.csv(datos, file = "Datos depurados.csv", row.names = FALSE)

