# Programa para analizar el impacto de diversidad animalia sobre seguridad alimentaria en
# pequeños productores pecuarios. Escrito por Julien Malard y Anh Bui.
# Contacto: julien.malard@mail.mcgill.ca (para dudas, errores, recomendaciones, complimentos)
# Cualquier problema con el programa o con R en general, quedo a la órden.

# Nota: usted tendrá que cambiar las partes apropiadas del código para su base de datos y equipo.

#### 1. Iniciar las bases de datos ####

rm(list=ls())  # Borrar todo. 90% de los problemas en R se arreglan con esta comanda.

# Cambiar al directorio en su equipo con las bases de datos
setwd("F:/Julien/Índices de biodiversidad/Anh/datos")

# La base de datos de hogares (con ingresos, animales, seguridad alimentaria.) 
# Cambiar según el equipo.
# Nota general: se deben guardar todas sus bases de datos excel en formato .csv
datos.hogares <- read.csv('Datos Hogares Guatemala.csv')
View(datos.hogares)

# Para los que convierten de Excel, reemplazar "#NULL!" por NA. Si su base de datos contiene
# otro marcador para datos que faltan, cambiarlo a NA también.
for (i in 1:ncol(datos.hogares)){
  datos.hogares[[i]][datos.hogares[[i]]=="#NULL!"] <- NA
  datos.hogares[[i]][datos.hogares[[i]]=="#N/A"] <- NA
}

# Migrar las columnas de interés de 'datos.hogares' hacia 'datos'. Cambiar los nombres de columnas
# según su base de datos.
col_Formulario = 'FORMULARIO'  # La columna con el número de identificación de cada hogar
col_Peso.estad = 'FACTOR'  # Pesos estadísticos (opcional)
col_dept = 'DEPTO'  # La columna con el código de departamento (o estado) del hogar
col_Tamañofamilia = 'Tamaño.de.las.familias'  # Número de miembros en el hogar
col_Ingresos = 'Ingresos'  # Ingresos monetarios totales del hogar
col_Totalanimales = 'Animales.traspatio'  # Número total de animales por hogar


# Quitar los hogares que no corresponden a los criterios de pequeños agricultores, si necesario.
# Se recomienda verificar la definición de "pequeño productor" en su lugar de estudio.
datos.hogares <- datos.hogares[datos.hogares[[col_Totalanimales]] > 0 & 
                                 datos.hogares[[col_Totalanimales]] <= 20,]

# La base de datos que constriurimos para nuestro análisis
datos <- data.frame(datos.hogares[[col_Formulario]])
names(datos)[1] <- 'FORMULARIO'

datos['Peso.estad'] <- datos.hogares[[col_Peso.estad]]
datos['Dept'] <- datos.hogares[[col_dept]]
# Si su encuesta no tiene pesos estadísticos distintos para cada hogar, saltar la línea anterior
# y activar la línea siguiente:

# datos['Peso.estad'] <- 1  # A activar si no hay pesos distintos para cada hogar

datos['Tamaño_familia'] <- datos.hogares[[col_Tamañofamilia]]
datos['Ingresos'] <- datos.hogares[[col_Ingresos]]
datos['Totalanimales'] <- datos.hogares[[col_Totalanimales]]

# Verificar sus datos:
hist(log(datos$Ingresos))
hist(datos$Tamaño_familia)

# Generar los otros datos necesarios para los análisis

datos["Ingresoporcápita"] = datos$Ingresos/datos$Tamaño_familia
datos["log_Ingresoporcápita"] = log(datos$Ingresoporcápita+1)  # Añadir 1 porque algunos tienen Ingreso de 0

#### 2. Calcular la seguridad alimentaria ####
datos["SegAli"] <- NA
datos["SegAli.puntaje"] <- NA

# Aquí, generar una lista de los nombres de las columnas de su base de datos que contienen
# las respuestas a la ELCSA (Escala Latinoamericana y Caribeña de Seguridad Alimentaria).
SegAli.col = NULL
for(a in 1:16){
  SegAli.col[a] <- if(a<10){paste("P01H0",a,sep="")} else {paste("P01H",a,sep="")}
}

# Poner 1 para respuestas positivas y 0 para negativas (ajustar a su base de datos)
# Ponemos 99 para datos que faltan (personas que no respondieron). Datos vacíos (NA) se consideran como 0.
for (b in 1:16){
  temp = datos.hogares[[SegAli.col[b]]]
  temp[temp == "2"] <- 0
  temp[temp == "1"] <- 1
  temp[temp == "9" | temp == "99"] <- 99
  temp[is.na(temp)] <- 0
  datos[[SegAli.col[b]]] <- temp
}

# Convertir las respuestas al cuestionario a las categorías de inseguridad alimentaria de la ELCSA
datos['SegAli'] <- NA
datos['SegAli.puntaje'] <- NA
for (a in 1:nrow(datos)){
  # Sumar las respuestas a las preguntas de la ELCSA
  temp <- 0
  for (b in 1:length(SegAli.col)) {
    temp <- sum(temp, datos[[SegAli.col[b]]][a], na.rm = T)
  }
  datos$SegAli.puntaje[a] <- temp
  
  # Convertir el total de respuestas positivas a la categoría de inseguridad alimentaria
  if(datos$SegAli.puntaje[a]==0) {datos$SegAli[a] <- 0}
  if(datos[[SegAli.col[9]]][a]==0){  # Si hay niños en el hogar
    if(datos$SegAli.puntaje[a]>=1 & datos$SegAli.puntaje[a]<=3){datos$SegAli[a]<- 1}
    if(datos$SegAli.puntaje[a]>=4 & datos$SegAli.puntaje[a]<=6){datos$SegAli[a]<- 2}
    if(datos$SegAli.puntaje[a]>=7 & datos$SegAli.puntaje[a]<=8){datos$SegAli[a]<- 3}
  } else if (datos[[SegAli.col[9]]][a]==1) {  # Si no hay niños
    if(datos$SegAli.puntaje[a]>=1 & datos$SegAli.puntaje[a]<=5){datos$SegAli[a]<- 1}
    if(datos$SegAli.puntaje[a]>=6 & datos$SegAli.puntaje[a]<=10){datos$SegAli[a]<- 2}
    if(datos$SegAli.puntaje[a]>=11 & datos$SegAli.puntaje[a]<=15){datos$SegAli[a]<- 3}
  }
  # Poner NA en las filas para cuales faltaban datos
  for (b in 1:16){
    if(datos[[SegAli.col[b]]][a]==9 | datos[[SegAli.col[b]]][a]==99) {datos[[SegAli.col[b]]][a]=NA}
  }
}

# Verificar los cálculos
weighted.mean(datos$SegAli.puntaje, datos$Peso.estad);weighted.mean(datos$SegAli, datos$Peso.estad, na.rm=T)
hist(datos$SegAli, main = 'Inseguridad alimentaria', ylab = 'Frecuencia', xlab = 'Categoría ELCSA')

# Guardar su trabajo para mañana
write.csv(datos,'Datos calculados.csv', row.names=FALSE)

#### 3. Calcular los índices de diversidad animalia: ####

# Índices normales
datos["Simpson"] <- NA
datos["Shannon"] <- NA
datos["Gini"] <- NA
datos["Margalef"] <- NA
datos["BuzasGibson"] <- NA

# Índices con peso por el valor de cada animal individual
datos["Simpson.val.ind"] <- NA
datos["Shannon.val.ind"] <- NA
datos["Gini.val.ind"] <- NA
datos["BuzasGibson.val.ind"] <- NA

# Índices con peso por el valor promedio departamental de cada animal
datos["Simpson.val.med"] <- NA
datos["Shannon.val.med"] <- NA
datos["Gini.val.med"] <- NA
datos["BuzasGibson.val.med"] <- NA

# Índices con peso por el valor promedio nacional de cada animal
datos["Simpson.val.dept"] <- NA
datos["Shannon.val.dept"] <- NA
datos["Gini.val.dept"] <- NA
datos["BuzasGibson.val.dept"] <- NA

# Índices con peso por las unidades ganaderas de cada animal
datos["Simpson.UG"] <- NA
datos["Shannon.UG"] <- NA
datos["Gini.UG"] <- NA
datos["BuzasGibson.UG"] <- NA


# Nuestros datos de animales están en otra base de datos. Si los suyos ya se encuentran en 
# 'datos', puede saltar estas líneas abajo.

# La base de datos con datos agropecuarios
Anim <- read.csv("F:/Julien/PhD/Iximulew/MDS SAN/Datos calibración MDS/Datos ENCOVI/Limp_Actividades pecuarias.csv")
col_valor.animal <- 'P14D04'  # La columna con el valor monetario del animal
col_núm.animal <- 'P14D03'  # La columna con el número de cada tipo de animal en el hogar
col_form_anim <- 'FORMULARIO'  # La columna con la identificación del hogar

# Crear una columna vacía para guardar el valor de cada animal
prim.Col <- ncol(datos) + 1 # El número de la primera columna
últ.col <-  prim.Col + length(nombres) - 1  # El número de la última columna

# Generar una lista de los nombres de las columnas que tienen el valor monetario
# de cada animal. Si su base de datos ya tiene estas columnas, llenar la lista a mano 
# en vez de utilizar el código abajo.
col_val.animales = paste(nombres, '.val', sep='')

# Generar columnas vacías para los valores y números de los animales
for (i in 1:length(nombres)) {  # Para cada tipo de animal
  datos[[col_val.animales[i]]] <- NA  # Crear una columna vacía
}
for (i in 1:length(nombres)) {  # Para cada tipo de animal
  datos[[nombres[i]]] <- NA  # Crear una columna vacía
}

# Llenar las columnas de datos del valor de cada animal
for (i in 1:nrow(datos)){  # Para cada hogar
  temp <- Anim[Anim[[col_form_anim]] == datos[[col_Formulario]][i],]
  if (nrow(temp) == 0) {
    datos[i,prim.Col:últ.col] <- 0
    datos[i,prim.Col+length(nombres):últ.col+length(nombres)] <- 0
  } else {
    temp[is.na(temp)] <- 0
    datos[i,prim.Col:últ.col] <- temp[[col_valor.animal]]
    datos[i,(prim.Col+length(nombres)):(últ.col+length(nombres))] <- temp[[col_núm.animal]]
  }
}

# Si estaba saltando el código arriba porque su base de datos ya tenía los valores 
# monetarios de los animales, aquí tiene que reempezar a seguir el código.
# En ese caso, no se le olvide generar la lista col_val.animales apropiada (ver arriba).

# Una lista de los nombres de los tipos de animales en su base de datos.
nombres = c("Vacas, Toros, Terneros","Cerdos","Ovejas, Peligueyes","Cabras",
            "Gallinas, Pollos","Pavos, Chompipes","Patos","Caballos, Burros, Mulas",
            "Colmenas","Peces, Camarones")

# Poner los valores de unidades ganaderas para cada tipo de animal.
# DEBE estar en el mismo orden que "nombres".
ValorUG <- c(1.0000, 0.3333, 0.3333, 0.3333, 0.0667, 0.0667, 0.0667, 1.5000, 0.2000, 0.0667)

datos["UniGan"] = NA
for (i in 1:length(nombres)) {
  datos['UniGan'] <- rowSums(data.frame(datos[['UniGan']], datos[[nombres[i]]] * ValorUG[i]), na.rm = T)
}
datos["log_UniGan"] = log(datos$UniGan)

### Generar y guardar un gráfico de los valores de los animales

# Calcular el valor de cada tipo de animal en la base de datos
ValorPorAnimal=list(NULL)
for (a in 1:length(col_val.animales)){
  ValorPorAnimal[[a]] <- datos[[col_val.animales[a]]]/datos[[nombres[a]]]
  ValorPorAnimal[[a]][is.nan(ValorPorAnimal[[a]])] <- NA
}

# Crear el directorio para guardar gráficos
if (!file.exists('Gráficos')) {
  dir.create(file.path('Gráficos'), showWarnings = F)
}

# Generar el gráfico
jpeg(filename='Gráficos/ValorPorAnimal.jpg')
plot.new()
boxplot(ValorPorAnimal,labels=FALSE, main = "Valor promedio de cada animal", ylab= "Valor")
axis(1, at=1:length(nombres), labels=nombres, las=2)
dev.off()

### Crear las funciones para calcular los Índices ###

Simpson <- function(animales){  #Simpson = sum (1/p^2); p = n/N
  animales <- animales[animales!=0]
  total <- sum(animales)
  valorSimpson <- 0
  for (i in 1:length(animales)){
    p <- animales[i]/total
    valorSimpson <- valorSimpson + (p^2)
  }
  valorSimpson <- 1/valorSimpson
  return(valorSimpson)
}

Shannon <- function(animales){  #Shannon = - sum (p*ln(p)); p = n/N ; n = no. de animales de un tipo ; N = total
  animales <- animales[animales!=0]
  total <- sum(animales)
  valorShannon <- 0
  for (i in 1:length(animales)){
    p <- animales[i]/total
    valorShannon <- valorShannon + (-p*log(p))
  }
  return(valorShannon)
}

Gini <- function(animales){ 
  # Gini = 1/S * (S+1-2*sum((S+1-i)*n)/sum(n)), donde
  # i = número de la especie n en orden de abundancia cresciendo
  animales <- animales[animales!=0]
  ordenado <- sort(animales, decreasing=FALSE)  # ordenar a los dados en ?rden de abundancia
  total <- sum(animales)
  S = length(animales[animales!=0]) # n?mero de especies
  sum <- 0
  for (i in 1:length(ordenado)){
    sum <- sum + ((S + 1 - i)*ordenado[i])
  }
  valorGini <- 1/S * (S + 1 - 2*sum/total)
  return(valorGini)
}

Margalef <- function(animales){  #Margalef = (S-1)/N; S = no. de especies
  animales <- animales[animales!=0]
  total <- sum(animales)
  S = length(animales[animales!=0]) # n?mero de especies
  valorMargalef <- (S-1)/log(total)
  return(valorMargalef)
}

BuzasGibson <- function(animales){  #BUzas-Gibson = (e^(Shannon))/S ; S = no. de especies
  #Calcular el índice Shannon
  animales <- animales[animales!=0]
  total <- sum(animales)
  S = length(animales[animales!=0]) # n?mero de especies
  valorShannon <- 0
  for (i in 1:length(animales)){
    p <- animales[i]/total
    valorShannon <- valorShannon + (-p*log(p))
  }
  #Calcular el índice Buzas-Gibson
  valorBuzasGibson <- exp(1)^(valorShannon)/S
  return(valorBuzasGibson)
}

### Caclular los índices: índices de base ###

for(a in 1:nrow(datos)){  # para cada hogar
  animaleshogar=NULL
  for(b in 1:length(nombres)){  # para cada tipo de animal
    animaleshogar[b] <- datos[[nombres[b]]][a]
  }
  if (sum(animaleshogar) > 0) {
    datos$Simpson[a] <- Simpson(animaleshogar)
    datos$Shannon[a] <- Shannon(animaleshogar)
    datos$Gini[a] <- Gini(animaleshogar)
    datos$Margalef[a] <- Margalef(animaleshogar)
    datos$BuzasGibson[a] <- BuzasGibson(animaleshogar)
  }
}

# Verificar resultados
weighted.mean(datos$Simpson, datos$Peso.estad, na.rm=T)
weighted.mean(datos$Shannon, datos$Peso.estad, na.rm=T)
weighted.mean(datos$Gini,datos$Peso.estad, na.rm=T)
weighted.mean(datos$Margalef,datos$Peso.estad, na.rm=T)
weighted.mean(datos$BuzasGibson,datos$Peso.estad, na.rm=T)

### Caclular los índices: valor auto-reportado (individual) de cada animal ###

for(a in 1:nrow(datos)){  # para cada hogar
  animaleshogar=NULL
  for(b in 1:length(nombres)){  # para cada tipo de animal
    animaleshogar[b] <- datos[[col_val.animales[b]]][a]
  }
  if (sum(animaleshogar) > 0) {
    datos$Simpson.val.ind[a] <- Simpson(animaleshogar)
    datos$Shannon.val.ind[a] <- Shannon(animaleshogar)
    datos$Gini.val.ind[a] <- Gini(animaleshogar)
    datos$BuzasGibson.val.ind[a] <- BuzasGibson(animaleshogar)
    # Notar: Para Margalef no se calculan modificaciones
  }
}

# Verificar resultados
weighted.mean(datos$Simpson.val.ind, datos$Peso.estad, na.rm=T)
weighted.mean(datos$Shannon.val.ind, datos$Peso.estad, na.rm=T)
weighted.mean(datos$Gini.val.ind,datos$Peso.estad, na.rm=T)
weighted.mean(datos$BuzasGibson.val.ind,datos$Peso.estad, na.rm=T)

write.csv(datos,'Data calculados.csv', row.names=FALSE)  # Guardar los resultados

### Caclular los índices: valores promedios nacionales ###

#Calcular los valores promedios nacionales
ValorMedio = NULL
for (a in 1:length(col_val.animales)){
  ValorMedio[a] <- weighted.mean(ValorPorAnimal[[a]][ValorPorAnimal[[a]]!=Inf], datos$Peso.estad[ValorPorAnimal[[a]]!=Inf], na.rm=T)
  if(is.na(ValorMedio[a])){ValorMedio[a] <- 0}
}

#Calcular los Índices
for(a in 1:nrow(datos)){    #para cada hogar
  animaleshogar=NULL
  for(b in 1:length(nombres)){    #para cada tipo de animal
    animaleshogar[b] <- datos[[nombres[b]]][a]*ValorMedio[b]
  }
  if (sum(animaleshogar) > 0) {
    datos$Simpson.val.med[a] <- Simpson(animaleshogar)
    datos$Shannon.val.med[a] <- Shannon(animaleshogar)
    datos$Gini.val.med[a] <- Gini(animaleshogar)
    datos$BuzasGibson.val.med[a] <- BuzasGibson(animaleshogar)
    # Notar: Para Margalef no se calculan modificaciones
  }
}

# Verificar resultados
weighted.mean(datos$Simpson.val.med, datos$Peso.estad, na.rm = T)
weighted.mean(datos$Shannon.val.med, datos$Peso.estad, na.rm = T)
weighted.mean(datos$Gini.val.med, datos$Peso.estad, na.rm = T)
weighted.mean(datos$BuzasGibson.val.med, datos$Peso.estad, na.rm = T)

write.csv(datos,'Datos calculados.csv', row.names=FALSE)  # guardar los resueltos

### Caclular los índices: valores regionales ###

departamentos <- levels(as.factor(datos.hogares[[col_dept]]))  # salvar los nombres de los departamientos

# Calcular el promedio departamental del valor de cada tipo de animal
ValorDept = list(NULL)
for(b in 1:length(departamentos)){
  ValorPorAnimalDept=list(NULL)
  pesos.estad = list(NULL)
  for (a in 1:length(col_val.animales)){
    ValorPorAnimalDept[[a]] <- datos[[col_val.animales[a]]][datos$Dept==departamentos[b]]/datos[[nombres[a]]][datos$Dept==departamentos[b]]  
    pesos.estad <- datos$Peso.estad[datos$Dept==departamentos[b]]
  }
  ValorDeptAnimal = NULL
  for (a in 1:length(col_val.animales)){
    ValorDeptAnimal[a] <- weighted.mean(ValorPorAnimalDept[[a]][ValorPorAnimalDept[[a]]!=Inf], pesos.estad[ValorPorAnimalDept[[a]]!=Inf], na.rm=T)
    if(is.na(ValorDeptAnimal[a])){ValorDeptAnimal[a] <- 0}
  }
  ValorDept[[b]] <- ValorDeptAnimal
}
names(ValorDept) <- departamentos

# Visualisar resultados
ValorAnimalPorDept = list(NULL)
for(a in 1:length(ValorDept[[1]])){
  temp = NULL
  for (b in 1:length(departamentos)){
    temp[b]= ValorDept[[b]][a]
  }
  ValorAnimalPorDept[[a]] <- temp
}

jpeg(filename='Gráficos/ValorAnimalporDept.jpg')
plot.new()
boxplot(ValorAnimalPorDept,labels=FALSE, main = "Valor promedio (departamiento) de cada animal", ylab= "Valor ($)")
axis(1, at=1:length(nombres), labels=nombres, las=2)
dev.off()

# Calcular los índices con peso por valor departamental

for(a in 1:nrow(datos)){  #para cada hogar
  animaleshogar=NULL
  for(b in 1:length(nombres)){  # para cada tipo de animal
    animaleshogar[b] <- datos[[nombres[b]]][a]*ValorDept[[datos$Dept[a]]][b]
  }
  if (sum(animaleshogar) > 0) {
    datos$Simpson.val.dept[a] <- Simpson(animaleshogar)
    datos$Shannon.val.dept[a] <- Shannon(animaleshogar)
    datos$Gini.val.dept[a] <- Gini(animaleshogar)
    datos$BuzasGibson.val.dept[a] <- BuzasGibson(animaleshogar)
    # Notar: Para Margalef no se calculan modificaciones
  }
}

# Verificar los resultados
weighted.mean(datos$Simpson.val.dept, datos$Peso.estad, na.rm = T)
weighted.mean(datos$Shannon.val.dept, datos$Peso.estad, na.rm = T)
weighted.mean(datos$Gini.val.dept, datos$Peso.estad, na.rm = T)
weighted.mean(datos$BuzasGibson.val.dept, datos$Peso.estad, na.rm = T)

write.csv(datos,'Datos calculados.csv', row.names=FALSE)  # guardar los resultados

### Calcular los Índices: unidades ganaderas ###

# Calcular los índices con peso UG

for(a in 1:nrow(datos)){  # para cada hogar
  animaleshogar=NULL
  for(b in 1:length(nombres)){  # para cada tipo de animal
    animaleshogar[b] <- datos[[nombres[b]]][a]*ValorUG[b]
  }
  datos$Simpson.UG[a] <- Simpson(animaleshogar)
  datos$Shannon.UG[a] <- Shannon(animaleshogar)
  datos$Gini.UG[a] <- Gini(animaleshogar)
  datos$BuzasGibson.UG[a] <- BuzasGibson(animaleshogar)
  # Notar: Para Margalef no se calculan modificaciones
}

# Verificar los resultados
weighted.mean(datos$Simpson.UG, datos$Peso.estad)
weighted.mean(datos$Shannon.UG, datos$Peso.estad)
weighted.mean(datos$Gini.UG, datos$Peso.estad)
weighted.mean(datos$BuzasGibson.UG, datos$Peso.estad)

write.csv(datos,'Datos calculados.csv', row.names=FALSE)  # guardar los resultados


#### 4. Escoger los índices de diversidad únicos ####

Índices <- c("Simpson","Shannon","Gini","Margalef","BuzasGibson")
Modificaciones <- c("",".val.ind",".val.med",".val.dept",".UG")

# Hacer una base de datos con únicamente los índices
datos.Índices <- NULL
for(a in 1:length(Índices)){
  for(b in 1:length(Modificaciones)){
    índice <- paste(Índices[a], Modificaciones[b], sep = "")
    if (índice %in% names(datos)) {
      datos.Índices[[length(datos.Índices) + 1]] <- datos[[índice]]
      names(datos.Índices)[length(datos.Índices)] <- índice
    }
  }
}

datos.Índices <- data.frame(datos.Índices)

# Ver las correlaciones entre distintos índices. (La generación del gráfico sí requiere tiempo.)
plot(datos.Índices)

# Generar las correlaciones entre índices con modelos lineares y cuadrados (.modif)
corr.índ.matrix <- matrix(nrow = ncol(datos.Índices), ncol = ncol(datos.Índices))
test.resid <- NULL; test.modif.resid <- NULL; test.rsq <- NULL; test.modif.rsq <- NULL;

for (a in 1:ncol(datos.Índices)){
  for (b in 1:ncol(datos.Índices)){
    test <- lm(datos.Índices[[a]]~datos.Índices[[b]], weights=datos$Peso.estad)
    modif <- datos.Índices[[b]]^2
    test.modif <- lm(datos.Índices[[a]] ~ modif, weights=datos$Peso.estad)
    # Guardar la mejor correlación
    corr.índ.matrix[a,b] <- max(summary(test)$r.squared, summary(test.modif)$r.squared)
    test.rsq[ncol(datos.Índices)*(a-1)+b] <- summary(test)$r.squared
    test.modif.rsq[ncol(datos.Índices)*(a-1)+b] <- summary(test.modif)$r.squared
    test.resid[[ncol(datos.Índices)*(a-1)+b]] <- summary(test)$residuals
    test.modif.resid[[ncol(datos.Índices)*(a-1)+b]] <- summary(test.modif)$residuals
  }
}

# Guardar las correlaciones
write.csv(corr.índ.matrix, file = "R2-Índices.csv", row.names=FALSE)

# Generar un gráfico de los residuales de los modelos lineares y cuadrados generados arriba
jpeg(filename="Gráficos/Residmejor.jpg", width=100*ncol(datos.Índices), height=100*ncol(datos.Índices))
par(mfrow=c(ncol(datos.Índices),ncol(datos.Índices)))
for (a in 1:ncol(datos.Índices)){
  for (b in 1:ncol(datos.Índices)){
    if (test.rsq[ncol(datos.Índices)*(a-1)+b] > test.modif.rsq[ncol(datos.Índices)*(a-1)+b]){
      plot(test.resid[[ncol(datos.Índices)*(a-1)+b]],
           datos.Índices[[b]][1:length(test.resid[[ncol(datos.Índices)*(a-1)+b]])],
           xlab=names(datos.Índices[b]), ylab="Residuals",
           main='')
    }else{
      plot(summary(test)$residuals,
           datos.Índices[[b]][1:length(summary(test.modif)$residuals)],
           xlab=paste(names(datos.Índices[b]),"^2"), ylab="Residuals",
           main='')
    }
  }
}
dev.off()


#### 5. Correr los análises estadísticos ####

# Primero, escoger los índices a incluir en el análisis estadístico. Yo quité índices con 
# correlaciones de 0.90 con otro índice. Para decidir cuál de los dos índices guardar, utilice la
# prioridad Shannon > Simpson, Gini > BuzasGibson, sin modificaciones > modificaciones, 
# val.med > val.ind

# Lista de índices escogidos (cambiar según sus resultados)
Índicesfinales <- c("Shannon","Shannon.val.med","Shannon.UG","Gini","Gini.val.ind","Gini.val.med", "Gini.UG", "Margalef")

datos.Índicesfin <- NULL
for (a in 1:length(Índicesfinales)){
  datos.Índicesfin[[a]] <- datos[[Índicesfinales[a]]]
  names(datos.Índicesfin)[a] <- Índicesfinales[a]
}
datos.Índicesfin <- data.frame(datos.Índicesfin)

# Visualizar las correlaciones entre los índices escogidos
plot(datos.Índicesfin)

install.packages("ordinal")  # Instalar si necesario
library("ordinal")


datos$SegAli <- as.factor(datos$SegAli)  # Cambio de formato necesario para los análisis que siguen

write.csv(datos,'Datos calculados.csv', row.names=FALSE)  # Guardar los datos

AIC <- NULL; logLik <- NULL  # Medidas de modelos estadísticos
modelos <- NULL  # Lista para guardar los resultados de los análisis
for(a in 1:length(Índicesfinales)){
  logit <- clm(SegAli~log_Ingresoporcápita*log_UniGan*datos[[Índicesfinales[a]]], weights=Peso.estad, data=datos)
  AIC[a] <- as.numeric(levels(logit$info$AIC)[as.integer(logit$info$AIC)])
  logLik[a] <- logit$logLik
  assign(paste("logit_",Índicesfinales[a],sep=""),logit)
  modelos[[a]] <- logit
}

# Un logLik negativo más pequeño es mejor
-logit_Shannon$logLik
-logit_Shannon.val.med$logLik
-logit_Shannon.UG$logLik
-logit_Margalef$logLik
-logit_Gini$logLik
-logit_Gini.val.ind$logLik
-logit_Gini.val.med$logLik
-logit_Gini.UG$logLik

# AIC pequeño es mejor
AIC

# Ver los resultados de los modelos
summary(logit_Shannon)
summary(logit_Shannon.val.med)
summary(logit_Shannon.UG)
summary(logit_Margalef)
summary(logit_Gini)
summary(logit_Gini.val.ind)
summary(logit_Gini.val.med)
summary(logit_Gini.UG)

# Si habían interacciones no significativas, quitarlas abajo y recorrer el análisis:

# for(a in 1:length(Índicesfinales)){
#   logit <- clm(SegAli~log_Ingresoporcápita+log_UniGan+datos[[Índicesfinales[a]]]
#                +log_UniGan*datos[[Índicesfinales[a]]]
#                , weights=Peso.estad, data=datos)
#   AIC[a] <- as.numeric(levels(logit$info$AIC)[as.integer(logit$info$AIC)])
#   logLik[a] <- logit$logLik
#   assign(paste("logit_",Índicesfinales[a],sep=""),logit)
#   modelos[a] <- logit
# }

# summary(logit_Shannon)
# summary(logit_Shannon.val.ind)
# summary(logit_Shannon.UG)
# summary(logit_Margalef)
# summary(logit_Gini)
# summary(logit_Gini.val.ind)
# summary(logit_Gini.val.med)
# summary(logit_Gini.UG)

# escoger un índice que rindió mejor que los otros
índ.escogido = datos$Shannon

#### 6. Analizar las interacciones ####

# Esta función permite de visualizar las interacciones entre dos variables
Inter <- function(x1,x2,y){
  # quant1 <- quantile(x1,na.rm=T) ; quant2 <- quantile(x2,na.rm=T)
  # O...
  mín = min(x1, na.rm=2); máx = max(x1, na.rm=2); med = mín+máx/2
  quant1 <- c(mín, (mín + med)/2, med, (med + máx)/2, máx)
  mín = min(x2, na.rm=2); máx = max(x2, na.rm=2); med = mín+máx/2
  quant2 <- c(mín, (mín + med)/2, med, (med + máx)/2, máx)
  print(quant1)
  print(quant2)
  
  combin <- data.frame(x1,x2,y)
  
  interpuntos <- matrix(nrow=4,ncol=4)
  for (a in 1:4){
    for(b in 1:4){
      interpuntos[a,b] <- mean(combin[combin[,1] >= quant1[a] &
                                        combin[,1] < quant1[a+1] &
                                        combin[,2] >= quant2[b] &
                                        combin[,2] < quant2[b+1]
                                      ,3],na.rm=T)
    }
  }
  
  seq = (quant2[2:5]+quant2[1:4])/2
  plot <- plot(interpuntos[1,]~seq, type="b", 
               ylim=c(min(interpuntos, na.rm=T),max(interpuntos, na.rm=T)),
               xlab="var2",ylab="Dependiente")
  points(interpuntos[2,]~seq, type="b",pch=2)
  points(interpuntos[3,]~seq, type="b",pch=3)
  points(interpuntos[4,]~seq, type="b",pch=4)
  legend("topright",legend=c("Q1","Q2","Q3","Q4"),pch=c(1,2,3,4),bty='n')
  
  return(c(interpuntos,plot))
}

# Verificar interactiones número de animales-diversidad
Inter(datos$log_UniGan, índ.escogido, as.numeric(datos$SegAli))
Inter(índ.escogido, datos$log_UniGan, as.numeric(datos$SegAli))

# Interacción entre ingresos y diversidad
Inter(índ.escogido, datos$log_Ingresoporcápita, as.numeric(datos$SegAli))
Inter(datos$log_Ingresoporcápita, índ.escogido, as.numeric(datos$SegAli))

# Interacción entre ingresos y número de animales
Inter(datos$log_UniGan, datos$log_Ingresoporcápita, as.numeric(datos$SegAli))
Inter(datos$log_Ingresoporcápita, datos$log_UniGan, as.numeric(datos$SegAli))


#### 7. Análisis de agrupamiento ####

# Cargar datos de producción de animales por los agricultores
datos.func = read.csv('F:/Julien/PhD/Iximulew/MDS SAN/Datos calibración MDS/Datos ENCOVI/Limp_Actividades pecuarias.csv', header=1)
datos.func.prod = read.csv('F:/Julien/PhD/Iximulew/MDS SAN/Datos calibración MDS/Datos ENCOVI/Limp_t27_cap14_e_final.csv', header = 1)

# Base de datos para las características funcionales de los animales
carac.func = data.frame(nombres)

# Aquí calculamos distintas características funcionales de los animales. Estas incluyen:
# 1. Porcentaje de animales vendidos recientemente, como índice del valor como seguro económico del animal
# (los que casi no se venden serían guardados para emergencias)
# 2. Ventas y consumo de productos 'de una vez' (p. ej. carne)
# 3. Ventas y consumo de productos 'conutinúos' (lana, leche, huevos)
# 4. Uso del animal como fuerza laboral
# 5. Hembra o varón (si puede o no reproducirse)
# 6. Unidades ganaderas del animal

# Porcentaje de animales vendidos recientemente
carac.func['ventas_animal'] = NA
# Porcentaje de productores con este animal que vendieron productos animales de tipo carne
carac.func['ventas_prod_perm'] = NA
# Porcentaje de productores con este animal que vendieron productos de producción contínua 
# (p.ej., leche, huevos), 
carac.func['ventas_prod_cont'] = NA
# Porcentaje de productores con este animal que consumieron productos animales de tipo carne de producción propia
carac.func['consumo_prod_perm'] = NA
# Porcentaje de productores con este animal que consumieron productos de producción contínua (p.ej., leche, huevos) de producción propia
carac.func['consumo_prod_cont'] = NA
# Si el animal se puede (1) o no (0) utilizar para trabajo
carac.func['trabajo'] = NA
# Si el animal es una hembra (0), varón (1), o no especificado (0.5)
carac.func['hembra'] = NA
# La cantidad de unidades ganaderas del animal
carac.func['UG'] = NA


# Calcular el % de animales de cada tipo que fueron vendidos el año pasado
for (i in 1:length(nombres)) {
  sub.datos = datos.func[datos.func$P14D02A==i & 
                           !is.na(datos.func$P14D03) & datos.func$P14D03 > 0 &
                           (datos.func$FORMULARIO %in% datos$FORMULARIO),]
  
  tenían = sum(sub.datos$P14D03 + sub.datos$P14D05A, na.rm=T)  # Los que tienen y los que vendieron
  vendieron = sum(sub.datos$P14D05A, na.rm=TRUE)
  if (is.na(vendieron)) {
    vendieron <- 0
  }
  carac.func$ventas_animal[i] = vendieron / tenían
}


# Si la base de datos contiene datos de ventas y consumo de productos de fuente animal,
# mejor calcular estos valores con los datos reales que tienen
carac.func['ventas_prod_perm'] = c(1,1,1,0,1,1,1,0,1,1)
carac.func['ventas_prod_cont'] = c(1/3,0,1,1,1,0,0,0,0,0)
carac.func['consumo_prod_perm'] = NA
carac.func['consumo_prod_cont'] = NA

# Cambiar según sus animales
carac.func['trabajo'] = c(1/3,0,0,0,0,0,0,1,0,0)
carac.func['hembra'] = 0.5

carac.func['UG'] = ValorUG

# Hacer los agrupamientos
rownames(carac.func) <- carac.func[,1]
carac.func <- carac.func[,-1]
normalizados = scale(carac.func)

d <- dist(as.matrix(normalizados))  # matriz de distancia
hc <- hclust(d)  # agrupammiento hierárchico 
plot(hc)  # Verificar el árbol de agrupamiento

# Escoger el número de grupos que quiere usted
n_grupos = 4

# Dibujar los agrupamientos en el árbol
rect.hclust(hc, k=n_grupos, border="red")

# Añadir el grupo a la base de datos de características de cada tipo de animal
carac.func$grupo <- as.factor(cutree(hc, n_grupos))


#### 8. Re-correr los modelos con diversidad funcional ####

# Crear las listas con los nombres de los animales en cada grupo
animales_grupos <- list(NULL)
for (i in 1:n_grupos) {
  animales_grupos[[i]] <- rownames(carac.func)[carac.func$grupo == i]
  datos[paste('DiversidadGrupo', i, sep = '')] <- NA
}

for (i in 1:n_grupos) {
  datos[paste('AnimalesGrupo', i, sep = '')] <- NA
  for (j in 1:length(animales_grupos[[i]])) {
    datos[paste('AnimalesGrupo', i, sep = '')] <- 
      rowSums(data.frame(datos[as.character(animales_grupos[[i]][j])], 
                        datos[paste('AnimalesGrupo', i, sep = '')]),
             na.rm = T)
  }
  datos[paste('Fracción', i, sep = '')] <- 
    datos[paste('AnimalesGrupo', i, sep = '')] / datos$Totalanimales
}

datos['Diversidad_interGrupos'] <- NA
for (i in 1:nrow(datos)) {
  temp2 <- NULL
  for (j in 1:n_grupos) {
    temp <- NULL
    temp1 <- NULL
    for (k in 1:length(animales_grupos[[j]])) {
      temp <- c(temp, datos[[animales_grupos[[j]][k]]][i])
      temp1 <- sum(temp1, datos[[animales_grupos[[j]][k]]][i])
    }
    datos[[paste('DiversidadGrupo', j, sep = '')]][i] <- 
      max(Shannon(temp),0)
    temp2 <- c(temp2, temp1)
  }
  datos[['Diversidad_interGrupos']][i] <- max(0, Shannon(c(temp2)))
}

hist(datos$Diversidad_interGrupos)
hist(datos$DiversidadGrupo2)
hist(datos$DiversidadGrupo3)

### Correr los análisis con diversidad inter y intra grupo funcional
modelo.intergrupo <- clm(SegAli~log_Ingresoporcápita*log_UniGan*Diversidad_interGrupos*
                           índ.escogido, 
                         weights=Peso.estad, data=datos)
summary(modelo.intergrupo)

# Si hay interacciones no significativas, quitarlas

### Diversidad intragrupo:

# Primero, añadamos la diversidad intragrupo.
# No ponemos la diversidad de grupos 1 y 4 porque estos sólo contienen un tipo de animal
# (su divesidad siempre será de 0)
modelo.diversIntragrupo <- clm(SegAli~log_Ingresoporcápita*log_UniGan*índ.escogido+
                                 DiversidadGrupo2+DiversidadGrupo3,
                               weights=Peso.estad, data=datos)
summary(modelo.diversIntragrupo)

# Ahora, con la fracción de animales en cada grupo
modelo.fracIntragrupo <- clm(SegAli~log_Ingresoporcápita*log_UniGan*índ.escogido+
                               Fracción1+Fracción2+Fracción3+Fracción4,
                             weights=Peso.estad, data=datos)
summary(modelo.fracIntragrupo)

# Ahora, ¡con los dos!
modelo.DFIntragrupo <- clm(SegAli~log_Ingresoporcápita*log_UniGan*índ.escogido+
                               Fracción1+Fracción2+Fracción3+Fracción4+
                               DiversidadGrupo2 + DiversidadGrupo3,
                             weights=Peso.estad, data=datos)
summary(modelo.DFIntragrupo)

modelo.DFIntraIntergrupo <- clm(SegAli~log_Ingresoporcápita*log_UniGan*índ.escogido+
                                  Fracción1+Fracción2+Fracción3+Fracción4+
                                  DiversidadGrupo2 + DiversidadGrupo3 +
                                  Diversidad_interGrupos + 
                                  Diversidad_interGrupos*índ.escogido,
                                weights=Peso.estad, data=datos)
summary(modelo.DFIntraIntergrupo)
