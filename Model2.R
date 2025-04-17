load("DataMercadolibre.RData")
mercadolibredata <- data.frame(dataset[2:11])

#mercadolibredata$vectorprice <- as.numeric(mercadolibredata$vectorprice)
library(tidyverse)
usados <- mercadolibredata %>% filter(str_detect(vectorsold, 'Usado'))
nuevos <- mercadolibredata %>% filter(str_detect(vectorsold, 'vendido'))
# Dado que en los usados no tenemos registros de los vendidos
# La base de datos de usados no sirve, por lo que debemos concentrarnos en
# la base de datos de los nuevos.

# Vamos a limpiar las variables removiendo caracteres no numéricos
nuevos$vectorsold <- as.numeric(gsub("[^0-9]", "", nuevos$vectorsold))
nuevos$goodfeedback <- as.numeric(gsub("[^0-9]", "", nuevos$goodfeedback))
nuevos$neutralfeedback <- as.numeric(gsub("[^0-9]", "", nuevos$neutralfeedback))
nuevos$badfeedback <- as.numeric(gsub("[^0-9]", "", nuevos$badfeedback))
nuevos$price <- as.numeric(gsub("\\.", "", nuevos$vectorprice))

newdata <- nuevos[c(-1, -2)]
names(newdata)[1]<-"SI"
names(newdata)[2]<-"Item"
names(newdata)[3]<-"Country"
names(newdata)[4]<-"PI"
names(newdata)[5]<-"PF"
names(newdata)[6]<-"NeuF"
names(newdata)[7]<-"NF"

newdata$Country <- recode(newdata$Country, 'Republica Dominicana'= 'Dominican Rep')
newdata <- mutate(newdata, experienceunit = ifelse(grepl("año", experience), "Years",
                                                   ifelse(grepl("mes", experience), "Month", "Other")))

newdata$experience <- as.numeric(gsub("[^0-9]", "", nuevos$experience))

newdata <- mutate(newdata, history = ifelse(experienceunit == "Years", experience * 12, experience * 1))
newdata <- mutate(newdata, trust = PF / (PF + NeuF + NF))
newdata$trustsq <- newdata$trust^2

rm(list=setdiff(ls(), "newdata"))
