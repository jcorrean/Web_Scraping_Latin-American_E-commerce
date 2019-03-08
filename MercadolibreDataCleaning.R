load("~/Documents/DataMercadolibre.RData")
mercadolibredata <- data.frame(df[2:9])
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

newdata <- nuevos[c(-1,-3,-4)]
names(newdata)[1]<-"SP"
names(newdata)[2]<-"QTS"
names(newdata)[3]<-"PF"
names(newdata)[4]<-"NeuF"
names(newdata)[5]<-"NF"

library(psych)
pairs.panels(newdata, smooth = TRUE, scale = FALSE, density = TRUE, hist.col = "green")


names(nuevos)[1]<-"SP"
names(nuevos)[2]<-"QTS"
names(nuevos)[3]<-"PF"
names(nuevos)[4]<-"NeuF"
names(nuevos)[5]<-"NF"





# Ahora vamos a segmentar los datos por productos
iphone <- nuevos %>% filter(vectorproduct == "Iphone 7" & vectorsold >= 0)
ps4 <- nuevos %>% filter(vectorproduct == "PS4")
Converse <- nuevos %>% filter(vectorproduct == "Converse")
Bible <- nuevos %>% filter(vectorproduct == "Biblia")

library(psych)
pairs.panels()




write.csv(iphone, file = "iphone.csv")

fit <- lm(iphone$vectorsold ~ iphone$price + iphone$vectorquestions + iphone$goodfeedback + iphone$neutralfeedback + iphone$badfeedback)
reg <- lm.beta(fit)
summary(reg)
