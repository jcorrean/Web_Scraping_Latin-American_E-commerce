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

library(MASS) #  Keep it here in case you want to do overdispersion checks later

# Model 1 (Table 2 - Model 1)
M1 <- glm(SI ~ trust + trustsq,
              data = newdata, family = "poisson")
summary(M1)

M2 <- glm(SI ~ trust + trustsq + history,
          data = newdata, family = "poisson")
summary(M2)

M3 <- glm(SI ~ trust + trustsq + history + log(price),
          data = newdata, family = "poisson")
summary(M3)

M4 <- glm(SI ~ trust + history + log(price) + PI,
          data = newdata, family = "poisson")
summary(M4)

M5 <- glm(SI ~ trust + trustsq + history + log(price) + PI,
          data = newdata, family = "poisson")
summary(M5)

M6 <- glm(SI ~ 0 + trust + trustsq + history + log(price) + PI, #  Notice the 0 +
          data = newdata, family = "poisson")
summary(M6)

library(margins)
marginal_effects_M6 <- margins(M6)
summary(marginal_effects_M6)

# Si deseas una tabla formateada de los efectos marginales
library(broom)
tidy_margins_M6 <- tidy(marginal_effects_M6)
print(tidy_margins_M6)


library(stargazer)
stargazer(M1, M2, M3, M4, M5, M6,
          title = "Table 2: Regression Results",
          dep.var.labels = "Sales (SI)",
          covariate.labels = c("Trust", "Trustsq", "history", "log(Price)", "PI"),
          #omit.stat = c("all.chi2", "aic", "lr"), #  Optional: Omit statistics you don't want
          type = "text")



M2 <- glm(SI ~ trust + I(trust^2) + history,
          data = newdata, family = "poisson")
summary(M2)

M3 <- glm(SI ~ trust + I(trust^2) + history + log(price),
          data = newdata, family = "poisson")
summary(M3)

M4 <- glm(SI ~ trust + history + log(price) + PI,
          data = newdata, family = "poisson")
summary(M4)

M5 <- glm(SI ~ trust + I(trust^2) + history + log(price) + PI,
          data = newdata, family = "poisson")
summary(M5)

M6A <- glm(SI ~ trust + I(trust^2) + history + log(price) + PI - 1, # Explicitly removing the intercept
          data = newdata, family = "poisson")
summary(M6A)
summary(M6)
