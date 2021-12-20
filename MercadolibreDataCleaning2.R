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

rm(list=setdiff(ls(), "newdata"))

library(ggridges)
Fig1 <- ggplot(newdata, aes(x=newdata$trust, y=as.factor(newdata$Country))) + geom_density_ridges(fill="green", alpha = 0.4) + ylab("Country") + xlab("Trust") + theme(axis.text.y = element_text(family="Arial", face="bold", colour="black", size=rel(3))) + theme(axis.text.x = element_text(family="Arial", face="bold", colour="black", size=rel(2)))

library(psych)
countries <- describeBy(newdata$trust, group = newdata$Country, mat = TRUE)

library(ggrepel)
row.names(countries) <- countries$group1
Fig2 <- ggplot(countries, aes(x=mean, y=sd)) + geom_point() + geom_smooth() + geom_text_repel(label = rownames(countries)) + xlab("Average Trust Score") + ylab("Standard Deviation of Trust Score")

library(ggpubr)

ggarrange(Fig1, Fig2, ncol = 2, labels = c("A", "B"))
cor.test(countries$mean, countries$sd, use = "everything", method = "spearman")


# Now, let's split our main dataset into four datasets 
# one for each product.
#iphone <- newdata %>% filter(Item == "Iphone 7" & SI >= 0)
#ps4 <- newdata %>% filter(Item == "PS4" & SI >= 0)
#Converse <- newdata %>% filter(Item == "Converse" & SI >= 0)
#Bible <- newdata %>% filter(Item == "Biblia" & SI >= 0)

# Let's see which countries have a minimum
# number of 50 valid observations to be
# included in the set of countries

#library(psych)
#describeBy(iphone$PI, group = iphone$Country, mat = TRUE)
# Coutries with a sufficient number of valid records for Iphone7 are
# Argentina (213), Brasil (312), Chile (50), Colombia (72), Ecuador (277), 
# Dominican Republic (97), Mexico (180), Panama (58)
# Peru (112), and Uruguay (76), Venezuela (98)

#describeBy(ps4$PI, group = ps4$Country, mat = TRUE)
# Countries with a sufficient number of valid records for PS4 are
# Argentina (539), Brasil (613), Chile (453), Colombia (189), 
# Costa Rica (119), Ecuador (318), Dominican Republic (66),
# Mexico (250), Peru (299), Uruguay (146), and Venezuela (201) 

#describeBy(Converse$PI, group = Converse$Country, mat = TRUE)
# Countries with a sufficient number of valid records for Converse
# are Argentina (109), Brasil (263), Chile (95), Colombia (327), 
# Costa Rica (72), Ecuador (73), Mexico (744), and Dominican Rep (59) 
# Peru (88), Uruguay (103), and Venezuela (169)

#describeBy(Bible$PI, group = Bible$Country, mat = TRUE)
# Countries with a sufficient number of valid records for The Bible
# are Argentina (263), Chile (146), Colombia (185), 
# Ecuador (56), Dominican Republic (59)
# and Mexico (1355), Uruguay (77), Venezuela (340)

# Summing up, a dataframe with vaid records is the one that contains
# Argentina, Chile, Colombia, Ecuador, Mexico, Uruguay, Venezuela
# Dominican Republic, as these countries showed valid records 
# for all four products.

ValidData <- newdata %>% filter(Country == "Argentina" | 
                                Country == "Chile" | 
                                Country == "Colombia" | 
                                Country == "Ecuador" | 
                                Country == "Mexico" |
                                Country == "Uruguay" | 
                                Country == "Venezuela" |
                                Country == "Brasil"
                                )

ValidData <- ValidData %>% filter(trust > 0.85)

ValidData <- mutate(ValidData, country = ifelse(grepl("Argentina", Country), "ARG",
                                         ifelse(grepl("Brasil", Country), "BRA",
                                         ifelse(grepl("Chile", Country), "CHI",
                                         ifelse(grepl("Colombia", Country), "COL",
                                         ifelse(grepl("Ecuador", Country), "ECU",
                                         ifelse(grepl("Mexico", Country), "MEX",
                                         ifelse(grepl("Uruguay", Country), "URU",
                                         "VEN"))))))))

##################################################
########### Bivariate Analysis  ##################
##################################################

# Let's begin with trust and its relationship with Purchase Intention (PI)
library(DescTools)
trust_PI <- ValidData %>% 
  split(as.character(.$country)) %>% 
  map(function(ValidData) SpearmanRho(ValidData$trust, ValidData$PI, use = "complete.obs", conf.level = 0.95))

trust_PI <- data.frame(matrix(unlist(trust_PI), nrow = length(trust_PI), byrow = TRUE))
row.names(trust_PI) <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")
names(trust_PI)[1] <- "Rho"
names(trust_PI)[2] <- "Lower"
names(trust_PI)[3] <- "Upper"
trust_PI$Country <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")

Fig3 <- ggplot(trust_PI, aes(x= Country, y=Rho)) + geom_bar(stat="identity", fill="lightgreen", alpha=0.7) +
  geom_errorbar(aes(x=Country, ymin=Lower, ymax=Upper), width=0.3, colour="black", alpha=0.9, size=1) + xlab("") + ylab("Trust-PI Correlation") +
  theme(axis.text.x = element_text(angle = 90, family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=20))

Fig4 <- ggplot(ValidData, aes(x=trust, y=PI, color=country)) +
  geom_point(size = 0.5, shape = 20) + 
  geom_smooth(method=loess, se=FALSE, fullrange=TRUE) + 
  theme_test() + scale_color_brewer(palette="Set1") +
  theme(legend.text = element_text(color = "black", size = 12), legend.position = "none", legend.title = element_blank(), legend.direction = "horizontal") +
  theme(axis.text.x = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.x=element_text(face="bold", colour="black", size=30)) + 
  theme(axis.title.y=element_text(face="bold", colour="black", size=30))

## Now let's explore the relationship between trust and sales (SI)
trust_sales <- ValidData %>% 
  split(as.character(.$country)) %>% 
  map(function(ValidData) SpearmanRho(ValidData$trust, log(ValidData$SI), use = "complete.obs", conf.level = 0.95))

trust_sales <- data.frame(matrix(unlist(trust_sales), nrow = length(trust_sales), byrow = TRUE))
row.names(trust_sales) <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")
names(trust_sales)[1] <- "Rho"
names(trust_sales)[2] <- "Lower"
names(trust_sales)[3] <- "Upper"
trust_sales$Country <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")

Fig5 <- ggplot(trust_sales, aes(x= Country, y=Rho)) + geom_bar(stat="identity", fill="lightgreen", alpha=0.7) +
  geom_errorbar(aes(x=Country, ymin=Lower, ymax=Upper), width=0.3, colour="black", alpha=0.9, size=1) + xlab("") + ylab("Trust-Sales Correlation") +
  theme(axis.text.x = element_text(angle = 90, family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=20))

Fig6 <- ggplot(ValidData, aes(x=trust, y=log(SI), color=country)) +
  geom_point(size = 0.5, shape = 20) + 
  geom_smooth(method=loess, se=FALSE, fullrange=TRUE) + 
  theme_test() + scale_color_brewer(palette="Set1") +
  theme(legend.text = element_text(color = "black", size = 12), legend.position = "none", legend.title = element_blank(), legend.direction = "horizontal") +
  theme(axis.text.x = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4)))+
  theme(axis.title.x=element_text(face="bold", colour="black", size=30)) + 
  theme(axis.title.y=element_text(face="bold", colour="black", size=30))

# trust - price
trust_price <- ValidData %>% 
  split(as.character(.$country)) %>% 
  map(function(ValidData) SpearmanRho(ValidData$trust, log(ValidData$price), use = "complete.obs", conf.level = 0.95))

trust_price <- data.frame(matrix(unlist(trust_price), nrow = length(trust_price), byrow = TRUE))
row.names(trust_price) <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")
names(trust_price)[1] <- "Rho"
names(trust_price)[2] <- "Lower"
names(trust_price)[3] <- "Upper"
trust_price$Country <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")

Fig7 <- ggplot(trust_price, aes(x= Country, y=Rho)) + geom_bar(stat="identity", fill="lightgreen", alpha=0.7) +
  geom_errorbar(aes(x=Country, ymin=Lower, ymax=Upper), width=0.3, colour="black", alpha=0.9, size=1) + xlab("") + ylab("Trust-Log(Price) Correlation") +
  theme(axis.text.x = element_text(angle = 90, family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=20))

Fig8 <- ggplot(ValidData, aes(x=trust, y=log(price), color=country)) +
  geom_point(size = 0.5, shape = 20) + 
  geom_smooth(method=loess, se=FALSE, fullrange=TRUE) + 
  theme_test() + scale_color_brewer(palette="Set1") +
  theme(legend.text = element_text(color = "black", size = 12), legend.position = "none", legend.title = element_blank(), legend.direction = "horizontal") +
  theme(axis.text.x = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.x=element_text(face="bold", colour="black", size=30)) + 
  theme(axis.title.y=element_text(face="bold", colour="black", size=30))

# Trust-History
trust_history <- ValidData %>% 
  split(as.character(.$country)) %>% 
  map(function(ValidData) SpearmanRho(ValidData$trust, ValidData$history, use = "complete.obs", conf.level = 0.95))

trust_history <- data.frame(matrix(unlist(trust_history), nrow = length(trust_history), byrow = TRUE))
row.names(trust_history) <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")
names(trust_history)[1] <- "Rho"
names(trust_history)[2] <- "Lower"
names(trust_history)[3] <- "Upper"
trust_history$Country <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")

Fig9 <- ggplot(trust_history, aes(x= Country, y=Rho)) + geom_bar(stat="identity", fill="lightgreen", alpha=0.7) +
  geom_errorbar(aes(x=Country, ymin=Lower, ymax=Upper), width=0.3, colour="black", alpha=0.9, size=1) + xlab("") + ylab("Trust-History Correlation") +
  theme(axis.text.x = element_text(angle = 90, family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=20))

Fig10 <- ggplot(ValidData, aes(x=trust, y=history, color=country)) +
  geom_point(size = 0.5, shape = 20) + 
  geom_smooth(method=loess, se=FALSE, fullrange=TRUE) + 
  theme_test() + scale_color_brewer(palette="Set1") +
  theme(legend.text = element_text(color = "black", size = 12), legend.position = "none", legend.title = element_blank(), legend.direction = "horizontal") +
  theme(axis.text.x = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.x=element_text(face="bold", colour="black", size=30)) + 
  theme(axis.title.y=element_text(face="bold", colour="black", size=30))


# PI-SI
PI_SI <- ValidData %>% 
  split(as.character(.$country)) %>% 
  map(function(ValidData) SpearmanRho(ValidData$PI, log(ValidData$SI), use = "complete.obs", conf.level = 0.95))

PI_SI <- data.frame(matrix(unlist(PI_SI), nrow = length(PI_SI), byrow = TRUE))
row.names(PI_SI) <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")
names(PI_SI)[1] <- "Rho"
names(PI_SI)[2] <- "Lower"
names(PI_SI)[3] <- "Upper"
PI_SI$Country <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")

Fig11 <- ggplot(PI_SI, aes(x= Country, y=Rho)) + geom_bar(stat="identity", fill="lightgreen", alpha=0.7) +
  geom_errorbar(aes(x=Country, ymin=Lower, ymax=Upper), width=0.3, colour="black", alpha=0.9, size=1) + xlab("") + ylab("PI-Log(SI) Correlation") +
  theme(axis.text.x = element_text(angle = 90, family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=20))

Fig12 <- ggplot(ValidData, aes(x=PI, y=log(SI), color=country)) +
  geom_point(size = 0.5, shape = 20) + 
  geom_smooth(method=loess, se=FALSE, fullrange=TRUE) + 
  theme_test() + scale_color_brewer(palette="Set1") +
  theme(legend.text = element_text(color = "black", size = 12), legend.position = c(0.5,0.85), legend.title = element_blank(), legend.direction = "horizontal") +
  theme(axis.text.x = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.x=element_text(face="bold", colour="black", size=30)) + 
  theme(axis.title.y=element_text(face="bold", colour="black", size=30)) +
  theme(legend.text=element_text(colour="black", size=40))

# Price-PI
Price_PI <- ValidData %>% 
  split(as.character(.$country)) %>% 
  map(function(ValidData) SpearmanRho(ValidData$price, ValidData$PI, use = "complete.obs", conf.level = 0.95))

Price_PI <- data.frame(matrix(unlist(Price_PI), nrow = length(Price_PI), byrow = TRUE))
row.names(Price_PI) <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")
names(Price_PI)[1] <- "Rho"
names(Price_PI)[2] <- "Lower"
names(Price_PI)[3] <- "Upper"
Price_PI$Country <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")

Fig13 <- ggplot(Price_PI, aes(x= Country, y=Rho)) + geom_bar(stat="identity", fill="lightgreen", alpha=0.7) +
  geom_errorbar(aes(x=Country, ymin=Lower, ymax=Upper), width=0.3, colour="black", alpha=0.9, size=1) + xlab("") + ylab("Log(Price)-PI Correlation") +
  theme(axis.text.x = element_text(angle = 90, family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=20))

Fig14 <- ggplot(ValidData, aes(x=log(price), y=PI, color=country)) +
  geom_point(size = 0.5, shape = 20) + 
  geom_smooth(method=loess, se=FALSE, fullrange=TRUE) + 
  theme_test() + scale_color_brewer(palette="Set1") +
  theme(legend.text = element_text(color = "black", size = 12), legend.position = "none", legend.title = element_blank(), legend.direction = "horizontal") +
  theme(axis.text.x = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.x=element_text(face="bold", colour="black", size=30)) + 
  theme(axis.title.y=element_text(face="bold", colour="black", size=30))

# History - PI

history_PI <- ValidData %>% 
  split(as.character(.$country)) %>% 
  map(function(ValidData) SpearmanRho(ValidData$history, ValidData$PI, use = "complete.obs", conf.level = 0.95))

history_PI <- data.frame(matrix(unlist(history_PI), nrow = length(history_PI), byrow = TRUE))
row.names(history_PI) <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")
names(history_PI)[1] <- "Rho"
names(history_PI)[2] <- "Lower"
names(history_PI)[3] <- "Upper"
history_PI$Country <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")

Fig15 <- ggplot(history_PI, aes(x= Country, y=Rho)) + geom_bar(stat="identity", fill="lightgreen", alpha=0.7) +
  geom_errorbar(aes(x=Country, ymin=Lower, ymax=Upper), width=0.3, colour="black", alpha=0.9, size=1) + xlab("") + ylab("History-PI Correlation") +
  theme(axis.text.x = element_text(angle = 90, family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=20))

Fig16 <- ggplot(ValidData, aes(x=history, y=PI, color=country)) +
  geom_point(size = 0.5, shape = 20) + 
  geom_smooth(method=loess, se=FALSE, fullrange=TRUE) + 
  theme_test() + scale_color_brewer(palette="Set1") +
  theme(legend.text = element_text(color = "black", size = 12), legend.position = "none", legend.title = element_blank(), legend.direction = "horizontal") +
  theme(axis.text.x = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.x=element_text(face="bold", colour="black", size=30)) + 
  theme(axis.title.y=element_text(face="bold", colour="black", size=30))

# Price - SI
Price_SI <- ValidData %>% 
  split(as.character(.$country)) %>% 
  map(function(ValidData) SpearmanRho(log(ValidData$price), ValidData$SI, use = "complete.obs", conf.level = 0.95))

Price_SI <- data.frame(matrix(unlist(Price_SI), nrow = length(Price_SI), byrow = TRUE))
row.names(Price_SI) <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")
names(Price_SI)[1] <- "Rho"
names(Price_SI)[2] <- "Lower"
names(Price_SI)[3] <- "Upper"
Price_SI$Country <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")

Fig17 <- ggplot(Price_SI, aes(x= Country, y=Rho)) + geom_bar(stat="identity", fill="lightgreen", alpha=0.7) +
  geom_errorbar(aes(x=Country, ymin=Lower, ymax=Upper), width=0.3, colour="black", alpha=0.9, size=1) + xlab("") + ylab("Price-SI Correlation") +
  theme(axis.text.x = element_text(angle = 90, family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=20))

Fig18 <- ggplot(ValidData, aes(x=log(price), y=log(SI), color=country)) +
  geom_point(size = 0.5, shape = 20) + 
  geom_smooth(method=loess, se=FALSE, fullrange=TRUE) + 
  theme_test() + scale_color_brewer(palette="Set1") +
  theme(legend.text = element_text(color = "black", size = 12), legend.position = "none", legend.title = element_blank(), legend.direction = "horizontal") +
  theme(axis.text.x = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.x=element_text(face="bold", colour="black", size=30)) + 
  theme(axis.title.y=element_text(face="bold", colour="black", size=30))

# History - SI
History_SI <- ValidData %>% 
  split(as.character(.$country)) %>% 
  map(function(ValidData) SpearmanRho(ValidData$history, ValidData$SI, use = "complete.obs", conf.level = 0.95))

History_SI <- data.frame(matrix(unlist(History_SI), nrow = length(History_SI), byrow = TRUE))
row.names(History_SI) <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")
names(History_SI)[1] <- "Rho"
names(History_SI)[2] <- "Lower"
names(History_SI)[3] <- "Upper"
History_SI$Country <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")

Fig19 <- ggplot(History_SI, aes(x= Country, y=Rho)) + geom_bar(stat="identity", fill="lightgreen", alpha=0.7) +
  geom_errorbar(aes(x=Country, ymin=Lower, ymax=Upper), width=0.3, colour="black", alpha=0.9, size=1) + xlab("") + ylab("History-log(SI) Correlation") +
  theme(axis.text.x = element_text(angle = 90, family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=20))

Fig20 <- ggplot(ValidData, aes(x=history, y=log(SI), color=country)) +
  geom_point(size = 0.5, shape = 20) + 
  geom_smooth(method=loess, se=FALSE, fullrange=TRUE) + 
  theme_test() + scale_color_brewer(palette="Set1") +
  theme(legend.text = element_text(color = "black", size = 12), legend.position = "none", legend.title = element_blank(), legend.direction = "horizontal") +
  theme(axis.text.x = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.x=element_text(face="bold", colour="black", size=30)) + 
  theme(axis.title.y=element_text(face="bold", colour="black", size=30))

# History-Price
History_Price <- ValidData %>% 
  split(as.character(.$country)) %>% 
  map(function(ValidData) SpearmanRho(ValidData$history, log(ValidData$price), use = "complete.obs", conf.level = 0.95))

History_Price <- data.frame(matrix(unlist(History_Price), nrow = length(History_Price), byrow = TRUE))
row.names(History_Price) <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")
names(History_Price)[1] <- "Rho"
names(History_Price)[2] <- "Lower"
names(History_Price)[3] <- "Upper"
History_Price$Country <- c("ARG", "BRA", "CHI", "COL", "ECU", "MEX", "URU", "VEN")

Fig21 <- ggplot(History_Price, aes(x= Country, y=Rho)) + geom_bar(stat="identity", fill="lightgreen", alpha=0.7) +
  geom_errorbar(aes(x=Country, ymin=Lower, ymax=Upper), width=0.3, colour="black", alpha=0.9, size=1) + xlab("") + ylab("History-log(Price) Correlation") +
  theme(axis.text.x = element_text(angle = 90, family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.y=element_text(face="italic", colour="black", size=20))

Fig22 <- ggplot(ValidData, aes(x=history, y=log(price), color=country)) +
  geom_point(size = 0.5, shape = 20) + 
  geom_smooth(method=loess, se=FALSE, fullrange=TRUE) + 
  theme_test() + scale_color_brewer(palette="Set1") +
  theme(legend.text = element_text(color = "black", size = 12), legend.position = "none", legend.title = element_blank(), legend.direction = "horizontal") +
  theme(axis.text.x = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) + 
  theme(axis.text.y = element_text(family="Times", face="bold",
                                   colour="black", size=rel(4))) +
  theme(axis.title.x=element_text(face="bold", colour="black", size=30)) + 
  theme(axis.title.y=element_text(face="bold", colour="black", size=30))

library(ggpubr)
FIG_A <- ggarrange(Fig4, Fig6, Fig8, Fig10, Fig12, Fig14, Fig16, Fig18, Fig20, Fig22, ncol = 5, nrow = 2, labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), font.label = list(size = 26, color = "red"))
FIG_B <- ggarrange(Fig3, Fig5, Fig7, Fig9, Fig11, Fig13, Fig15, Fig17, Fig19, Fig21, ncol = 5, nrow = 2, labels = c("K", "L", "M", "N", "O", "P", "Q", "R", "S", "T"), font.label = list(size = 26, color = "red"))

FIG2 <- ggarrange(FIG_A, FIG_B, ncol = 1, nrow = 2)

