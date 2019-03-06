load("/home/juan/Documents/PaperMercadolibre/db_mercadolibre.RData")
rm(list=setdiff(ls(), "df"))
mercadolibredata <- data.frame(df[2:9])

library(tidyverse)
usados <- mercadolibredata %>% filter(str_detect(vectorsold, 'Usado'))
nuevos <- mercadolibredata %>% filter(str_detect(vectorsold, 'vendido'))
# We discarded the records of used items as they are
# not in the interest of our research

# Let's clean the records of the variables by
# removing non-numeric characters and set the 
# variables up as numeric.
nuevos$vectorprice <- as.numeric(gsub("\\D", "", nuevos$vectorprice))
nuevos$vectorsold <- as.numeric(gsub("[^0-9]", "", nuevos$vectorsold))
nuevos$goodfeedback <- as.numeric(gsub("[^0-9]", "", nuevos$goodfeedback))
nuevos$neutralfeedback <- as.numeric(gsub("[^0-9]", "", nuevos$neutralfeedback))
nuevos$badfeedback <- as.numeric(gsub("[^0-9]", "", nuevos$badfeedback))

newdata <- nuevos

# Let's rename our variables
# vectorprice is price
names(newdata)[1] <- "Price"
# vectorsold is sold products "SP"
names(newdata)[2] <-"SP"
# vectorproduct is "Item" 
names(newdata)[3] <-"Item"
# country is "Country"
names(newdata)[4] <- "Country"
# vectorquestions is Purchase Intention "PI"
names(newdata)[5] <- "PI"
# goodfeedback is Positive Feedback "PF"
names(newdata)[6]<-"PF"
# neutralfeedback is Neutral Feedback "NeuF"
names(newdata)[7]<-"NeuF"
# badfeedback is Negative Feedback "NF"
names(newdata)[8]<-"NF"

newdata <- newdata[complete.cases(newdata), ]
rm(list=setdiff(ls(), "newdata"))

# Now let's split our dataframe in four parts, each part is a product
iphone <- newdata %>% filter(Item == "Iphone 7" & SP >= 0)
ps4 <- newdata %>% filter(Item == "PS4" & SP >= 0)
Converse <- newdata %>% filter(Item == "Converse" & SP >= 0)
Bible <- newdata %>% filter(Item == "Biblia" & SP >= 0)


library(psych)
describeBy(iphone$PI, group = iphone$Country, mat = TRUE)
# Coutries with a sufficient number of valid records for Iphone7 are
# Argentina (223), Brasil (326), Chile (57), Colombia (78), Ecuador (108), 
# Mexico (92), and Peru (59)

describeBy(ps4$PI, group = ps4$Country, mat = TRUE)
# Countries with a sufficient number of valid records for PS4 are
# Argentina (541), Brasil (1017), Chile (352), Colombia (215), 
# Costa Rica (81), Ecuador (210), Mexico (218), Peru (85), and 
# Dominican Rep (53)

describeBy(Converse$PI, group = Converse$Country, mat = TRUE)
# Countries with a sufficient number of valid records for Converse
# are Argentina (106), Brasil (1697), Chile (86), Colombia (734), 
# Costa Rica (64), Ecuador (62), Mexico (515), and Dominican Rep (77) 

describeBy(Bible$PI, group = Bible$Country, mat = TRUE)
# Countries with a sufficient number of valid records for The Bible
# are Argentina (245), Brasil (1109), Chile (156), Colombia (248), 
# and Mexico (1065)

# Summing up, a dataframe with vaid records is the one that contains
# Argentina, Brasil, Chile, Colombia, and Mexico, as these countries
# showed valid records for all four products.

ValidData <- newdata %>% filter(Country == "Argentina" | Country == "Brasil" | Country == "Chile" | Country == "Colombia" | Country == "Mexico")
nrow(ValidData)

# Now let's include current exhange rates 
# between Latin-American countries to 
# US Dollars (as of March 6 2019) 
# with Google Currency Converter

Argentina <- 0.025
Brasil <- 0.26
Chile <- 0.0015
Colombia <- 0.00032
Mexico <- 0.052

class(Mexico)

library(dplyr)
ValidData <- ValidData %>% mutate(USPrice = if_else(Country == "Argentina", Price*Argentina, 
                               if_else(Country == "Brasil", Price*Brasil, 
                               if_else(Country == "Chile", Price*Chile,
                               if_else(Country == "Colombia", Price*Colombia, Price*Mexico)))))
head(ValidData) 
