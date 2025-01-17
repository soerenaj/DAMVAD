mydata <- as.data.frame(read.csv(choose.files()))

summary(mydata)
mydata[1,]

#Lad os kigge p� et mindre datas�t
tail(sort(table(mydata$DECLARANT_ISO)),10)

# DK som declarant
subset <- mydata[mydata$DECLARANT_ISO=="DK"&
                   mydata$VALUE_IN_EUROS>0&mydata$QUANTITY_IN_KG>0,]

mylm <- lm(log(VALUE_IN_EUROS) ~ factor(TRADE_TYPE) +
        factor(PRODUCT_SECTION) + factor(FLOW) +
        log(QUANTITY_IN_KG), data = subset)
summary(mylm)

library(stats)
drop1(mylm,test = "F")

#unders�ger en interaktionseffekt mellem TRADE_TYPE og FLOW
table(factor(subset$FLOW),factor(subset$TRADE_TYPE))
mylm2 <- lm(log(VALUE_IN_EUROS) ~ factor(TRADE_TYPE)*factor(FLOW) +
             factor(PRODUCT_SECTION) +
             log(QUANTITY_IN_KG), data = subset)
summary(mylm2)

#tester effekten
anova(mylm,mylm2)

#effekten er signifikant

unique(subset$TRADE_TYPE)
exp(confint(mylm2,parm = c(2,3,4,27,28)))
#Blandt produkter hvor Danmark er declarant
#er v�rdien af produkter af trade-type I ca. 25 % mindre
#end ditto af trade-type E, if�lge modellen.

#V�rdien af produkter fra flow 2 er signifikant st�rre end fra
#flow 1 for trade-type E og I. For trade-type K er det dog omvendt

# er v�rdien af produkter fra product sektion 14 generelt st�rre?
subset$PRODUCT_SECTION <- relevel(factor(subset$PRODUCT_SECTION),ref = "14")
mylm2 <- lm(log(VALUE_IN_EUROS) ~ factor(TRADE_TYPE)*factor(FLOW) +
              factor(PRODUCT_SECTION) +
              log(QUANTITY_IN_KG), data = subset)
summary(mylm2)
#Produkt-sektion 14 har generelt produkter med h�jest v�rdi.