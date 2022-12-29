#install packeges: RSQLite

library(RSQLite)
# Load the RSQLite Library

#!!! Please point to you directory where are the csv files are saved!!!
setwd("C:\\Users\\pkorzystko\\Downloads")

carslist <- read.csv("auto-mpg.csv")
mpgreal <- read.csv("mpg_real.csv")

colnames(mpgreal)[1] <- 'id'

head(mpgreal)
conn <- dbConnect(RSQLite::SQLite(), "CarsDB.db")

dbWriteTable(conn,"refcars", carslist, overwrite = TRUE)
dbWriteTable(conn,"mpgreal", mpgreal, overwrite = TRUE)

---------------------------
#A)

# base on 2 data set , one with car attributes and other with fuel consumation reading
# we will fit a model to identify if a car american, Origin 1 or not, origin 2 or 3
# our explanatory variables are avg(mpg_real) , cylinders,displacement,horsepower
# weight, acceleration


#check if the tables exist
dbListTables(conn)

regdata <- dbGetQuery(conn, "SELECT case origin when 1 then 0 else 1 end origin , 
                      avg(mpg_real) mpg, cylinders,displacement,horsepower
                      weight, acceleration
                      FROM refcars 
                      JOIN mpgreal on refcars.id = mpgreal.id
                      GROUP BY refcars.id")

#obtain train / test data seta 

summary(regdata)
set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(regdata), replace=TRUE, prob=c(0.8,0.2))
train  <- regdata[sample, ]
test   <- regdata[!sample, ]

#1 model include all variables
# Y is mp, we will explain origin  frommgp  n.cylinders displacement weight and acceleration  
model1 <- glm( origin ~ mpg + cylinders + displacement + weight + acceleration , data=train)
summary(model1)

model_final <- glm( origin ~ mpg + cylinders + weight , data=train)
summary(model_final)

---------------------------
#B)

summary(model_final)
# By the interpretation of the result at significance of 5%, mgp and acelaration are not significant

#removing no significant variable displacement iteratively we obtain: 
#as the correlation between cylinders and displacement is high we can only use one or another 
model2 <- glm( origin ~  mpg + cylinders  , data=train, family=binomial)

summary(model2)
# the best model based on AIC criterium is origin ~  mpg + cylinder

#All remaining varaibles are significant 
cor(regdata)

---------------------------
#C)

#predict result based on model2 
predicted <- predict(model_final, test, type="response")

head(predicted)
summary(predicted)

---------------------------
#D)

#array(confusion matrix)

#!!!please inatall caret library

library(caret)
confusionMatrix(as.factor(ifelse(predicted < 0.5,0,1)) , as.factor(test$origin))

#Acurracy obtained is .7606 
