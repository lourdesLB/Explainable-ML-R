# ----------------------------------------------------------------------------
# Lectura del dataset y preprocesado

install.packages("tidyverse")
library(tidyverse)
library(readr)

coches <- read.table("Car_sales.txt", encoding = "UTF-8")

coches <- coches %>%
  select(c("reventa", everything()))
View(coches)

coches$fabricante = factor(coches$fabricante)
coches$modelo = factor(coches$modelo)
coches$tipo = factor(coches$tipo)

coches2 <- na.omit(coches)
summary(coches2)

coches2 <- coches2[,-c(2,3,5)]
View(coches2)


# ----------------------------------------------------------------------------

# Representaciones 

boxplot(coches2)
heatmap(abs( cor(coches2) ), scale="none")



# ----------------------------------------------------------------------------

# Modelo de regresion lineal multiple con todas las variables

# split the data
index <- sample(1:nrow(coches2), 0.8*nrow(coches2))
train <- coches2[index,]
test <- coches2[-index,]

reglineal = lm(reventa ~., data=train)
summary(reglineal)

predict(reglineal, test)


# ----------------------------------------------------------------------------

# ModelStudio para explicabilidad

install.packages("DALEX")
install.packages("DALEXtra")
install.packages("mlr")
install.packages("xgboost")
library(modelStudio)
library(xgboost)
library(DALEX)

train_matrix <- model.matrix(reventa ~.-1, train)
test_matrix <- model.matrix(reventa ~.-1, test)

xgb_matrix <- xgb.DMatrix(train_matrix, label = train$reventa)
params <- list(max_depth = 3, 
               objective = "reg:linear", 
               eval_metric = "rmse")
model <- xgb.train(params, xgb_matrix, nrounds = 500)

explainer <- explain(model,
                     data = test_matrix,
                     y = test$reventa,
                     type = "regression",
                     label = "xgboost")

modelStudio::modelStudio(explainer)
