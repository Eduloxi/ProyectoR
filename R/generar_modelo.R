

#Input = DataFrame [y, X1, X2, ..., Xn]
sample(100, 11)
#Genereamos un df sintético con la forma del df input
y = sample(100, 11)
X1 = sample(100, 11)
X2 = sample(100, 11)
X3 = sample(100, 11)
X4 = sample(100, 11)

df = data.frame(y, X1, X2, X3, X4)  


#Una vez recibido el dataset lo pasamos al modelo de regresion lineal multiple

list_model2 <- colnames(df)

list_model <- list()



for (i in colnames(df)) {                        
  list_model <- append(list_model, i)                        
}


lm(y ~ mV+mV2+mV)

train_test_split <- function(df) {
  smp_size <- floor(0.75 * nrow(df))
  
  set.seed(42)
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  
  y_train <- train[[1]]
  y_test <- test[[1]]
  X_train <- train[-1]
  X_test <- test[-1]
  
  return(list(X_train, X_test, y_train, y_test))
}

lista <- train_test_split(df)

y_train <- lista[[3]]
y_test <- lista[[4]]

X_train <- lista[[1]]
X_test <- lista[[2]]

X1 <- lista[[3]][1]
X2 <- 

#Aquí dividimos el tamaño


target <- df[1]
variables <- df[-1]
y_train 
X_train


model <- lm(y_train ~., X_train)

summary(model)

predictions <- predict(model)


