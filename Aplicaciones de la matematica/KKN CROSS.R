library("class")

TasaError = c()
TasaError1 = c()
TasaError2 = c()
TasaError3 = c()
TasaError4 = c()
TasaError5 = c()
x = 1:200

for (i in 1:100)
{
  train_number = sample(x,180,replace = FALSE)
  train = datos_T2[train_number,]
  test = datos_T2[-train_number,]
  #Modelo K = 3
  test_pred = knn(train = cbind(train$X2,train$X3),test = cbind(test$X2,test$X3),cl = train$Y,k=3,prob = T)
  count = 0
  for (j in  1:20){
    if (test_pred[j]==factor(test$Y)[j]){
      count = count + 1 
    }
  }
  TasaError[i] = (20 - count)/20
  #Modelo K = 5
  test_pred5 = knn(train = cbind(train$X2,train$X3),test = cbind(test$X2,test$X3),cl = train$Y,k=5,prob = T)
  count5 = 0
  for (j in  1:20){
    if (test_pred5[j]==factor(test$Y)[j]){
      count5 = count5 + 1 
    }
  }
  TasaError5[i] = (20 - count5)/20
  #Modelo K = 9
  test_pred1 = knn(train = cbind(train$X2,train$X3),test = cbind(test$X2,test$X3),cl = train$Y,k=9,prob = T)
  count1 = 0
  for (j in  1:20){
    if (test_pred1[j]==factor(test$Y)[j]){
      count1 = count1 + 1 
    }
  }
  TasaError1[i] = (20 - count1)/20
  #Modelo K = 19
  test_pred2 = knn(train = cbind(train$X2,train$X3),test = cbind(test$X2,test$X3),cl = train$Y,k=19,prob = T)
  count2 = 0
  for (j in  1:20){
    if (test_pred2[j]==factor(test$Y)[j]){
      count2 = count2 + 1 
    }
  }
  TasaError2[i] = (20 - count2)/20
  #Modelo K = 41
  test_pred3 = knn(train = cbind(train$X2,train$X3),test = cbind(test$X2,test$X3),cl = train$Y,k=41,prob = T)
  count3 = 0
  for (j in  1:20){
    if (test_pred3[j]==factor(test$Y)[j]){
      count3 = count3 + 1 
    }
  }
  TasaError3[i] = (20 - count3)/20
  #Modelo K = 61
  test_pred4 = knn(train = cbind(train$X2,train$X3),test = cbind(test$X2,test$X3),cl = train$Y,k=61,prob = T)
  count4 = 0
  for (j in  1:20){
    if (test_pred4[j]==factor(test$Y)[j]){
      count4 = count4 + 1 
    }
  }
  TasaError4[i] = (20 - count4)/20
}
hist(TasaError,main = "Tasa de Error para K=3",xlab = "Tasa de Error")
hist(TasaError5,main = "Tasa de Error para K=5",xlab = "Tasa de Error")
hist(TasaError1,main = "Tasa de Error para K=9",xlab = "Tasa de Error")
hist(TasaError2,main = "Tasa de Error para K=19",xlab = "Tasa de Error")
hist(TasaError3,main = "Tasa de Error para K=41",xlab = "Tasa de Error")
hist(TasaError4,main = "Tasa de Error para K=61",xlab = "Tasa de Error")