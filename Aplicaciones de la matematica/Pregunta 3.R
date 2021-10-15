library("naivebayes")

matrix = matrix(nrow = 10, ncol = 10)
colnames(matrix) <- c("pinkfloyd","thebeatles","queen","oasis","radiohead","greenday","thestrokes",
                      "linkinpark","foofighters","coldplay")
Test = matrix(nrow = 1, ncol = 10)
Test[1,1]=1
Test[1,2]=0
Test[1,3]=1
Test[1,4]=1
Test[1,5]=0
Test[1,6]=1
Test[1,7]=0
Test[1,8]=0
Test[1,9]=0
Test[1,10]=0
colnames(Test) <- c("pinkfloyd","thebeatles","queen","oasis","radiohead","greenday","thestrokes",
                      "linkinpark","foofighters","coldplay")
for (i in 1:10){
  if (gustos_musicales$pinkfloyd[i] == "no"){
    matrix[i,1] = 0
  }
  else{
    matrix[i,1] = 1
  }
  if (gustos_musicales$thebeatles[i] == "no"){
    matrix[i,2] = 0
  }
  else{
    matrix[i,2] = 1
  }
  if (gustos_musicales$queen[i] == "no"){
    matrix[i,3] = 0
  }
  else{
    matrix[i,3] = 1
  }
  if (gustos_musicales$oasis[i] == "no"){
    matrix[i,4] = 0
  }
  else{
    matrix[i,4] = 1
  }
  if (gustos_musicales$radiohead[i] == "no"){
    matrix[i,5] = 0
  }
  else{
    matrix[i,5] = 1
  }
  if (gustos_musicales$greenday[i] == "no"){
    matrix[i,6] = 0
  }
  else{
    matrix[i,6] = 1
  }
  if (gustos_musicales$thestrokes[i] == "no"){
    matrix[i,7] = 0
  }
  else{
    matrix[i,7] = 1
  }
  if (gustos_musicales$linkinpark[i] == "no"){
    matrix[i,8] = 0
  }
  else{
    matrix[i,8] = 1
  }
  if (gustos_musicales$foofighters[i] == "no"){
    matrix[i,9] = 0
  }
  else{
    matrix[i,9] = 1
  }
  if (gustos_musicales$coldplay[i] == "no"){
    matrix[i,10] = 0
  }
  else{
    matrix[i,10] = 1
  }
}

gustos_musicales$etiqueta = as.factor(gustos_musicales$etiqueta)
modelo = bernoulli_naive_bayes(x = matrix, y = gustos_musicales$etiqueta)
modelo
summary(modelo)
coef(modelo)
plot(modelo)

predict(modelo, type = "prob", newdata = Test)
   
cor(matrix)
#Francisco Nilsson 201810015-3
