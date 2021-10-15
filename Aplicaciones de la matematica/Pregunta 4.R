attach(datos_heart_disease)
histfam = c()
for (i in 1:462){
  if (famhist[i]=="Present"){
    histfam[i] = 1
  }
  else{
    histfam[i] = 0
  }
}

model = glm(chd ~ sbp + tobacco + ldl + histfam + obesity + alcohol + age, family = binomial (link = "logit"))
summary(model)

model = glm(chd ~ sbp + tobacco + ldl + histfam + obesity + age, family = binomial (link = "logit"))
summary(model)

model = glm(chd ~ tobacco + ldl + histfam + obesity + age, family = binomial (link = "logit"))
summary(model)

model = glm(chd ~ tobacco + ldl + histfam + age, family = binomial (link = "logit"))
summary(model)
#Francisco Nilsson 201810015-3
