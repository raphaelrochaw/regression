#### Disciplina Aprendizado de Maquinas - Regressao ####

##pacotes utilizados
#install.packages("caTools")
#install.packages("corrgram")
library(dplyr)
library(ggplot2)
library(caTools)
library(corrgram)
library(caret)
library(psych)

### Regressao Linear Simples ###

##lendo arquivo
setwd("D:/Raphael/VETERINARIA/Disciplinas_Pos_Graduacao/Machine_Learning_Regressao")
TV1<-read.table("dados_TV.csv",sep=",",h=T)
str(TV1)

##analise descritiva
describe(TV1)

##grafico de dispersao
ggplot(data=TV1, aes(x=TARMEDIA, y=TVMEDIA)) +
  geom_point(aes())

##coeficiente de correlacao
cor(TV1$TVMEDIA,TV1$TARMEDIA)

##separacao de dados
#aqui os dados são separados desconsiderando as observacoes repetidas dos animais
#aconselho manter as diferentes observacoes de um mesmo animal sempre no mesmo banco de dados
samp <- createDataPartition(TV1$ANIMAL, p = 0.9, list = FALSE)
training <- TV1[samp,]
testing <- TV1[-samp,]
str(training)
str(testing)

##analise de regressao linear simples usando a funcao lm do R base
model <- lm(TVMEDIA ~ TARMEDIA, data=training)
summary(model)

preds <- predict(model, testing)
modelEval <- cbind(testing$TVMEDIA, preds)
colnames(modelEval) <- c("Actual", "Predicted")
modelEval <- as.data.frame(modelEval)
head(modelEval)
#erro quadratico medio
mse <- mean((modelEval$Actual - modelEval$Predicted)^2)
mse
#raiz quadrada do erro quadratico medio
rmse <- sqrt(mse)
rmse
#erro absoluto medio
mae <- mean(abs((modelEval$Actual - modelEval$Predicted))) 
mae

## avaliacao usando o pacote caret
#analise usando validacao cruzada leaving-one-out
trainControl(method = "LOOCV")
lm1 <- train(TVMEDIA~TARMEDIA, data = TV1, method = "lm",trControl=trainControl())
summary(lm1$finalModel)
lm1$results

#analise usando validacao cruzada k-fold com 10 reparticoes 
tc <- trainControl(method = "cv", number = 10)
# Include the setup in your model
lm1_cv <- train(TVMEDIA~TARMEDIA, data =TV1, method = "lm",
                trControl = tc) # here
lm1_cv
lm1_cv$results

### Regressao Linear Multipla ###

library(dplyr)
library(ggplot2)
library(caTools)
library(corrgram)
library(caret)
library(psych)
#install.packages("car")
library(car)
#install.packages("corrplot")
library(corrplot)
library(MASS)

setwd("D:/Raphael/VETERINARIA/Disciplinas_Pos_Graduacao/Machine_Learning_Regressao")
TV1<-read.table("dados_TV.csv",sep=",",h=T)
str(TV1)

##analise descritiva
describe(TV1)
TV2<- as.data.frame(cbind(TV1$ANIMAL,TV1$PRODCLASS,TV1$TVMEDIA,TV1$TARMEDIA,TV1$GRAUSANGUE))
str(TV2)
colnames(TV2)<-c("ANIMAL","PRODCLASS","TVMEDIA","TARMEDIA","GRAUSANGUE")
TV2$PRODCLASS<-as.factor(TV2$PRODCLASS)
TV2$GRAUSANGUE<-as.factor(TV2$GRAUSANGUE)
TV2$ANIMAL<-as.factor(TV2$ANIMAL)

### Visao Estatistica ###
mc<-lm(TVMEDIA~PRODCLASS+GRAUSANGUE+TARMEDIA,data=TV2)
summary(mc)
stepAIC(mc, direction = "both", trace = FALSE)
#lembrar de avaliar premissas do modelo

##coeficiente de correlacao
cor(TV2$TVMEDIA,TV2$TARMEDIA)

## Visao Aprendizado de Máquinas

##separacao de dados
#aqui os dados são separados desconsiderando as observacoes repetidas dos animais
#aconselho manter as diferentes observacoes de um mesmo animal sempre no mesmo banco de dados
samp <- createDataPartition(TV2$ANIMAL, p = 0.9, list = FALSE)
training <- TV2[samp,]
testing <- TV2[-samp,]
str(training)
str(testing)

tc <- trainControl(method = "cv", number = 10)
# Include the setup in your model
lm1_cv <- train(TVMEDIA~TARMEDIA, data =TV2, method = "lm",
                trControl = tc) # here
lm1_cv
lm1_cv$results

lm2_cv <- train(TVMEDIA~PRODCLASS, data =TV2, method = "lm",
                trControl = tc) # here
lm2_cv
lm2_cv$results

lm3_cv <- train(TVMEDIA~GRAUSANGUE, data =TV2, method = "lm",
                trControl = tc) # here
lm3_cv
lm3_cv$results

lm4_cv <- train(TVMEDIA~TARMEDIA+PRODCLASS, data =TV2, method = "lm",
                trControl = tc) # here
lm4_cv
lm4_cv$results

lm5_cv <- train(TVMEDIA~TARMEDIA+GRAUSANGUE, data =TV2, method = "lm",
                trControl = tc) # here
lm5_cv
lm5_cv$results

lm6_cv <- train(TVMEDIA~PRODCLASS+GRAUSANGUE, data =TV2, method = "lm",
                trControl = tc) # here
lm6_cv
lm6_cv$results

lm7_cv <- train(TVMEDIA~TARMEDIA+PRODCLASS+GRAUSANGUE, data =TV2, method = "lm",
                trControl = tc) # here
lm7_cv
lm7_cv$results

summary(resamples(list(
  model1 = lm1_cv, 
  model2 = lm2_cv, 
  model3 = lm3_cv,
  model4 = lm4_cv,
  model5 = lm5_cv,
  model6 = lm6_cv,
  model7 = lm7_cv)))





