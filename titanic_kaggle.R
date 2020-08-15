# Problema de negócio: criar um modelo para prever pessoas mortas no
# desastre do navio titanic

# Competição Kaggle
# Base dados baixadas no site do kaggle desafio titanic

# Carregando conjunto de dados
titanic <- read.csv("train.csv", header = TRUE, stringsAsFactors = TRUE)
dados_teste_final <- read.csv("test.csv", header = TRUE, stringsAsFactors = TRUE)
titanicc <- titanic[,-2]
titanic <- rbind(titanicc, dados_teste_final)
View(titanic)
str(titanic)

# Exploração de dados
str(titanic)
View(titanic)

# Número de valores NA por coluna
sapply(titanic, function(x) sum(is.na(x)))

# visualizando os dados
require(dplyr)
require(ggplot2)
require(stringr)
require(tidyr)

titanic %>%
  filter(Sex == "female") %>%
  ggplot(mapping = aes(x = Pclass, y = Age, fill = factor(Pclass))) +
  geom_boxplot() + ggtitle("Boxplot Female") +
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="red", fill="red") +
  theme(panel.grid = element_line(size = rel(0.7))) +
  scale_y_continuous(breaks = seq(0,70,2), labels = seq(0,70,2))

titanic %>%
  filter(Sex == "male") %>%
  ggplot(mapping = aes(x = Pclass, y = Age, fill = factor(Pclass))) +
  geom_boxplot() + ggtitle("Boxplot Male") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red") +
  theme(panel.grid = element_line(size = rel(0.7))) +
  scale_y_continuous(breaks = seq(0,80,2), labels = seq(0,80,2))

titanic %>%
  ggplot(mapping = aes(x = 1, y = Fare)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="red", fill="red") +
  theme(panel.grid = element_line(size = rel(0.7))) +
  scale_y_continuous(breaks = seq(0,600,10), labels = seq(0,600,10))

titanic %>%
  ggplot(mapping = aes(x = Survived)) +
  geom_bar()

# Imputando valores para as idades (escolhemos o valor da mediana)
imputate.ages <- function(age, class, sex) {
  age.vec <- c()
  for(i in 1:length(age)) {
    if(is.na(age[i])) {
      if(class == 1 && sex == "female") {
        age.vec[i] = 36
      }
      else if(class == 2 && sex == "female") {
        age.vec[i] = 28
      }
      else if(class == 3 && sex == "female") {
        age.vec[i] = 22
      }
      else if(class == 1 && sex == "male") {
        age.vec[i] = 42
      }
      else if(class == 2 && sex == "male") {
        age.vec[i] = 29.5
      }
      else {
        age.vec[i] = 25
      }
    }
    else {
      age.vec[i] = age[i]
    }
  }
  return(age.vec)
}

ages <- imputate.ages(titanic$Age, titanic$Pclass, titanic$Sex)
titanic$Age <- ages

# Substituindo todos os pontos por caracter vazio na coluna Ticket
titanic$Ticket <- str_replace_all(titanic$Ticket, "\\.", "")

# Retirando os números e espaços da coluna Cabin
titanic$Cabin <- gsub('[0-9]+', '', titanic$Cabin)

# Substituindo o caracter espaço pelo caracter vazio da coluna Ticket no padrão
# TON/O 2
for(i in 1:length(titanic$Ticket)) {
  if(str_detect(titanic$Ticket[i], "(TON/O 2)")) {
    titanic$Ticket[i] <- str_replace(titanic$Ticket[i], 
                                          "(?<=TON/O)\\s", '')
  }
  else {
    titanic$Ticket[i] <- titanic$Ticket[i]
  }
}

# Limpando alguns dados
for(i in 1:length(titanic$Ticket)) {
  if(str_detect(titanic$Ticket[i], "(STON/O)")) {
    titanic$Ticket[i] <- str_replace(titanic$Ticket[i], 
                                          "(?<=)STON", "SOTON")
  }
  else if(str_detect(titanic$Ticket[i], "(SC/Pa)")) {
    titanic$Ticket[i] <- str_replace(titanic$Ticket[i],"(?<=)Paris","PARIS")
  }
  else if(str_detect(titanic$Ticket[i], "(Basle)")) {
    titanic$Ticket[i] <- str_replace(titanic$Ticket[i],"(?<=H)\\sBasle","")
  }
  else if(str_detect(titanic$Ticket[i], "(WEP)")) {
    titanic$Ticket[i] <- str_replace(titanic$Ticket[i],"(WEP)","WE/P")
  }
  else if(str_detect(titanic$Ticket[i], "(Fa)")) {
    titanic$Ticket[i] <- str_replace(titanic$Ticket[i],"(Fa)","")
  }
  else {
    titanic$Ticket[i] <- titanic$Ticket[i]
  }
}

# Colocando espaço nos Tickets que começam com número
for(i in 1:length(titanic$Ticket)) {
  if(str_detect(titanic$Ticket[i],"^[:digit:]")) {
    titanic$Ticket[i] <- str_replace(titanic$Ticket[i],"(?=[:digit:])", " ")
  }
  else {
    titanic$Ticket[i] <- titanic$Ticket[i]
  }
}

for(i in 1:length(titanic$Ticket)) {
  if(str_detect(titanic$Ticket[i],"^(A\\s)")) {
    titanic$Ticket[i] <- str_replace(titanic$Ticket[i],"(?<=A)\\s", "\\/")
  }
  else {
    titanic$Ticket[i] <- titanic$Ticket[i]
  }
}

# Separando a coluna Ticket em duas colunas
titanic <- separate(titanic, Ticket, c("TicketPrefix", "TicketNumber"), sep = "\\s")

# Substituindo valor NA por caracter vazio
titanic$TicketNumber <- sapply(titanic$TicketNumber, function(x) ifelse(is.na(x), '', x))

# Deixando apenas uma letra
remove.letters <- function(vec) {
  lett <- c()
  for(i in 1:length(vec)) {
    if(nchar(vec[i]) > 1) {
      if(str_detect(vec[i],"\\s")) {
        lett[i] <- min(strsplit(vec[i]," ")[[1]])
      }
      else {
        lett[i] <- min(strsplit(vec[i],"")[[1]])
      }
    }
    else {
      lett[i] <- vec[i]
    }
  }
  return(lett)
}

titanic$Cabin <- remove.letters(titanic$Cabin)

# Eliminando letras maiores que G
remove.lgtT <- function(x) {
  let <- c()
  for(i in 1:length(x)) {
    if(x[i] > "G") {
      let[i] <- ""
    }
    else {
      let[i] <- x[i]
    }
  }
  return(let)
}

titanic$Cabin <- remove.lgtT(titanic$Cabin)

# Continuando a limpar os dados
for(i in 1:length(titanic$TicketPrefix)) {
  if(str_detect(titanic$TicketPrefix[i], "(SOC)")) {
    titanic$TicketPrefix[i] <- str_replace(titanic$TicketPrefix[i], "(?<=)SOC", "SO/C")
  }
  else if(str_detect(titanic$TicketPrefix[i], "^(A4)")) {
    titanic$TicketPrefix[i] <- str_replace(titanic$TicketPrefix[i], titanic$TicketPrefix[i], "A/4") 
  }
  else if(str_detect(titanic$TicketPrefix[i], "^(A5)")) {
    titanic$TicketPrefix[i] <- str_replace(titanic$TicketPrefix[i], "(?<=)A5", "A/5") 
  }
  else if(str_detect(titanic$TicketPrefix[i], "(CA/SOTON)")) {
    titanic$TicketPrefix[i] <- str_replace(titanic$TicketPrefix[i], "(?<=)CA/SOTON", "CA")
  }
  else if(str_detect(titanic$TicketPrefix[i], "A/S")) {
    titanic$TicketPrefix[i] <- str_replace(titanic$TicketPrefix[i], "A/S", "A/5")
  }
  else if(str_detect(titanic$TicketPrefix[i], "FC")) {
    titanic$TicketPrefix[i] <- str_replace(titanic$TicketPrefix[i], titanic$TicketPrefix[i], "FCC")
  }
  else if(str_detect(titanic$TicketPrefix[i], "(P/)")) {
    titanic$TicketPrefix[i] <- str_replace(titanic$TicketPrefix[i], "(?<=)P/PP", "PP")
  }
  else if(str_detect(titanic$TicketPrefix[i], "(SC)")) {
    titanic$TicketPrefix[i] <- str_replace(titanic$TicketPrefix[i], titanic$TicketPrefix[i], "SC")
  }
  else if(str_detect(titanic$TicketPrefix[i], "SP")) {
    titanic$TicketPrefix[i] <- str_replace(titanic$TicketPrefix[i], "(?<=)SP", "SO")
  }
  else if(str_detect(titanic$TicketPrefix[i], "SO/PP")) {
    titanic$TicketPrefix[i] <- str_replace(titanic$TicketPrefix[i], "(?<=)SO/PP", "SO")
  }
  else if(str_detect(titanic$TicketPrefix[i], "SOP")) {
    titanic$TicketPrefix[i] <- str_replace(titanic$TicketPrefix[i], "(?<=)SOP", "SO")
  }
  else if(str_detect(titanic$TicketPrefix[i], "SW/PP")) {
    titanic$TicketPrefix[i] <- str_replace(titanic$TicketPrefix[i], "(?<=)SW/PP", "PP")
  }
  else {
    titanic$TicketPrefix[i] <- titanic$TicketPrefix[i]
  }
}

for(i in 1:length(titanic$TicketPrefix)) {
  if(str_detect(titanic$TicketPrefix[i], "^A")) {
    titanic$TicketPrefix[i] <- str_replace(titanic$TicketPrefix[i], titanic$TicketPrefix[i], "A")
  }
  else {
    titanic$TicketPrefix[i] <- titanic$TicketPrefix[i]
  }
}

for(i in 1:length(titanic$TicketPrefix)) {
  if(str_detect(titanic$TicketPrefix[i], "^LP")) {
    titanic$TicketPrefix[i] <- str_replace(titanic$TicketPrefix[i], titanic$TicketPrefix[i], "")
  }
  else {
    titanic$TicketPrefix[i] <- titanic$TicketPrefix[i]
  }
}

table(factor(titanic$TicketPrefix))

# Transformando TicketPrefix em número
titanic$TicketNumber <- as.numeric(titanic$TicketNumber)
is.numeric(titanic$TicketNumber)
View(titanic)

# Retirando algumas colunas que podem causar overfitting
# colunas a serem tiradas: Passengerid
# Dado um vetor do nome das colunas e um vetor de pattern retorna 
# o índice desse padrão na lista
indice <- function(pattern, vectorPattern) {
  index <- c()
  for(i in 1:length(pattern)) {
    index[i] <- grep(pattern[i], vectorPattern,value = FALSE)
  }
  return(index)
}

# Subistitui valores NA por 0 na coluna Ticket Number
titanic$TicketNumber <- sapply(titanic$TicketNumber, function(x) ifelse(is.na(x), 0, x))

titan1 <- read.csv("train.csv", header = TRUE, stringsAsFactors = TRUE)
titan2 <- cbind(Survived = titan1$Survived, titanic[1:891,])
names(titan2)[1] <- "Survived"
titanicTrain <- titan2
titanicTest <- titanic[892:1309,]
str(titanicTest)
View(titanicTrain)
str(titanicTrain)
titanicTrain$TicketPrefix <- factor(titanicTrain$TicketPrefix)
titanicTrain$Cabin <- factor(titanicTrain$Cabin)

# Gravando o dataset modificado
write.csv(titan2, "titanTrain.csv", sep = ",", col.names = TRUE)
write.csv(titanic[892:1309,], "titanTest.csv", sep = ",", col.names = TRUE)

# Carregando dados
titanicTrain2 <- read.csv("titanTrain.csv", sep = ",", header = TRUE)
View(titanicTrain2)
str(titanicTrain2)
table(factor(titanic$TicketPrefix))

# retirando a algumas colunas para criar o modelo 1
indices <- indice(c("Name", "PassengerId", "TicketPrefix", "TicketNumber", "Cabin"),
                  names(titanicTrain))

titanic2 <- titanicTrain[,-indices]
View(titanic2)
class(titanicTrain)
class(titanicTrain2)
str(titanicTrain)
str(titanicTrain2)
titanicTrain$TicketPrefix <- factor(titanicTrain$TicketPrefix)
titanicTrain$Cabin <- factor(titanicTrain$Cabin)

# Quantizando usando a quantidade de elementos 
quantization.prop <- function(vector.var, percents) {
  vec <- vector.var
  quant <- 1:length(vector.var)
  vec.quantile <- quantile(vector.var, probs = percents)
  for(j in 1:length(vec.quantile)) {
    for(i in 1:length(vector.var)) {
      if(vec[i] <= vec.quantile[j]) {
        quant[i] <- j
        vec[i] <- Inf
      }
      else {
        vec[i] <- vec[i]
      }
    }
  }
  return(quant)
}

# teste função
titanic3$TicketNumber <- quantization.prop(titanic3$TicketNumber, c(0.25,0.5,0.75,1))

# Quantização NumberTicket usando valores máximos e mínimos como parâmetro
quantization <- function(vector.var, levels.quantization) {
  vec <- vector.var
  quant <- 1:length(vector.var)
  interval <- max(vector.var) - min(vector.var)
  reason <- interval/(levels.quantization)
  steps <- min(vector.var) + reason
  value <- 1
  while(value <= levels.quantization) {
    for(i in 1:length(vector.var)) {
      if(vec[i] <= steps) {
        quant[i] <- value
        vec[i] <- Inf
      }
      else {
        vec[i] <- vec[i]
      }
    }
    steps <- steps + reason
    value <- value + 1
  }
  return(quant)
}

# testando a função
vetor <- c(1,2,3,2,3,4,5,1,2,3,4,6,7,5,8)
quantization(vetor, 3)

vetor1 <- c(2,3,1,4,2,3,5,4,32,54,3,4,67,45,44,32,67,78)
quantization(vetor1, 4)

test <- quantization(titanic$TicketNumber, 2)


# ROSE - balanceamento dos dados
library(ROSE)
table(titanicTrain$Survived)
balancedTitanic <- ROSE(Survived ~ ., titanic3, p = 0.5, seed = 4)$data
View(balancedTitanic)
table(balancedTitanic$Survived)
titanic3 <- balancedTitanic

# Criando um modelo
# Gerar vários modelos diferentes
modelsGeneratorGLMTitanic <- function(fraction_train, n) {
  Performance_models <- c()
  seeds <- c()
  seed <- 1
  for(i in 1:n) {
    seeds[i] <- seed
    set.seed(seed)
    rows <- sample(1:nrow(titanic2), fraction_train*nrow(titanic2), replace = FALSE)
    dados_treino <- titanic2[rows,]
    dados_teste <- titanic2[-rows,]
    while(length(levels(factor(dados_treino$Embarked))) != 4) {
      seed <- seed + 1
      seeds[i] <- seed
      set.seed(seed)
      rows <- sample(1:nrow(titanic2), fraction_train*nrow(titanic2), replace = FALSE)
      dados_treino <- titanic2[rows,]
      dados_teste <- titanic2[-rows,]
    }
    glm.fit <- glm(Survived ~ ., data = dados_treino, family = binomial(link = "logit"))
    glm.probs <- predict(glm.fit, dados_teste[,-1], type = "response")
    glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
    Performance_models[i] <- mean(glm.pred == dados_teste[,1])
    seed <- seed + 1
  }
  return(PerformanceSeed <- list(Performance_models, seeds))
}

modelsGeneratorGLMTitanic(0.8, 100)
# 0.8491620
# 82 e 76

# Modificando o dataset titanic
titanicTrain <- read.csv("titanTrain.csv", sep = ",", header = TRUE)
View(titanicTrain)
indices <- indice(c("Name", "PassengerId"),
                  names(titanicTrain))

titanic3 <- titanicTrain[,-indices]
str(titanic3)
View(titanic3)

# Gerar vários modelos diferentes
modelsGeneratorGLMTitanic3 <- function(fraction_train, n) {
  Performance_models <- c()
  seeds <- c()
  seed <- 1
  for(i in 1:n) {
    seeds[i] <- seed
    set.seed(seed)
    rows <- sample(1:nrow(titanic3), fraction_train*nrow(titanic3), replace = FALSE)
    dados_treino <- titanic3[rows,]
    dados_teste <- titanic3[-rows,]
    while(length(levels(factor(dados_treino$Embarked))) != 4 | length(levels(factor(dados_treino$Cabin))) != 8 | length(levels(factor(dados_treino$TicketPrefix))) != 15) {
      seed <- seed + 1
      seeds[i] <- seed
      set.seed(seed)
      rows <- sample(1:nrow(titanic3), fraction_train*nrow(titanic3), replace = FALSE)
      dados_treino <- titanic3[rows,]
      dados_teste <- titanic3[-rows,]
    }
    glm.fit <- glm(Survived ~ ., data = dados_treino, family = binomial(link = "logit"))
    glm.probs <- predict(glm.fit, dados_teste[,-1], type = "response")
    glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
    Performance_models[i] <- mean(glm.pred == dados_teste[,1])
    seed <- seed + 1
  }
  return(PerformanceSeed <- list(Performance_models, seeds))
}

modelsGeneratorGLMTitanic3(0.8,100)
# 0.8603352
# 82 e 95

# Usando algoritmo de árvore de decisão
library(rpart)
library(rpart.plot)

# Criando modelo de árvore de decisão
DecisionTreeTitanic <- function(fraction_train, n) {
  Performance_models <- c()
  seeds <- c()
  seed <- 1
  for(i in 1:n) {
    seeds[i] <- seed
    set.seed(seed)
    rows <- sample(1:nrow(titanic3), fraction_train*nrow(titanic3), replace = FALSE)
    dados_treino <- titanic3[rows,]
    dados_teste <- titanic3[-rows,]
    while(length(levels(factor(dados_treino$Embarked))) != 4 | length(levels(factor(dados_treino$Cabin))) != 8 | length(levels(factor(dados_treino$TicketPrefix))) != 15) {
      seed <- seed + 1
      seeds[i] <- seed
      set.seed(seed)
      rows <- sample(1:nrow(titanic3), fraction_train*nrow(titanic3), replace = FALSE)
      dados_treino <- titanic3[rows,]
      dados_teste <- titanic3[-rows,]
    }
    model.rpart <- rpart(Survived ~ ., data = dados_treino, 
                         parms = list(prior = c(0.7,0.3)), method = "class")
    DecisionTree <- predict(model.rpart, newdata = dados_teste[,-1], type = "class")
    Performance_models[i] <- mean(DecisionTree == dados_teste[,1])
    seed <- seed + 1
  }
  return(PerformanceSeed <- list(Performance_models, seeds))
}

DecisionTreeTitanic(0.8,100)
# 0.8826816
# 45
View(titanic3)
# Algoritmo randomforest
library(randomForest)

paste.formula <- function(resposta, preditora) {
  form <- paste(resposta, sep = " ", "~ ")
  for(i in 1:length(preditora)) {
    if(i == 1) {
      form <- paste(form, sep = "", preditora[i])
    }
    else {
      form <- paste(form, sep = " + ", preditora[i])
    }
  }
  return(as.formula(form))
}

colunas <- names(titanic3)

View(titanic2)
x <- titanic2[,2:8]
y <- factor(titanic2[,1])
seed <- 1
set.seed(seed)
bestmtry <- tuneRF(x, y, stepFactor = 1.5, improve = 1e-5, ntreeTry = 500)
print(bestmtry)
str(titanic3)
titanic3$Survived <- factor(titanic3$Survived)
titanic3$Pclass <- factor(titanic3$Pclass)

mtry <- sqrt(ncol(x))
metric <- "Accuracy"
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(paste.formula(colunas[1], colunas[2:8]), data=titanic2, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

View(titanic3)
RandomForestTitanic <- function(fraction_train, n) {
  Performance_models <- c()
  seeds <- c()
  seed <- 1
  for(i in 1:n) {
    seeds[i] <- seed
    set.seed(seed)
    rows <- sample(1:nrow(titanic3), fraction_train*nrow(titanic3), replace = FALSE)
    dados_treino <- titanic3[rows,]
    dados_teste <- titanic3[-rows,]
    while(length(levels(factor(dados_treino$Embarked))) != 4 | length(levels(factor(dados_treino$Cabin))) != 8 | length(levels(factor(dados_treino$TicketPrefix))) != 15) {
      seed <- seed + 1
      seeds[i] <- seed
      set.seed(seed)
      rows <- sample(1:nrow(titanic3), fraction_train*nrow(titanic3), replace = FALSE)
      dados_treino <- titanic3[rows,]
      dados_teste <- titanic3[-rows,]
    }
    dados_treino$Survived = factor(dados_treino$Survived)
    dados_teste$Survived = factor(dados_teste$Survived)
    model.rf <- randomForest(x = dados_treino[,-1], 
                             importance = TRUE, proximity = TRUE, 
                             y = dados_treino$Survived, mtry = 2)
    model.pred <- predict(model.rf, dados_teste[,-1], type = "class")
    Performance_models[i] <- mean(model.pred == dados_teste[,1])
    seed <- seed + 1
  }
  return(PerformanceSeed <- list(Performance_models, seeds))
}

RandomForestTitanic(0.8,100)
# set.seed(92)
# 0.9162011

RandomForestTitanic2 <- function(fraction_train, n) {
  Performance_models <- c()
  seeds <- c()
  seed <- 1
  for(i in 1:n) {
    seeds[i] <- seed
    set.seed(seed)
    rows <- sample(1:nrow(titanic2), fraction_train*nrow(titanic2), replace = FALSE)
    dados_treino <- titanic2[rows,]
    dados_teste <- titanic2[-rows,]
    while(length(levels(factor(dados_treino$Embarked))) != 4) {
      seed <- seed + 1
      seeds[i] <- seed
      set.seed(seed)
      rows <- sample(1:nrow(titanic2), fraction_train*nrow(titanic2), replace = FALSE)
      dados_treino <- titanic2[rows,]
      dados_teste <- titanic2[-rows,]
    }
    dados_treino$Survived = factor(dados_treino$Survived)
    dados_teste$Survived = factor(dados_teste$Survived)
    model.rf <- randomForest(x = dados_treino[,-1], 
                             importance = TRUE, proximity = TRUE, 
                             y = dados_treino$Survived, mtry = 2)
    model.pred <- predict(model.rf, dados_teste[,-1], type = "class")
    Performance_models[i] <- mean(model.pred == dados_teste[,1])
    seed <- seed + 1
  }
  return(PerformanceSeed <- list(Performance_models, seeds))
}

RandomForestTitanic2(0.8,100)
# set.seed(92)
# 0.9162011

# Modelo Boosted
library(caret)
BoostedTitanic <- function(fraction_train, n) {
  Performance_models <- c()
  seeds <- c()
  seed <- 1
  for(i in 1:n) {
    seeds[i] <- seed
    set.seed(seed)
    rows <- sample(1:nrow(titanic3), fraction_train*nrow(titanic3), replace = FALSE)
    dados_treino <- titanic3[rows,]
    dados_teste <- titanic3[-rows,]
    while(length(levels(factor(dados_treino$Embarked))) != 4 | length(levels(factor(dados_treino$Cabin))) != 8 | length(levels(factor(dados_treino$TicketPrefix))) != 15) {
      seed <- seed + 1
      seeds[i] <- seed
      set.seed(seed)
      rows <- sample(1:nrow(titanic3), fraction_train*nrow(titanic3), replace = FALSE)
      dados_treino <- titanic3[rows,]
      dados_teste <- titanic3[-rows,]
    }
    dados_treino$Survived = factor(dados_treino$Survived)
    dados_teste$Survived = factor(dados_teste$Survived)
    modelBoosted <- train(factor(Survived)~.,data=dados_treino,method="gbm", verbose=FALSE)
    model.pred <- predict(modelBoosted, dados_teste)
    Performance_models[i] <- mean(model.pred == factor(dados_teste$Survived))
    seed <- seed + 1
  }
  return(PerformanceSeed <- list(Performance_models, seeds))
}

BoostedTitanic(0.7,100)

# Dados de teste
titanicTest <- read.csv("titanTest.csv", sep = ",", header = TRUE)
test <- read.csv("titanTest.csv", sep = ",", header = TRUE)
View(titanicTest)
View(test)

sapply(titanicTest, function(x) sum(is.na(x)))

for(i in 1:nrow(titanic)) {
  if(is.na(titanicTest$Fare[i])) {
    titanicTest$Fare[i] <- 15
  }
  else {
    titanicTest$Fare[i] <- titanicTest$Fare[i]
  }
}

levels(titanicTest$TicketPrefix) <- levels(titanicTrain$TicketPrefix)
levels(titanicTest$Cabin) <- levels(titanicTrain$Cabin)
levels(titanicTest$Embarked) <- levels(titanicTrain$Embarked)

titanicTrain <- titanicTrain[,-1]
titanicTest <- titanicTest[,-1]

indices1 <- indice(c("Name", "PassengerId"),
                  names(titanicTrain))
indices2 <- indice(c("Name", "PassengerId"),
                  names(titanicTest))

titanicTrain <- titanicTrain[,-indices1]
titanicTest <- titanicTest[,-indices2]

titanicTrain$Survived = factor(titanicTrain$Survived)

model.rf <- randomForest(x = titanicTrain[,-1], 
                         importance = TRUE, proximity = TRUE, 
                         y = titanicTrain$Survived)

model.pred <- predict(model.rf, titanicTest, type = "class")

Survived <- model.pred
PassengerId <- test$PassengerId

Compare <- as.data.frame(cbind(PassengerId, Survived))
Compare$Survived <- ifelse(Compare$Survived == 1, 0, 1)

write.csv(Compare, "Result.csv", row.names = FALSE)
View(Compare)

df1 <- read.csv("Result.csv", sep = ",", header = TRUE)
View(df1)

df <- read.csv("gender_submission.csv", header = TRUE, sep = ",")
View(df)

mean(Compare$Survived == df$Survived)







