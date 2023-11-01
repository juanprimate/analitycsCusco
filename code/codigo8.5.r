my_df0<-read.csv('floresta.csv')

class(my_df0)
names(my_df0)
dim(my_df0)
#---vemos los na ou datos faltantes---#
#
colSums(is.na(my_df0))
#
which(colSums(is.na(my_df0))>0)

library(dplyr)

#TIPO_CAMBIO: eliminando tiene NAs
# Remove using subset
my_df <- subset(my_df0, select = -c(TIPO_CAMBIO))
colSums(is.na(my_df))
which(colSums(is.na(my_df))>0)
dim(my_df)

#en esta linea quitamos los na's
#en esta fila quitamos los na's
#eliminamos las filas que tienen datos faltantes
my_df<-subset(my_df, !is.na(EDAD))

# table(my_df$EDAD)
# Quick Examples
dim(my_df)
str(my_df)

#seleccionamos solo numericos para calcular los cuantiles
numeric_cols <- sapply(my_df, is.numeric)
#cuantiles

# solo numericos
my_df1 <- subset(my_df, select = numeric_cols)
str(my_df1)
#cuantiles
probs=c(0.01,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99)
out<-apply(my_df1, 2, quantile, probs = probs)
boxplot(out)

#IDPRESTAMO: eliminamos id es codigo de prestamo 
#no aporta informacion
my_df1 <- subset(my_df1, select = -c(IDPRESTAMO))
out<-apply(my_df1, 2, quantile, probs = probs)
boxplot(out)
summary(out)

#PERIODO: eliminamos es fecha de año y mes junto que no aporta nada
my_df1 <- subset(my_df1, select = -c(PERIODO))
out<-apply(my_df1, 2, quantile, probs = probs)
boxplot(out)

#which.max(subset(my_df1, select = EDAD))
min(my_df1[,'EDAD'])
max(my_df1[,'EDAD'])

output<-boxplot(as.numeric(my_df1$EDAD))
output[['out']]
#  [1]  78   1  79  91  79  79  81 104   0  79  82  80  79  81
#  t(subset(my_df1, select = EDAD))
hist(as.numeric(my_df1$EDAD))
abline(v=output[['out']])
# abline(v=c(78, 1, 79, 91, 79, 79, 81, 104, 0, 79, 82, 80, 79, 81))
 head(subset(my_df1, select = EDAD))
tail(subset(my_df1, select = EDAD))
###------------------------###
# hist(as.numeric(unlist(subset(my_df, select = -c(SALDO_VENCIDO)))))
# boxplot(as.numeric(unlist(subset(my_df, select = -c(SALDO_VENCIDO)))))

# SALDO_VENCIDO: valores extremos
# INTERES_DEVENGADO: valores extremos
# TOTAL_GARANTIAS: valores extremos

# hist(as.numeric(unlist(subset(my_df, select = -c(INTERES_DEVENGADO)))))
# boxplot(as.numeric(unlist(subset(my_df, select = -c(INTERES_DEVENGADO)))))

# hist(as.numeric(unlist(subset(my_df, select = -c(TOTAL_GARANTIAS)))))
# boxplot(as.numeric(unlist(subset(my_df, select = -c(TOTAL_GARANTIAS)))))
summary(my_df1$TOTAL_GARANTIAS)
table(my_df1$TOTAL_GARANTIAS)
head(my_df1$TOTAL_GARANTIAS)

# TOTAL_GARANTIAS: eliminamos porque tiene exceso de zeros 12138

my_df2 <- subset(my_df, select = -c(COD_AGE,NOMBRES,MONEDA,REPONSE_60,REPONSE_90))

# library(dplyr)
# View(my_df2)

# PAGARE: eliminamos porque no tenemos el tiempo de inicio ou punto de partida

# OTORGA: eliminamos pues tenemos multiples fechas

table(subset(my_df2, select = c(NOM_PRESTAMO)))

table(subset(my_df2, select = c(PRODUCTO)))

hist(as.numeric(unlist(subset(my_df2, select = c(MONTO_PRESTAMO)))))
out=boxplot(subset(my_df, select = c(MONTO_PRESTAMO)))

hist(as.numeric(unlist(subset(my_df2, select = c(SALDO_PRES)))))
#table(as.numeric(unlist(subset(my_df2, select = c(SALDO_PRES)))))
# SALDO_PRES
# lo dejamos con duda o para borrarlo despues

table(as.numeric(unlist(subset(my_df2, select = c(NRO_DIAS_ATRASADOS)))))
# NRO_DIAS_ATRASADOS
# lo dejamos con duda o para borrarlo despues

table(unlist(subset(my_df2, select = c(RIES_IND))))
# RIES_IND
# lo dejamos con duda o para borrarlo despues
table(unlist(subset(my_df2, select = c(RIES_GRUP))))

table(unlist(subset(my_df2, select = c(ANALISTA_ORIGINAL))))
dim(table(unlist(subset(my_df2, select = c(ANALISTA_ORIGINAL)))))
# ANALISTA_ORIGINAL: con duda para ser retirado

table(unlist(subset(my_df2, select = c(AMP_REF))))
#AMP_REF: no sabemos que es

boxplot(unlist(subset(my_df2, select = c(PROV_REQU))))
hist(unlist(subset(my_df2, select = c(PROV_REQU))))
#PROV_REQU: no sabemos que es

boxplot(unlist(subset(my_df2, select = c(SALDO_VENCIDO))))
hist(unlist(subset(my_df2, select = c(SALDO_VENCIDO))))
#SALDO_VENCIDO: (v. continua) no sabemos que es

boxplot(unlist(subset(my_df2, select = c(INTERES_DEVENGADO))))
hist(unlist(subset(my_df2, select = c(INTERES_DEVENGADO))))
#INTERES_DEVENGADO: (v. continua) no sabemos que es

head(subset(my_df2, select = c(TIPO_GAR)))
#TIPO_GAR: eliminamos pues tiene exceso de NA

table(subset(my_df2, select = c(FECHA_ULT_PAGO)))
# FECHA_ULT_PAGO: eliminamos pues no tenemos la diferencia de los tiempos

boxplot(unlist(subset(my_df2, select = c(PLAZO))))
# PLAZO: no sabemos que es

hist(unlist(subset(my_df2, select = c(PAGADAS))))
# PAGADAS: no sabemos que es (v. continua)

table(unlist(subset(my_df2, select = c(PENDIENTES))))
# PAGADAS: no sabemos que es (v. continua)

hist(unlist(subset(my_df2, select = c(ATRASADAS))))
# ATRASADAS: no sabemos que es (v. continua)

hist(unlist(subset(my_df2, select = c(TASA_INTERES))))
boxplot(unlist(subset(my_df2, select = c(TASA_INTERES))))
# TASA_INTERES: no sabemos que es (v. continua)

which(subset(my_df2,select= SEXO)=="")
out<-which(subset(my_df2,select= SEXO)=="")
# [1] 8507 8686
# subset(my_df2, SEXO!="F"&SEXO!="M")
my_df2<-subset(my_df2, SEXO=="F"| SEXO=="M")
# > dim(my_df2)
# [1] 12151    36
# > dim(my_df3)
# [1] 12149    36
names(my_df2)

hist(unlist(subset(my_df2, select = c(EDAD))))
boxplot(unlist(subset(my_df2, select = c(EDAD))))
outliers<-boxplot(subset(my_df2, select = c(EDAD)))[['out']]
my_df2<-subset(my_df2, !(EDAD %in% outliers))

table(subset(my_df2, select = c(DEPARTAMENTO)))
my_df2<-subset(my_df2, !(DEPARTAMENTO %in% ""))
# DEPARTAMENTO: (v. cualitativa)
#se deber quita AMAZONAS DE DEPARTAMENTO POR TENER 2 CASOS

table(subset(my_df2, select = c(DISTRITO)))
summary(subset(my_df2, select = c(DISTRITO)))
# DISTRITO: (v. cualitativa)

table(subset(my_df2, select = c(FREC_PAGO)))
head(subset(my_df2, select = c(FREC_PAGO)))

table(subset(my_df2, select = c(DESTINO)))
head(subset(my_df2, select = c(DESTINO)))
# DESTINO: (v. cualitativa)

table(subset(my_df2, select = c(CUOTA_MENSUAL)))
head(subset(my_df2, select = c(CUOTA_MENSUAL)))
# CUOTA_MENSUAL: (v. continua)

table(subset(my_df2, select = c(FORMA_PAGO)))
head(subset(my_df2, select = c(FORMA_PAGO)))
# FORMA_PAGO: (v. continua)
#  CUENTA DIRECTO 
#       1   12144 
# FORMA_PAGO: eliminamos pues no da informacion relevante (no tiene variabilidad)

table(subset(my_df2, select = c(ESTADO)))
head(subset(my_df2, select = c(ESTADO)))
# ESTADO: (v. cualitativa) nose que es

table(subset(my_df2, select = c(aporte)))
hist(unlist(subset(my_df2, select = c(aporte))))
summary(unlist(subset(my_df2, select = c(aporte))))
boxplot(subset(my_df2, select = c(aporte)))
# aporte: (v. continua) nose que es

table(subset(my_df2, select = c(MTOSLDOVIG)))
hist(unlist(subset(my_df2, select = c(MTOSLDOVIG))))
summary(unlist(subset(my_df2, select = c(MTOSLDOVIG))))
boxplot(subset(my_df2, select = c(MTOSLDOVIG)))
# MTOSLDOVIG: (v. continua) nose que es, pero es algo parecido a "aporte"

table(subset(my_df2, select = c(ACTIVIDAD_ECONOMICA)))
# hist(unlist(subset(my_df2, select = c(ACTIVIDAD_ECONOMICA))))
summary(unlist(subset(my_df2, select = c(ACTIVIDAD_ECONOMICA))))
# boxplot(subset(my_df2, select = c(ACTIVIDAD_ECONOMICA)))
head(subset(my_df2, select = c(ACTIVIDAD_ECONOMICA)))
# ACTIVIDAD_ECONOMICA: (v. cualitativa) esta muy correlacionada con 'DESTINO'

table(subset(my_df2, select = c(colocacion.bruta)))
hist(unlist(subset(my_df2, select = c(colocacion.bruta))))
summary(unlist(subset(my_df2, select = c(colocacion.bruta))))
boxplot(subset(my_df2, select = c(colocacion.bruta)))
head(subset(my_df2, select = c(colocacion.bruta)))
# colocacion.bruta: (v. continua) 
# valores atipicos muy altos observar despues

table(subset(my_df2, select = c(colocacion.neta)))
hist(unlist(subset(my_df2, select = c(colocacion.neta))))
summary(unlist(subset(my_df2, select = c(colocacion.neta))))
boxplot(subset(my_df2, select = c(colocacion.neta)))
head(subset(my_df2, select = c(colocacion.neta)))
# colocacion.neta: (v. continua) 
# valores atipicos muy altos observar despues

table(subset(my_df2, select = c(deduccion)))
hist(unlist(subset(my_df2, select = c(deduccion))))
summary(unlist(subset(my_df2, select = c(deduccion))))
boxplot(subset(my_df2, select = c(deduccion)))
head(subset(my_df2, select = c(deduccion)))
# deduccion: (v. continua) probablemente debemos remover pues tiene exceso de valores extremos 
# valores atipicos muy altos observar despues

names(my_df2)

############################
######################################
##############################################################################################################################################################################################################################################################################################################################################################################################################
out<-cor(my_df1)
dim(out)
heatmap(out, 
        annot = TRUE, # Add numeric annotations to the heat map
        symm = TRUE, # Make the heat map symmetric around 0
        main = "Correlation Matrix Heatmap")

# SALDO_VENCIDO con PROV_REQU 
# NRO_DIAS_ATRASADOS ou ATRASADAS
# PENDIENTES ou PLAZO
# colocacion.neta ou colocacion.bruta ou SALDO_PRES ou MONTO_PRESTAMO
# aqui ivamos jugar hacer varios modelos solo una de ellas ojo ojito

my_df3<-subset(my_df2, select = -c(IDPRESTAMO,
PERIODO,
TOTAL_GARANTIAS,
PAGARE,
OTORGA,
TIPO_GAR,
FECHA_ULT_PAGO,
FORMA_PAGO))
# View(my_df3)
dim(my_df3)


# install.packages('party')
#library('party')
set.seed(2023)
train <- sample(1:nrow(my_df3), 0.6*nrow(my_df3))
train_data <- my_df3[train, ]
temp <- setdiff(1:nrow(my_df3), train)
valid <- sample(temp, 0.2*nrow(my_df3))
valid_data <- my_df3[valid, ]
test <- setdiff(temp, valid)
test_data <- my_df3[test, ]
##############################################
##arboels de decicion
#library(caret)

#set.seed(2023)  # Set a seed for reproducibility
#train_indices <- createDataPartition(my_df3$REPONSE_30, p = 0.7, list = FALSE)
#train_data <- my_df3[train_indices, ]
#test_data <- my_df3[-train_indices, ]

#my_df3$REPONSE_30 <- as.factor(my_df3$REPONSE_30)
#my_df3$SEXO <- as.factor(my_df3$SEXO)

# Check for missing values in my_df3
#missing_values <- is.na(my_df3)

## Step 4: Train the decision tree model
#model <- rpart(REPONSE_30 ~ MONTO_PRESTAMO + SEXO, data = train_data, method = "class")

## Print the trained decision tree model
#print(model)
#library(rattle)
#library(rpart.plot)
#library(RColorBrewer)

#fancyRpartPlot(model, caption = NULL)
##############################################
names(my_df3)
my_df3$REPONSE_30 <- as.factor(my_df3$REPONSE_30)
my_df3$MONTO_PRESTAMO <- as.numeric(my_df3$MONTO_PRESTAMO)
my_df3$PRODUCTO <- as.factor(my_df3$PRODUCTO)
my_df3$RIES_IND <- as.factor(my_df3$RIES_IND)
my_df3$RIES_GRUP <- as.factor(my_df3$RIES_GRUP)
my_df3$AMP_REF <- as.factor(my_df3$AMP_REF)
my_df3$SEXO <- as.factor(my_df3$SEXO)
my_df3$DEPARTAMENTO <- as.factor(my_df3$DEPARTAMENTO)
my_df3$PROVINCIA <- as.factor(my_df3$PROVINCIA)
my_df3$FREC_PAGO <- as.factor(my_df3$FREC_PAGO)
my_df3$DESTINO <- as.factor(my_df3$DESTINO)
my_df3$ESTADO <- as.factor(my_df3$ESTADO)
my_df3$NOM_PRESTAMO<- as.factor(my_df3$NOM_PRESTAMO)


dim(table(my_df3$NOM_PRESTAMO))
str(my_df3)
my_df3<-subset(my_df3, select = -c(ANALISTA_ORIGINAL,DISTRITO,ACTIVIDAD_ECONOMICA))

out<-cor(my_df1)
dim(out)
heatmap(out, 
        annot = TRUE, # Add numeric annotations to the heat map
        symm = TRUE, # Make the heat map symmetric around 0
        main = "Correlation Matrix Heatmap")
#ANALISTA_ORIGINAL: eliminamos porque existe muchos analistas
#> dim(table(my_df3$ANALISTA_ORIGINAL))
#[1] 408
#DISTRITO: eliminamos porque existe muchos observaciones
#> dim(table(my_df3$DISTRITO))
#[1] 148

#ACTIVIDAD_ECONOMICA: eliminamos porque existe muchos observaciones
#> dim(table(my_df3$ACTIVIDAD_ECONOMICA)
#)
#[1] 267

# Select the input variables for the model
#input_vars <- c("REPONSE_30","TASA_INTERES","MONTO_PRESTAMO", "SALDO_PRES", "NRO_DIAS_ATRASADOS", "RIES_IND", "SEXO", "EDAD")

# Subset the dataframe to include only the selected variables
#data <- my_df3[, input_vars]
#str(data)

# Specify the target variable
#target <- as.factor(my_df3$REPONSE_30)
# Fit the decision tree model
#install.packages("rpart")
#library(rpart)
#tree_model <- rpart(target ~ ., data = data, method = "class")
#tree_model <- rpart(REPONSE_30 ~ ., data = my_df3, method = "class")
#names(tree_model)
#melhor_cp=tree_model$cptable[which.min(tree_model$cptable[,'xerror']),'CP']
#pfit=rpart::prune(tree_model,cp=melhor_cp)
#library(rpart.plot)
#rpart.plot(pfit)
#tree_model <- rpart(REPONSE_30~TASA_INTERES+MONTO_PRESTAMO+SALDO_PRES+NRO_DIAS_ATRASADOS+RIES_IND+SEXO+EDAD,data=data, method = "class")
#tree_model <- rpart(REPONSE_30~TASA_INTERES+MONTO_PRESTAMO,data=data, method = "class")


#valid_predictions <- predict(tree_model, data)
##############################################
#cran/randomForest
#install.packages("randomForest")
library(randomForest)
#table(train_data$REPONSE_30)
#write(my_df3)
#write.csv(my_df3, file = "my_df3.csv", row.names = FALSE)
set.seed(2023)
# formula=REPONSE_30~.
# readingSkills.cf <- randomForest(REPONSE_30~.,data = my_df3,ntree=100,importance=TRUE, proximity=TRUE)
# varImpPlot(readingSkills.cf)
# #tiene gini y acuracia

# model001 <- randomForest(REPONSE_30~.,data = my_df3,proximity=TRUE)
# model001
# varImpPlot(model001)


# distance.matrix <- dist(1-model001$proximity)
# mds.stuff <- cmdscale(distance.matrix,eig=TRUE,x.ret=TRUE)
# mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100,1)

##To do next meet: terminar estos tres código y quitar algunas variables ultimas

#https://www.youtube.com/watch?v=cw4qG9ePZ9Y
#Gini Index: The Gini index is a measure of impurity or node impurity used in random forest models. It is commonly used as the splitting criterion for building decision trees within the random forest algorithm. The Gini index measures how often a randomly selected element from a dataset would be incorrectly labeled if it were randomly labeled according to the distribution of labels in that subset. In the context of random forests, the Gini index is used to evaluate the quality of a split when constructing decision trees and selecting the most informative variables.
#Accuracy: Accuracy, on the other hand, is a measure of how well the model predicts the correct class labels for the given data. It is a common evaluation metric used in classification tasks. Accuracy is calculated by dividing the number of correctly predicted instances by the total number of instances in the dataset. It provides a general overview of the model's overall predictive performance.
#Buscar una tesis o ejemplo de analasis con Random Forest


my_df4<-subset(my_df3, select = -c(MONTO_PRESTAMO))
str(my_df4)
#NO ESTAMOS SEGUROS DE QUITAR O DEJARLO
#por mientras ejecutamos sin esa covariable pues despues ajustamos con ella (volvemos)

# SALDO_VENCIDO con PROV_REQU 
2
# NRO_DIAS_ATRASADOS ou ATRASADAS
2
# PENDIENTES ou PLAZO
2
# colocacion.neta ou colocacion.bruta ou SALDO_PRES ou MONTO_PRESTAMO
4


# readingSkills.cf4 <- randomForest(REPONSE_30~.,data = my_df4,ntree=100,importance=TRUE)
# varImpPlot(readingSkills.cf4)


###############################################################################
# # Split the data into training, validation, and test sets
# set.seed(2023)  # Set a seed for reproducibility
# train_ratio <- 0.6  # 60% for training
# val_ratio <- 0.2  # 20% for validation
# test_ratio <- 0.2  # 20% for testing

# Create indices for splitting the data


#install.packages("caret")
library("caret")
# #library(ggplot2)
# indices <- createDataPartition(data$REPONSE_30, p = train_ratio, times = 1)
# train_data <- data[indices[[1]], ]
# remaining_data <- data[-indices[[1]], ]

# indices <- createDataPartition(remaining_data$REPONSE_30, p = val_ratio/(1-train_ratio), times = 1)
# validation_data <- remaining_data[indices[[1]], ]
# test_data <- remaining_data[-indices[[1]], ]
data = my_df4
#--- presentacion ---#
#contraejemplo del modelo randomforest (acuracy=1) estaba incorrecto pues 
#presencia de exceso de ceros en algumas variables cuantitativas 
#Overfitting: An accuracy of 100% could be a sign of overfitting. Overfitting occurs when a model has learned the training data so well that it memorizes the training data points rather than generalizing from them. In such cases, the model may perform poorly on new, unseen data.

# Data Issues: Perfect accuracy might result from data issues, such as data leakage, data errors, or a too-small dataset. Ensure that your data is clean and correctly labeled.

# data = my_df4[,1:9]
# my_df4<-subset(my_df4, select = -c(deduccion,CUOTA_MENSUAL))

# my_df5<-subset(my_df4, select = -c(CUOTA_MENSUAL))

my_df5<-subset(my_df4, select = -c(deduccion,CUOTA_MENSUAL))

data=my_df5
################
train_indices <- sample(1:nrow(data), nrow(data) * 0.7)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]
################
rf_model<-randomForest(REPONSE_30~.,data = train_data,ntree=100,importance=TRUE, proximity=TRUE)
varImpPlot(rf_model)

output<-confusionMatrix(predictions, test_data$REPONSE_30)

output$overall["Accuracy"]
#########################
# Assuming your dataset is named 'data'
# Select only the numerical columns from the dataset
numerical_cols <- data[, sapply(data, is.numeric)]

# Create histograms for each numerical column
par(mfrow=c(5, 4))  # Adjust the layout for the number of columns you have
for (col in names(numerical_cols)) {
  hist(numerical_cols[, col], main=col, xlab=col)
}

filesSources<-list.files(path="/home/juan/Documentos/Estatcamp/estatcamp/EstatBinomialModel/EstatBinomialModel8/R", pattern=".R",all.files=TRUE,full.names=TRUE)

sapply(filesSources, source)
#######################
varY <-"REPONSE_30"
varResp <- "REPONSE_30"
	# form <- paste(paste(varX, collapse = "+"))
    # form <- paste(varResp, "~", paste(varX, collapse = "+"))
varX <- names(data)[names(data)!="REPONSE_30"]
form <- paste(paste(varX, collapse = "+"))
#####################
factor_columns <- sapply(data[,varX], is.factor)
# List the column names that are numeric
varCat <- names(factor_columns[factor_columns])
#####################
numeric_columns <- sapply(data[,varX], is.numeric)
# List the column names that are numeric
VarNum <- names(numeric_columns[numeric_columns])
################
varsTRV <- varCat

################
	novosDados <- NULL
	showPrev <- FALSE

	fLig <- "logit"
	pDadosCompletos <- TRUE
	varEnsaios <- NULL
	inter <- TRUE
	fOdds <- TRUE
	nDivs <- 15
	Residuo <- FALSE

	intercepto <- ""

	confLevel <- 0.95
	varFixas <- ""

	fPerformance <- fProbAd <- fTRV <- fHL <- fDev <- fPear <- fROC <- pCont <- TRUE
	plotRegFact <- plotCont <- plotContArea <- plotSuperf <- plotSuperf3D <- FALSE
	plotDisp <- TRUE
	residEscolha <- rep(TRUE,7)

	showPred <- showResiduos <- TRUE
	peso <- 1
	
	g <- 10
	corte <- 10
	input <- list(
		dados = data,
		form = form,
		varResp = varResp,
		varEnsaios = varEnsaios,
		fLig = fLig,
		confLevel = confLevel,
		intercepto = intercepto,
		fOdds = fOdds,
		fProbAd = fProbAd,
		showPrev = showPrev,
		novosDados = novosDados,
		fROC = fROC,
		fPerformance = fPerformance,
		corte = corte,
		fDev = fDev,
		fPear = fPear,
		fHL = fHL,
		g = g,
		fTRV = fTRV,
		varsTRV = varsTRV,
		pDadosCompletos = pDadosCompletos,
		Residuo = Residuo,
		varFixas = varFixas,
		varCat = varCat,
		varY = varY,
		varX = varX,
		plotRegFact = plotRegFact,
		upLim = NULL,
		lowLim = NULL,
		plotCont = plotCont,
		plotContArea = plotContArea,
		plotSuperf = plotSuperf,
		plotSuperf3D = plotSuperf3D,
		plotDisp = plotDisp,
		nDivs = nDivs)    
outgg=binomialModel(input)

	form2 = createFormula(pDadosCompletos, intercepto, varResp, form, varEnsaios)

    # Transforma a vari\u00E1vel categ\u00F3rica
	novo = novosDados
    if (!is.null(varCat)) {
        d<-dados
        dados=setCategoricalVars(d, varResp, varCat)
      if (showPrev) {
        d<-novosDados
        novosDados=setCategoricalVars(d, varResp, varCat)
      }
    }

# binomialModel <- function(input)
	# Calculo Modelo
	result = glmLog(dados=data, form2, fLig, confLevel=confLevel)
	modelo = result$modelo

barplot(table(data$deduccion == 0))

library(rpart)
library(rpart.plot)

outtree=rpart(REPONSE_30 ~ ., data = data)
prp(outtree)
par(mfrow=c(1, 1))
############################
# Fit a logistic regression model
model <- glm(REPONSE_30 ~ ., data = data, family = binomial)

# Summarize the model
summary(model)
###################
model <- glm(form11, data = dat, family = binomial)
#####################
######################
# data1=data.frame(ab=sample(c('a','b'),nrow(dat), replace = T, prob = NULL),sex=dat$Sex,survived=dat$Survived)
# glm(survived~ab+sex, data = data1, family = binomial)
# ######################
# data1=data.frame(ab=sample(c('a','b','c'),nrow(dat), replace = T, prob = NULL),sex=dat$Sex,survived=dat$Survived)
# glm(survived~ab+sex, data = data1, family = binomial)
# #delineamiento cuando tienes covariables categoricas 

##########################
# Split the data into a training set and a test set
set.seed(123)
train_indices <- sample(1:nrow(data), nrow(data) * 0.7)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Create a random forest model
rf_model <- randomForest(REPONSE_30 ~ ., data = train_data, ntree = 100)
# rf_model<-randomForest(REPONSE_30~.,data = train_data,ntree=100,importance=TRUE, proximity=TRUE)

varImpPlot(rf_model)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_data)

# Create a confusion matrix
confusion_matrix <- table(predictions, test_data$REPONSE_30)
print(confusion_matrix)
output<-confusionMatrix(predictions, test_data$REPONSE_30)

output$overall["Accuracy"]
# write.csv(my_df4, file = "my_df4.csv", row.names = FALSE)

###################
# readingSkills.cf4 <- randomForest(REPONSE_30~.,data = my_df4,ntree=100,importance=TRUE)
# varImpPlot(readingSkills.cf4)

# Tune hyperparameters using the validation set (e.g., adjust the number of trees or other parameters)
# Example: Finding the optimal number of trees
n_trees <- c(50, 100, 150, 200)  # Try different numbers of trees
accuracy <- numeric(length(n_trees))

for (i in 1:length(n_trees)) {
  rf_model <- randomForest(REPONSE_30 ~ ., data = train_data, ntree = n_trees[i])
  predictions <- predict(rf_model, newdata = validation_data)
  accuracy[i] <- confusionMatrix(predictions, validation_data$REPONSE_30)$overall["Accuracy"]
}
accuracy

# Find the best number of trees based on accuracy
best_n_trees <- n_trees[which.max(accuracy)]
best_rf_model <- randomForest(REPONSE_30 ~ ., data = train_data, ntree = best_n_trees)

# Evaluate the final model using the test set
test_predictions <- predict(best_rf_model, newdata = test_data)
test_accuracy <- confusionMatrix(test_predictions, test_data$REPONSE_30)$overall["Accuracy"]
test_accuracy
#https://www.r-bloggers.com/2019/03/shapper-is-on-cran-its-an-r-wrapper-over-shap-explainer-for-black-box-models/

# REALIZAR EL SHAPPER
# ESTA EN R PERO LLAMA A PYTHON AL FINAL Y NO SABEMOS QUE M.... HACE

# shapper::shap
# ESTUDIAR LA TEORIA DE SHAPPER

# https://proceedings.neurips.cc/paper_files/paper/2017/file/8a20a8621978632d76c43dfd28b67767-Paper.pdf

# https://github.com/sebas-prog/Econometria-/blob/main/proyecto_de_enaho.ipynb
#en python

# https://www.youtube.com/watch?v=v6VJ2RO66Ag
# para la presentacion
# haremos regression logistica para presentar al loco  
