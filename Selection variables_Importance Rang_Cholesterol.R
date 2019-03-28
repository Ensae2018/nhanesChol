#####################################
#Selection des variables#############
#####################################
library(glmnet)#contrainte
library(randomForest)#Foret
library(gbm)#Boosting
library(e1071)#SVM
library(rpart)#Arbre
library(plyr)
library(caret)#pour l'utilisation des fonctions Caret
library(dplyr)

# Jdd Choix 2
#don <- read.csv("data chol/JddChol_Choix2_MICE_14012019.csv")
don <- read.csv("data chol/JddChol_Choix2_MICEsansSEQN.csv")
#donS <- read.csv("data chol/JddChol_Choix2_MICEavecSEQN.csv")

#don$X.1 <- NULL
don$X <- NULL
#donS$X <- NULL

unique(don$Y) #il y a des 1,2 et 9
unique(don$HOQ065_hoq)
#unique(donS$Y)

#don[don$nhanes.y=="2",92]<-c("0")
#don[don$nhanes.y=="1",92]<-c("1")
#don[don$nhanes.y=="9",92]<-c("0")

#unique(don$nhanes.y)

# Transformer toutes les variables avec dif levels en FACTOR
#RIDSTATR_demo  : Factor w/ 1 level "2": 1
#don$RIDSTATR_demo <- as.factor(don$RIDSTATR_demo)
don$RIAGENDR_demo <- as.factor(don$RIAGENDR_demo)
#don$INDHHIN2_demo <- as.factor(don$INDHHIN2_demo)
#don$INDFMIN2_demo <- as.factor(don$INDFMIN2_demo)
don$BPQ020_bpq <- as.factor(don$BPQ020_bpq)
don$DIQ010_diq <- as.factor(don$DIQ010_diq)
don$HEQ010_heq <- as.factor(don$HEQ010_heq)
don$HEQ030_heq <- as.factor(don$HEQ030_heq)
don$HOQ065_hoq <- as.factor(don$HOQ065_hoq)
don$HIQ011_hiq <- as.factor(don$HIQ011_hiq)
#don$IMQ020_imq <- as.factor(don$IMQ020_imq)
#don$IMQ011_imq <- as.factor(don$IMQ011_imq)
don$MCQ010_mcq <- as.factor(don$MCQ010_mcq)
don$MCQ080_mcq <- as.factor(don$MCQ080_mcq)
don$OHAREC_ohxref <- as.factor(don$OHAREC_ohxref)
don$PAQ605_paq <- as.factor(don$PAQ605_paq)
don$PAQ620_paq <- as.factor(don$PAQ620_paq)
don$PAQ635_paq <- as.factor(don$PAQ635_paq)
don$PAQ650_paq <- as.factor(don$PAQ650_paq)
don$PAQ665_paq <- as.factor(don$PAQ665_paq)
don$SLQ050_slq <- as.factor(don$SLQ050_slq)
#DRABF_dr1tot   : Factor w/ 1 level "2": 1
#don$DRABF_dr1tot <- as.factor(don$DRABF_dr1tot)
don$DRQSDIET_dr1tot <- as.factor(don$DRQSDIET_dr1tot)

str(don)

don$Y <- as.numeric(don$Y)
str(don) # avant cetait INT

write.csv(don,"data chol/JddChol_Choix2_MICEsansSEQNavecFactorpourImp.csv")

# # Transformer toutes les variables avec dif levels en FACTOR
# #RIDSTATR_demo  : Factor w/ 1 level "2": 1
# #donS$RIDSTATR_demo <- as.factor(donS$RIDSTATR_demo)
# donS$RIAGENDR_demo <- as.factor(donS$RIAGENDR_demo)
# #donS$INDHHIN2_demo <- as.factor(donS$INDHHIN2_demo)
# #donS$INDFMIN2_demo <- as.factor(donS$INDFMIN2_demo)
# donS$BPQ020_bpq <- as.factor(donS$BPQ020_bpq)
# donS$DIQ010_diq <- as.factor(donS$DIQ010_diq)
# donS$HEQ010_heq <- as.factor(donS$HEQ010_heq)
# donS$HEQ030_heq <- as.factor(donS$HEQ030_heq)
# donS$HOQ065_hoq <- as.factor(donS$HOQ065_hoq)
# donS$HIQ011_hiq <- as.factor(donS$HIQ011_hiq)
# donS$IMQ020_imq <- as.factor(donS$IMQ020_imq)
# #donS$IMQ011_imq <- as.factor(donS$IMQ011_imq)
# donS$MCQ010_mcq <- as.factor(donS$MCQ010_mcq)
# donS$MCQ080_mcq <- as.factor(donS$MCQ080_mcq)
# donS$OHAREC_ohxref <- as.factor(donS$OHAREC_ohxref)
# donS$PAQ605_paq <- as.factor(donS$PAQ605_paq)
# donS$PAQ620_paq <- as.factor(donS$PAQ620_paq)
# donS$PAQ635_paq <- as.factor(donS$PAQ635_paq)
# donS$PAQ650_paq <- as.factor(donS$PAQ650_paq)
# donS$PAQ665_paq <- as.factor(donS$PAQ665_paq)
# donS$SLQ050_slq <- as.factor(donS$SLQ050_slq)
# #DRABF_dr1tot   : Factor w/ 1 level "2": 1
# #donS$DRABF_dr1tot <- as.factor(donS$DRABF_dr1tot)
# donS$DRQSDIET_dr1tot <- as.factor(donS$DRQSDIET_dr1tot)
# 
# str(donS)
# 
# donS$Y <- as.numeric(donS$Y)
# str(donS) # avant cetait INT

#write.csv(donS,"data chol/JddChol_Choix2_MICEavecSEQNavecFactordansImp.csv")

XX <- as.matrix(model.matrix(~.,don)[,-ncol(model.matrix(~.,don))])
YY <- as.matrix(model.matrix(~.,don)[,ncol(model.matrix(~.,don))])

# Création de la fonction pour calculer l'importance des variables
# x est le modele; k est le nombre de variable => de 10 à 15
# valeur de t: (1 pour glm et randomforest, 2 pour glmnet, 3 pour gbm)
variable_imp <- function(x,k=15,t=1,mot=""){
  switch(t,
         x <- varImp(x), #on utilise varImp de Caret pour glm et randomforest
         x <- as.data.frame(as.matrix(x)), # utile pour mise au format dataframe glmnet
         x[,1] <- NULL # utile pour enlever une colonne en trop pour gbm
  )
  tempo <- cbind(row.names(x),x)
  row.names(tempo) <- NULL
  colnames(tempo)[1] <- "variable"
  colnames(tempo)[2] <-  "importance"
  tempo[,1] <- gsub("YES$","",tempo[,1],ignore.case = TRUE) #A VERIFIER POUR CHOL (ex. VAR Travail Oui, SLQ 9, ...)
  tempo[,1] <- gsub("NO$","",tempo[,1],ignore.case = TRUE)
  tempo[,2] <- (tempo[,2]-mean(tempo[,2]))/sqrt(var(tempo[,2])) # on centre réduit
  tempo <- arrange(tempo, desc(importance))[1:k,]
  colnames(tempo)[2] <-  paste("imp",mot,sep = "_")
  tempo$rank <- seq(1:k)
  colnames(tempo)[3] <- paste("rang",mot,sep = "_")
  return(tempo)
}

# 1)Importance variable pour le modele logistique
mod_log <- glm(Y~.,data=don,family="binomial")
varimplog <- variable_imp(x=mod_log,t=1,mot="log")

# 2)Importance variable pour le modele ridge
tmp <- cv.glmnet(XX,YY,alpha=0,family="binomial")
mod_ridge  <- glmnet(XX,YY,alpha=0,lambda=tmp$lambda.min, family="binomial")
#varimpridge <- variable_imp(x=mod_ridge$beta,t=2,mot="ridge") #ajouter abs
varimpridge <- variable_imp(x=abs(mod_ridge$beta),t=2,mot="ridge") #ajouter abs

# 3)Importance variable pour le modele lasso (colnames(XX)[mod_lasso$beta@i])
tmp <- cv.glmnet(XX,YY, alpha=1, family="binomial")
mod_lasso <- glmnet(XX,YY,alpha=1, lambda =tmp$lambda.1se,family="binomial" )
#varimplasso <- variable_imp(x=mod_lasso$beta,t=2, mot="lasso") #ajouter abs
varimplasso <- variable_imp(x=abs(mod_lasso$beta),t=2, mot="lasso") #ajouter abs

# 4)Importance variable pour le modele elastic
tmp <- cv.glmnet(XX,YY, alpha=0.5, family="binomial")
mod_elastic <- glmnet(XX,YY,alpha = 0.5, lambda = tmp$lambda.min, family="binomial")
#varimpelastic <- variable_imp(mod_elastic$beta,t=2,mot = "elastic") #ajouter abs
varimpelastic <- variable_imp(abs(mod_elastic$beta),t=2,mot = "elastic") #ajouter abs

# Y factor necessaire pour la méthode FORET
don2<-don
don2$Y<-factor(don2$Y)

# 5)Importance variable pour le modele Foret => MODIF AS FACTOR
mod_foret <- randomForest(Y~., data = don2, ntree=500)
varimpforet <- variable_imp(mod_foret,t=1,mot="foret")

# 6)Importance variable pour le modele adaboost => MODIF SANS NUMERIC (DEJA FAIT), et SANS -1
tmp <- gbm(Y~.,data = don, distribution = "adaboost", interaction.depth = 2,
           shrinkage = 0.1,n.trees = 500)
M <- gbm.perf(tmp)[1]
mod_adaboost <- gbm(Y~.,data = don, distribution = "adaboost", interaction.depth = 2,
                    shrinkage = 0.1,n.trees = M)
varimpada <- variable_imp(summary(mod_adaboost),t=3, mot="adaboost")

# 7)Importance variable pour le modele logiboost => MODIF SANS NUMERIC (DEJA FAIT), et SANS -1
tmp <- gbm(Y~.,data=don, distribution="bernoulli", interaction.depth = 2,
           shrinkage=0.1,n.trees=500)
M <- gbm.perf(tmp)[1]
mod_logiboost <- gbm(Y~.,data=don, distribution="bernoulli", interaction.depth = 2,
                     shrinkage=0.1,n.trees=M)
varimplogibo <- variable_imp(summary(mod_logiboost),t=3,mot="logiboost")

# ?)Importance variable pour le modele SVM (je ne sais pas appliquer la feature selection) => CA NE MARCHE PAS
# mod_svm <- svm(Y~.,data=don, kernel="linear",probability=T)
# tmp <- tune(svm,Y~.,data=don, kernel="linear",probability=T,range=list(cost=c(0.1,1,10)))
# mod <- tmp$best.model

# Croisement des tables d'importance des variables
choix_var <- varimplog %>%
  full_join(varimpridge) %>%
  full_join(varimplasso) %>%
  full_join(varimpelastic) %>%
  full_join(varimpforet) %>%
  full_join(varimpada) %>%
  full_join(varimplogibo)

choix_var <- cbind(choix_var[,1],choix_var[,c(which(grepl("^imp",names(choix_var))))])

choix_var <- as.data.frame(choix_var)
names(choix_var)[1] <- "variable"
write.csv2(choix_var,"data chol/choix_var_chol.csv",row.names = FALSE)

# choix_var_chol<-read.table("data chol/choix_var_chol.csv", header=T, sep=";",dec=".",row.names = NULL)
# str(choix_var_chol)
# choix_var_chol$imp_log <- as.numeric(choix_var_chol$imp_log)
# choix_var_chol$imp_ridge <- as.numeric(choix_var_chol$imp_ridge)
# choix_var_chol$imp_lasso <- as.numeric(choix_var_chol$imp_lasso)
# choix_var_chol$imp_elastic <- as.numeric(choix_var_chol$imp_elastic)
# choix_var_chol$imp_foret <- as.numeric(choix_var_chol$imp_foret)
# choix_var_chol$imp_adaboost <- as.numeric(choix_var_chol$imp_adaboost)
# choix_var_chol$imp_logiboost <- as.numeric(choix_var_chol$imp_logiboost)
# str(choix_var_chol)
# write.csv2(choix_var_chol,"data chol/choix_var_chol_num.csv",row.names = FALSE)
# 
# choix_var_hyp<-read.table("choix_var_hyp.csv", header=T, sep=";",dec=".",row.names = NULL)
# str(choix_var_hyp)
