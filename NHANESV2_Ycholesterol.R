
####################################################################################################################################
## NHANES 2015-2016 Questionnaire Data : BPQ_I
## BPQ080 - Doctor told you - high cholesterol level
##
##  {Have you/Has SP} ever been told by a doctor or other health professional that {your/his/her} blood cholesterol level was high?
##  Target: Both males and females 16 YEARS - 150 YEARS
##  1 = YES, 0 = NO, 7 refused, 9 don't know, NA 0
####################################################################################################################################

library(data.table)

##############################################################################################################################
##
## CREATION JEUX DE DONNEES POUR PREDIRE LE CHOLESTEROL
##
##############################################################################################################################

##########################################################################################
## ON PART de nhanes_29122018.csv (Jdd d'entr?e travaill? par JV)
##########################################################################################

nhanes <- read.csv("data init/nhanes_16012019.csv", sep = ",") # version 29122018 + 3 variables plomb dans le sang
#nhanes <- read.csv("data init/nhanes_29122018.csv", sep = ",") 

class(nhanes)
dim(nhanes)#8339 indiv et 147=>151 variables (+1 X)
colnames(nhanes)
str(nhanes)
summary(nhanes)
summary(nhanes$BPQ080_bpq) #mon Y cholesterol est d??j?? disponible : 2758 NA

unique(nhanes$BPQ080_bpq)

nhanesdt <- as.data.table(nhanes)
nhanesdt[BPQ080_bpq==1,.N,] #1767
nhanesdt[BPQ080_bpq==2,.N,] #3778
nhanesdt[BPQ080_bpq==9,.N,] #36
apply(nhanesdt,2,function (x) {sum(is.na(x))}) #2758

# renommer les 2 variables (oubli?es par JV) avec _dr1tot pour faciliter la transco ? posteriori : DR1TFIBE et DR1TATOC
colnames(nhanes)
names(nhanes)[92] <- "DR1TFIBE_dr1tot"
names(nhanes)[98] <- "DR1TATOC_dr1tot"
colnames(nhanes)

#########################################################################################
## CRITERE N?1 : garder les individus SEQN avec mon Y Cholesterol sans NA
#########################################################################################

# Creer le vecteur des individus qui n'ont pas repondu ?? la question d'cholesterol
ind_a_enlever <- which(is.na(nhanes$BPQ080_bpq))

# enlever ces individus de la table nhanes
nhanes <- nhanes[-ind_a_enlever,]
dim(nhanes)#5581x151 => OK : 8339-2758

######################################################################################################
## DIFFERENTS CRITERES METIERS : pour la r?duction des variables et la cr?ation de nouvelles variables
######################################################################################################

# analyse du crit?re Age : bcp des NA pour les jeunes ?
min(nhanes$RIDAGEYR_demo) #L'age commence ? 16 ans pq notre Y BPQ080 est collect?e pour les indiv entre 16 et 150 ans
temp <- subset(nhanes, RIDAGEYR_demo %in% c(16,17) )
apply(temp,2,function (x) {sum(is.na(x))}) #somme des NA par variable
sum(is.na(subset(nhanes, RIDAGEYR_demo %in% c(16,17) ))) #somme totale des NA
options(max.print=1000000) #pour afficher les NA dans le suivant summary
summary(temp)
# Et en plus, les NA entre 16 et 17 ans ne sont pas ?normes => conclusion : pas de filtre selon l'age (idem pour BPQ020) ? diff?rence de diab?tes

# conversion en data.table pour pouvoir int??grer le code de cr?ation et r?duction de variables de JV
nhanes <- as.data.table(nhanes)

# reduction du nombre de level de la variable education
nhanes[DMDEDUC2_demo %in% c("1","2","3"),Var_EDUCATION:="secondaire",]
nhanes[DMDEDUC2_demo %in% c("4","5"),Var_EDUCATION:="superieur",]
nhanes[is.na(DMDEDUC2_demo) | DMDEDUC2_demo %in% c("7","9"),Var_EDUCATION:=NA,]
nhanes[,c("DMDEDUC2_demo"):=NULL,]
dim(nhanes)#5581x148 (1 variable remplac?e par une nouvelle variable)

# reduction du nombre de level de la variable emploi
nhanes[OCD150_ocq %in% c("1","2"),Var_TRAVAIL:="oui",]
nhanes[OCD150_ocq %in% c("3", "4","7","9") | is.na(OCD150_ocq),Var_TRAVAIL:="non",]
nhanes[,c("OCD150_ocq"):=NULL,]
dim(nhanes)#5581x148

# reduction du nombre de variable li??e ?? la drogue
nhanes[DUQ200_duq=="1" | DUQ240_duq=="1" | DUQ370_duq=="1",Var_DROGUE:="oui",]
nhanes[!(DUQ200_duq=="1" | DUQ240_duq=="1" | DUQ370_duq=="1"),Var_DROGUE:="non",]
nhanes[,c("DUQ200_duq","DUQ240_duq","DUQ370_duq"):=NULL,]
dim(nhanes)#5581x146 (3 variables remplac?es par une nouvelle variable)

# reduction du nombre de level de la variable depression
nhanes[DPQ020_dpq=="0",Var_DEPRESSION:="non",]
nhanes[DPQ020_dpq %in% c("1","2","3"),Var_DEPRESSION:="oui",]
nhanes[DPQ020_dpq %in% c("7","9") | is.na(DPQ020_dpq),Var_DEPRESSION:=NA, ]
nhanes[,c("DPQ020_dpq"):=NULL,]
dim(nhanes)#5581x146

# reduction du nombre de level de la variable status marital
nhanes[DMDMARTL_demo %in% c("1","6"),Var_SITUATION:="couple",]
nhanes[DMDMARTL_demo %in% c("2","3","4","5"),Var_SITUATION:="seul",]
nhanes[DMDMARTL_demo %in% c("77","99") | is.na(DMDMARTL_demo),Var_SITUATION:=NA,]
nhanes[,c("DMDMARTL_demo"):=NULL,]
dim(nhanes)#5581x146

# traitement de la table alq (alcool)
apply(nhanes[,.(ALQ120Q_alq,ALQ120U_alq,ALQ130_alq),],2,function (x) sum(is.na(x)))

nhanes[ALQ120U_alq=="1",temp:=52,] #semaines dans l'ann?e
nhanes[ALQ120U_alq=="2",temp:=12,] #mois dans l'ann?e
nhanes[ALQ120U_alq=="3",temp:=1,]

nhanes[!(is.na(ALQ120Q_alq) | ALQ120Q_alq=="777" | ALQ120Q_alq=="999" | is.na(ALQ130_alq) | ALQ130_alq=="777" | ALQ130_alq=="999"),Var_ALQ:=ALQ120Q_alq*temp*ALQ130_alq,]
nhanes[,c("temp","ALQ120Q_alq","ALQ120U_alq","ALQ130_alq"):=NULL,]
dim(nhanes)#5581x144
# REFAIRE avec le code DPLYR d'Insaf !

# analyse de la mesure du Cholesterol : DR1TCHOL
summary(nhanes$DR1TCHOL_dr1tot) #BONNE NOUVELLE : 0 NA!!

# j'enl?ve les individus avec un r?gime low Cholesterol car l'alimentation est modifi?e dans ce cas(DRQSDT2_dr1tot=2)
summary(nhanes$DRQSDT2_dr1tot)  #5470 NA => pas grande chose ? supprimer sur les 5581 individus
nhanes[DRQSDT2_dr1tot=="2",.N,] #111 indiv avec cette di?te!! 
nhanes<-nhanes[is.na(DRQSDT2_dr1tot),,]
dim(nhanes)#5470x144

# traitement de la table smq
nhanes[SMQ040_smq %in% c("1","2"),Var_FUMEUR:="oui",]
nhanes[SMQ040_smq %in% c("3"),Var_FUMEUR:="non",]
nhanes[,c("SMQ040_smq"):=NULL,]
nhanes$Var_FUMEUR<-factor(nhanes$Var_FUMEUR)
table(nhanes$Var_FUMEUR,useNA="always")
dim(nhanes)#5470x144

# traitement de la table smqfam
nhanes[SMD460_smqfam=="0",Var_COFUMEUR:="non",]
nhanes[SMD460_smqfam %in% c("1","2","3"),Var_COFUMEUR:="oui",]
nhanes[,c("SMD460_smqfam"):=NULL,]
nhanes$Var_COFUMEUR<-factor(nhanes$Var_COFUMEUR)
table(nhanes$Var_COFUMEUR,useNA="always")
dim(nhanes)#5470x144

# exploration des variables du nombre des personnes qui vivent dans la famille ou le household
nhanes[DMDHHSIZ_demo!=DMDFMSIZ_demo,.(DMDHHSIZ_demo,DMDFMSIZ_demo),]
nhanes[DMDFMSIZ_demo>DMDHHSIZ_demo,.N,]
dim(nhanes)#5470x144

#traitement de l'argent destin? ? la consommation
nhanes[,Var_ARGENTALIM:=(CBD071_cbq+CBD111_cbq+CBD121_cbq+CBD131_cbq),]
nhanes[,c("CBD071_cbq","CBD111_cbq","CBD121_cbq","CBD131_cbq","CBD091_cbq"):=NULL,]
nhanes[is.na(Var_ARGENTALIM),.N,]
dim(nhanes)#5470x140 => 5470x143

# traitement des tensions art?rielles : pas appliqu?, je garde les 3 variables BPXSY1_bpx+BPXSY2_bpx+BPXSY3_bpx
# traitement de suppression manuelle de variables : pas appliqu?, on attend voir les supressions du 10% NA


#traitement des variables avec levels 7 et 9 (refused or dont know) : remarque avec varImp package caret
summary(nhanes$SLQ050_slq)
summary(nhanes$PAQ635_paq)
summary(nhanes$PAQ650_paq)
summary(nhanes$PAQ605_paq)
summary(nhanes$PAQ620_paq)
summary(nhanes$HOQ065_hoq)
summary(nhanes$HIQ011_hiq)
summary(nhanes$IMQ011_imq)
summary(nhanes$HEQ030_heq)

nhanesdt[SLQ050_slq==9,.N,] 
nhanesdt[PAQ635_paq==9,.N,] 
nhanesdt[PAQ650_paq==9,.N,] 
nhanesdt[PAQ605_paq==9,.N,] 
nhanesdt[PAQ620_paq==9,.N,] 
nhanesdt[HOQ065_hoq==9,.N,] 
nhanesdt[HIQ011_hiq==7,.N,] 
nhanesdt[IMQ011_imq==7,.N,] 
nhanesdt[HEQ030_heq==9,.N,] 

#SLQ050_slq9
nhanes[SLQ050_slq %in% c("7","9") | is.na(SLQ050_slq),SLQ050_slq:=NA,]
#PAQ635_paq9
nhanes[PAQ635_paq %in% c("7","9") | is.na(PAQ635_paq),PAQ635_paq:=NA,]
#PAQ650_paq9
nhanes[PAQ650_paq %in% c("7","9") | is.na(PAQ650_paq),PAQ650_paq:=NA,]
#PAQ605_paq9
nhanes[PAQ605_paq %in% c("7","9") | is.na(PAQ605_paq),PAQ605_paq:=NA,]
#PAQ620_paq9
nhanes[PAQ620_paq %in% c("7","9") | is.na(PAQ620_paq),PAQ620_paq:=NA,]
#HOQ065_hoq9 
nhanes[HOQ065_hoq %in% c("7","9") | is.na(HOQ065_hoq),HOQ065_hoq:=NA,]
#HIQ011_hiq7
nhanes[HIQ011_hiq %in% c("7","9") | is.na(HIQ011_hiq),HIQ011_hiq:=NA,]
#IMQ011_imq7
nhanes[IMQ011_imq %in% c("7","9") | is.na(IMQ011_imq),IMQ011_imq:=NA,]
#HEQ030_heq9
nhanes[HEQ030_heq %in% c("7","9") | is.na(HEQ030_heq),HEQ030_heq:=NA,]

# INDFMIN2_demo9 => pas 99
nhanes[INDFMIN2_demo %in% c("77","99") | is.na(INDFMIN2_demo),INDFMIN2_demo:=NA,]

# INDHHIN2_demo99
nhanes[INDHHIN2_demo %in% c("77","99") | is.na(INDHHIN2_demo),INDHHIN2_demo:=NA,]

# MCQ010_mcq9
nhanes[MCQ010_mcq %in% c("7","9") | is.na(MCQ010_mcq),MCQ010_mcq:=NA,]

#BPQ020_bpq9
nhanes[BPQ020_bpq %in% c("7","9") | is.na(BPQ020_bpq),BPQ020_bpq:=NA,]

#DIQ010_diq
nhanes[DIQ010_diq %in% c("7","9") | is.na(DIQ010_diq),DIQ010_diq:=NA,]

#MCQ080_mcq
nhanes[MCQ080_mcq %in% c("7","9") | is.na(MCQ080_mcq),MCQ080_mcq:=NA,]

#DRQSDIET_dr1tot
nhanes[DRQSDIET_dr1tot %in% c("7","9") | is.na(DRQSDIET_dr1tot),DRQSDIET_dr1tot:=NA,]

#HEQ010_heq
nhanes[HEQ010_heq %in% c("7","9") | is.na(HEQ010_heq),HEQ010_heq:=NA,]

#HOD050_hoq
nhanes[HOD050_hoq %in% c("777","999") | is.na(HOD050_hoq),HOD050_hoq:=NA,]

#IMQ020_imq
nhanes[IMQ020_imq %in% c("7","9") | is.na(IMQ020_imq),IMQ020_imq:=NA,]

#PAQ665_paq
nhanes[PAQ665_paq %in% c("7","9") | is.na(PAQ665_paq),PAQ665_paq:=NA,]

#PAD680_paq
nhanes[PAD680_paq %in% c("7777","9999") | is.na(PAD680_paq),PAD680_paq:=NA,]

##################################################################################################
## DERNIER CRITERE : la suppression des variables X avec un nbr de NA sup?rieur ? 10 %
##################################################################################################

nhanes <- as.data.frame(nhanes)

########
# Choix 2
########

# reperer les variable qui ne depassent pas les 10% de NA's (Yfan l'a appliqu? pour 5%)
var_na_acceptable <- labels(which(apply(nhanes,2,function (x) sum(is.na(x)))<=floor(dim(nhanes)[1]/10))) 
which(apply(nhanes,2,function (x) sum(is.na(x)))<=floor(dim(nhanes)[1]/10)) #93 variables

# un petit topo des variables que finalement nous allons supprimer
var_na_pasacceptable <- labels(which(apply(nhanes,2,function (x) sum(is.na(x)))>floor(dim(nhanes)[1]/10))) 
which(apply(nhanes,2,function (x) sum(is.na(x)))>floor(dim(nhanes)[1]/10))  #47 variables
# si on applique 5% seulement INDFMPIR_demo (variable seuil de pauvret? de la famille) est supprim?e en plus

# Choix 2 : VARIABLES SUPPRIMEES :

#  DMDEDUC3_demo   INDFMPIR_demo     
# => education before 19ans et poverty

#  BMDBMIC_bmx      BPXSY4_bpx      BPXDI4_bpx      
#  DIQ160_diq      ECD010_ecq      ECQ020_ecq 
#  ECD070B_ecq     MCQ160A_mcq     MCQ160N_mcq     MCQ160B_mcq     MCQ160C_mcq     MCQ160D_mcq     MCQ160E_mcq     MCQ160F_mcq 
#  MCQ160G_mcq     MCQ160M_mcq     MCQ160K_mcq     MCQ160L_mcq      MCQ220_mcq     MCQ230A_mcq     MCQ230B_mcq     MCQ230C_mcq 
#  MCQ230D_mcq      OCQ180_ocq      SMD030_smq   SMQ720_smqrtu   
# => pressure, body, medical conditions and smoking

#  DBD100_dr1tot  DRQSDT1_dr1tot  DRQSDT2_dr1tot  DRQSDT3_dr1tot DRQSDT4_dr1tot  DRQSDT5_dr1tot  DRQSDT6_dr1tot  
#  DRQSDT7_dr1tot  DRQSDT8_dr1tot  DRQSDT9_dr1tot DRQSDT10_dr1tot DRQSDT11_dr1tot DRQSDT12_dr1tot DRQSDT91_dr1tot   
# =>  sel et diètes

#  Var_EDUCATION      Var_DROGUE  Var_DEPRESSION   Var_SITUATION         Var_ALQ      Var_FUMEUR 
# => ... Alcool

summary(nhanes$LBXBPB)
summary(nhanes$LBDBPBSI)
summary(nhanes$LBDBPBLC) #2864NA pour les 3var

# netoyage de nhanes pour ne garder que les variables moins de 10% de NA's
dim(nhanes) #5470x140 => 143
nhanes <- nhanes[,var_na_acceptable]
dim(nhanes) #5470x93

###################################################################################################
## PRODUIRE LE JDD FINAL qu'on utilisera pour chercher le meilleur mod?le de pr?diction 
###################################################################################################

# enlever la champs "X" qui ne sert ?? rien
nhanes$X <- NULL
dim(nhanes)
str(nhanes)

# renommer la variable d'etude en y -> cholesterol = BPQ080
#colnames(nhanes)[grep("BPQ080*",colnames(nhanes))] <- "y"
names(nhanes)[14] <- "Y" #ATTENTION

# deplacer le y en dernier
#nhanes <- cbind(subset(nhanes,select=-y),nhanes$y)
nhanes <- cbind(subset(nhanes,select=-Y),nhanes$Y)

summary(nhanes)
str(nhanes)
uniqueN(nhanes$`nhanes$Y`)

nhanesdt <- as.data.table(nhanes)
str(nhanesdt)
uniqueN(nhanesdt$`nhanes$Y`)

nhanesdt[`nhanes$Y`==1,.N,] #1699
nhanesdt[`nhanes$Y`==2,.N,] #3735
nhanesdt[`nhanes$Y`==9,.N,] #36
apply(nhanesdt,2,function (x) {sum(is.na(x))}) 

# le fichier de sortie 
write.csv(nhanes,"data chol/JddChol_Choix2_14012019.csv")

####################################################################################################
## LUI APPLIQUER LA TRANSCODIFICATION D'Yfan
####################################################################################################



####################################################################################################
## LUI APPLIQUER LA METHODE MICE D'IMPUTATION DES NAs
####################################################################################################
library(mice)
library(VIM)
library(data.table)

don <- read.csv("data chol/JddChol_Choix2_14012019.csv", sep = ",")
#don <- read.csv("data chol/JddChol_Choix2_14012019_old.csv", sep = ",")

apply(don,2,function (x) {sum(is.na(x)) })

table(apply(don,2,function (x) {sum(is.na(x))})>0)
colnames(don)

# COMMENT CHOISIR LES VAR A SUPPRIMER APRES LE PLOT DE VIM ??
aggr_plot <- aggr(don, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.7, gap=3,
                  ylab=c("Histogram of missing data","Pattern"))

# Remplacement des valeurs NA ?? l'aide de la m??thode Mice
tail(md.pattern(don))
md.pairs(don)
#imp<-mice(don)
imp <- mice(don,seed=1234)
#imp <- mice(don,m=1,seed=1234) #cr?er juste 1 valeur d'imputation : plus rapide, en attendant le param de MICE!

summary(imp)
imp$imp$INDFMPIR_demo # les 5 valeurs d'imputation propos??es par MICE 
#imp$imp$DR1TCHOL_dr1tot # 0 NA
imp$imp$BPXSY3_bpx
imp$imp$OHAREC_ohxref
imp$imp$BMXBMI_bmx

don2<-complete(imp)

don2$INDFMPIR_demo # par d??faut MICE imput avec la 1ere valeur qui n'est pas forcement pertinente : param de MICE pour choisir la moy entre les 5 valeurs ??
don2$BPXSY3_bpx
don2$OHAREC_ohxref
don2$BMXBMI_bmx

summary(don2)
dim(don2) #5470x93
table(apply(don2,2,function (x) {sum(is.na(x))})>0)

don2dt <- as.data.table(don2)
don2dt[nhanes.Y==1,.N,] #1699
don2dt[nhanes.Y==2,.N,] #3735
don2dt[nhanes.Y==9,.N,] #36
apply(don2dt,2,function (x) {sum(is.na(x))}) 

# le fichier de sortie
#write.csv(don2,"data chol/JddChol_Choix2_MICE_14012019.csv")
write.csv(don2,"data chol/JddChol_Choix2_MICEavecSeed.csv")
colnames(don2)
summary(don2)
str(don2)

####################################################################################################
## ANALYSER LES DIFFERENTES METHODES DE MODELISATION POUR PREDIRE LE CHOLESTEROL
####################################################################################################
library(glmnet)#contrainte
library(randomForest)#Foret
library(gbm)#Boosting
library(e1071)#SVM
library(rpart)#Arbre
library(foreach)#parallel => apres avoir fait avec for normal

#don <- read.csv("data chol/JddChol_Choix2_MICE_14012019.csv")
don <- read.csv("data chol/JddChol_Choix2_MICEavecSeed.csv")
summary(don)
str(don)
dim(don)

names(don)[92] <- "Y" #ATTENTION

unique(don$Y) #il y a des 1,2 et 9

colnames(don)
don$X <- NULL
don$X.1 <- NULL #AJOUTER


dim(don) #5470x91
colnames(don)

# Y avec des 0 et 1
don[don$Y=="2",90]<-c("0") #ATTENTION
don[don$Y=="1",90]<-c("1")
don[don$Y=="9",90]<-c("0")

# Y numeric
str(don)
don$Y <- as.numeric(don$Y)
str(don) # avant cetait CHAR
unique(don$Y)

#write.csv(don,"data chol/JddChol_Choix2_MICE_24022019.csv")
write.csv(don,"data chol/JddChol_Choix2_MICEavecSEQN.csv") #pour classification
don$SEQN <- NULL #AJOUTER
write.csv(don,"data chol/JddChol_Choix2_MICEsansSEQN.csv")
colnames(don)
summary(don)

# ############################################
# # Logistique 
# ############################################

don <- read.csv("data chol/JddChol_Choix2_MICEsansSEQN.csv")

don$X <- NULL

# ## Regression logistique 
reglog<-glm(Y~.,data=don,family="binomial")
summary(reglog) #VB6 avec étoile; mais NA pour RIDSTATR et DRABF

unique(don$RIDSTATR_demo) #seulement valeur 2
unique(don$DRABF_dr1tot) #seulement valeur 2

# ############################################
# # Logistique avec STEP
# ############################################
# 
# ## Regression logistique amelioree avec le choix de variables step
null=glm(Y~1, data=don,family=binomial)
null
full=glm(Y~., data=don,family=binomial)
full

bestmodel=step(null, scope=list(lower=null, upper=full), direction="forward") #AIC=5312.3
summary(bestmodel) #5 iterations

## Variables choisies par Step : nhanes_29122018 ?!
# RIDAGEYR_demo + BPQ020_bpq + MCQ080_mcq + 
# DR1TFIBE_dr1tot + DIQ010_diq + OHAREC_ohxref + RIAGENDR_demo + 
# BMXBMI_bmx + Var_TRAVAIL + SLQ050_slq + DRQSDIET_dr1tot + 
# DR1TFF_dr1tot + BPXDI2_bpx + BPXSY3_bpx + DR1.320Z_dr1tot + 
# BMXHT_bmx + BMXWT_bmx + INDFMPIR_demo + DR1TALCO_dr1tot

#don <- don[,c("SEQN","RIAGENDR_demo", "RIDAGEYR_demo", "INDFMPIR_demo", "BMXBMI_bmx", "BMXHT_bmx", "BMXWT_bmx","BPXDI2_bpx", "BPXSY3_bpx",
#                   "BPQ020_bpq", "MCQ080_mcq",  "DIQ010_diq", "OHAREC_ohxref","SLQ050_slq","Var_TRAVAIL",
#                 "DRQSDIET_dr1tot", "DR1TFIBE_dr1tot","DR1TFF_dr1tot","DR1.320Z_dr1tot","DR1TALCO_dr1tot", "nhanes.y")]

## Variables choisies par Step : nhanes_16012019 et sans SEQN/X/X1 ?!
# RIDAGEYR_demo + BPQ020_bpq + MCQ080_mcq + DR1TFIBE + 
#   DIQ010_diq + OHAREC_ohxref + RIAGENDR_demo + BMXBMI_bmx + 
#   Var_TRAVAIL + SLQ050_slq + BPXDI2_bpx + BPXSY3_bpx + DR1TALCO_dr1tot + 
#   DRQSDIET_dr1tot + DR1TMOIS_dr1tot + DR1TFF_dr1tot + BMXHT_bmx + 
#   BMXWT_bmx + HOD050_hoq + PAQ635_paq + DR1TLZ_dr1tot + DR1TVC_dr1tot

#DR1TVC - Vitamin C (mg)
#DR1TMOIS - Moisture (gm)
#DR1TLZ - Lutein + zeaxanthin (mcg)
#HOD050 - Number of rooms in home
#PAQ635 - Walk or bicycle In a typical week {do you/does SP} walk or use a bicycle for at least 10 minutes continuously to get to and from places?

# don <- don[,c("RIDAGEYR_demo", "INDFMPIR_demo", "BMXBMI_bmx", "BMXHT_bmx", "BMXWT_bmx","BPXDI2_bpx", "BPXSY3_bpx",
#               "BPQ020_bpq", "MCQ080_mcq",  "DIQ010_diq", "OHAREC_ohxref","SLQ050_slq","Var_TRAVAIL",
#              "DRQSDIET_dr1tot", "DR1TFIBE_dr1tot","DR1TFF_dr1tot","DR1TALCO_dr1tot","HOD050_hoq", "PAQ635_paq", "DR1TLZ_dr1tot", "DR1TVC_dr1tot", "DR1TMOIS_dr1tot","Y")]

## Variables choisies par Step : 10/03/2019 apres 7et9 levels en tant q NA
# RIDAGEYR_demo + BPQ020_bpq + MCQ080_mcq + DR1TFIBE_dr1tot + 
#   DIQ010_diq + OHAREC_ohxref + RIAGENDR_demo + SLQ050_slq + 
#   Var_TRAVAIL + BMXBMI_bmx + DR1TALCO_dr1tot + BPXDI2_bpx + 
#   BPXSY3_bpx + DRQSDIET_dr1tot + HOQ065_hoq + DR1TMOIS_dr1tot + 
#   DR1TFF_dr1tot + BMXHT_bmx + BMXWT_bmx + HIQ011_hiq + HOD050_hoq

#don <- don[,c("RIDAGEYR_demo", "RIAGENDR_demo","BMXBMI_bmx", "BMXHT_bmx", "BMXWT_bmx","BPXDI2_bpx", "BPXSY3_bpx",
#              "BPQ020_bpq", "MCQ080_mcq",  "DIQ010_diq", "OHAREC_ohxref","SLQ050_slq","Var_TRAVAIL",
#              "DRQSDIET_dr1tot", "DR1TFIBE_dr1tot","DR1TFF_dr1tot","DR1TALCO_dr1tot","HOD050_hoq","HOQ065_hoq","HIQ011_hiq","DR1TMOIS_dr1tot","Y")]

## Variables choisies par Step : 10/03/2019 apres 7et9 levels deuxieme fois
# RIDAGEYR_demo + BPQ020_bpq + MCQ080_mcq + DR1TFIBE_dr1tot + 
#   DIQ010_diq + OHAREC_ohxref + RIAGENDR_demo + SLQ050_slq + 
#   Var_TRAVAIL + BMXBMI_bmx + DR1TALCO_dr1tot + BPXDI2_bpx + 
#   BPXSY3_bpx + HIQ011_hiq + DR1TMOIS_dr1tot + DR1TFF_dr1tot + 
#   BMXHT_bmx + BMXWT_bmx + DRQSDIET_dr1tot

#don <- don[,c("RIDAGEYR_demo", "RIAGENDR_demo","BMXBMI_bmx", "BMXHT_bmx", "BMXWT_bmx","BPXDI2_bpx", "BPXSY3_bpx",
#              "BPQ020_bpq", "MCQ080_mcq",  "DIQ010_diq", "OHAREC_ohxref","SLQ050_slq","Var_TRAVAIL",
#              "DRQSDIET_dr1tot", "DR1TFIBE_dr1tot","DR1TFF_dr1tot","DR1TALCO_dr1tot","HIQ011_hiq","DR1TMOIS_dr1tot","Y")]

## Variables choisies par Step : 11/03/2019 apres 7et9 levels DEFINITIVE : ok pas dif :-)
# RIDAGEYR_demo + BPQ020_bpq + MCQ080_mcq + DR1TFIBE_dr1tot + 
#   DIQ010_diq + BMXBMI_bmx + OHAREC_ohxref + RIAGENDR_demo + 
#   SLQ050_slq + Var_TRAVAIL + DR1TALCO_dr1tot + BPXDI2_bpx + 
#   BPXSY3_bpx + DRQSDIET_dr1tot + HIQ011_hiq + BMXHT_bmx + BMXWT_bmx + 
#   DR1TMOIS_dr1tot + DR1TFF_dr1tot

don <- don[,c("RIDAGEYR_demo", "RIAGENDR_demo","BMXBMI_bmx", "BMXHT_bmx", "BMXWT_bmx","BPXDI2_bpx", "BPXSY3_bpx",
              "BPQ020_bpq", "MCQ080_mcq",  "DIQ010_diq", "OHAREC_ohxref","SLQ050_slq","Var_TRAVAIL",
              "DRQSDIET_dr1tot","DR1TFIBE_dr1tot","DR1TFF_dr1tot","DR1TALCO_dr1tot","HIQ011_hiq","DR1TMOIS_dr1tot","Y")]

#write.csv2(don,"data chol/JddChol_Choix2_MICE_STEP_17022019.csv",row.names = FALSE)
#write.csv(don,"data chol/JddChol_Choix2_MICE_STEP.csv")
write.csv(don,"data chol/JddChol_Choix2_MICE_STEPsansSEQN.csv")
colnames(don)
str(don)
summary(don)
dim(don) #5470   23


################################################
# NBR VAR DE STEP vers NBR VAR IMP : de 20 à 15
################################################

# RIDAGEYR_demo
# BPQ020_bpq2
# MCQ080_mcq2
# DIQ010_diq2
# BMXBMI_bmx
# BMXHT_bmx
# SLQ050_slq2
# RIAGENDR_demo2
# BMXWT_bmx
# Var_TRAVAILoui
# DR1TVB6_dr1tot
# DR1TFIBE_dr1tot
# DR1TCHOL_dr1tot
# DR1TB12A_dr1tot
# DRQSDIET_dr1tot2

don <- read.csv("data chol/JddChol_Choix2_MICEsansSEQN.csv")

don$X <- NULL

don <- don[,c("RIDAGEYR_demo", "RIAGENDR_demo","BMXBMI_bmx", "BMXHT_bmx", "BMXWT_bmx",
              "BPQ020_bpq","MCQ080_mcq","DIQ010_diq","SLQ050_slq","Var_TRAVAIL",
              "DR1TVB6_dr1tot","DR1TFIBE_dr1tot","DR1TCHOL_dr1tot","DR1TB12A_dr1tot","DRQSDIET_dr1tot","Y")]


write.csv(don,"data chol/JddChol_Choix2_MICE_STEPsansSEQN_REDVARIMP.csv")
colnames(don)
str(don)
summary(don)
dim(don) #5470   16


############################################
# Validation croisee 
############################################
library(glmnet)#contrainte
library(randomForest)#Foret
library(gbm)#Boosting
library(e1071)#SVM
library(rpart)#Arbre
library(foreach)#parallel => apres avoir fait avec for normal

don6 <- read.csv("data chol/JddChol_Choix2_MICE_STEPsansSEQN_REDVARIMP.csv")
str(don6)
dim(don6) #5470   15
don6$X <- NULL


don3 <- read.csv("data chol/JddChol_Choix2_MICE_STEPsansSEQN.csv")
str(don3)
dim(don3) #5470   23
don3$X <- NULL

don <- read.csv("data chol/JddChol_Choix2_MICEsansSEQN.csv")
str(don)
dim(don) #5470   92
don$X <- NULL

# Transformer toutes les variables avec dif levels en FACTOR
#RIDSTATR_demo  : Factor w/ 1 level "2": 1
#don$RIDSTATR_demo <- as.factor(don$RIDSTATR_demo)
don$RIAGENDR_demo <- as.factor(don$RIAGENDR_demo)
#don$INDHHIN2_demo <- as.factor(don$INDHHIN2_demo) #apres le 7et 9 NA
#don$INDFMIN2_demo <- as.factor(don$INDFMIN2_demo) #apres le 7et 9 NA
don$BPQ020_bpq <- as.factor(don$BPQ020_bpq)
#don$DIQ010_diq <- as.factor(don$DIQ010_diq)
don$HEQ010_heq <- as.factor(don$HEQ010_heq)
don$HEQ030_heq <- as.factor(don$HEQ030_heq)
don$HOQ065_hoq <- as.factor(don$HOQ065_hoq)
don$HIQ011_hiq <- as.factor(don$HIQ011_hiq)
#don$IMQ020_imq <- as.factor(don$IMQ020_imq)
#don$IMQ011_imq <- as.factor(don$IMQ011_imq) #apres le 7et 9 NA
don$MCQ010_mcq <- as.factor(don$MCQ010_mcq)
don$MCQ080_mcq <- as.factor(don$MCQ080_mcq)
#don$OHAREC_ohxref <- as.factor(don$OHAREC_ohxref)
don$PAQ605_paq <- as.factor(don$PAQ605_paq)
don$PAQ620_paq <- as.factor(don$PAQ620_paq)
#don$PAQ635_paq <- as.factor(don$PAQ635_paq)
#don$PAQ650_paq <- as.factor(don$PAQ650_paq)
don$PAQ665_paq <- as.factor(don$PAQ665_paq)
don$SLQ050_slq <- as.factor(don$SLQ050_slq)
#DRABF_dr1tot   : Factor w/ 1 level "2": 1
#don$DRABF_dr1tot <- as.factor(don$DRABF_dr1tot)
don$DRQSDIET_dr1tot <- as.factor(don$DRQSDIET_dr1tot)

summary(don)

str(don)
write.csv(don,"data chol/JddChol_Choix2_MICEsansSEQNavecFactor.csv")

# .... REFAIRE pour don3 
don3$BPQ020_bpq <- as.factor(don3$BPQ020_bpq)
#don3$DIQ010_diq <- as.factor(don3$DIQ010_diq)
don3$MCQ080_mcq <- as.factor(don3$MCQ080_mcq)
#don3$OHAREC_ohxref <- as.factor(don3$OHAREC_ohxref)
#don3$PAQ635_paq <- as.factor(don3$PAQ635_paq)
don3$SLQ050_slq <- as.factor(don3$SLQ050_slq)
don3$DRQSDIET_dr1tot <- as.factor(don3$DRQSDIET_dr1tot)
don3$HIQ011_hiq <- as.factor(don3$HIQ011_hiq) #apres le 7et 9 NA

str(don3)
write.csv(don3,"data chol/JddChol_Choix2_MICE_STEPsansSEQNavecFactor.csv")

# .... REFAIRE pour don6 
don6$BPQ020_bpq <- as.factor(don6$BPQ020_bpq)
#don6$DIQ010_diq <- as.factor(don6$DIQ010_diq)
don6$MCQ080_mcq <- as.factor(don6$MCQ080_mcq)
#don6$OHAREC_ohxref <- as.factor(don6$OHAREC_ohxref)
#don6$PAQ635_paq <- as.factor(don6$PAQ635_paq)
don6$SLQ050_slq <- as.factor(don6$SLQ050_slq)
don6$DRQSDIET_dr1tot <- as.factor(don6$DRQSDIET_dr1tot)
#don6$HIQ011_hiq <- as.factor(don6$HIQ011_hiq) #apres le 7et 9 NA

str(don6)
write.csv(don6,"data chol/JddChol_Choix2_MICE_STEPsansSEQN_REDVARIMPavecFactor.csv")

# Y numeric pour les méthodes dif à FORET et SVM
don$Y <- as.numeric(don$Y) #Y int 
don3$Y <- as.numeric(don3$Y) #Y int 
don6$Y <- as.numeric(don6$Y) #Y int 

# Y factor necessaire pour la méthode FORET
don2<-don
don2$Y<-factor(don2$Y)

# Y factor necessaire pour la méthode SVM
don4<-don #avec 91 var
don4$Y<-factor(don4$Y)
don5<-don3 #avec 23 var
don5$Y<-factor(don5$Y)


#unique(don3$PAQ635_paq)
#levels(don3$PAQ635_paq)



str(don)
str(don2)
str(don3)
str(don4)
str(don5)
str(don6)



## les 4 blocs
bloc <- 4
ind= sample(1:nrow(don) %% bloc+1)
res <- data.frame(Y=don$Y, log90=0, ridge=0, lasso=0, elastic=0,
                  foret=0, adabo=0, logibo=0, svm=0, pm=0, arbre=0, logStep20=0, svmStep20=0, logRed15=0) #supprimer pm et arbre et svm

# demarrage du compteur de temps et de la graine
set.seed(1234)
deb <-   Sys.time()

# la boucle pour les differentes methodes appliquees aux 4 blocs
for (i in 1:bloc)
#foreach (i = 1:bloc, .packages = c("gbm","e1071","glmnet","randomForest", "rpart")) %dopar% 
{
  
  ## Logistique
  mod <- glm(Y~.,data=don[ind!=i,],family="binomial")
  res[ind==i,"log90"] <- predict(mod,don[ind==i,],type="response")

  # Decoupage necessaire pour les methodes sauf Logistique et Foret
  XX<-model.matrix(don$Y~.,data=don)
  XXA <- XX[ind!=i,]
  YYA <- as.matrix(don[ind!=i,"Y"])
  XXT <- XX[ind==i,] #pour le predict

  ## Ridge
  tmp <- cv.glmnet(XXA,YYA,alpha=0,family="binomial")
  mod <- glmnet(XXA,YYA,alpha=0,lambda=tmp$lambda.min, family="binomial")
  res[ind==i,"ridge"] <- predict(mod,newx=XXT,type="response")

  ## Lass0
  tmp <- cv.glmnet(XXA,YYA, alpha=1, family="binomial")
  mod <- glmnet(XXA,YYA,alpha=1, lambda =tmp$lambda.1se,family="binomial" ) #utiliser lambda.min a la place de lambda.1se?
  res[ind==i,"lasso"] <- predict(mod,newx=XXT, type="response")

  ## Elasticnet
  tmp <- cv.glmnet(XXA,YYA, alpha=0.5, family="binomial")
  mod <- glmnet(XXA,YYA,alpha = 0.5, lambda = tmp$lambda.min, family="binomial")
  res[ind==i,"elastic"] <- predict(mod,newx=XXT,type="response")

  ## Foret
  #don2 a la place de don pq besoin de Y factorise
  mod <- randomForest(Y~.,data=don2[ind!=i,])
  res[ind==i,"foret"] <- predict(mod,don2[ind==i,],type="prob")[,2]

  # Adaboost
  tmp <- gbm(Y~.,data = don[ind!=i,], distribution = "adaboost", interaction.depth = 2,
             shrinkage = 0.1,n.trees = 500)
  M <- gbm.perf(tmp)[1]
  mod <- gbm(Y~.,data = don[ind!=i,], distribution = "adaboost", interaction.depth = 2,
             shrinkage = 0.1,n.trees = M)
  res[ind==i, "adabo"] <- predict(mod, newdata=don[ind==i,], type = "response", n.trees = M)

  # Logiboost
  tmp <- gbm(Y~.,data=don[ind!=i,], distribution="bernoulli", interaction.depth = 2,
             shrinkage=0.1,n.trees=500)
  M <- gbm.perf(tmp)[1]
  mod <- gbm(Y~.,data=don[ind!=i,], distribution="bernoulli", interaction.depth = 2,
             shrinkage=0.1,n.trees=M)
  res[ind==i, "logibo"] <- predict(mod,newdata=don[ind==i,], type= "response", n.trees = M)

  # # SVM => CA NE MARCHE PAS quand forte dimension (91var)
  # mod <- svm(Y~.,data=don4[ind!=i,], kernel="linear",probability=T)
  # res[ind==i,"svm"] <- attr(predict(mod,newdata = don4[ind==i,],probability = T),"probabilities")[,2] #on garde la proba du succès
  
  # Arbre => LA FORET CEST MIEUX
  # mod <- rpart(XXA,YYA)
  # res[ind==i,"arbre"] <- predict(mod,XX[ind==i,])
  
  # # Logistique avec don du step 23 var et sans seqn
  mod <- glm(Y~.,data=don3[ind!=i,],family="binomial")
  res[ind==i,"logStep20"] <- predict(mod,don3[ind==i,],type="response")


  # SVM => CA MARCHE AVEC 23var
  #don3$Y <- as.factor(don3$Y)
  #class(don3$Y)
  mod <- svm(Y~.,data=don5[ind!=i,], kernel="linear",probability=T)
  #names(mod)
  #summary(mod)
  res[ind==i,"svmStep20"] <- attr(predict(mod,newdata = don5[ind==i,],probability = T),"probabilities")[,2]
  
  # # # Logistique avec don du step 15 var et sans seqn
  mod <- glm(Y~.,data=don6[ind!=i,],family="binomial")
  res[ind==i,"logRed15"] <- predict(mod,don6[ind==i,],type="response")
  
}

# temps decoule entre debut et fin
fin <- Sys.time()
fin-deb


# SVM => DEBUGAGE

# names(mod)
# summary(mod)
# ressvm <- attr(predict(mod,newdata = don3[ind==1,],probability = T),"probabilities")[,2]
# 
# predict(mod,newdata = don3[ind==1,])
# 
# pred <- predict(mod,newdata = don3[ind==1,],probability = TRUE, decision.values = TRUE)
# attr(pred,"probabilities")[1:4,]
# attr(pred,"decision.values")
# attr(pred,"probabilities")
# names(pred)
# class(don3$Y)
# 
# don3[ind==1,]

############################################
# Perceptron multicouche => SI DU TEMPS
############################################
# library(keras)
# 
# for (i in 1:bloc){
#   
#   # instanciation du model
#   pm.keras <- keras_model_sequential()
#   
#   # architecture
#   pm.keras %>%
#     layer_dense(units = 2, input_shape=ncol(XXA), activation = "sigmoid") %>%
#     layer_dense(units = 1, activation = "sigmoid")
#   
#   # configuration de l'apprentissage
#   pm.keras %>% compile(
#     loss="mean_squared_error",
#     optimizer="sgd",
#     metrics="mae"
#   )
#   
#   # lancement de l'apprentissage
#   pm.keras %>% fit(
#     XXA <- XX[ind!=i,],
#     YYA <- YY[ind!=i,],
#     epochs=80,
#     batch_size=8
#   )
#   
#   # proba prediction
#   res[ind==i,"pm"] <- pm.keras %>% predict(XX[ind==i,])
# }

####################################################
# Récupération de RES pour l'utiliser dans Shiny
####################################################

## Onglet Choix modele avec JDD sans step (93var)

head(res)
str(res)
#write.csv(res,"data chol/RES_Chol_Choix2MICEsansStep.csv")
#write.csv2(res,"data chol/RES_Chol_Choix2MICEsansStep.csv",row.names = FALSE)
write.csv2(res,"data chol/RES_Chol_Choix2MICEsansSEQN_sansStep.csv",row.names = FALSE)

#####################################################
# Evaluation des mod??les avec les erreurs et courbes
#####################################################

#####
## 1
#####

monerreur <- function(X, Y, seuil=0.5){
  table(cut(X, breaks = c(0,seuil,1)), Y)
}

# matrice de confusion
monerreur(res[,2],res[,1])#log
monerreur(res[,3],res[,1])#Ridge
monerreur(res[,4],res[,1])#Lasso
monerreur(res[,5],res[,1])#Elastic
monerreur(res[,6],res[,1])#Foret
monerreur(res[,7],res[,1])#Adabo
monerreur(res[,8],res[,1])#Logibo
# monerreur(res[,9],res[,1])#SVM
# monerreur(res[,10],res[,1])#perceptron
# monerreur(res[,11],res[,1])#arbre
monerreur(res[,12],res[,1])#logStep23
monerreur(res[,13],res[,1])#svmStep23
monerreur(res[,14],res[,1])#logRed15

#####
## 2
#####

library(pROC)
# erreur
monerreurb <- function(X,Y,seuil=0.5){
  Xc <- cut(X,breaks=c(0,seuil,1),labels=c(0,1))
  sum(as.factor(Y)!=Xc)
}
apply(res[,-1],2,monerreurb,Y=res[,1],seuil=0.5)


auc(res[,1],res[,2])#log
auc(res[,1],res[,3])#Ridge
auc(res[,1],res[,4])#Lasso
auc(res[,1],res[,5])#Elastic
auc(res[,1],res[,6])#Foret
auc(res[,1],res[,7])#Adabo
auc(res[,1],res[,8])#Logibo
# auc(res[,1],res[,9])#SVM
# auc(res[,1],res[,10])#perceptron
# auc(res[,1],res[,11])#arbre
auc(res[,1],res[,12])#logStep23
auc(res[,1],res[,13])#svmStep23
auc(res[,1],res[,14])#logRed15

#####
## 3
#####

plot(roc(res[,1],res[,2]))#log
lines(roc(res[,1],res[,3]), col="red")#Ridge
lines(roc(res[,1],res[,4]), col="blue")#Lasso
lines(roc(res[,1],res[,5]), col="green")#Elastic
lines(roc(res[,1],res[,6]), col="brown")#Foret
lines(roc(res[,1],res[,7]), col="yellow")#Adabo
lines(roc(res[,1],res[,8]), col="purple")#Logibo
# lines(roc(res[,1],res[,9]), col="grey")#SVM
# lines(roc(res[,1],res[,9]), col="black")#perceptron
# lines(roc(res[,1],res[,9]), col="orange")#arbre
lines(roc(res[,1],res[,12]), col="orange")#LogStep23
lines(roc(res[,1],res[,13]), col="orange")#svmStep23
lines(roc(res[,1],res[,14]), col="orange")#logRed15

###############################################################################################################
## Validation croisee Avec interactions et indicatrice
###############################################################################################################
library(glmnet)#contrainte
library(randomForest)#Foret
library(gbm)#Boosting
library(e1071)#SVM
library(rpart)#Arbre
library(foreach)#parallel => apres avoir fait avec for normal

don3 <- read.csv("data chol/JddChol_Choix2_MICE_STEPsansSEQN.csv")
str(don3)
dim(don3) #5470   23
don3$X <- NULL

don <- read.csv("data chol/JddChol_Choix2_MICEsansSEQN.csv")
str(don)
dim(don) #5470   92
don$X <- NULL

# Transformer toutes les variables avec dif levels en FACTOR
#RIDSTATR_demo  : Factor w/ 1 level "2": 1
#don$RIDSTATR_demo <- as.factor(don$RIDSTATR_demo)
don$RIAGENDR_demo <- as.factor(don$RIAGENDR_demo)
don$INDHHIN2_demo <- as.factor(don$INDHHIN2_demo)
don$INDFMIN2_demo <- as.factor(don$INDFMIN2_demo)
don$BPQ020_bpq <- as.factor(don$BPQ020_bpq)
#don$DIQ010_diq <- as.factor(don$DIQ010_diq)
don$HEQ010_heq <- as.factor(don$HEQ010_heq)
don$HEQ030_heq <- as.factor(don$HEQ030_heq)
don$HOQ065_hoq <- as.factor(don$HOQ065_hoq)
don$HIQ011_hiq <- as.factor(don$HIQ011_hiq)
don$IMQ020_imq <- as.factor(don$IMQ020_imq)
don$IMQ011_imq <- as.factor(don$IMQ011_imq)
don$MCQ010_mcq <- as.factor(don$MCQ010_mcq)
don$MCQ080_mcq <- as.factor(don$MCQ080_mcq)
#don$OHAREC_ohxref <- as.factor(don$OHAREC_ohxref)
don$PAQ605_paq <- as.factor(don$PAQ605_paq)
don$PAQ620_paq <- as.factor(don$PAQ620_paq)
#don$PAQ635_paq <- as.factor(don$PAQ635_paq)
#don$PAQ650_paq <- as.factor(don$PAQ650_paq)
don$PAQ665_paq <- as.factor(don$PAQ665_paq)
don$SLQ050_slq <- as.factor(don$SLQ050_slq)
#DRABF_dr1tot   : Factor w/ 1 level "2": 1
#don$DRABF_dr1tot <- as.factor(don$DRABF_dr1tot)
don$DRQSDIET_dr1tot <- as.factor(don$DRQSDIET_dr1tot)

str(don)
write.csv(don,"data chol/JddChol_Choix2_MICEsansSEQNavecFactor.csv")

# .... REFAIRE pour don3 
don3$BPQ020_bpq <- as.factor(don3$BPQ020_bpq)
#don3$DIQ010_diq <- as.factor(don3$DIQ010_diq)
don3$MCQ080_mcq <- as.factor(don3$MCQ080_mcq)
#don3$OHAREC_ohxref <- as.factor(don3$OHAREC_ohxref)
#don3$PAQ635_paq <- as.factor(don3$PAQ635_paq)
don3$SLQ050_slq <- as.factor(don3$SLQ050_slq)
don3$DRQSDIET_dr1tot <- as.factor(don3$DRQSDIET_dr1tot)

str(don3)
write.csv(don3,"data chol/JddChol_Choix2_MICE_STEPsansSEQNavecFactor.csv")

# Y numeric pour les méthodes dif à FORET et SVM
don$Y <- as.numeric(don$Y) #Y int 
don3$Y <- as.numeric(don3$Y) #Y int 

# Y factor necessaire pour la méthode FORET
don2<-don
don2$Y<-factor(don2$Y)

# Y factor necessaire pour la méthode SVM
don4<-don #avec 91 var
don4$Y<-factor(don4$Y)
don5<-don3 #avec 23 var
don5$Y<-factor(don5$Y)

unique(don3$PAQ635_paq)
levels(don3$PAQ635_paq)

str(don)
str(don2)
str(don3)
str(don4)
str(don5)

don6 <- subset(don3, don3$RIDAGEYR_demo>50)
don7 <- subset(don, don$RIAGENDR_demo=="1", select=-c(INDFMIN2_demo,INDHHIN2_demo,INDFMPIR_demo))
don8 <- subset(don, don$RIAGENDR_demo=="1")

levels(don$RIAGENDR_demo)

## sans blocs

## Logistique
mod <- glm(Y~.^2,data=don6,family="binomial")
resInter <- predict(mod,don6,type="response")

summary(mod)

## Ridge


## les 4 blocs
bloc <- 4
ind= sample(1:nrow(don) %% bloc+1)
resInter <- data.frame(Y=don$Y, log=0, ridge=0, lasso=0, elastic=0,
                  foret=0, adabo=0, logibo=0, logStep23=0, svmStep23=0) #supprimer pm et arbre et svm

# demarrage du compteur de temps et de la graine
set.seed(1234)
# for (jj in 1:5)
# set.seed(123jj)
deb <-   Sys.time()

# la boucle pour les differentes methodes appliquees aux 4 blocs
for (i in 1:bloc)
#foreach (i = 1:bloc, .packages = c("gbm","e1071","glmnet","randomForest", "rpart")) %dopar% 
{
  
  ## Logistique
  mod <- glm(Y~.^2,data=don3[ind!=i,],family="binomial")
  resInter[ind==i,"log"] <- predict(mod,don3[ind==i,],type="response")
  
  # Decoupage necessaire pour les methodes sauf Logistique et Foret
  XX<-model.matrix(don3$Y~.^2,data=don3)
  XXA <- XX[ind!=i,]
  YYA <- as.matrix(don3[ind!=i,"Y"])
  XXT <- XX[ind==i,] #pour le predict

  ## Ridge
  tmp <- cv.glmnet(XXA,YYA,alpha=0,family="binomial")
  mod <- glmnet(XXA,YYA,alpha=0,lambda=tmp$lambda.min, family="binomial")
  resInter[ind==i,"ridge"] <- predict(mod,newx=XXT,type="response")
  
  ## Lass0
  tmp <- cv.glmnet(XXA,YYA, alpha=1, family="binomial")
  mod <- glmnet(XXA,YYA,alpha=1, lambda =tmp$lambda.1se,family="binomial" ) #utiliser lambda.min a la place de lambda.1se?
  resInter[ind==i,"lasso"] <- predict(mod,newx=XXT, type="response")

  ## Elasticnet
  tmp <- cv.glmnet(XXA,YYA, alpha=0.5, family="binomial")
  mod <- glmnet(XXA,YYA,alpha = 0.5, lambda = tmp$lambda.min, family="binomial")
  resInter[ind==i,"elastic"] <- predict(mod,newx=XXT,type="response")
  
  # ## Foret
  # #don2 a la place de don pq besoin de Y factorise
  # mod <- randomForest(Y~.^2,data=don2[ind!=i,])
  # resInter[ind==i,"foret"] <- predict(mod,don2[ind==i,],type="prob")[,2]
  # 
  # # Adaboost
  # tmp <- gbm(Y~.^2,data = don[ind!=i,], distribution = "adaboost", interaction.depth = 2,
  #            shrinkage = 0.1,n.trees = 500)
  # M <- gbm.perf(tmp)[1]
  # mod <- gbm(Y~.^2,data = don[ind!=i,], distribution = "adaboost", interaction.depth = 2,
  #            shrinkage = 0.1,n.trees = M)
  # resInter[ind==i, "adabo"] <- predict(mod, newdata=don[ind==i,], type = "response", n.trees = M)
  # 
  # # Logiboost
  # tmp <- gbm(Y~.^2,data=don[ind!=i,], distribution="bernoulli", interaction.depth = 2,
  #            shrinkage=0.1,n.trees=500)
  # M <- gbm.perf(tmp)[1]
  # mod <- gbm(Y~.^2,data=don[ind!=i,], distribution="bernoulli", interaction.depth = 2,
  #            shrinkage=0.1,n.trees=M)
  # resInter[ind==i, "logibo"] <- predict(mod,newdata=don[ind==i,], type= "response", n.trees = M)
  # 
  # # # Logistique avec don du step 23 var et sans seqn
  # mod <- glm(Y~.^2,data=don3[ind!=i,],family="binomial")
  # resInter[ind==i,"logStep23"] <- predict(mod,don3[ind==i,],type="response")
  # 
  # # SVM => CA MARCHE AVEC 23var
  # mod <- svm(Y~.^2,data=don5[ind!=i,], kernel="linear",probability=T)
  # resInter[ind==i,"svmStep23"] <- attr(predict(mod,newdata = don5[ind==i,],probability = T),"probabilities")[,2]
  
}

# temps decoule entre debut et fin
fin <- Sys.time()
fin-deb


