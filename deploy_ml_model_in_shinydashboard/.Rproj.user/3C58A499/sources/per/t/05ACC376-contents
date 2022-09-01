# importation des librairies

library(tidyverse)

library(ggthemes)

library(ROSE)

library(pROC)

library(caret)

#Parametre du rendu des graphiques

options(repr.plot.width=6, repr.plot.height=4)

theme_set(theme_minimal())

# Importation des donnees
url = 'C:/Users/pc/OneDrive/Documents/Environement R/Risque de credit/Dataset/credit_risk_dataset.csv'
df <- read.csv(url)
df
head(df)

# Structure de la dataframe

str(df)

# Resumé statisques
summary(df)

# Transformation de la variable cible en variable categorielle

df$loan_status <- as.factor(df$loan_status)

# Table de frequence de la variable cible 'loan_status'

print(prop.table(table(df$loan_status)))

# Diagramme a barre de la variable 'loan_status'

plot(df$loan_status, main= 'Statut du credit')

# Nuage de point entre la variable cible et duree d'activite professionnelle

ggplot(df, aes(x=person_emp_length, y=loan_percent_income)) +
  geom_point(aes(color = loan_status, shape = loan_status))

#Loan_percent_income vs loan status

ggplot(df, aes(x = "", y=loan_percent_income, fill=loan_status)) +
  geom_boxplot() +
  labs(x = "", y="loan_percent_income")


# Fonction de contruction d'un graphique montrant La relation
# entre une variable indépendante et La variable cible

show_relation <- function(data, feature, title_y){
  ggplot(data, aes(x = "", y=feature, fill=loan_status)) +
    geom_boxplot() +
    labs(x = "", y = title_y)
}


# loan_amnt vs loan_status

show_relation(df, df$loan_amnt, "loan_amnt")

# Histogrammedu montant du credit

hist(df$loan_amnt, main = "Montant du credit")

#Histogramme de 'loan_amnt' discretisé par 'loan_status'

ggplot(df, aes(x = loan_amnt)) +
  geom_histogram(color="red", fill="red", bins=50) +
  facet_wrap(~ loan_status, scales="free", ncol=2)

# Histogramme des revenus moyens

hist(df$person_income, main = "Histogramme du Revenu annuel")

# Nuage de points

plot(df$person_income, ylab="Revenu annuel")

# person_income vs loan_status

show_relation(subset(df, df$person_income < 100000), subset(df, df$person_income < 100000)$person_income, "person_income")

# Histogramme du taux d'interet

hist(df$loan_int_rate, main = "Taux d'intéret du crédit")

# loan_int_rate vs loan_status

show_relation(df, df$loan_int_rate, "loan_int_rate")

# Histogramme de la variable person_emp_length

hist(df$person_emp_length)

# person_emp_length vs loan_status

show_relation(subset(df, df$person_emp_length < 50), 
              subset(df, df$person_emp_length < 50)$person_emp_length, 
              "person_emp_length")


# Histogramme de la variable indiquant l'age


hist(df$person_age)

# person_age vs loan_status

show_relation(df, df$person_age, "person_age")

# variable 'person_home_ownership'

df %>%
  ggplot(aes(x=person_home_ownership, fill = person_home_ownership)) +
  geom_bar()+
  theme_bw()

# person_home_ownership

df %>%
  ggplot(aes(x=person_home_ownership, fill = person_home_ownership)) +
  geom_bar()+
  theme_bw()+
  facet_wrap(~ loan_status, scales = "free", ncol = 2)

# Motif de l'emprunt

df %>%
  ggplot(aes(x=loan_intent, fill = loan_intent)) +
  geom_bar()+
  theme_bw()

# Motif de l'emprunt discretise selon la variable cible

df %>%
  ggplot(aes(x=loan_intent, fill = loan_intent)) +
  geom_bar()+
  theme_bw()+
  facet_wrap(~ loan_status, scales = "free", ncol = 2)

# creation d'une copie du dataframe df

df_clean <- df

# Nombre de lignes initial dans df_clean

nrow(df_clean)

# Identification des valeurs aberantes au niveau de la variable 'person_income'

index_outlier_income <- which(df_clean$person_income < quantile(df_clean$person_income, 0.25) - 1.5 * IQR(df_clean$person_income) | df_clean$person_income > quantile(df_clean$person_income, 0.75) + 1.5 * IQR(df_clean$person_income))

# Suppression des valeurs aberantes au niveau de la variable 'person_income'

df_clean <- df_clean[-index_outlier_income, ]

# Verification : Histogramme des revenus annuels 

hist(df_clean$person_income, main="Histogramme du revenu annuel")

# Suppression des valeures aberantes au niveau de la variable 'person_age'

df_clean <- subset(df_clean, df_clean$person_age < 100)

# Nombre de lignes final dans df_clean

nrow(df_clean)

# variable person_emp_length

index_NA_person_emp_length <- which(is.na(df_clean$person_emp_length))

df_clean$person_emp_length[index_NA_person_emp_length] <- median(df_clean$person_emp_length, na.rm=TRUE)

# Variable loan_int_rate

index_NA_rate <- which(is.na(df_clean$loan_int_rate))

df_clean$loan_int_rate[index_NA_rate] <- median(df_clean$loan_int_rate, na.rm=TRUE)


# verification (person_emp_length)

print(summary(df_clean$person_emp_length))

# verification (loan_int_rate)

print(summary(df_clean$loan_int_rate))

# Creation d'une copie de df_clean



# Creation d'une fonction de normalisation

df_clean2 <- df_clean

# Normalisation des données
# répartir les valeurs entre 0 et 1 tout en gardant les distributions originales

normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

# verification de la normalisation 

for (col in colnames(df_clean2)){
  if((class(df_clean2[, col]) != 'factor')){
    df_clean2[, col] <- normalize(df_clean2[, col])
  }
}

# verification de la normalisation

head(df_clean2)

# Données d'entrainement (80%) et de test (20%) (Division aleatoire avec la fonction sample)

seed <- 131

set.seed(seed)

index_train <- sample(1:nrow(df_clean2), 0.8 * nrow(df_clean2))

train_set <- df_clean2[index_train, ]

test_set <- df_clean2[-index_train, ]

# Nombre de lignes dans train_set et test_set

print(nrow(train_set))

print(nrow(test_set))

# Table de fréquence de la variable cible dans l'ensemble d'entrainement

prop.table(table(train_set$loan_status))

# Table de fréquence de la variable cible dans l'ensemble de test

prop.table(table(test_set$loan_status))

# Nombre d'orservation de la classe majoritaire (non-défauts)

nrow(subset(train_set, train_set$loan_status == 0))

# Nombre d'orservation de la classe ,minoritaire (défauts)

nrow(subset(train_set, train_set$loan_status == 1))

# Methode de sur-échantillonage de la classe minoritaire (ROS)
train_oversampled <- ovun.sample(formula = loan_status ~., data = train_set, method = 'over', seed=seed)

# Affichage du resultat
print(class(train_oversampled))
print(summary(train_oversampled))

# Dataframe obtenue par la methode de sur-échantillonage de la classe minoritaire

train_oversampled_df <- train_oversampled$data

head(train_oversampled_df)


prop.table(table(train_oversampled_df$loan_status))

# Methode de sous-echantillonage de la classe majoritaire (RUS)

train_undersampled <- ovun.sample(formula = loan_status ~ ., data = train_set, method = 'under', seed=seed)

# Affichage du resultat

print(class(train_undersampled))

print(summary(train_undersampled))

# Dataframe obtenue par la methode de sous-échantillonage de la classe majoritaire

train_undersampled_df <- train_undersampled$data

prop.table(table(train_undersampled_df$loan_status))

# Combinaison des techniques ROS et RUS

train_ros_rus <- ovun.sample(formula = loan_status ~ ., data = train_set, method = 'both', N=12000, seed=seed)

# Resultat

train_ros_rus_df <- train_ros_rus$data

prop.table(table(train_ros_rus_df$loan_status))

# Creation d'une fonction de construction d'un modele de regression Logistique

  log_modeling <- function(train){
  model <- glm(loan_status ~ ., family = 'binomial', data = train)
  return (model)
}

# Construction d'un modèLe de régression Logistique avec tous Les prédicteurs disponibles

log_model <- log_modeling(train_set)

# Résumé du modèle

summary(log_model)

# Prediction sur les donnees de test

preds_log <- predict(log_model, newdata = test_set, type = 'response')

head(preds_log)

# Definition d'un seuil

seuil <- 0.3

# Conversion des probabilites en resultats (0 et 1) de la variable 'loan_status'

preds_status <- ifelse(preds_log > seuil, 1, 0)

# Matrice de confusion

conf_mat <- table(test_set$loan_status, preds_status)

conf_mat

# composants de la matrice de confusion

TP <- conf_mat[2, 2]

TN <- conf_mat[1, 1]

FP <- conf_mat[1, 2]

FN <- conf_mat[2, 1]

# Score de classification

accuracy <- (TP+TN) / nrow(test_set)

accuracy

# Sensibilite du model

sensitivity <- TP / (FN + TP)

sensitivity

# Specificite du model

specificity <- TN / (TN + FP)

specificity

# Création d'une fonction d'évaluation de modèle
model_evaluation <- function(Model, Seuil) {
  
  predictions <- predict (Model, newdata = test_set, type = 'response')
  predicted_status <- ifelse(predictions > Seuil, 1, 0)
  Conf_Mat <- table(test_set$loan_status, predicted_status)
  Accuracy <- (Conf_Mat[2,2] + Conf_Mat[1,1]) / nrow(test_set)
  Sensitivity <- Conf_Mat[2,2] / (Conf_Mat[2,2] + Conf_Mat[2,1])
  Specificity <- Conf_Mat[1,1] / (Conf_Mat[1,1] + Conf_Mat[1,2])
  One_minus_spec <- 1 - Specificity
  
  results <- list (Conf_Mat, Accuracy, Sensitivity, Specificity, One_minus_spec)
  names (results) <- c('Matrice de confusion','Score de classification',
                       'Sensibilité du modèle','Spécicité du modèle',
                       'One minus Specificity')
  
  return (results)
}

# verification de la fonction

model_evaluation(log_model, 0.3)

# Creation d'une d'une fonction d'affichage des resultats de models pour divers seuils

print_results <- function(Model) {
  
  # definition des seuils
  seuils <- seq(0.01, 0.99, by = 0.01)
  
  # definition des vecteurs vides pour stocker les metriques d'evaluation du model pour divers seuils
  acc_model <- c()
  sens_model <- c()
  spec_model <- c()
  one_minus_spec_model <- c()
  
  for (i in seuils) {
    r <- model_evaluation(Model, i)
    acc_model <- append(acc_model, r[['Score de classification']])
    sens_model <- append(sens_model, r[['Sensibilité du modèle']])
    spec_model <- append(spec_model, r[['Spécicité du modèle']])
    one_minus_spec_model <- append(one_minus_spec_model, r[['One minus Specificity']])
  }
  
  # Dataframe des metriques pour divers seuils
  resultats <- data.frame(cbind(seuils, acc_model, sens_model, spec_model, one_minus_spec_model))
  
  # Graphique montrant des metriques pour differents seuils
  plots <- ggplot() +
    geom_line(data = resultats, aes(x=seuils, y=acc_model, color='red')) +
    geom_line(data = resultats, aes(x=seuils, y=sens_model, color='green')) +
    geom_line(data = resultats, aes(x=seuils, y=spec_model, color='blue')) +
    labs(x = 'Seuil', y = 'Scrore') +
    scale_color_discrete(name = 'Metriques', labels = c("Accuracy", "sensitivity","Specificity"))
  
  # Courbe ROC
  roc_curve <- ggplot() +
    geom_line(data = resultats, aes(x=one_minus_spec_model, y=sens_model)) +
    labs(x = '1 - Specificity', y = 'Sensitivity')
  
  # Resultats de la fonction
  all_results <- list(resultats, plots, roc_curve)
  names(all_results) <- c('Metrics', 'Plot', 'ROC curve')
  
  return (all_results)
}

# Metriques de log_model pour divers seuils

head(print_results(log_model)[['Metrics']])

tail(print_results(log_model)[['Metrics']])

# Graphiques montrant les metriques de log_model pour divers seuils

print_results(log_model)[['Plot']]

# Courbe ROC de log_model

print_results(log_model)[['ROC curve']]


# AUC seuils de log_model

auc_model <- function(model) {
  predictions <- predict(model, newdata = test_set, type = "response")
  return (auc(roc(test_set$loan_status, predictions)))
}

auc_model(log_model)

# Aide sur la fonction step
#?step

# Selection automatique des variables (forward stepwise regression)

null_model <- glm(loan_status ~ 1, data = train_set, family='binomial')

forward_model <- step(null_model,
                      scope = list(lower = null_model, upper = log_model),
                      direction = 'forward',
                      steps = 2000)

head(print_results(forward_model))

# Graphique montrant les metriques de forward_model pour divers seuil

print_results(forward_model)[['Plot']]

# Courbe ROC de forward_model

print_results(forward_model)[['ROC curve']]

# AUC de forward_model

auc_model(forward_model)

# Modele avec les données d'entrainement sur-echantillonnées

log_model_ros <- log_modeling(train_oversampled_df)

summary(log_model_ros)

# Metriques de log_model pour divers seuils

print_results(log_model_ros)[['Plot']]

# Courbe ROC de log_model_ros

print_results(log_model_ros)[['ROC curve']]

# AUC de log_model_ros

auc_model(log_model_ros)

# Modele avec les données d'entrainement sous-echantillonnées

log_model_rus <- log_modeling(train_undersampled_df)

summary(log_model_rus)

# Metriques de log_model pour divers seuils

print_results(log_model_rus)[['Plot']]

# Courbe ROC de log_model_ros

print_results(log_model_rus)[['ROC curve']]

# AUC de log_model_rus

auc_model(log_model_rus)

# Modele avec les données d'entrainement obtenues par combinaison de sur-echantillonage sous-echantillonage

log_model_ros_rus <- log_modeling(train_ros_rus_df)

summary(log_model_ros_rus)

# Metriques de log_model pour divers seuils

print_results(log_model_ros_rus)[['Plot']]

# Courbe ROC de log_model_ros

print_results(log_model_ros_rus)[['ROC curve']]

# AUC de log_model_rus

auc_model(log_model_ros_rus)

log_model_ros_rus

####  Modele de foret aleatoire

library(caret)
fitControl = trainControl(method="cv", number=3)

rf_model <- train(loan_status~.,
                  data = train_set,
                  method="rf",
                  trControl=fitControl)
rf_model

# Enregistrer le model au format RDS
saveRDS(rf_model, "app/credit_scoring_rf_model.rds")

# Exemple d'utilisation du modele
new_data = data.frame(
  "person_age" = 20,
  "person_income" = 500000,
  "person_home_ownership" = "RENT",
  "person_emp_length" = 5,
  "loan_intent" = "PERSONAL",
  "loan_grade" = "D",
  "loan_amnt" = 40000,
  "loan_int_rate" = 17,
  "loan_percent_income" = 0.6,
  "cb_person_default_on_file" = "Y",
  "cb_person_cred_hist_length" = 5
)

new_data

pred <- predict(rf_model, newdata = new_data,type = "prob")
pred

#
# 