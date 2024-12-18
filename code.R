# Packages nécessaires pour le TD sans les cartes
library(FactoMineR)
library(explor)
library(ggplot2)
library(readr)
library(factoextra)
library(dplyr)
library(tibble)
library(corrplot)

# Question 1 : charger la base --------------------------------------------------------------


# Attribution du répertoire de travail : c'est l'emplacement physique où se trouvent les jeux de données qui seront utilisés et crées
setwd("C:/Users/quent/Documents/2025/Analyse de données/projet_analyse_de_donnees")

# Charger la base
police <- read.csv("police_killings.csv")

# Exploration
dim(police)
str(police)
summary(police)
head(police)


#ETAPE 2 : Nettoyage et présentation des données que l'on va utiliser :



# Création de groupes d'âge pour notre ACM :

# Convertir la variable 'age' en numérique
police$age <- as.numeric(as.character(police$age))

# Calculer la moyenne des âges en ignorant les NA
mean_age <- mean(police$age, na.rm = TRUE)

# Remplacer les NA par la moyenne calculée
police$age[is.na(police$age)] <- mean_age

# Créer des groupes d'âge
police$age_group <- cut(police$age, breaks = c(0, 30, 45, 60, 100), 
                        labels = c("18-30", "31-45", "46-60", "60+"), include.lowest = TRUE)


# Transformation de la variable gender en facteur 
police$gender <- as.factor(police$gender)


# Nettoyage de la variable raceethnicity

# Regrouper les catégories sous-représentées dans "Other"
police$raceethnicity <- as.factor(police$raceethnicity)  

police$raceethnicity <- recode(police$raceethnicity, 
                               "Asian/Pacific Islander" = "Other",
                               "Native American" = "Other",
                               "Unknown" = "Other")

# Vérification des nouvelles catégories
table(police$raceethnicity)


# Créer une table de fréquence des incidents par État
table(police$state)


# Nettoyer et regrouper les valeurs de 'armed'
police$armed_group <- ifelse(police$armed %in% c("Firearm", "Non-lethal firearm"), "Armed (Firearm)",
                             ifelse(police$armed == "No", "Unarmed", "Other Weapon"))

# Vérification des résultats
table(police$armed_group)


# Calculer la moyenne de comp_income en ignorant les valeurs manquantes
mean_comp_income <- mean(police$comp_income, na.rm = TRUE)

# Remplacer les valeurs NA par la moyenne calculée
police$comp_income[is.na(police$comp_income)] <- mean_comp_income

# Création de catégories pour comp_income
police$income_group <- cut(police$comp_income,
                           breaks = c(-Inf, 0.8, 1.2, Inf),  # Seuils ajustables
                           labels = c("Below Average", "Average", "Above Average"),
                           include.lowest = TRUE)

# Vérification des résultats
table(police$income_group)

#Visualisation de la répartition 
barplot(table(police$income_group), 
        main = "Répartition des revenus relatifs", 
        col = "lightblue", 
        ylab = "Effectif", 
        xlab = "Groupe de revenus")


# Création du top 10 des villes les plus touchées
city_freq <- sort(table(police$city), decreasing = TRUE)
head(city_freq, 10)


# Présentation de cause : 
table(police$cause)


# Convertir toutes les variables qualitatives en facteurs
police[sapply(police, is.character)] <- lapply(police[sapply(police, is.character)], as.factor)

# Vérification des types de variables
str(police)



# ETAPE 3 : 

# Liste des variables principales
primary_vars <- c("age_group", "gender", "raceethnicity", "state", "armed_group", "income_group")


# Génération du tableau résumé pour les variables principales
library(gtsummary)
summary_table <- police %>%
  select(all_of(primary_vars)) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)"), # Effectif et pourcentage
    missing_text = "NA"                                 # Afficher les valeurs manquantes
  )

# Afficher le tableau
summary_table






