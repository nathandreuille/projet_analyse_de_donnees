# Packages nécessaires pour le TD sans les cartes
library(FactoMineR)
library(explor)
library(dplyr)
library(stringr)
library(gtsummary)
library(questionr)
library(kableExtra)





# Attribution du répertoire de travail : 
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
moyenne_age <- mean(police$age, na.rm = TRUE)

# Remplacer les NA par la moyenne calculée
police$age[is.na(police$age)] <- moyenne_age

# Créer des groupes d'âge
police$age_group <- cut(police$age, breaks = c(0, 30, 45, 60, 100), 
                        labels = c("18-30", "31-45", "46-60", "60+"), include.lowest = TRUE)



# Nettoyage de la variable raceethnicity

# Regrouper les catégories sous-représentées dans "Other"

police$raceethnicity <- recode(police$raceethnicity, 
                               "Asian/Pacific Islander" = "Other",
                               "Native American" = "Other",
                               "Unknown" = "Other")

# Vérification des nouvelles catégories
table(police$raceethnicity)


# Nettoyer et regrouper les valeurs de 'armed'
police$armed_group <- ifelse(police$armed %in% c("Firearm", "Non-lethal firearm"), "Armed (Firearm)",
                             ifelse(police$armed == "No", "Unarmed", "Other Weapon"))

# Vérification des résultats
table(police$armed_group)


# Calculer la moyenne de comp_income en ignorant les valeurs manquantes
moyenne_comp_income <- mean(police$comp_income, na.rm = TRUE)

# Remplacer les valeurs NA par la moyenne calculée
police$comp_income[is.na(police$comp_income)] <- moyenne_comp_income

# Création de catégories pour comp_income
police$income_group <- cut(police$comp_income,
                           breaks = c(-Inf, 0.8, 1.2, Inf),  
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


# Création du top 10 des états les plus touchés
state_freq <- sort(table(police$state), decreasing = TRUE)
head(state_freq, 10)



# Convertir toutes les variables qualitatives en facteurs
police[sapply(police, is.character)] <- lapply(police[sapply(police, is.character)], as.factor)

# Vérification des types de variables
str(police)



# ETAPE 3 : Statistiques descriptives univariées et bivariées 

# Tri univarié des variables principales :

# Liste des variables actives
variables_actives <- c("age_group", "gender", "raceethnicity", "armed_group", "income_group")

# Génération du tableau résumé pour les variables principales

summary_table <- police %>%
  select(all_of(variables_actives)) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)"), # Effectif et pourcentage
    missing_text = "NA"                                 # Afficher les valeurs manquantes
  ) %>%
  bold_labels() %>%                                     # Mettre les labels en gras
  modify_header(label ~ "**Variable**") %>%             # Renommer la colonne des variables
  modify_caption("**Résumé des variables principales des données 'police_killings'**") # Ajouter un titre

# Afficher le tableau
summary_table



# Statistiques descriptives bivariées des variables principales : 


# Relation entre raceethnicity et armed_group :

# Créer un tableau croisé avec xtabs
tab1 <- xtabs(~ raceethnicity + armed_group, data = police)


# Calculer les pourcentages en ligne
l1 <- lprop(tab1)


# Générer un tableau 
kable(round(l1, 1), digits = 1, caption = "Pourcentages en ligne: Relation entre raceethnicity et armed_group") %>% kable_styling()

# Test du Khi-2 pour le tableau croisé entre raceethnicity et armed_group

chisq.test(tab1)

# Calcul du V de Cramer pour le tableau croisé entre raceethnicity et armed_group

cramer.v(tab1)


# Relation entre income_group et raceethnicity :

# Créer un tableau croisé avec xtabs
tab2 <- xtabs(~ income_group + raceethnicity, data = police)

# Calculer les pourcentages en ligne
l2 <- lprop(tab2)


# Générer un tableau bien formaté
kable(round(l2, 1), digits = 1, caption = "Pourcentages en ligne: Relation entre income_group et raceethnicity") %>% kable_styling()

# Test du Khi-2 pour le tableau croisé entre income_group et raceethnicity

chisq.test(tab2)

# Calcul du V de Cramer pour le tableau croisé entre income_group et raceethnicity

cramer.v(tab2)



# Etape 4 : Réalisation de l'ACM 

# Liste des variables que l'on va utiliser pour notre acm

police_acm <- subset(police, select = c("cause","state","age_group", "gender", "raceethnicity", "armed_group", "income_group"))


# Analyse factorielle multiple :
acm1 <- MCA(police_acm, quali.sup = c(1:2) ,row.w=police$poidsnorm,graph = F)

summary(acm1)

# Afficher ACM
explor(acm1)

# Afficher les valeurs propres 
vp<-acm1$eig

# On ne conserve que les valeurs propres en valeur aboslue
vp[,1]
sum(vp[,1])
# On regarde lesquelles sont supérieures à l'inertie moyenne
vp[,1]>mean(vp[,1])
