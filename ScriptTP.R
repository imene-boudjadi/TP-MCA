install.packages("readxl")
library(readxl)

# chemin d'accès complet vers le fichier de données
chemin_fichier <- "/Users/mac/Desktop/DataSet_TP_ANAD.csv"

#importer les données
donnees <- read.csv(chemin_fichier, fileEncoding = "UTF-8", header = TRUE, stringsAsFactors = FALSE)

# Affichez les premières lignes du dataframe
head(donnees)

# Charger la bibliothèque ggplot2 (pour les graphes)
library(ggplot2)


#********************************************** Q1 ********************************************************
df_q1 <- data.frame(Q1 = donnees[[1]])

# Histogramme Q1
q1_colors <- c("#F9E8D9", "#F7B787", "#EE7214", "#9ADCFF")

ggplot(df_q1, aes(x = Q1, fill = Q1)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = q1_colors) + 
  labs(title = "Répartition des individus par fonction",
       x = "Fonction",
       y = "Nombre d'individus") +
  theme_minimal()

# Fréquence Q1
col_num_q1 <- 1
freq_table_q1 <- table(df_q1[[col_num_q1]])
pie(freq_table_q1, 
    labels = paste(names(freq_table_q1), sprintf("%.1f%%", prop.table(freq_table_q1) * 100)),
    col = q1_colors,
    main = "Répartition des individus par fonction")


#********************************************** Q2 ********************************************************
df_q2 <- data.frame(Q2 = donnees[[2]])

# Histogramme Q2
q2_colors <- c("#FFC0D9", "#CAEDFF", "#EE7214")

ggplot(df_q2, aes(x = Q2, fill = Q2)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = q2_colors) +  
  labs(title = "Répartition des individus par age",
       x = "Age",
       y = "Nombre d'individus") +
  theme_minimal()

# Fréquence Q2
col_num_q2 <- 1
freq_table_q2 <- table(df_q2[[col_num_q2]])
pie(freq_table_q2, 
    labels = paste(names(freq_table_q2), sprintf("%.1f%%", prop.table(freq_table_q2) * 100)),
    col = q2_colors,
    main = "Répartition des individus par age")
#********************************************** Q3 ********************************************************
df_q3 <- data.frame(Q3 = donnees[[3]])

# Histogramme Q3
q3_colors <- c("#FBFFDC", "#D0F5BE")

ggplot(df_q3, aes(x = Q3, fill = Q3)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = q3_colors) +  
  labs(title = "Répartition des individus par genre",
       x = "Genre",
       y = "Nombre d'individus") +
  theme_minimal()

# Fréquence Q3
col_num_q3 <- 1
freq_table_q3 <- table(df_q3[[col_num_q3]])
pie(freq_table_q3, 
    labels = paste(names(freq_table_q3), sprintf("%.1f%%", prop.table(freq_table_q3) * 100)),
    col = q3_colors,
    main = "Répartition des individus par Genre")
#********************************************** Q4 ********************************************************
df_q4 <- data.frame(Q4 = donnees[[4]])

# Histogramme Q4
q4_colors <- c("#85E6C5", "#FEFFAC", "#FF9EAA", "#78C1F3","#EEEEEE")

ggplot(df_q4, aes(x = Q4, fill = Q4)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = q4_colors) + 
  labs(title = "Fréquence du visite du site LibGen",
       x = "Fréquence visite",
       y = "Nombre d'individus") +
  theme_minimal()

# Fréquence Q4
col_num_q4 <- 1
freq_table_q4 <- table(df_q4[[col_num_q4]])
pie(freq_table_q4, 
    labels = paste(names(freq_table_q4), sprintf("%.1f%%", prop.table(freq_table_q4) * 100)),
    col = q4_colors,
    main = "Fréquence du visite du site LibGen")
#********************************************** Q5 ********************************************************
df_q5 <- data.frame(Q5 = donnees[[5]])

# Histogramme Q5
q5_colors <- c("#85E6C5", "#FEFFAC", "#FF9EAA", "#78C1F3")

ggplot(df_q5, aes(x = Q5, fill = Q5)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = q5_colors) +  
  labs(title = "Notes de l'interface graphique du site LibGen",
       x = "Note de l'interface",
       y = "Nombre d'individus") +
  theme_minimal()

# Fréquence Q5
col_num_q5 <- 1
freq_table_q5 <- table(df_q5[[col_num_q5]])
pie(freq_table_q5, 
    labels = paste(names(freq_table_q5), sprintf("%.1f%%", prop.table(freq_table_q5) * 100)),
    col = q5_colors,
    main = "Notes de l'interface graphique du site LibGen")
#********************************************** Q6 ********************************************************
df_q6 <- data.frame(Q6 = donnees[[6]])

# Histogramme Q6
q6_colors <- c("#85E6C5", "#FEFFAC", "#FF9EAA", "#78C1F3")

ggplot(df_q6, aes(x = Q6, fill = Q6)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = q6_colors) +  
  labs(title = "Note de l'experience utilisateur du site LibGen",
       x = "Note UX",
       y = "Nombre d'individus") +
  theme_minimal()

# Fréquence Q6
col_num_q6 <- 1
freq_table_q6 <- table(df_q6[[col_num_q6]])
pie(freq_table_q6, 
    labels = paste(names(freq_table_q6), sprintf("%.1f%%", prop.table(freq_table_q6) * 100)),
    col = q6_colors,
    main = "Note de l'experience utilisateur du site LibGen")


#********************************************** Visualisation de la fréquence des categories des variables ******************************************
frequence_tables <- sapply(donnees,function(x) table(x))
print(frequence_tables)

#********************************************** TABLEAU DISJONCTIF ******************************************

# Créer un tableau disjonctif
tableau_disjonctif <- model.matrix(~ . - 1, data = donnees)

#Affichage des premieres lignes
head(tableau_disjonctif)

#********************************************** Faire une AFCM ******************************************
install.packages("FactoMineR")
library(FactoMineR)

# Effectuer l'AFCm
DataAfcm <- MCA(donnees, graph = FALSE)

# Afficher un résumé des résultats
summary(DataAfcm)


#***************************** VALEURS PROPRES ***************************************************************
install.packages("factoextra")
library ("factoextra")

valeurs_propres <- get_eigenvalue (DataAfcm)
#Affichage du tableau des valeurs propres
valeurs_propres

# Afficher les 3 premieres lignes du tableau des valeurs propres
head(valeurs_propres)

# afficher les valeurs propres en fonction des axes factoriels
fviz_screeplot(DataAfcm,addlabels =TRUE, ylim = c(0,15))

# **************************** Biplot des individus et variables catégories **********************************
fviz_mca_biplot(DataAfcm, repel = TRUE, ggtheme = theme_minimal())

#  ****************************  Tableau de contribution **********************************
tableau_contribution <- get_mca_var(DataAfcm)$contrib
tableau_contribution

#visualisation du biplot en prenant en consideration la contribution des variables
fviz_mca_biplot(DataAfcm, repel = TRUE, col.var = "contrib", gradient.cols = c("#1B78AF", "#D28782"))

# ***************************** Coordonées des modalites de variables ********************
fviz_mca_var(DataAfcm, col.var="#1B78AF", shape.var = 15,repel = TRUE)


# *****************************  Contribution des catégories de variables aux dimensions ********************
# Contribution à la dimension 1
fviz_contrib(DataAfcm, choice = "var", axes = 1, top = 15)
# Contributions à la dimension 2
fviz_contrib(DataAfcm, choice = "var", axes = 2, top = 15)
# Contributions à la dimension 3
fviz_contrib(DataAfcm, choice = "var", axes = 3, top = 15)

# ********************************* Contribution et Qualité des individus ********************
fviz_mca_ind(DataAfcm, col.ind = "cos2", 
             gradient.cols = c("red", "blue", "green"),
             repel = TRUE,
             ggtheme = theme_minimal())  # colorer les individus selon leurs cos2
fviz_cos2(DataAfcm, choice = "ind", axes = 1, top = 20) # cos2 des individus dans l'axe 1
fviz_contrib(DataAfcm, choice = "ind", axes = 1, top = 20) # contributions des individus dans l'axe 1
fviz_cos2(DataAfcm, choice = "ind", axes = 2, top = 20) # cos2 des individus dans l'axe 2
fviz_contrib(DataAfcm, choice = "ind", axes = 2, top = 20) # contributions des individus dans l'axe 2
fviz_cos2(DataAfcm, choice = "ind", axes = 3, top = 20) # cos2 des individus dans l'axe 3
fviz_contrib(DataAfcm, choice = "ind", axes = 3, top = 20) # contributions des individus dans l'axe 3

# ******************* TABLEAU DE CONTINGENCE ************************
df_q1 <- data.frame(Q1 = donnees[[1]])
df_q2 <- data.frame(Q2 = donnees[[2]])

df_q1$Q2 <- factor(df_q1$Q1)
df_q2$Q1 <- factor(df_q2$Q2)

table_contingence <- table(df_q1$Q1, df_q2$Q2)
new_row_names <- c("Etudiant", "Enseignat", "Alumni")
new_col_names <- c("moins 18 ans", "entre 18 et 25 ans", "plus de 35 ans")
rownames(table_contingence) <- new_row_names
colnames(table_contingence) <- new_col_names
table_contingence

# Effectuer l'AFC
afc_result <- CA(table_contingence)

# Afficher les résultats
summary(afc_result)
