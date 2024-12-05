#CRÉATION DU DATAFRAME
#Création les vecteurs.
cops <- c("COP1 (1992)", "COP2 (1994)", "COP3 (1995)", "COP4 (1998)", "COP5 (1999)", "COP6 (2002)", "COP7 (2004)", "COP8 (2006)",
          "COP9 (2008)", "COP10 (2011)", "COP11 (2013)", "COP12 (2015)", "COP13 (2017)", "COP14 (2019)", "COP15 (2021)", "COP16 (2023)")

chemicals <- c(2, 7, 1, 17, 19, 40, 40, 48, 33, 130, 52, 130, 161, 96, 203, 157)

ewaste <- c(0, 0, 0, 13, 0, 39, 11, 145, 55, 47, 49, 80, 86, 122, 126, 89)

plastics <- c(0, 2, 0, 9, 8, 12, 23, 4, 0, 1, 3, 0, 18, 127, 135, 113)

microplastics <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 22, 8, 4)

resins <- c(0, 0, 0, 8, 0, 1, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0)

batteries <- c(0, 0, 0, 10, 1, 20, 5, 9, 1, 2, 1, 1, 4, 18, 39, 33)

household <- c(1, 1, 0, 0, 0, 2, 5, 11, 6, 1, 1, 21, 35, 27, 20, 27)

nanomaterials <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 30, 22, 13)

metals <- c(3, 8, 2, 40, 6, 90, 26, 4, 2, 2, 0, 0, 1, 2, 0, 1)

oils <- c(1, 5, 0, 4, 1, 23, 4, 33, 1, 3, 1, 1, 4, 4, 6, 4)

biomedical <- c(0, 0, 0, 1, 3, 14, 7, 8, 6, 3, 0, 1, 3, 2, 0, 1)

pesticides <- c(0, 0, 0, 4, 11, 10, 19, 36, 13, 12, 8, 18, 19, 16, 51, 37)

rubber <- c(0, 0, 0, 6, 4, 0, 0, 3, 30, 7, 7, 0, 1, 2, 10, 28)

mercury <- c(0, 0, 0, 10, 0, 2, 11, 17, 23, 31, 27, 26, 19, 32, 52, 22)

pops <- c(0, 0, 0, 6, 13, 26, 33, 34, 25, 72, 46, 59, 50, 42, 56, 56)

# Créer le dataframe.
df_regimes <- data.frame(COP = cops, Chemicals = chemicals, Ewaste = ewaste, Plastics = plastics, 
                         Microplastics = microplastics, Resins = resins, Batteries = batteries, Household = household, 
                         Nanomaterials = nanomaterials, Metals = metals, Oils = oils, Biomedical = biomedical, 
                         Pesticides = pesticides, Rubber = rubber, Mercury = mercury, POPs = pops)

#Créer la visualisation.

# Charger les librairies nécessaires
library(ggplot2)
library(tidyr)
library(dplyr)

# Créer une colonne numérique "COP_number" pour l'ordre des COP
df_regimes$COP_number <- as.numeric(gsub("COP(\\d+) .*", "\\1", df_regimes$COP))

# Créer le dataframe long (reshaping)
df_long <- df_regimes %>%
  pivot_longer(cols = -c(COP, COP_number), names_to = "Waste", values_to = "Value")

# Convertir COP en facteur ordonné selon COP_number
df_long$COP <- factor(df_long$COP, levels = df_regimes$COP[order(df_regimes$COP_number)])

# Définir une palette de 15 couleurs distinctes
custom_colors <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
  "#CC79A7", "#999999", "#FF5733", "#33FF57", "#5733FF", "#FF33A1", 
  "#33A1FF", "#FF8333", "#8B33FF"
)

# Créer le line plot avec ggplot
ggplot(df_long, aes(x = COP, y = Value, color = Waste, group = Waste)) +
  geom_line() + 
  geom_point() +  # Ajouter des points pour mieux visualiser les valeurs
  labs(title = "Évolution du régime des déchets à travers les COPs de la Convention de Bâle", 
       x = NULL, 
       y = "Fréquence des mots associés", 
       color = "Types de déchets") + 
  scale_color_manual(values = custom_colors) +  # Appliquer la palette personnalisée
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))  # Incliner l'axe des X pour une meilleure lisibilité




