#PACKAGES À INSTALLER
install.packages(c("pdftools", "tm", "tidyverse", "textclean", "quanteda", "quanteda.textstats"))
install.packages("tesseract")
library(tesseract)
library(pdftools)  # Pour extraire le texte des PDF
library(tm)  # Pour la manipulation du texte
library(tidyverse)  # Pour le nettoyage et la manipulation de données
library(textclean)  # Pour le nettoyage du texte
library(quanteda)  # Pour l'analyse textuelle
library(quanteda.textstats)

# Fonction pour extraire le texte avec gestion des erreurs
extract_text_safe <- function(pdf) {
  tryCatch({
    text <- pdf_text(pdf)
    if (length(text) == 0) {
      stop("Aucun texte extrait")
    }
    return(text)
  }, error = function(e) {
    message(paste("Erreur lors de l'extraction du texte de", pdf, ":", e$message))
    return(NULL)
  })
}

# Fonction OCR pour les fichiers scannés
ocr_text <- function(pdf) {
  text <- extract_text_safe(pdf)
  if (is.null(text)) {
    text <- ocr(pdf)
  }
  return(text)
}

# Liste des fichiers PDF
pdf_files <- list.files(path = "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP", pattern = "*.pdf", full.names = TRUE)

# Extraire le texte des fichiers
text_data <- lapply(pdf_files, ocr_text)

#NETTOYAGE DU TEXTE
# Fonction de nettoyage du texte
clean_text <- function(text) {
  text <- tolower(text)  # Convertir en minuscules
  text <- removePunctuation(text)  # Enlever la ponctuation
  text <- removeNumbers(text)  # Enlever les chiffres
  text <- stripWhitespace(text)  # Supprimer les espaces blancs supplémentaires
  return(text)
}

# Extraire et nettoyer les textes des 17 fichiers
clean_text_data <- lapply(text_data, function(text) {
  if (!is.null(text)) {
    cleaned <- sapply(text, clean_text)  # Appliquer le nettoyage sur chaque page
    return(paste(cleaned, collapse = " "))  # Combiner le texte des différentes pages du PDF
  }
  return("")  # Si aucun texte n'a été extrait
})

# Vérifiez que le nettoyage fonctionne
head(clean_text_data)  # Affiche les premiers textes nettoyés

# Supposons que `file_paths` contient les chemins complets des fichiers
file_paths <- c("/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP1.pdf", 
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP2.pdf",
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP3.pdf",
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP4.pdf",
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP5.pdf",
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP6.pdf",
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP7.pdf",
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP8.pdf",
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP9.pdf",
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP10.pdf",
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP11.pdf",
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP12.pdf",
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP13.pdf",
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP14.pdf",
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP15.pdf",
                "/Users/jeannedesrosiers/Desktop/Outils_numeriques/Mini travail recherche/Donnees/COP/COP16.pdf")

# Extraire les noms des fichiers (sans le chemin)
doc_names <- basename(file_paths)

#CRÉATION DE LA BASE DE DONNÉES TEXTUELLES
# Créer un data.frame avec l'identifiant du document et le texte nettoyé
cop_data <- data.frame(
  document = doc_names,  # Identifier les documents (1 à n, où n est la longueur de clean_text_data)
  text = unlist(clean_text_data),  # Assurez-vous que clean_text_data est une liste de textes
  stringsAsFactors = FALSE  # Eviter la conversion automatique en facteur
)

# Vérifiez le contenu du data.frame
head(cop_data)


#ANALYSE TEXTUELLE DE LA FRÉQUENCE DES RÉGIMES
# Créer un corpus quanteda
corpusREG <- corpus(cop_data$text)

# Tokeniser le texte (transformer le texte en une liste de mots)
tokens_data <- tokens(corpusREG, 
                      remove_punct = FALSE,       # Garder la ponctuation si nécessaire
                      remove_symbols = FALSE,    # Garder les symboles comme les tirets
                      remove_numbers = TRUE)     # Optionnel : éliminer les numéros

# Créer la matrice de termes (DTM) à partir des tokens
dtm <- dfm(tokens_data, tolower = TRUE)  # Convertir le texte en minuscules

# Liste de mots d'intérêt à inclure
selected_words <- c("ewaste", "electronics", "electronicwaste", "ewastes", "organic", "plastic",
                    "plastics", "microplastics", "resins", "resin", "organic", "batteries", "household", "households", "nanomaterials",
                    "metal", "métaux", "metals", "metallic", "oils", "oil", "biomedical", "medical", "pesticides", "pesticide", "tyres", "tires") #À changer pour les régimes à observer

# Sélectionner les mots d'intérêt dans la DTM
dtm_selected <- dfm_select(dtm, pattern = selected_words)

# Convertir la DTM sélectionnée en une matrice
dtm_selected_matrix <- as.matrix(dtm_selected)


# Ajouter les noms des documents au tableau des fréquences par document
freq_per_document <- data.frame(Document = doc_names, dtm_selected_matrix)

# Afficher un aperçu des fréquences par document
head(freq_per_document)

# Si vous souhaitez obtenir la somme des fréquences pour chaque document (par exemple, combien de fois chaque document mentionne les mots-clés en total),
# vous pouvez utiliser la fonction `rowSums()` pour obtenir la somme de la fréquence par document.
freq_per_document_sum <- data.frame(Document = freq_per_document$Document, Frequency = rowSums(freq_per_document[, -1]))

# Afficher les résultats avec la somme des fréquences par document
head(freq_per_document_sum)
