
packages <- c("dplyr", "magrittr", "quanteda", "stm")

# install if necessary
install.packages(setdiff(packages, rownames(installed.packages())))

# load
lapply(packages, library, character.only = TRUE)

# data
rezensionen_data_raw <- read.csv("~/aUni/M.A. Soziologie/24_25 WiSe/Forschungsprakt CSS/term_paper_css/collected_data/06_data_raw_no_doubles.csv")
head(rezensionen_data_raw)



# Preprocessing

rezensionen_data_raw$rezension <- rezensionen_data_raw$rezension
rezensionen_data_raw$bewertung <- rezensionen_data_raw$bewertung

table(rezensionen_data_raw$bewertung)

corpus <- corpus(as.character(rezensionen_data_raw$rezension),
                 docvars = data.frame(genre = rezensionen_data_raw$bewertung))

# Tokenisierung
toks <- tokens(corpus, remove_punct = T,
               remove_numbers = T,
               remove_symbols = T,
               split_hyphens = T,
               remove_url = T,
               include_docvars = T)

# Stopworder entfernen und Stemmen
toks <- tokens_remove(toks, stopwords("de"), case_insensitive = TRUE)
toks <- tokens_wordstem(toks, language = "german")

# Create DFM
rezensionen_dfm <- dfm(toks)

rezensionen_dfm <- rezensionen_dfm[!rowSums(rezensionen_dfm) == 0, ] # leere Zeilen entfernen

nrow(rezensionen_dfm)

head(rezensionen_dfm)


# STM

k <- 12 # number of topics

set.seed(4242)
rezensionen_topics <- stm(rezensionen_dfm,
                          K = k,
                          prevalence ~ genre,
                          verbose = FALSE)

summary(rezensionen_topics)