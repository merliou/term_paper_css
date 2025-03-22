
packages <- c("dplyr", "magrittr", "quanteda", "stm", "tidyr", "BTM", "textplot", "ggraph", "concaveman")

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


# ------------- STM -------------------

k <- 9 # number of topics

set.seed(42)
rezensionen_topics <- stm(rezensionen_dfm,
                          K = k,
                          prevalence = ~ genre,
                          verbose = FALSE)

summary(rezensionen_topics)

# Highest Prob = Highest Probability
# FREX = Frequency - Exclusivity; 'Charakteristische Wörter'
# Lift = Überproportional häufige Worte
# Score = Gesamtwert aus Prob, FREX und Lift

plot(rezensionen_topics)

# Entsprechende Artikel begutachten !! FUNZT NET; maybe places_id oben einfügen und damit?
findThoughts(rezensionen_topics, texts = rezensionen_data_raw$rezension, n = 3, topics = 8)


# Texting for Differences Across Genres
# estimating effects (Regression)

genre_diff <- estimateEffect(1:k ~ genre,
                             rezensionen_topics,
                             meta = rezensionen_dfm@docvars
                             )
# Visualizing Differences
plot(genre_diff,
     topics = 9,
     covariate = "genre",
     method = "pointestimate",
     main = "Topic 9")


# SEARCHING K
out <- convert(rezensionen_dfm, to = "stm")

set.seed(42)
kresult <- searchK(out$documents,
                   out$vocab,
                   seq(3, 20, 3),
                   prevalence = ~ genre,
                   data = out$meta,
                   verbose = FALSE)

# Visualising k search results
plot(kresult)

# Evaluating Exclusivity and Semantic Coherence
plot(kresult$results$exclus, kresult$results$semcoh,
     xlab = "Semantic Coherence",
     ylab = "Exclusivity")

text(kresult$results$exclus, kresult$results$semcoh,
     labels = kresult$results$K, pos = 4)


# Evaluate Actual Topics
topicQuality(model = rezensionen_topics,
             documents = rezensionen_dfm)



# ------------- BTM -------------------

install.packages('BTM')
library(BTM)

# preprocessing
toks_2 <- convert(rezensionen_dfm, to = "data.frame")
toks_2 <- toks_2 %>% pivot_longer(cols = c(!doc_id), names_to = "tokens")

toks_2 <- toks_2[toks_2$value > 0, ]
toks_2 <- toks_2[, c("doc_id", "tokens")]

# run model
library(BTM)
k <- 9
set.seed(42)
bi_topics <- BTM(toks_2,
                 k = k,
                 background = TRUE) #background topic filters common words

terms(bi_topics, top_n = 10)

# plotting
library(textplot)
plot(bi_topics, which = 2:10, subtitle = "All 10 topics",
     labels = paste(round(bi_topics$theta*100, 2), "%", sep = ""), top_n = 15)
