##################
#calculation of collostructional strength
##################
source("https://www.stgries.info/teaching/groningen/coll.analysis.r")
coll.analysis()
#the input file should the following format
#column1: the construction that we are investigating
#column2: the words that cooccur with the construction 
#note: the output is named "12_correlation.csv"

##################
#Network Analysis: collostructional strength
##################
# Read the data
corr <- read.csv("12_correlation.csv")

# Filter and pivot the data
corre1 <- corr %>%
  filter(SUMABSDEV > 15) %>%
  select(-c(LARGESTPREF, SUMABSDEV)) %>%
  pivot_longer(!COLLOCATE, names_to = "FROM", values_to = "CORR")

corre1 <- corre1 %>%
  mutate(CORR = round(CORR)) %>%
  filter(CORR > 0)
colnames(corre1)[c(1)]<-c("TO")

corre3 <- corre1 %>%
  group_by(TO) %>%
  summarise(CORR = sum(CORR))

corre2 <- corre1 %>%
  group_by(FROM) %>%
  summarise(CORR = sqrt(sum(CORR)) * 5) 
colnames(corre2)[1] <- "TO"

wordtest <- rbind(corre3, corre2)

# Create the graph
ig <- igraph::graph_from_data_frame(d = corre1, vertices = wordtest, directed = FALSE)
tg <- tidygraph::as_tbl_graph(ig) %>% 
  tidygraph::activate(nodes) %>% 
  dplyr::mutate(label = name)

# Calculate node and edge attributes
set.seed(12345)
v.size <- sqrt(V(tg)$CORR)
E(tg)$weight <- sqrt(E(tg)$CORR)

# Perform sentiment analysis
node_sentiment_results <- analyzeSentiment(sapply(tg, "[")$nodes$name)
sent_result <- node_sentiment_results$SentimentQDAP

# Map sentiment labels to words
for (i in seq_along(sent_result)) {
  if (sent_result[i] == -1) {
    sent_result[i] <- "neg"
  } else if (sent_result[i] == 1) {
    sent_result[i] <- "pos"
  } else {
    sent_result[i] <- "neu"
  }
}

sent_result[c((length(sent_result)-nrow(corre2)+1):length(sent_result))] <- "intensifier"


# Visualize the graph with sentiment colors

tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour = "gray50",
                lineend = "round",
                strength = .1,
                aes(edge_width = weight)) +
  geom_node_text(aes(label = name, color = sent_result), 
                 repel = F, 
                 point.padding = unit(0.2, "lines"), 
                 size = v.size) +
  scale_color_manual(name = "word sentiment", 
                     values = c("neg" = "lightblue", 
                                "pos" = "pink", 
                                "neu" = "lightyellow", 
                                "intensifier" = "orange")) +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "gray10") +
  theme(legend.position = "NA") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

ggsave("correlation_intensifiers", device = 'tiff', width = 34, height = 24, dpi = 500)

##################
#Network Analysis: based on cooccurrence 
##################
library(flextable)
library(GGally)
library(ggraph)
library(gutenbergr)
library(igraph)
library(Matrix)
library(network)
library(quanteda)
library(sna)
library(tidygraph)
library(tidyverse)
library(tm)
library(tibble)
# activate klippy for copy-to-clipboard button
klippy::klippy()

library(data.table)
intensifiers <- fread("instensifiers_test.csv")
intensifiers
intensifiers %>%
  group_by(to)%>%
  summarize(occurrences = sum(freq)) -> va_1
intensifiers%>%
  group_by(from)%>%
  summarize(occurrences = sum(freq)) -> va_2
colnames(va_1)[1] <-"word"
colnames(va_2)[1] <-"word"
rbind(va_1,va_2)->va
va %>%
  group_by(word)%>%
  summarise(occurrences = sum(occurrences))->va


va
intensifiers

ig <- igraph::graph_from_data_frame(d=intensifiers,vertices =va, directed = FALSE)
tg <- tidygraph::as_tbl_graph(ig) %>% 
  tidygraph::activate(nodes) %>% 
  dplyr::mutate(label=name)
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1,
                alpha = .1) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 colour="gray10") +
  theme_graph(background = "white") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

v.size <- V(tg)$occurrences
# inspect
v.size

Copy# set seed
set.seed(12345)
# edge size shows frequency of co-occurrence
tg %>%
  ggraph(layout = "fr") +
  geom_edge_arc(colour= "gray50",
                lineend = "round",
                strength = .1) +
  geom_node_point(size=log(v.size)*2) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 size=sqrt(v.size), 
                 colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE)
