library(tidyverse)
library(igraph)
library(networkD3)

#### zapisane dane ####

albums_moods <- readRDS("data/albums_moods.RDS")
similar_albums <- readRDS("data/similar_albums.RDS")


top_artist <- albums_moods %>%
  count(artist, sort = T) %>%
  pull(artist) %>%
  .[1:50]

top_mood <- albums_moods %>%
  count(mood, sort = T) %>%
  pull(mood) %>%
  .[1:20]

albums_moods %>%
  count(artist, mood, sort = T) %>%
  filter(artist %in% top_artist) %>%
  group_by(artist) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  filter(mood %in% top_mood) %>%
  ggplot() +
  geom_col(aes(artist, p, fill = mood)) +
  coord_flip()


albums_moods %>%
  filter(artist %in% top_artist) %>%
  count(artist, mood, sort = T) %>%
  group_by(artist) %>%
  mutate(p = 100*n/sum(n)) %>%
  top_n(1, p) %>%
  ungroup() %>%
  arrange(artist, desc(p))


albums_moods %>%
  count(year, mood) %>%
  group_by(year) %>%
  mutate(p = 100*n/sum(n)) %>%
  top_n(1, p) %>%
  ungroup()




g <- albums_moods %>%
  filter(artist %in% top_artist) %>%
  filter(artist != "Various Artists") %>%
  count(artist, mood, sort = T) %>%
  graph_from_data_frame(directed = FALSE)


plot(g,
     vertex.color = if_else(V(g)$name %in% top_artist, "blue", "lightblue"),
     vertex.size = 3,
     vertex.label.cex = 1,
     vertex.label.color = "gray40",
     edge.curved = TRUE)



simpleNetwork(albums_moods %>%
                filter(artist %in% top_artist) %>%
                filter(artist != "Various Artists") %>%
                count(artist, mood, sort = T),
              zoom = TRUE)





similar_albums %>%
  select(org_artist, artist, org_title, title) %>%
  count(org_artist, artist, sort = T)

graph_df <- similar_albums %>%
  select(org_artist, artist, org_title, title) %>%
  distinct() %>%
  count(org_artist, artist, sort = T) %>%
  filter(org_artist != artist) %>%
  filter(n >= quantile(n, 0.998))


g <- graph_from_data_frame(graph_df, directed = FALSE)

wc <- cluster_walktrap(g)
V(g)$group <- wc$membership


plot(wc, g,
     vertex.color = V(g)$group,
     vertex.size = 3,
     vertex.label.cex = 0.8,
     vertex.label.color = "gray40")

simpleNetwork(graph_df, zoom = TRUE)




# pink floyd
sel_artists <- similar_albums %>%
  filter(org_artist == "Pink Floyd" | artist == "Pink Floyd") %>%
  select(org_artist, artist) %>%
  distinct()

sel_artists <- unique(c(sel_artists$org_artist, sel_artists$artist))

pf_graph_df <- similar_albums %>%
  filter(org_artist %in% sel_artists | artist %in% sel_artists) %>%
  select(org_artist, artist, org_title, title) %>%
  distinct() %>%
  count(org_artist, artist, sort = T) %>%
  filter(org_artist != artist) %>%
  filter(n >= quantile(n, 0.75))


simpleNetwork(pf_graph_df, zoom = TRUE)

