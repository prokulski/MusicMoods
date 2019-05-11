library(tidyverse)
library(rvest)

#### zapisane dane ####

albums_moods <- readRDS("data/albums_moods.RDS")




safe_read_html <- safely(read_html)


#### funkcja pobierająca podobne albumy ####

get_similar_albums <- function(album_url) {
  page <- safe_read_html(paste0("https://www.allmusic.com", album_url, "/similar/mobile"))

  if(is_null(page$error)) {
    page <- page$result
  } else {
    return(tibble())
  }

  similar_url <- page %>%
    html_nodes("div.common-album-grid-item") %>%
    html_node("a") %>%
    html_attr("href")

  similar_title <- page %>%
    html_nodes("div.common-album-grid-item") %>%
    html_node("a") %>%
    html_attr("title")

  similar_artist <- page %>%
    html_nodes("div.common-album-grid-item") %>%
    html_node("div.artist") %>%
    html_text() %>%
    str_squish()

  return(tibble(artist = similar_artist,
                title = similar_title,
                url = similar_url))
}




#### unikalne albumy ###

album_urls_uq <- unique(albums_moods$url)


#### pobieramy podobne dla każdego z albumów ####

similar_albums <- tibble()

for(i in seq_along(album_urls_uq)) {
  cat(i, " / ", length(album_urls_uq), "    \r")

  org_df <- albums_moods %>%
    filter(url == album_urls_uq[i]) %>%
    select(artist, title, url) %>%
    distinct() %>%
    set_names(c("org_artist", "org_title", "org_url"))

  tmp_df <- get_similar_albums(album_urls_uq[i])

  if(nrow(tmp_df) != 0) {
    tmp_df <- tmp_df %>%
      mutate(org_url = album_urls_uq[i]) %>%
      left_join(org_df, by = "org_url")

    similar_albums <- bind_rows(similar_albums, tmp_df)
  }

  if(i %% 25 == 0) {
    saveRDS(similar_albums, "data/similar_albums.RDS")
    cat("\nSaved\n")
  }

}


#### zapisujemy dane lokalnie ####

saveRDS(similar_albums, "data/similar_albums.RDS")
