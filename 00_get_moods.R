library(tidyverse)
library(rvest)


safe_read_html <- safely(read_html)



#### pobranie listy moodów ####
page <- read_html("https://www.allmusic.com/moods")

moods <- page %>%
  html_nodes("ul.mood-list") %>%
  html_nodes("li") %>%
  html_nodes("h2") %>%
  html_nodes("a")

moods_df <- tibble(
  mood = moods %>% html_text(),
  url = moods %>% html_attr("href")
) %>%
  mutate(url_ajax = paste0(str_replace_all(url, "/mood/", "/mood/ajax/"), "/albums/listview"))



#### funkcja pobierająca albumy dla moodu ####
get_mood_table <- function(mood_name, mood_url) {

  page_mood <- safe_read_html(mood_url)

  if(is_null(page_mood$error)) {
    page_mood <- page_mood$result
  } else {
    return(tibble())
  }

  tds <- page_mood %>% html_nodes("td")

  album_url <- vector("character", length(tds) / 5)
  album_year <- vector("integer", length(tds) / 5)
  album_title <- vector("character", length(tds) / 5)
  album_artist <- vector("character", length(tds) / 5)
  album_id <- 1

  for(i in seq_along(tds)) {

    if(i %% 5 == 0) album_id <- album_id + 1

    switch(tds[[i]] %>% html_attr("class"),
           "cover" = {
             album_url[album_id] <- tds[[i]] %>%
               html_node("a") %>%
               html_attr("href")
           },
           "year" = {
             album_year[album_id] <- tds[[i]] %>%
               html_text() %>%
               str_squish() %>%
               as.integer()
           },
           "album-artist" = {
             album_title[album_id] <- tds[[i]] %>%
               html_node("div.album") %>%
               html_text() %>%
               str_trim()

             album_artist[album_id] <- tds[[i]] %>%
               html_node("div.artist") %>%
               html_text() %>%
               str_trim()
           },
           {
           }
    )

  }

  return(tibble(artist = album_artist,
                title = album_title,
                year = album_year,
                url = album_url) %>%
           mutate(mood = mood_name))

}



#### pobranie albumów do moodów ####

albums_moods <- tibble()

for(i in seq_len(nrow(moods_df))) {

  cat(i, " / ", nrow(moods_df), "   \r")

  tmp_df <- get_mood_table(moods_df$mood[[i]], moods_df$url_ajax[[i]])

  albums_moods <- bind_rows(albums_moods, tmp_df)
}


#### zapisanie danych lokalnie ####

saveRDS(albums_moods, "data/albums_moods.RDS")

