library(httr)
library(glue)

# Spotify Web API Base URL
BASE_URL <- "https://api.spotify.com"

SPOTIFY_CLIENT_ID <- "7a10a16f6b5649b8a3d4c837ab30b1d2"
SPOTIFY_SECRET <- "a970c16c23c64b02a5a04af82bb571c4"

SPOTIFY_AUTHORIZATION_URL <- "https://accounts.spotify.com/api/token"

SPOTIFY_ACCESS_TOKEN <- POST(SPOTIFY_AUTHORIZATION_URL,
              authenticate(SPOTIFY_CLIENT_ID,SPOTIFY_SECRET),
              # add_headers(Authorization = glue("Basic ",SPOTIFY_CLIENT_ID,":",SPOTIFY_SECRET,.sep = "")),
              body = list(grant_type = "client_credentials"),
              encode = "form"
              ) %>% content() %>% {.$access_token}

test_url <- "https://api.spotify.com/v1/tracks/3n3Ppam7vgaVa1iaRUc9Lp"
test_track <- GET(test_url,
                  add_headers(Authorization = glue('Bearer {SPOTIFY_ACCESS_TOKEN}'))
                  ) 

