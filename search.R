library(httr)
library(glue)
library(magrittr)
library(stringr)
library(purrr)
library(tidyverse)
# Spotify Web API Base URL
BASE_URL <- "https://api.spotify.com"

SPOTIFY_CLIENT_ID <- "7a10a16f6b5649b8a3d4c837ab30b1d2"
SPOTIFY_SECRET <- "a970c16c23c64b02a5a04af82bb571c4"

SPOTIFY_AUTHORIZATION_URL <- "https://accounts.spotify.com/api/token"

request_access_token <- function(client_id, secret_key){
    api_response <- POST(SPOTIFY_AUTHORIZATION_URL,
                     authenticate(client_id, secret_key),
                     body = list(grant_type = "client_credentials"),
                     encode = "form"
                     ) %>% 
        stop_for_status("create access token") %>% 
        content() %>%
        {.$access_token}
}

SPOTIFY_ACCESS_TOKEN <- request_access_token(SPOTIFY_CLIENT_ID, SPOTIFY_SECRET)

test_url <- "https://api.spotify.com/v1/tracks/3n3Ppam7vgaVa1iaRUc9Lp"
test_track <- GET(test_url,
                  add_headers(Authorization = glue('Bearer {SPOTIFY_ACCESS_TOKEN}'))
                  ) 

test_query <- "Bryce Vine"

is_valid_search_string <- function(search_string){
    if(!is.character(search_string)) return(FALSE)
    if(length(search_string) > 1) return(FALSE)
    return(TRUE)
}

is_valid_limit <- function(limit){
    if(!is.numeric(limit)) return(FALSE)
    if(limit < 0) return(FALSE)
    return(TRUE)
}

search_tracks <- function(search_string, by = c("name", "artist", "album"), limit  = 20){
    if(!is_valid_search_string(search_string)) stop("Invalid search_string")
    if(!is_valid_limit(limit)) stop("Invalid Limit")
    by <- match.arg(by)
    search_string <- str_replace_all(search_string, " ", "+")
    api_response <- GET(modify_url(BASE_URL, path = "/v1/search"),
                        add_headers(Authorization = glue('Bearer {SPOTIFY_ACCESS_TOKEN}')),
                        query = list(q = search_string, type = "track", limit = limit)
                        )
    tracks_raw <- content(api_response)$tracks$items
    tracks <- map_df(tracks_raw, 
           ~data_frame(name = .$name, 
                       artist = .$artists[[1]]$name, 
                       album = .$album$name, 
                       `track no` = .$track_number, 
                       popularity = .$popularity, 
                       duration = .$duration_ms, 
                       explicit = .$explicit, 
                       uri = .$uri
                       )
           )
    tracks
}

make_GET_request <- function(url, headers = NULL, query_params = NULL){
    call <- match.call()
    api_resposne <- do.call("GET", call[-1])
}
get_albums <- function(id, album_type = c("album", "single", "appears_on", "compilation"),
                       market = "US", limit = 20){
    #Validate inputs
    if(!is_valid_search_string(id)) stop("Invalid ID")
    if(!is.character(market) | length(market) != 2) stop("Invalid market")
    if(!is_valid_limit(limit)) stop("Invalid Limit")
    album_type <- match.arg(album_type)
    
    api_response <- GET(modify_url(BASE_URL, path = glue('v1/artists/{id}/albums')),
                    add_headers(Authorization = glue('Bearer {SPOTIFY_ACCESS_TOKEN}')),
                    query = list(album_type = album_type,
                                 market = market,
                                 limit = limit
                                 )
                    )
    
    album_list <- content(api_response)$albums$items
    
}