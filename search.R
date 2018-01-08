library(httr)
library(glue)
library(magrittr)
library(stringr)
library(purrr)
library(tidyverse)
library(lubridate)
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
        stop_for_status("create access token")
    access_token <- api_response %>% content() %>% {.$access_token}
}

SPOTIFY_ACCESS_TOKEN <- request_access_token(SPOTIFY_CLIENT_ID, SPOTIFY_SECRET)

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

add_authorization_header <- function(){
    authorization_header <- add_headers(Authorization = glue('Bearer {SPOTIFY_ACCESS_TOKEN}'))
}

extract_artists <- function(artist_objects){
    map_chr(artist_objects, ~.$name) %>% paste0(collapse = ", ")
}

make_GET_request <- function(url, headers = NULL, query_params = NULL){
    call <- match.call()
    api_resposne <- do.call("GET", call[-1])
}

search_tracks <- function(search_string, by = c("name", "artist", "album"), limit  = 20){
    if(!is_valid_search_string(search_string)) stop("Invalid search_string")
    if(!is_valid_limit(limit)) stop("Invalid Limit")
    by <- match.arg(by)
    search_string <- str_replace_all(search_string, " ", "+")
    api_response <- GET(modify_url(BASE_URL, path = "/v1/search"),
                        add_authorization_header(),
                        query = list(q = search_string, type = "track", limit = limit)
                        ) %>% stop_for_status(task = "complete search request")
    tracks_raw <- content(api_response)$tracks$items
    tracks <- map_df(tracks_raw, 
                     ~data_frame(name = .$name, 
                                 artist = extract_artists(.$artists), 
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



get_albums <- function(id, album_type = c("album", "single", "appears_on", "compilation"),
                       market = "US", limit = 20){
    #Validate inputs
    if(!is_valid_search_string(id)) stop("Invalid ID")
    if(!is.character(market) | nchar(market) != 2) stop("Invalid market")
    if(!is_valid_limit(limit)) stop("Invalid Limit")
    album_type <- match.arg(album_type)
    
    api_response <- GET(modify_url(BASE_URL, path = glue('v1/artists/{id}/albums')),
                        add_authorization_header(),
                        query = list(album_type = album_type,
                                     market = market,
                                     limit = limit
                                     )
                        ) %>% stop_for_status("complete search request")
    albums_raw <- content(api_response)$items
    album_ids <- map_df(albums_raw, ~data_frame(id = .$id))
    album_ids <- paste0(album_ids$id, collapse = ",")
    
    api_response <- GET(modify_url(BASE_URL, path = glue('v1/albums')),
                        add_authorization_header(),
                        query = list(ids = album_ids)
                        ) %>% stop_for_status("complete search request")
    albums_raw <- content(api_response)$albums
    albums <- map_df(albums_raw,
                     ~data_frame(name = .$name,
                                 artist = extract_artists(.$artists),
                                 album_type = .$album_type,
                                 no_tracks = length(.$tracks$items),
                                 release_date = ymd(.$release_date),
                                 popularity = .$popularity,
                                 id = .$id,
                                 )
                     )
    albums
}

get_similar_artists <- function(id){
    if(!is_valid_search_string(id)) stop("Invalid ID")
    
    api_response <- GET(modify_url(BASE_URL, path = glue('/v1/artists/{id}/related-artists')),
                        add_authorization_header()
                        ) %>% stop_for_status("complete search request")
    artists_raw <- content(api_response)$artists
    artists <- map_df(artists_raw,
                      ~data_frame(name = .$name,
                                  popularity = .$popularity,
                                  id = .$id
                                  )
                      )
    artists
}
test_album_id <-  "5PxCTrv3Y1xVACfngpt7D2"