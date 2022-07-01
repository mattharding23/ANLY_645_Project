# ------------------------------------
# Import Libraries
# ------------------------------------
library(tidyverse)
library(spotifyr)
library(Rspotify)
library(httr)

# ------------------------------------
# Spotify API Credentials
# ------------------------------------


# These are specific to this project so we can all use them, there are two sets because
# There is a limit to calls in a single day 

#Sys.setenv(SPOTIFY_CLIENT_ID = '0afb90db8c28480bafd35f35823a5302')
#Sys.setenv(SPOTIFY_CLIENT_SECRET = '2cc0060eaebc480db8234ac1fbad7bf8')
Sys.setenv(SPOTIFY_CLIENT_ID = "c7b47eda15304b7c81a4cb1067bcb0e2")
Sys.setenv(SPOTIFY_CLIENT_SECRET = '67b983bbd33040e1b69a0e17fb7d616f')

access_token = get_spotify_access_token()


# ------------------------------------
# Get Related Artists function
# ------------------------------------

# Function that gets artists related to other artists from those artist's id's
find_related_artists = function(artist_df,multi = FALSE,old_id = NULL){
  # Check for overlaps from previous artists in case of a multi-layered related artist search
  if(multi){
    artist_df = artist_df %>%
      filter(!id %in% old_id)
  }
  
  
  # Set data frame
  related_artists = tibble("name" = character(),
                           "related_to" = character(),
                           "genres" = character(),
                           "id" = character(),
                           "related_id" = character(),
                           "popularity" = numeric(),
                           "followers" = numeric())
  
  # Loop through artists to get each of their related artists
  for(i in unique(artist_df$id)){
    temp_artist = artist_df$name[artist_df$id == i][1]
    temp = spotifyr::get_related_artists(id = i)
    temp2 = tibble("name" = temp$name,
                   "related_to" = rep(temp_artist,nrow(temp)),
                   "genres" = temp$genres,
                   "id" = temp$id,
                   "related_id" = i,
                   "popularity" = temp$popularity,
                   "followers" = temp$followers.total)
    
    # attach to overall data frame
    related_artists = rbind(related_artists,temp2)
  }
  
  # return full data frame
  return(related_artists)
}



# ------------------------------------
# Puts all artists and relationships in one data frame
# ------------------------------------

# Function that puts together artists twice removed from original artists, by inputting a genre
artist_relations = function(genre,AT,limit = 50){
  
  # Get initial artists and clean up data
  artists = spotifyr::get_genre_artists(genre = genre, market = NULL, limit = limit,
                                        offset = 0, authorization = get_spotify_access_token()) %>%
    select(-href,-images,-type,-uri,-external_urls.spotify,-followers.href)
  
  # Get related artists 
  related_artists1 = find_related_artists(artists)
  
  # Get artists related to those artists
  related_artists2 = find_related_artists(related_artists1,TRUE,artists$id)
  
  # Combine all artists and rename columns
  artists = tibble("name" = artists$name,
                   "related_to" = NA,
                   "genres" = artists$genres,
                   "id" = artists$id,
                   "related_id" = NA,
                   "popularity" = artists$popularity,
                   "followers" = artists$followers.total)
  
  all_artists = rbind(artists,related_artists1,related_artists2) %>%
    rename(artist = name)
  
  # Return data frame
  return(all_artists)
  
}


# 
# setwd("..")
# wd = getwd()
# all_artists = artist_relations("rap",access_token, limit = 2) %>%
#   write_csv(paste0(wd,"/data/artists_relations.csv"))





# ------------------------------------
# Function gets top 10 songs from each artist in network
# ------------------------------------
get_top_10 = function(all_artists){
  
  # Get all unique artists
  arts = all_artists$id %>%
    na.omit() %>%
    unique()
  
  # initialize df
  all_top_10 = tibble("song_id" = character(),
                      "song_name" = character(),
                      "song_popularity" = numeric(),
                      "artist_id" = numeric())
  
  # Loop through artists
  for(art in arts){
    
    # Get top 10 songs
    top_10 = spotifyr::get_artist_top_tracks(art)
    
    # Catch errors
    if(length(top_10)>0){
      top_10 = top_10 %>%
        select(id,name,popularity) %>%
        rename(song_id = id,song_name = name, song_popularity = popularity)
      top_10$artist_id = art
    }
    # Write to top 10 df
    else{
      top_10 = tibble("song_id" = NA,
                      "song_name" = NA,
                      "song_popularity" = NA,
                      "artist_id" = art)
    }
    
    # Combine all songs into one
    all_top_10 = rbind(all_top_10,top_10)
  }
  return(all_top_10)
}


#top_10 = get_top_10(all_artists)


# ------------------------------------
# Get song features for top 10 songs
# ------------------------------------

song_feat = function(top_10){
  
  # Initialize df
  song_features = tibble()
  
  # For each song, get song features
  for(id in top_10$song_id){
    
    # Error catch
    if(!is.na(id)){
      song = spotifyr::get_track_audio_features(id) %>%
        select(-type,-id,-uri, -track_href, -analysis_url, -time_signature) 
    }else{
      song = rep(NA,13)
    }
    # Combine all song features in one df
    song_features = rbind(song_features,song)
    
  }
  
  # Merge features df with top 10 songs df
  top_10_songs = cbind(top_10,song_features)
  return(top_10_songs)
  
}


# setwd("..")
# wd = getwd()
# top_10_w_feats = song_feat(top_10) %>%
#   write_csv(paste0(wd,"/data/artists_top_10_songs.csv"))


# ------------------------------------
# Get all songs from an artist
# ------------------------------------
artist_all_songs = function(artist_id){
  # Get all artist songs
  artist_songs = try(get_artist_audio_features(artist_id),TRUE)
  if (is.character(artist_songs)){
    artist_songs = c("")
  }else{
    
    # Get only artists that worked on song
    artist_songs = artist_songs %>%
      select(artists) %>%
      unnest(cols = c(artists)) %>%
      select(name)
  }
  return(artist_songs)
}

# ------------------------------------
# Find collaborations between artists
# ------------------------------------

collaboration = function(all_artists){
  
  # create new collaboration column
  all_artists$collaborations = 0
  
  # get list of all unique artists who were related to
  artists = unique(all_artists$related_id)
  
  # loop through artists
  for(art in artists){
    # Get all artists that have worked on related artists songs
    colab_artists = artist_all_songs(art)
    
    # get all artists who artist is related to
    related_arts = all_artists$artist[all_artists$related_id == art]
    
    # Loop through those artists
    for(rel in related_arts){
      c = 0
      # number of collaborations
      c = length(which(colab_artists == rel))
      
      # Update number in df
      all_artists$collaborations[all_artists$related_id == art & 
                                   all_artists$artist == rel] = c
    }
    
  }
  
  return(all_artists)
}

# setwd("..")
# wd = getwd()
# colabs = collaboration(all_artists) %>%
#   write_csv(paste0(wd,"/data/artists_relations.csv"))

# ------------------------------------
# Run all functions to output csv files
# ------------------------------------
get_spotify_data = function(genre,limit = 50){
  setwd("..")
  wd = getwd()
  all_artist_relations = artist_relations(genre,limit)
  all_artists = collaboration(all_artist_relations) %>%
    write_csv(paste0(wd,"/ANLY_645_Project/data/artists_relations.csv"))
  top_10_songs = get_top_10(all_artists)
  top_10_w_feats = song_feat(top_10_songs) %>%
    write_csv(paste0(wd,"/ANLY_645_Project/data/artists_top_10_songs.csv"))
  
  
}
# Get data call 
get_spotify_data("rap",2)

