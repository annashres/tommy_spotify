#####
#START HERE
######
###new shit##
install.packages("cellranger")
install.packages("tidyverse")
install.packages("tidyjson")
install.packages('DBI')
install.packages("devtools", dependencies = TRUE)
library(devtools)
install_github("tiagomendesdantas/Rspotify")
install.packages('Rspotify')

library(tidyverse)
library(lubridate)
library(tidyjson)   # this library
library(dplyr) 
library(Rspotify)
library(devtools)


#appid, clientid, client secret
keys <- spotifyOAuth("playlist", clientid, clientsecret)

##Get your user_id from spotify and paste it 
user <- getUser("1212667926",token=keys)
user$display_name # user name
user$id


user$display_name # user name
user$id


#get list of playlists
tom_playlist_list <- getPlaylist(user$id, token=keys)

#get song uuids from a 'Your Top Songs 2017' playlist
top_2017_id <- tom_playlist_list %>% filter(name == 'Your Top Songs 2017') %>%
  select(id)
top_2017_id[1]
##Store the songs uuids
tom_playlist_list <- getPlaylistSongs("spotify",top_2017_id[1],token=keys)
#Get each songs data
getFeatures(tom_playlist_list$id[1], token = keys)
#rbind all songs feature into 1 DF
get_playlist_features <- function(feature_df, song_uuids){
  for (i in 1:length(song_uuids)){
    temp_feature <- getFeatures(tom_playlist_list$id[i], token = keys)
    feature_df <- rbind(feature_df,temp_feature)
  }
  return(feature_df)
}
####
tom_2017_df = data.frame()
song_uuids <- tom_playlist_list$id
fav_2017_song_data <-get_playlist_features(tom_2017_df, song_uuids) %>% mutate(love = as.factor(1))
full_fav <- merge(tom_playlist_list,fav_2017_song_data)
str(fav_2017_song_data)


# #how to get song data easily
# req <- httr::GET(paste0("https://api.spotify.com/v1/audio-features/", 
#                         '1idBirsBGvCMsLvv4cTv7k'), httr::config(token = keys))
# json1 <- httr::content(req)
# #get playlist data..
# req <- httr::GET(paste0("https://api.spotify.com/v1/users/", 
#                         user$id, "/playlists/",top_2017_id[1], "/tracks?&limit=100&offset=", 
#                         0), httr::config(token = keys))
# json1 <- httr::content(req)
# json2 <- jsonlite::fromJSON(jsonlite::toJSON(json1))$items


#rap workout as holder for songs I hate
rap_id <- '37i9dQZF1DX76t638V6CA8'
rap_song_list <- getPlaylistSongs("spotify",rap_id,token=keys)
rap_song_list$id
songs_i_hate <- get_playlist_features(tom_2017_df, rap_song_list$id) %>% mutate(love = as.factor(0))
###

##Combine songs like and songs hate
will_i_like <- rbind(fav_2017_song_data,songs_i_hate)


## Train the model
song_logsitic <- glm(love ~ danceability + energy + key + loudness + speechiness +acousticness+ instrumentalness+ valence + tempo + duration_ms,  data = will_i_like, family = 'binomial')
summary(song_logsitic)
#none of this is stat sig...

###
###Get probabilites if I'll like this song
##
covey_call_home_uuid <- '6kGkA6EPiuaZX4Kv5s1tcl'
covey_df <-getFeatures(covey_call_home_uuid, token = keys) %>%
  select(danceability, energy, key, loudness, speechiness,acousticness, instrumentalness, valence, tempo, duration_ms)
#Add probabilty variable to DF
covey_df$prob_to_love <- predict(song_logsitic, newdata = covey_df, type ='response')

oxford_comma <- '2Ml0l8YWJLQhPrRDLpQaDM'
oxford_df <-getFeatures(oxford_comma, token = keys) %>%
  select(danceability, energy, key, loudness, speechiness,acousticness, instrumentalness, valence, tempo, duration_ms)
oxford_df$prob_to_love <- predict(song_logsitic, newdata = oxford_df, type ='response')

down_valley <- '5Gtn8HgCAo0TUiaKKgP6us'
down_valley_df <-getFeatures(down_valley, token = keys) %>%
  select(danceability, energy, key, loudness, speechiness,acousticness, instrumentalness, valence, tempo, duration_ms)
down_valley_df$prob_to_love <- predict(song_logsitic, newdata = down_valley_df, type ='response')

