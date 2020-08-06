
##########################################################

#IMPORT PACKAGES AND ACCES SPOTIFY API

##########################################################

options(stringsAsFactors = FALSE)
library(magrittr)
library(dplyr)
library(spotifyr)
library(tidyverse)
library(knitr)
library(WDI)
library(RJSONIO)
library(syuzhet)
library(data.table)
library(rAmCharts)

Sys.setenv(SPOTIFY_CLIENT_ID = "client_id")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "client_secret")

access_token <- get_spotify_access_token()

european_countries <- WDI(start = 2018, end = 2018, extra = TRUE) %>% 
  subset(region == "Europe & Central Asia") %>% 
    select(country) 

##########################################################

#SOURCE 1 FOR PLAYLIST DATA RETRIEVAL

##########################################################
spotify_lists <- get_user_playlists('spotify')
spotify_top_lists <- subset(spotify_lists, grepl("(^| )top 50", playlist_name, ignore.case = TRUE))

char_country <- as.character(c(european_countries$country))
char_playlist <- as.character(c(spotify_top_lists$playlist_name))

european_top_lists <- c()

for (country in char_country)
{
  for (playlist_name in char_playlist)
  {
    if ((paste(country, "Top 50", sep=" ") %in% playlist_name) == TRUE)
    {european_top_lists <- rbind(european_top_lists, c(playlist_name))

    }
  }
}
colnames(european_top_lists)<- 'playlist_name'
european_top_lists<-merge(european_top_lists, spotify_top_lists)

europe50_tracks <- get_playlist_tracks(european_top_lists)
europe50_features <- get_track_audio_features(europe50_tracks)

europe50 <- merge(europe50_tracks, europe50_features ) %>% 
  select(playlist_name,track_name, artist_name, danceability, energy, speechiness, acousticness, instrumentalness, liveness, tempo, valence) 

europe50_resume <- europe50 %>%
  group_by(playlist_name) %>% 
  summarise(valence_mean = mean(valence),
            danceability_mean = mean(danceability),
            energy_mean = mean(energy), 
            speechiness_mean = mean (speechiness),
            acousticness_mean = mean (acousticness),
            instrumentalness_mean = mean (instrumentalness),
            liveness_mean = mean (liveness),
            tempo_mean = mean(tempo)
  )

##########################################################

#SOURCE 2 FOR PLAYLIST DATA RETRIEVAL

##########################################################

country_toplists <- get_user_playlists('dgeizkipsrsk26i9krksl8wav')

char_country_2 <- as.character(c('UKRAINE', 'ROMANIA', 'MONACO', 'MALTA', 'LUXEMBOURG', 'LIECHENSTEIN', 'CZECH REPUBLIC', 'CYPRUS','ANDORRA','BULGARIA', 'GREECE', 'HUNGARY', 'SLOVAKIA'))
  
char_playlist_2 <- as.character(c(country_toplists$playlist_name))
european_top_lists_2 <- c()

for (country in char_country_2)
{playlist_count <- 0
  for (playlist_name in char_playlist_2)
  {playlist_count <-playlist_count + 1
    if (grepl(paste("TOP 50", country, sep=" "), playlist_name) == TRUE)
    {european_top_lists_2 <- rbind(european_top_lists_2, c(paste(country, "Top 50", sep=" "),playlist_name, country_toplists$playlist_uri[playlist_count], country_toplists$playlist_tracks_url[playlist_count], country_toplists$playlist_num_tracks[playlist_count], country_toplists$playlist_img[playlist_count]))
    
    }
  }
}
colnames(european_top_lists_2)<- c('playlist_name2','playlist_name', 'playlist_uri', 'playlist_tracks_url', 'playlist_num_tracks', 'playlist_img')

europe50_tracks_2 <- get_playlist_tracks(country_toplists)
europe50_tracks_2 <- merge(european_top_lists_2, europe50_tracks_2)
europe50_features_2 <- get_track_audio_features(europe50_tracks_2)

europe50_2 <- merge(europe50_tracks_2, europe50_features_2 ) %>% 
  select(playlist_name2,track_name, artist_name,valence, danceability, energy, speechiness, acousticness, instrumentalness, liveness, tempo)
colnames(europe50_2)<-(c('playlist_name','track_name', 'artist_name', 'valence', 'danceability', 'energy', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'tempo')) 

europe50_resume_2<-europe50_2 %>%
  group_by(playlist_name) %>% 
  summarise(valence_mean = mean(valence),
            danceability_mean = mean(danceability),
            energy_mean = mean(energy), 
            speechiness_mean = mean (speechiness),
            acousticness_mean = mean (acousticness),
            instrumentalness_mean = mean (instrumentalness),
            liveness_mean = mean (liveness),
            tempo_mean = mean(tempo)
  )
colnames(europe50_resume_2)<- c('playlist_name', 'valence_mean', 'danceability_mean', 'energy_mean', 'speechiness_mean', 'acousticness_mean', 'instrumentalness_mean', 'liveness_mean', 'tempo_mean') 

#BIND BOTH SOURCES LISTS
europe <- rbind(europe50_resume, europe50_resume_2)

##########################################################

#CREATE ROW WITH MEANS OF ALL THE COUNTRIES ON DIFFERENT FEATURES

##########################################################

valence_mean <- mean(europe [["valence_mean"]])
danceability_mean <- mean(europe [["danceability_mean"]])
energy_mean <- mean(europe [["energy_mean"]])
speechiness_mean <- mean(europe [["speechiness_mean"]])
acousticness_mean <- mean(europe [["acousticness_mean"]])
instrumentalness_mean <- mean(europe [["instrumentalness_mean"]])
liveness_mean <- mean(europe [["liveness_mean"]])
tempo_mean <- mean(europe [["tempo_mean"]])

playlist_name<-'EUROPEAN MEAN'
means_europe<-data.frame(playlist_name, valence_mean, danceability_mean, energy_mean, speechiness_mean, acousticness_mean, instrumentalness_mean, liveness_mean, tempo_mean)
europe <- rbind(europe, means_europe)

##########################################################

#GEOGRAPHICAL SPLIT INTO NORTHERN, CENTRAL OR SHOUTERN COUNTRIES IN EUROPE

##########################################################
all_songs <- rbind(europe50, europe50_2)

#NORTHERN COUNTRIES
north_countries <- data.frame(rbind('Denmark Top 50', 'Estonia Top 50', 'Finland Top 50', 'Germany Top 50', 'Iceland Top 50', 'Latvia Top 50', 'Lithuania Top 50', 'Norway Top 50', 'Poland Top 50','Sweden Top 50'))
colnames(north_countries)<- 'playlist_name'
north_countries <- merge(north_countries,europe)
north_countries <- rbind(north_countries, means_europe)
north_songs <- merge(north_countries, all_songs) %>%
  select(playlist_name,track_name, artist_name, valence, danceability, energy, speechiness, acousticness, instrumentalness, liveness, tempo) 

#CENTRAL COUNTRIES
central_countries <- data.frame(rbind('Austria Top 50','Belgium Top 50','Ireland Top 50', 'Luxembourg Top 50', 'Netherlands Top 50', 'Switzerland Top 50', 'United Kingdom Top 50', 'CZECH REPUBLIC Top 50', 'HUNGARY Top 50', 'LIECHENSTEIN Top 50', 'LUXEMBOURG Top 50', 'SLOVAKIA Top 50', 'UKRAINE Top 50'))
colnames(central_countries)<- 'playlist_name'
central_countries <- merge(central_countries,europe)
central_countries <- rbind(central_countries, means_europe)
central_songs <- merge(central_countries, all_songs) %>%
  select(playlist_name,track_name, artist_name, valence, danceability, energy, speechiness, acousticness, instrumentalness, liveness, tempo) 

#SOUTHERN COUNTRIES
south_countries <- data.frame(rbind('France Top 50','Italy Top 50', 'Portugal Top 50','Spain Top 50', 'Turkey Top 50', 'ANDORRA Top 50', 'BULGARIA Top 50', 'CYPRUS Top 50', 'GREECE Top 50', 'MALTA Top 50', 'MONACO Top 50', 'ROMANIA Top 50'))
colnames(south_countries)<- 'playlist_name'
south_countries <- merge(south_countries,europe)
south_countries <- rbind(south_countries, means_europe)
south_songs <- merge(south_countries, all_songs) %>%
  select(playlist_name,track_name, artist_name, valence, danceability, energy, speechiness, acousticness, instrumentalness, liveness, tempo) 

#SOUTHERN-CENTRAL COUNTRIES
sc_countries <- data.frame(rbind('France Top 50','Italy Top 50', 'Portugal Top 50','Spain Top 50', 'Turkey Top 50', 'ANDORRA Top 50', 'BULGARIA Top 50', 'CYPRUS Top 50', 'GREECE Top 50', 'MALTA Top 50', 'MONACO Top 50', 'ROMANIA Top 50', 'Austria Top 50','Belgium Top 50','Ireland Top 50', 'Luxembourg Top 50', 'Netherlands Top 50', 'Switzerland Top 50', 'United Kingdom Top 50', 'CZECH REPUBLIC Top 50', 'HUNGARY Top 50', 'LIECHENSTEIN Top 50', 'LUXEMBOURG Top 50', 'SLOVAKIA Top 50', 'UKRAINE Top 50'))
colnames(sc_countries)<- 'playlist_name'
sc_countries <- merge(sc_countries,europe)
sc_countries <- rbind(sc_countries, means_europe)
sc_songs <- merge(sc_countries, all_songs) %>%
  select(playlist_name,track_name, artist_name, valence, danceability, energy, speechiness, acousticness, instrumentalness, liveness, tempo) 


##########################################################

#PLOTS

##########################################################

#VALENCE (C-S)
h1 <- central_songs$valence
h2 <- south_songs$valence

xLimits <- range(c(h1,h2))
breakPoints <- seq(xLimits[1], xLimits[2], length.out = 22)

hist1 <- hist(h1, breaks = breakPoints, plot = F)
hist1Percentage = hist1$counts/sum(hist1$counts)*100

hist2 <- hist(h2, breaks = breakPoints, plot = F)
hist2Percentage = hist2$counts/sum(hist2$counts)*100

barplot(hist1Percentage, col = "#0000ff50")
mp<-barplot(hist2Percentage, col = "#00ff0050", add = T, main = "Valence comparison: Central-Southern Europe", xlab = "Valence", ylab = "Percentage")
legend("topleft", legend = c("Central", "Southern"), fill = c("#0000ff50", "#00ff0050"))
axis(1,at=mp,labels=seq(0,1,0.05))

  
#VALENCE(N-C-S)
h1 <- north_songs$valence
h2 <- central_songs$valence
h3 <- south_songs$valence
xLimits <- range(c(h1,h2, h3))
breakPoints <- seq(xLimits[1], xLimits[2],xLimits[3], length.out = 22)

hist1 <- hist(h1, breaks = breakPoints, plot = F)
hist1Percentage = hist1$counts/sum(hist1$counts)*100

hist2 <- hist(h2, breaks = breakPoints, plot = F)
hist2Percentage = hist2$counts/sum(hist2$counts)*100

hist3 <- hist(h3, breaks = breakPoints, plot = F)
hist3Percentage = hist3$counts/sum(hist3$counts)*100

barplot(hist1Percentage, col = "#0000ff50")
barplot(hist2Percentage, col = "#ff000050", add=T)
mp<-barplot(hist3Percentage, col = "#00ff0050", add = T, main = "Valence comparison: N-C-S Europe", xlab = "Valence", ylab = "Percentage")
legend("topleft", legend = c("Northern", "Central", "Southern"), fill = c("#0000ff50", "#ff000050", "#00ff0050"))
axis(1,at=mp,labels=seq(0,1,0.05))

#VALENCE (N-CS)
h1 <- north_songs$valence
h2 <- sc_songs$valence

xLimits <- range(c(h1,h2))
breakPoints <- seq(xLimits[1], xLimits[2], length.out = 22)

hist1 <- hist(h1, breaks = breakPoints, plot = F)
hist1Percentage = hist1$counts/sum(hist1$counts)*100

hist2 <- hist(h2, breaks = breakPoints, plot = F)
hist2Percentage = hist2$counts/sum(hist2$counts)*100

barplot(hist1Percentage, col = "#0000ff50")
mp<-barplot(hist2Percentage, col = "#00ff0050", add = T, main = "Valence comparison", xlab = "Valence", ylab = "Percentage")
legend("topleft", legend = c("Northern", "SC"), fill = c("#0000ff50", "#00ff0050"))
axis(1,at=mp,labels=seq(0,1,0.05))


#DANCEABILITY (N-CS)
h1 <- north_songs$danceability
h2 <- sc_songs$danceability

xLimits <- range(c(h1,h2))
breakPoints <- seq(xLimits[1], xLimits[2], length.out = 22)

hist1 <- hist(h1, breaks = breakPoints, plot = F)
hist1Percentage = hist1$counts/sum(hist1$counts)*100

hist2 <- hist(h2, breaks = breakPoints, plot = F)
hist2Percentage = hist2$counts/sum(hist2$counts)*100

barplot(hist1Percentage, col = "#0000ff50")
mp<-barplot(hist2Percentage, col = "#00ff0050", add = T, main = "Danceability comparison", xlab = "Danceability", ylab = "Percentage")
legend("topleft", legend = c("Northern", "SC"), fill = c("#0000ff50", "#00ff0050"))
axis(1,at=mp,labels=seq(0,1,0.05))


#ENERGY (N-CS)
h1 <- north_songs$energy
h2<-sc_songs$energy

xLimits <- range(c(h1,h2))
breakPoints <- seq(xLimits[1], xLimits[2], length.out = 22)

hist1 <- hist(h1, breaks = breakPoints, plot = F)
hist1Percentage = hist1$counts/sum(hist1$counts)*100

hist2 <- hist(h2, breaks = breakPoints, plot = F)
hist2Percentage = hist2$counts/sum(hist2$counts)*100

barplot(hist1Percentage, col = "#0000ff50")
mp<-barplot(hist2Percentage, col = "#00ff0050", add= T,  main = "Energy comparison", xlab = "Energy", ylab = "Percentage")
legend("topleft", legend = c("Northern", "SC"), fill = c("#0000ff50", "#00ff0050"))
axis(1,at=mp,labels=seq(0,1,0.05))

#TEMPO (N-CS)
h1 <- north_songs$tempo
h2<-sc_songs$tempo

xLimits <- range(c(h1,h2))
breakPoints <- seq(xLimits[1], xLimits[2], length.out = 22)

hist1 <- hist(h1, breaks = breakPoints, plot = F)
hist1Percentage = hist1$counts/sum(hist1$counts)*100

hist2 <- hist(h2, breaks = breakPoints, plot = F)
hist2Percentage = hist2$counts/sum(hist2$counts)*100

barplot(hist1Percentage, col = "#0000ff50")
mp<-barplot(hist2Percentage, col = "#00ff0050", add= T,  main = "Tempo comparison", xlab = "Tempo (BPM)", ylab = "Percentage")
legend("topleft", legend = c("Northern", "SC"), fill = c("#0000ff50", "#00ff0050"))
axis(1,at=mp,labels=seq(0,200,10))

