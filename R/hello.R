# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(sqldf)
library(TraMineR)
library(seqHMM)

setwd("S:/")

jams <- loadjams("/thisismyjam-datadump/archive/jams.tsv")

jams_genre <- load_allmusic_genres(jams,"/LFM-1b_UGP/genres_allmusic.txt","/LFM-1b_UGP/LFM-1b_artist_genres_allmusic.txt")

jams_genre_clean <-removeUsersWithoutGenre(jams_genre)

genre_distribution <- sqldf("Select genre, count(genre) from jams_genre_clean group by genre")

timestamp_sequence <- sqldf("Select user_id, creation_date, genre from jams_genre_clean order by user_id, creation_date")

# Add discrete timestamp to sequences

timestamp_sequence$timestamp <- with(timestamp_sequence, ave(seq_along(user_id), user_id, FUN=seq_along))

# Add numeric id to sequences

timestamp_sequence <- transform(timestamp_sequence,id=as.numeric(factor(user_id)))

# Create data for tse sequence

timestamp_sequence <- sqldf("Select id,timestamp,genre as event from timestamp_sequence")

sts <- TSE_TO_STS2(timestamp_sequence)


# Generate genre list
allmusic_genres <- as.character(unique(timestamp_sequence$event))

sts_seq <- seqdef(sts, alphabet = allmusic_genres, states = allmusic_genres,
                  labels = allmusic_genres, xtstep = 1)

init_hmm_music <- build_hmm(observations = sts_seq, n_states = 2)

fit_hmm_music <- fit_model(init_hmm_music, control_em = list(restart = list(times = 50)))
hmm_music <- fit_hmm$model

TSE_TO_STS2 <- function(TSE_sequence){

  # Generate genre list
  allmusic_genres <- as.character(unique(TSE_sequence$event))

  num_col <- max(TSE_sequence$timestamp)
  num_rows <- length(unique(TSE_sequence$id))

  sts <- matrix(, num_rows, num_col)

  x <- 1

  while(x <= length(TSE_sequence$id)) {
    sts[TSE_sequence[x,1],TSE_sequence[x,2]] <- as.character(TSE_sequence[x,3]); x <- x+1;}

  return(sts)
}


loadjams <- function(path){
  jams <- read.delim(
    path,
    sep="\t", header=TRUE, fill=TRUE, quote="", colClasses = c("character", "character", "character", "character", "character", "character", "character"))
  jam_artists <- data.frame(artist=unique(jams$artist))
  jam_users_with_corr_data <- sqldf('SELECT user_id FROM jams where creation_date = "" or artist = "" or title = "" or length(jam_id) <> 32')

  # Remove jams of users who possess at least 1 corrupted value

  jams_clean <- sqldf("select * from jams a where user_id not in jam_users_with_corr_data")

  return(jams_clean)
}

####################################################################################################################

load_allmusic_genres <- function(jams_clean,lexicon_path,artist_genre_path){

  # Read allmusic genre lexicon

  allmusic_genres <- read.delim(
    lexicon_path,
    sep="\t", header=FALSE, quote="", fill=TRUE, encoding = "utf-8")

  # Add index to datatable

  colnames(allmusic_genres) <- "genre_name"

  allmusic_genres$genre_id <- 0:(nrow(allmusic_genres)-1)


  # Read allmusic Artist table

  colClasses = c("character", "character", "character", "character", "character", "character", "character","character","character","character","character","character","character","character","character")

  allmusic <- read.table(
    artist_genre_path,
    sep="\t", header=FALSE, quote="", fill=TRUE, encoding = "utf-8", comment.char = "", colClasses = colClasses
  )

  # Remove Artists without a genre

  allmusic_clean <- subset(allmusic, is.na(V2) == FALSE & V2 != "")

  # Remove columns

  allmusic_clean <- data.frame(artist=allmusic_clean$V1, genre_id=allmusic_clean$V2)

  # Keep artists with at least one genre

  allmusic <- sqldf("Select artist,max(genre_id) as genre_id from allmusic_clean group by artist")


  allmusic_merged <- merge(allmusic, allmusic_genres, by = "genre_id")

  #sum(!is.na(allmusic_merged$genre))

  jam_artists <- data.frame(artist=unique(jams$artist))

  final_table <- merge(x = jam_artists, y = allmusic_merged, by = "artist", all.x = TRUE)

  #length(unique(final_table$Genre))

  #sum(!is.na(final_table$Genre))

  artist_genre <- data.frame(artist=final_table$artist, genre=final_table$genre_name)

  jams_genre <- merge(jams_clean, artist_genre, by = "artist")

  users_without_genre <- subset(jams_genre, is.na(genre) == TRUE)

  return(jams_genre)
}

removeUsersWithoutGenre <- function(jams_genre){
  #Recieve users who have no genre
  users_without_genre <- subset(jams_genre, is.na(genre) == TRUE)
  # Recieve the first jam of a jam sequence that has no genre
  users_without_genre2 <- sqldf('SELECT user_id, min(creation_date) as creation_date FROM users_without_genre group by user_id')

  final_data <- sqldf('SELECT * FROM jams_genre a left join users_without_genre2 b on a.user_id = b.user_id where b.user_id = null or b.creation_date > a.creation_date')

  # Keep data of users that have at least 2 jams

  final_data_clean <- sqldf('select * from final_data a where user_id in (Select user_id from final_data group by user_id having count(user_id) >= 2)')

  return (final_data_clean)
}

