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
library(HMM)
library(hmm.discnp)

setwd("S:/")

jams <- load_jams("S:/")

jams_genre <- add_genres_to_jams(jams,"S:/")

timestamp_sequence <- create_time_sequence(jams_genre)

# Split data into training and test

splitted_data <- split_data(timestamp_sequence)

train <- splitted_data[[1]]
test  <- splitted_data[[2]]

trained_MIR_hmm <- MIR_hmm(train, tolerance = 0.001)

save_model(trained_MIR_hmm,"2_state_hmm")

hmm3 <- load_model("2_state_hmm")

vanillahmm <- trained_MIR_hmm[[2]]



# FUNCTIONS #####################################################################################################################

create_time_sequence <- function(clearedData){

  timestamp_sequence <- sqldf("Select user_id, creation_date, genre from clearedData order by user_id, creation_date")

  # Add discrete timestamp to sequences

  timestamp_sequence$timestamp <- with(timestamp_sequence, ave(seq_along(user_id), user_id, FUN=seq_along))

  # Add numeric id to sequences

  timestamp_sequence <- transform(timestamp_sequence,id=as.numeric(factor(user_id)))

  # Create data for tse sequence

  timestamp_sequence <- sqldf("Select id,timestamp,genre as event from timestamp_sequence")

  sts_sequence <- TSE_TO_STS2(timestamp_sequence)

  return(sts_sequence)

}

## Transforms TSE to STS format (see Traminer description). This function works like the traminer one with the only difference that
## empty cells won't be filled with the value of the last non NA cell but with NA values. Furthermore it doesn't have initial state

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

test_model <- function(trained_MIR_hmm,testdata){

  results <- matrix(, nrow(testdata), 2)
  colnames(results) <- c("ACTUAL","PREDICTED")

  for (row in 1:nrow(testdata)){
    NonNAindex <- which(!is.na(testdata[row,]))
    lastNonNA <- max(NonNAindex)

    results[row,1] <- testdata[row,lastNonNA]

    results[row,2] <- predict(trained_MIR_hmm,testdata[row,1:(lastNonNA-1)])

  }

  return(data.frame(results,stringsAsFactors=F))
}

calcAccuracy <- function(testresult){
  return(nrow(model_test[testresult$ACTUAL == testresult$PREDICTED,])/nrow(testresult))
}

predict <- function(MIR_hmm, obs){

  hmm <- MIR_hmm[[2]]

  # Remove NA values from sequence
  filtered_obs <- Filter(Negate(is.na), obs)

  # Define Max Values
  pMax <- 0
  sMax <- ""

  # Iterate through all symbols and calculate likelihood of given observation with symbol at On+1 to be represented by the given hmm
  for (symbol in hmm$Symbols){

    logForwardProbabilities = forward(hmm,append(filtered_obs,symbol))
    forwardProbabilities = exp(logForwardProbabilities)

    finalProbability = sum(forwardProbabilities[,ncol(forwardProbabilities)])

    if(finalProbability>pMax){
      pMax <- finalProbability
      sMax <- symbol
    }
  }

  # Return symbol with the greatest likelihood
  return(sMax)
}

load_jams <- function(jams_path){

  path <- paste(jams_path, "/thisismyjam-datadump/archive/jams.tsv", sep="")

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

add_genres_to_jams <- function(jams_clean, LFM_path){

  lexicon_path <-  paste(LFM_path, "LFM-1b_UGP/genres_allmusic.txt", sep="")

  artist_genre_path <-  paste(LFM_path, "LFM-1b_UGP/LFM-1b_artist_genres_allmusic.txt", sep="")

  # Read allmusic genre lexicon

  allmusic_genres <- read.delim(
    lexicon_path,
    sep="\t", header=FALSE, quote="", fill=TRUE, encoding = "utf-8")

  # Add index to datatable

  colnames(allmusic_genres) <- "genre_name"

  allmusic_genres$genre_id <- 0:(nrow(allmusic_genres)-1)


  # Read allmusic Artist table

  #colClasses = c("character", "character", "character", "character", "character", "character", "character","character","character","character","character","character","character","character","character")
  colClasses = c("character", "character", "character", "character", "character", "character", "character")

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

  # Merge jams with genre

  jams_genre <- merge(jams_clean, artist_genre, by = "artist")

  users_without_genre <- subset(jams_genre, is.na(genre) == TRUE)


  #################### Remove Users without genre ###########################################

  #Recieve users who have no genre
  users_without_genre <- subset(jams_genre, is.na(genre) == TRUE)
  # Recieve the first jam of a jam sequence that has no genre
  users_without_genre2 <- sqldf('SELECT user_id, min(creation_date) as creation_date FROM users_without_genre group by user_id')

  final_data <- sqldf('SELECT * FROM jams_genre a left join users_without_genre2 b on a.user_id = b.user_id where b.user_id = null or b.creation_date > a.creation_date')

  # Keep data of users that have at least 2 jams

  final_data_clean <- sqldf('select * from final_data a where user_id in (Select user_id from final_data group by user_id having count(user_id) >= 2)')

  return (final_data_clean)

}

split_data <- function(data, split_ratio = .50){

  set.seed(101) # Set Seed so that same sample can be reproduced in future also
  # Now Selecting 50% of data as sample from total 'n' rows of the data
  sample <- sample.int(n = nrow(data), size = floor(split_ratio*nrow(data)), replace = F)
  list <- list()
  list[[1]] <- data[sample, ]
  list[[2]] <- data[-sample, ]

  return(list)
}

# big_hmm contains a hidden markov model of the discnp package and one of the hmm package

MIR_hmm <- function(training_data, K = 2, verbose = TRUE, tolerance = 0.001, itmax = 300) {

  # Retrieve all genres to pass as symbols argrument to HMM constructor

  allmusic_genres <- unique(c(training_data))
  allmusic_genres <- as.character(allmusic_genres[!is.na(allmusic_genres)])

  # Create list of state names

  state_names <- vector(mode="numeric", length=0)

  for(i in 1:K){
    state_name <- paste("State", as.character(i), sep=" ")
    state_names <- c(state_names, state_name)
  }

  # Transforms training data which is in STS format to list of vectors
  training_list <- as.list(data.frame(t(training_data)))

  # Train discnp model
  discnp_hmm <- hmm(training_list,allmusic_genres, K = K, verbose = verbose, tolerance = tolerance, itmax = itmax, keep.y=FALSE)
  # Create hmm model with data from discnp model
  hmm <- initHMM(state_names, allmusic_genres,startProbs = discnp_hmm$ispd, transProbs= discnp_hmm$tpm, emissionProbs=t(discnp_hmm$Rho))

  value <- list(discnp_hmm,hmm)

  attr(value, "class") <- "MIR_hmm"
  value
}

save_model <- function(model,model_name){

model_path <- paste(model_name, ".rds", sep="")

saveRDS(model, model_path)

}

load_model <- function(model_name){

model_path <- paste(model_name, ".rds", sep="")

return(readRDS(model_path))

}
