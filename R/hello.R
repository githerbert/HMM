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

#' @import sqldf
#' @importFrom hmm.discnp hmm
#' @importFrom HMM initHMM forward
#' @importFrom utils read.delim read.table
NULL

#setwd("S:/")

#jams <- load_jams("S:/")

#jams_genre <- add_genres_to_jams(jams,"S:/")

#timestamp_sequence <- create_time_sequence(jams_genre)

# Split data into training and test

#splitted_data <- split_data(timestamp_sequence)

#train <- splitted_data[[1]]
#test  <- splitted_data[[2]]

#trained_MIR_hmm <- MIR_hmm(train, tolerance = 0.001)

#save_model(trained_MIR_hmm,"2_state_hmm")

#hmm3 <- load_model("2_state_hmm")

# FUNCTIONS #####################################################################################################################

#' Creates time sequences out of jam genre data
#'
#' This function transforms the jam data with the attached genres into the time sequence format STS (state sequences). In comparison
#' to the original jam genre data that contains one observation per row the STS format contains one whole sequence per row. This function
#' needs to applied to the jam data to proceed them further.
#'
#' @param clearedData A matrix of jams with genres as returned by the function add_genres_to_jams()
#' @return sequences in STS (state sequences) format which contains one sequence per row
#' @examples
#' # Example for the case that all datasets are located in S:/
#' jams <- load_jams("S:/", loadExample = TRUE)
#' jams_with_genre <- add_genres_to_jams(jams, "S:/", loadExample = TRUE)
#' jam_sequence <- create_time_sequence(jams_with_genre)
#' @export
create_time_sequence <- function(clearedData){

  timestamp_sequence <- sqldf("Select user_id, creation_date, genre from clearedData order by user_id, creation_date")

  # Add discrete timestamp to sequences

  timestamp_sequence$timestamp <- with(timestamp_sequence, ave(seq_along(user_id), user_id, FUN=seq_along))

  # Add numeric id to sequences

  timestamp_sequence <- transform(timestamp_sequence,id=as.numeric(factor(user_id)))

  # Create TSE time sequence

  timestamp_sequence <- sqldf("Select id,timestamp,genre as event from timestamp_sequence")

  sts_sequence <- TSE_TO_STS2(timestamp_sequence)

  return(sts_sequence)

}

## Transforms TSE to STS format (see Traminer description). This function works like the traminer one with the only difference that
## empty cells won't be filled with the value of the last non NA cell but with NA values. Furthermore it doesn't have initial state

#' Transform a TSE timesequence to a STS timesequence
#'
#' This function transforms a matrix that contains sequential data in TSE (time stamped event sequences) format
#' to a matrix in STS (state sequences) format. This function works like the TSE_TO_STS() function of the TraMineRextras package but differs
#' in two points: If the passed dataset contains a sequence that is shorter than the longest sequence of the dataset TraMineRextras fills the
#' missing values with the last non missing value. This function will not do this and let the missing values be NA. Furthermore it doesn't have
#' a first state at the first value in the sequence.
#'
#' @param TSE_sequence A sequence in TSE format
#' @return A matrix in STS format containing one sequence per row
#' @export
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

#' Test trained Hidden Markov Model with test data
#'
#' This function tests a Hidden Markov Model for Music Information retriaval by predicting
#' the last genre of each sequence in the testdata set. Based on the result of this function
#' the accuracy of a model can be calculated with the function calcAccuracy().
#'
#' @param trained_MIR_hmm An object of the class MIR_hmm as returned by MIR_hmm().
#' @param testdata Testdata as returned by the function split_data().
#' @return A matrix that contains the following two values for each row in the testdata:
#' \item{ACTUAL}{The actual last value of the sequence.}
#' \item{PREDICTED}{The predicted last value by the Hidden Markov Model.}
#' @examples
#' # Example for the case that all datasets are located in S:/
#'
#' # Prepare data
#' jams <- load_jams("S:/", loadExample = TRUE)
#' jams_with_genre <- add_genres_to_jams(jams, "S:/", loadExample = TRUE)
#' jam_sequence <- create_time_sequence(jams_with_genre)
#' splitted_data <- split_data(jam_sequence)
#' training_dataset <- splitted_data[[1]]
#' test_dataset <- splitted_data[[2]]
#'
#'# Train model
#' hmm <- MIR_hmm(training_dataset)
#' test_results <- test_model(hmm,test_dataset)
#' @export
test_model <- function(trained_MIR_hmm,testdata){

  results <- matrix(, nrow(testdata), 2)
  colnames(results) <- c("ACTUAL","PREDICTED")

  for (row in 1:nrow(testdata)){
    NonNAindex <- which(!is.na(testdata[row,]))
    lastNonNA <- max(NonNAindex)

    results[row,1] <- testdata[row,lastNonNA]

    results[row,2] <- predict_next(trained_MIR_hmm,testdata[row,1:(lastNonNA-1)])

  }

  return(data.frame(results,stringsAsFactors=F))
}

#' Calculate the accuracy of a model
#'
#' The accuracy is the fraction of predictions that were true. It is calculated by dividing the number of correct predictions through
#' the total number of predictions.
#'
#' @param testresult A matrix containing actual and predicted values as returned by the function test_model().
#' @return A percentage value between 0 and 1. 0.5 means that 50% of the predictions were correct.
#' @export
calcAccuracy <- function(testresult){
  return(nrow(model_test[testresult$ACTUAL == testresult$PREDICTED,])/nrow(testresult))
}

#' Predict next genre
#'
#' This function predicts the next observation for a given listining history of genres. This is achieved
#' by calculating probability by each genre to be the next genre. The genre with the highest
#' probability is considered to be the next favorite genre of a user.
#'
#' @param MIR_hmm An object of the class MIR_hmm as returned by MIR_hmm().
#' @param obs A sequence of genres as list.
#' @return Next probable genre.
#' @examples
#' # Load MIR HMM
#' hmm <- load_model("2_state_hmm")
#' # Create sequence of observations
#' seq <- c("rock","heavy metal", "pop")
#' # Predict next genre
#' prediction <- predict_next(hmm,seq)
#' print(prediction)
#' @export
predict_next <- function(MIR_hmm, obs){

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

#' Load jams from disk
#'
#' This function loads the jams.tsv file from the THIS IS MY JAM dataset into R.
#' Furthermore users who have at least one corrupted value will be deleted.
#'
#'
#' @param jams_path Path to the location of the unzipped thisismyjam folder
#' @param loadExample If set to true this checks only the validity of the function. The first argument will be ignored
#' @return returns a matrix of jams containing the following columns
#' \item{jam_id}{Identification number of a jam}
#' \item{user_id}{Identification number of the user that created the jam}
#' \item{artist}{Artist of the jam}
#' \item{title}{Title of song of the jam}
#' \item{creation_date}{Title of song of the jam}
#' @examples
#' jams <- load_jams("S:/", loadExample = TRUE)
#' @export
load_jams <- function(jams_path, loadExample = FALSE){

  path <- paste(jams_path, "/thisismyjam-datadump/archive/jams.tsv", sep="")

  if(loadExample){
    path <- system.file("extdata", "jams_example.tsv", package="hmm.mir")

  }

  jams <- read.delim(
    path,
    sep="\t", header=TRUE, fill=TRUE, quote="", colClasses = c("character", "character", "character", "character", "character", "character", "character"))

  jam_artists <- data.frame(artist=unique(jams$artist))
  jam_users_with_corr_data <- sqldf('SELECT user_id FROM jams where creation_date = "" or artist = "" or title = "" or length(jam_id) <> 32')
  # Remove jams of users who possess at least 1 corrupted value

  jams_clean <- sqldf("select jam_id, user_id, artist, title, creation_date from jams a where user_id not in jam_users_with_corr_data")

  return(jams_clean)
}

####################################################################################################################

#' Add genre to jams
#'
#' This function loads the genres of the artist from the XXX dataset and merge them with the jams recieved from loadJams() function.
#' Preprocessing: jams of users that weren't merged to a genre will be deleted.
#'
#' @param jams_clean return value of the loadJams() function
#' @param LFM_path Path to the location of the unzipped LFM folder
#' @param loadExample If set to true this checks only the validity of the function. The first argument will be ignored
#' @return Returns a matrix containing the same columns as the return value of the load_jams() function except for one additional value:
#' \item{genre}{The genre the artist belongs to according to last.fm}
#' @examples
#' # Example for the case that all datasets are located in S:/
#' jams <- load_jams("S:/", loadExample = TRUE)
#' jams_with_genre <- add_genres_to_jams(jams, "S:/", loadExample = TRUE)
#' print(jams_with_genre)
#'
#' @export
add_genres_to_jams <- function(jams_clean, LFM_path, loadExample = FALSE){

  # Create paths

  lexicon_path <-  paste(LFM_path, "LFM-1b_UGP/genres_allmusic.txt", sep="")

  artist_genre_path <-  paste(LFM_path, "LFM-1b_UGP/LFM-1b_artist_genres_allmusic.txt", sep="")

  if(loadExample){
    lexicon_path <- system.file("extdata", "genres_allmusic.txt", package="hmm.mir")

    artist_genre_path <- system.file("extdata", "artist_genre_example.txt", package="hmm.mir")
  }

  # Read allmusic genre lexicon

  allmusic_genres <- read.delim(
    lexicon_path,
    sep="\t", header=FALSE, quote="", fill=TRUE, encoding = "utf-8")

  # Add index to datatable

  colnames(allmusic_genres) <- "genre_name"

  allmusic_genres$genre_id <- 0:(nrow(allmusic_genres)-1)


  # Read allmusic Artist table

 colClasses = c("character", "character", "character", "character", "character", "character", "character")

  allmusic <- read.table(
    artist_genre_path,
    sep="\t", header=FALSE, quote="", fill=TRUE, encoding = "utf-8", comment.char = "", colClasses = colClasses
  )

  # Remove Artists without a genre

  allmusic_clean <- subset(allmusic, is.na(allmusic$V2) == FALSE & allmusic$V2 != "")

  # Keep ony first genre. This is the genre with the most votes at last.fm

  allmusic_clean <- data.frame(artist=allmusic_clean$V1, genre_id=allmusic_clean$V2)

  # Keep artists with at least one genre

  allmusic <- sqldf("Select artist,max(genre_id) as genre_id from allmusic_clean group by artist")

  # Merge artist with genre

  allmusic_merged <- merge(allmusic, allmusic_genres, by = "genre_id")


  jam_artists <- data.frame(artist=unique(jams$artist))

  final_table <- merge(x = jam_artists, y = allmusic_merged, by = "artist", all.x = TRUE)

  artist_genre <- data.frame(artist=final_table$artist, genre=final_table$genre_name)

  # Merge jams with artist_genre

  jams_genre <- merge(jams_clean, artist_genre, by = "artist")

  users_without_genre <- subset(jams_genre, is.na(genre) == TRUE)


  #################### Remove Users without genre ###########################################

  #Recieve users who have no genre
  users_without_genre <- subset(jams_genre, is.na(genre) == TRUE)
  # Recieve the first jam of a jam sequence that has no genre
  users_without_genre2 <- sqldf('SELECT user_id, min(creation_date) as creation_date FROM users_without_genre group by user_id')
  if(nrow(users_without_genre2)>0){
  final_data <- sqldf('SELECT * FROM jams_genre a left join users_without_genre2 b on a.user_id = b.user_id where b.user_id = null or b.creation_date > a.creation_date')
  }else{
  final_data <- jams_genre
  }

  # Keep data of users that have at least 2 jams

  final_data_clean <- sqldf('select jam_id, user_id, artist, title, creation_date, genre from final_data a where user_id in (Select user_id from final_data group by user_id having count(user_id) >= 2)')

  return (final_data_clean)

}

#' Split data into training and test dataset
#'
#' This function draws a random sample of a dataset as training dataset.
#'
#' @param data a matrix of sequences in STS format as returend by the function create_time_sequence().
#' @param split_ratio The split ratio that determines how many percent of the original dataset should be used as training dataset.
#' @return A list containing the training dataset at list index 1 and the test dataset at list index 2.
#' @examples
#' # Example for the case that all datasets are located in S:/
#' jams <- load_jams("S:/", loadExample = TRUE)
#' jams_with_genre <- add_genres_to_jams(jams, "S:/", loadExample = TRUE)
#' jam_sequence <- create_time_sequence(jams_with_genre)
#' splitted_data <- split_data(jam_sequence)
#' training_dataset <- splitted_data[[1]]
#' test_dataset <- splitted_data[[2]]
#' @export
split_data <- function(data, split_ratio = .50){

  set.seed(101) # Set Seed so that same sample can be reproduced in future also
  # Now Selecting 50% of data as sample from total 'n' rows of the data
  sample <- sample.int(n = nrow(data), size = floor(split_ratio*nrow(data)), replace = F)
  list <- list()
  list[[1]] <- data[sample, ]
  list[[2]] <- data[-sample, ]

  return(list)
}

# MIR_hmm contains a hidden markov model of the discnp package and one of the hmm package

#' Initialise a hidden markov model for music information retrieval with training data
#'
#' This function initialise a Hidden Markov Model for music information retrieval. Thereby the model is basically a wrapper
#' for two other HMM classes of different packages. The HMM package and the hmm.discnp package. hmm.discnp is used for fitting
#' the model to the training dataset while hmm is used to predict next genres.
#'
#' @param training_data training dataset as returned by the function split_data().
#' @param K The number of hidden states of the hidden markov model. See also hmm.discnp documentation for further information.
#' @param verbose Determines if the single EM algorithm steps should be printed to console.
#' @param tolerance The percentage change in log-likelihood that determines if the convergence citeria is met.
#' @param itmax The maximum number of iterations of the EM Algorithm.
#' @return An object of the class MIR_hmm that contains a list of the following hidden markov models:
#' \item{discnp_hmm}{A Hidden Markov Model from the package hmm.discnp.}
#' \item{hmm}{A Hidden Markov Model from the package HMM.}
#' @examples
#' # Example for the case that all datasets are located in S:/
#'
#' # Prepare data
#' jams <- load_jams("S:/", loadExample = TRUE)
#' jams_with_genre <- add_genres_to_jams(jams, "S:/", loadExample = TRUE)
#' jam_sequence <- create_time_sequence(jams_with_genre)
#' splitted_data <- split_data(jam_sequence)
#' training_dataset <- splitted_data[[1]]
#' test_dataset <- splitted_data[[2]]
#'
#'# Train model
#' hmm <- MIR_hmm(training_dataset)
#' print(hmm)
#' @export
MIR_hmm <- function(training_data, K = 2, verbose = TRUE, tolerance = 0.001, itmax = 300) {

  # Retrieve all genres to pass as symbols argrument to HMM constructor

  genres_path <- system.file("extdata", "genres_allmusic.txt", package="hmm.mir")

  allmusic_lexicon <- read.delim(
    genres_path,
    sep="\t", header=FALSE, quote="", fill=TRUE, encoding = "utf-8")

  allmusic_genres <- as.character(unique(allmusic_lexicon$V1))

  # Create list of state names

  state_names <- vector(mode="numeric", length=0)

  for(i in 1:K){
    state_name <- paste("State", as.character(i), sep=" ")
    state_names <- c(state_names, state_name)
  }

  # Transforms training data which is in STS format to a list of vectors since discnp hmm function only accepts this format as input
  training_list <- as.list(data.frame(t(training_data)))

  # Train discnp model
  discnp_hmm <- hmm(training_list,allmusic_genres, K = K, verbose = verbose, tolerance = tolerance, itmax = itmax, keep.y=FALSE)
  # Create hmm model with data from discnp model
  hmm <- initHMM(state_names, allmusic_genres,startProbs = discnp_hmm$ispd, transProbs= discnp_hmm$tpm, emissionProbs=t(discnp_hmm$Rho))

  value <- list(discnp_hmm,hmm)

  attr(value, "class") <- "MIR_hmm"
  value
}

#' Save Hidden Markov model to disk
#'
#' This function saves a hidden markov model as serialised object (.rds format) to disk.
#'
#' @param model An object of the class MIR_hmm as returned by MIR_hmm().
#' @param model_name Name of the model on disk.
#' @examples
#' hmm <- load_model("2_state_hmm")
#'
#' save_model(hmm, "2_state_hmm")
#' @export
save_model <- function(model,model_name){

file_name <- paste(model_name, ".rds", sep="")

model_path <- system.file("extdata", file_name, package="hmm.mir")

saveRDS(model, model_path)

}

#' Load a Hidden Markov model from disk
#'
#' This function loads a saved Hidden Markov Model for Music Information Retrieval from disk.
#'
#' @param model_name Name of the model on disk.
#' @return An object of the class MIR_hmm as returned by MIR_hmm().
#' @examples
#' hmm <- load_model("2_state_hmm")
#' print(hmm)
#' @export
load_model <- function(model_name){

file_name <- paste(model_name, ".rds", sep="")

model_path <- system.file("extdata", file_name, package="hmm.mir")

return(readRDS(model_path))

}
