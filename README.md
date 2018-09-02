# hmm.mir
This project uses Hidden Markov Models to give music recommendations on genres

## To use the package download the following files and unzip them:
https://archive.org/download/thisismyjam-datadump/thisismyjam-datadump.zip

http://www.cp.jku.at/datasets/LFM-1b/LFM-1b_UGP.zip

## Use the package

The following code reads the dataset, trains a Hidden Markov Model, tests the model and prints its accuracy.

    # Load and prepare data. Note that you have to replace "S:/" with the path where the unzipped files from above are located.
    jams <- load_jams("S:/")
    jams_with_genre <- add_genres_to_jams(jams, "S:/")
    jam_sequence <- create_time_sequence(jams_with_genre)
    splitted_data <- split_data(jam_sequence)
    training_dataset <- splitted_data[[1]]
    test_dataset <- splitted_data[[2]]

    # Train model
    hmm <- MIR_hmm(training_dataset)
    test_results <- test_model(hmm,test_dataset)
    # Calculate accuracy
    print(calcAccuracy(test_results))

## Use Case: Predict the next genre based on your own listening history

Assuming you listened to rock, heavy metal and pop in this order

    # Load MIR HMM
    hmm <- load_model("2_state_hmm")
    # Create a sequence of your listening history
    seq <- c("rock","heavy metal", "pop")
    # Predict next genre
    prediction <- predict_next(hmm,seq)
    print(prediction)
    
## Reference

Authors of the datasets:

\[1\] Jansson A., Raffel C., and Weyde T. 2015. "This is my Jam -- Data Dump", in 16th International Society for Music Information Retrieval Conference Late Breaking and Demo Papers, 2015.

\[2\] Schedl, M. and Ferwerda, B. 2017. Large-scale Analysis of Group-specific Music Genre Taste From Collaborative Tags. Proceedings of the 19th IEEE International Symposium on Multimedia (ISM 2017), Taichung, Taiwan, December 2017.

