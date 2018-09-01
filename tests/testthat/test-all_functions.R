context("Testing all functions")

test_that("all functions work correctly", {
  # Prepare data
  jams <- load_jams("S:/", loadExample = TRUE)
  jams_with_genre <- add_genres_to_jams(jams, "S:/", loadExample = TRUE)
  jam_sequence <- create_time_sequence(jams_with_genre)
  splitted_data <- split_data(jam_sequence)
  training_dataset <- splitted_data[[1]]
  test_dataset <- splitted_data[[2]]
  # Train model
  hmm <- MIR_hmm(training_dataset)
  test_results <- test_model(hmm,test_dataset)
  expect_gt( calcAccuracy(test_results), 0.0 )
})
