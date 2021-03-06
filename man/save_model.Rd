% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hello.R
\name{save_model}
\alias{save_model}
\title{Save Hidden Markov model to disk}
\usage{
save_model(model, model_name)
}
\arguments{
\item{model}{An object of the class MIR_hmm as returned by MIR_hmm().}

\item{model_name}{Name of the model on disk.}
}
\description{
This function saves a hidden markov model as serialised object (.rds format) to disk.
}
\examples{
hmm <- load_model("2_state_hmm")

save_model(hmm, "2_state_hmm")
}
