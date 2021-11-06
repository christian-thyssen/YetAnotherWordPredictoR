# Cleaning the Data
extract_sample <- function(source_file_path, sample_file_path, sample_prob) {
    source_con <- file(source_file_path, "r")
    sample_con <- file(sample_file_path, "w")
    
    line <- readLines(source_con, 1)
    while(length(line) != 0) {
        if (as.logical(rbinom(1, 1, sample_prob))) writeLines(line, sample_con)
        line <- readLines(source_con, 1)
    }
    
    close(source_con)
    close(sample_con)
}

source_file_path <- file.path("final", "en_US", "en_US.twitter.txt")
sample_file_path <- file.path("final", "en_US", "en_US.twitter.sample.txt")
extract_sample(source_file_path, sample_file_path, .1)

library(quanteda)

extract_tokens <- function(file_path) {
    corp <- corpus(readLines(file_path))
    tokens(
        corp,
        remove_punct = TRUE,
        remove_symbols = TRUE,
        remove_numbers = TRUE
    )
}

toks <- extract_tokens(sample_file_path)
