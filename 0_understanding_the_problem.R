# Getting the Data
file_name <- "Coursera-SwiftKey.zip"
url <- paste0("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/", file_name)
if (!file.exists(file_name)) download.file(url, file_name)
unzip(file_name)

# Question 1
file_path <- file.path("final", "en_US", "en_US.blogs.txt")
file.info(file_path)$size / 2 ^ 10 / 2 ^ 10

# Question 2
file_path <- file.path("final", "en_US", "en_US.twitter.txt")
length(readLines(file_path))

# Question 3
file_paths <- list.files(pattern = "en_US\\..*\\.txt", recursive = TRUE)
sapply(file_paths, function(file_path) {
    lines <- readLines(file_path)
    nchars <- sapply(lines, nchar)
    max(nchars)
})

# Question 4
file_path <- file.path("final", "en_US", "en_US.twitter.txt")
lines <- readLines(file_path)
length(grep("love", lines)) / length(grep("hate", lines))

# Question 5
file_path <- file.path("final", "en_US", "en_US.twitter.txt")
lines <- readLines(file_path)
grep("biostats", lines, value = TRUE)

# Question 6
file_path <- file.path("final", "en_US", "en_US.twitter.txt")
lines <- readLines(file_path)
length(grep("^A computer once beat me at chess, but it was no match for me at kickboxing$", lines))
