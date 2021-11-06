# Load Libraries

library(quanteda)
library(dplyr)
library(tictoc)
library(stringi)

# Set the seed of the random number generator for reproducability

set.seed(1)

# Split Files

split_file <- function(file_path, train = .6, valid = .2, test = .2) {
    lines <- readLines(
        file_path,
        warn = FALSE,
        encoding = "UTF-8",
        skipNul = TRUE
    )
    
    n <- length(lines)
    indices <- sample(1:n, n)
    
    writeLines(
        lines[indices[1:floor(train * n)]],
        file.path(dirname(file_path), paste0("train_", basename(file_path)))
    )
    writeLines(
        lines[indices[(floor(train * n) + 1):floor((train + valid) * n)]],
        file.path(dirname(file_path), paste0("valid_", basename(file_path)))
    )
    writeLines(
        lines[indices[(floor((train + valid) * n) + 1):floor((train + valid + test) * n)]],
        file.path(dirname(file_path), paste0("test_", basename(file_path)))
    )
}

blogs_file_path <- file.path("final", "en_US", "en_US.blogs.txt")
news_file_path <- file.path("final", "en_US", "en_US.news.txt")
twitter_file_path <- file.path("final", "en_US", "en_US.twitter.txt")

split_file(blogs_file_path, .5, .0001, .0001)
split_file(news_file_path, .5, .0001, .0001)
split_file(twitter_file_path, .5, .0001, .0001)

rm(blogs_file_path, news_file_path, twitter_file_path)

# Training

create_corpus <- function(blogs_file_path, news_file_path, twitter_file_path) {
    blogs_docs <- readLines(
        blogs_file_path,
        warn = FALSE,
        encoding = "UTF-8",
        skipNul = TRUE
    )
    news_docs <- readLines(
        news_file_path,
        warn = FALSE,
        encoding = "UTF-8",
        skipNul = TRUE
    )
    twitter_docs <- readLines(
        twitter_file_path,
        warn = FALSE,
        encoding = "UTF-8",
        skipNul = TRUE
    )
    rm(blogs_file_path, news_file_path, twitter_file_path)
    
    blogs_corp <- corpus(
        blogs_docs,
        docnames = paste0("blogs", 1:length(blogs_docs)),
        docvars = data.frame(source = rep("blogs", length(blogs_docs)))
    )
    news_corp <- corpus(
        news_docs,
        docnames = paste0("news", 1:length(news_docs)),
        docvars = data.frame(source = rep("news", length(news_docs)))
    )
    twitter_corp <- corpus(
        twitter_docs,
        docnames = paste0("twitter", 1:length(twitter_docs)),
        docvars = data.frame(source = rep("twitter", length(twitter_docs)))
    )
    rm(blogs_docs, news_docs, twitter_docs)
    
    corp <- blogs_corp + news_corp + twitter_corp
    rm(blogs_corp, news_corp, twitter_corp)
    
    corpus_reshape(corp, to = "sentences")
}

train_blogs_file_path <- file.path("final", "en_US", "train_en_US.blogs.txt")
train_news_file_path <- file.path("final", "en_US", "train_en_US.news.txt")
train_twitter_file_path <- file.path("final", "en_US", "train_en_US.twitter.txt")

train_corp <- create_corpus(
    train_blogs_file_path,
    train_news_file_path,
    train_twitter_file_path
)

rm(train_blogs_file_path, train_news_file_path, train_twitter_file_path)

find_profane_sentences <- function(corp) {
    tic("Find profane sentences")
    
    file_name <- "badwords.txt"
    if (!file.exists(file_name)) {
        url <- paste0("https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/badwordslist/", file_name)
        download.file(url, file_name, mode = "wb")
    }
    profane_words <- readLines(
        file_name,
        warn = FALSE,
        encoding = "UTF-8",
        skipNul = TRUE
    )
    profane_words <- unique(profane_words)
    for (mchar in c("\\", ".", "|", "(", ")", "[", "{", "^", "$", "*", "+", "?")) {
        profane_words <- gsub(mchar, paste0("\\", mchar), profane_words, fixed = TRUE)
    }
    regexp <- paste0("[^[:alnum:]](", paste0(profane_words, collapse = "|"), ")[^[:alnum:]]")
    profane <- grepl(regexp, as.character(corp))
    
    toc()
    
    profane
}

docvars(train_corp, "profane") <- find_profane_sentences(train_corp)

create_summary_of_profane_sentences <- function(corp) {
    blogs_sentences <- length(corpus_subset(corp, source == "blogs"))
    news_sentences <- length(corpus_subset(corp, source == "news"))
    twitter_sentences <- length(corpus_subset(corp, source == "twitter"))
    
    blogs_profane_sentences <- length(corpus_subset(corp, source == "blogs" & profane == TRUE))
    news_profane_sentences <- length(corpus_subset(corp, source == "news" & profane == TRUE))
    twitter_profane_sentences <- length(corpus_subset(corp, source == "twitter" & profane == TRUE))
    
    tibble(
        source = c(
            "blogs",
            "news",
            "twitter"
        ),
        sentences = c(
            blogs_sentences,
            news_sentences,
            twitter_sentences
        ),
        profane.sentences = c(
            blogs_profane_sentences,
            news_profane_sentences,
            twitter_profane_sentences
        ),
        percentage = c(
            blogs_profane_sentences / blogs_sentences * 100,
            news_profane_sentences / news_sentences * 100,
            twitter_profane_sentences / twitter_sentences * 100
        )
    )
}

create_summary_of_profane_sentences(train_corp)

quanteda_options("pattern_hashtag" = NULL)

create_tokens <- function(corp) {
    toks <- tokens(
        corp,
        remove_punct = TRUE,
        remove_symbols = TRUE,
        remove_numbers = TRUE,
        remove_url = TRUE,
        remove_separators = TRUE
    )
    toks <- tokens_tolower(toks)
    ts <- types(toks)
    toks <- tokens_replace(toks, ts, stri_trans_general(ts, "latin-ascii"), "fixed")
    toks
}

env <- new.env()

env$toks <- create_tokens(corpus_subset(train_corp, profane == FALSE))

rm(train_corp)

create_vocabulary <- function(env, size) {
    dfmat <- dfm(env$toks)
    dfmat <- dfm_sort(dfmat)
    vocabulary <- featnames(dfmat)[1:min(size, length(featnames(dfmat)))]
    c(vocabulary, "<bos>", "<eos>", "<unk>")
}

env$vocabulary <- create_vocabulary(env, 30000)

replace_rare_tokens <- function(env, toks) {
    ts <- types(toks)
    tokens_replace(toks, ts, if_else(ts %in% env$vocabulary, ts, "<unk>"))
}

env$toks <- replace_rare_tokens(env, env$toks)

create_ngmats <- function(env, n) {
    ngmats = list()
    
    for (i in 1:n) {
        tic(paste0("Create ", i, "-gram frequency matrix"))
        
        ngrams <- tokens_ngrams(
            tokens(lapply(env$toks, function(x) c(rep("<bos>", i - 1), x, "<eos>"))),
            i,
            concatenator = " "
        )
        
        dfmat <- dfm(ngrams)
        rm(ngrams)
        
        freq <- featfreq(dfmat)
        rm(dfmat)
        
        ngmat <- cbind(
            matrix(match(unlist(strsplit(names(freq), " ")), env$vocabulary),
                   ncol = i,
                   byrow = TRUE,
                   dimnames = NULL),
            as.integer(freq)
        )
        colnames(ngmat) <- c(paste0("word", 1:i), paste0("freq", i))
        rm(freq)
        
        ngmats[[paste0("ngmat", i)]] <- ngmat
        rm(ngmat)
        
        toc()
    }
    
    ngmats
}

env$ngmats <- create_ngmats(env, 5)

rm(toks, envir = env)

# Preliminary Pruning

paste("Overall size (before removing n-grams occuring once):",
      format(object.size(env$vocabulary) + object.size(env$ngmats) + object.size(env$svecs),
             units = "auto",
             standard = "SI"))

create_list_of_indices_of_common_toks <- function(env, threshold = 2) {
    list_of_indices <- list()
    
    counter <- 0L
    for (i in 1:length(env$ngmats)) {
        list_of_indices[[i]] <- which(env$ngmats[[i]][, paste0("freq", i)] >= threshold)
        counter <- counter + nrow(env$ngmats[[i]]) - length(list_of_indices[[i]])
    }
    print(paste(counter, "entries flagged for deletion!"))
    
    list_of_indices
}

list_of_indices <- create_list_of_indices_of_common_toks(env)
for (i in 1:length(env$ngmats)) {
    env$ngmats[[i]] <- env$ngmats[[i]][list_of_indices[[i]], , drop = FALSE]
}
rm(list_of_indices, i)

paste("Overall size (after removing n-grams occuring once):",
      format(object.size(env$vocabulary) + object.size(env$ngmats) + object.size(env$svecs),
             units = "auto",
             standard = "SI"))

# Sorting

create_list_of_indices_for_sorting <- function(env) {
    list_of_indices <- list()
    
    for (i in 1:length(env$ngmats)) {
        indices <- 1:nrow(env$ngmats[[i]])
        for (j in i:1) indices <- indices[order(env$ngmats[[i]][indices, paste0("word", j)])]
        list_of_indices[[i]] <- indices
    }
    
    list_of_indices
}

list_of_indices <- create_list_of_indices_for_sorting(env)
for (i in 1:length(env$ngmats)) {
    env$ngmats[[i]] <- env$ngmats[[i]][list_of_indices[[i]], , drop = FALSE]
}
rm(list_of_indices, i)

# Preparation for Stupid Backoff

calculate_probs_for_ngmat <- function(env, n, alpha = 0) {
    # Look up frequencies of (n-1)-grams.
    if (n == 1) {
        freqs <- sum(env$ngmats[[1]][, "freq1"])
    } else {
        freqs <- rep(0L, nrow(env$ngmats[[n]]))
        
        # Look up frequencies of w_1 ... w_(n-1) != <bos> ... <bos>.
        i <- 1
        j <- 1
        while (i <= nrow(env$ngmats[[n]]) && j <= nrow(env$ngmats[[n - 1]])) {
            # Compare i-th row of n-gram frequency matrix with j-th row of (n-1)-gram frequency matrix
            comp <- 0L
            for (k in 1:(n - 1)) {
                if (env$ngmats[[n]][i, paste0("word", k)] < env$ngmats[[n - 1]][j, paste0("word", k)]) {
                    comp <- -1L
                    break
                } else if (env$ngmats[[n]][i, paste0("word", k)] > env$ngmats[[n - 1]][j, paste0("word", k)]) {
                    comp <- 1L
                    break
                }
            }
            if (comp == -1L) {
                i <- i + 1
            } else if (comp == 1L) {
                j <- j + 1
            } else {
                freqs[i] <- env$ngmats[[n - 1]][j, paste0("freq", n - 1)]
                i <- i + 1
            }
        }
        
        # Calculate frequencies of w_1 ... w_(n-1) = <bos> ... <bos>.
        bos <- which(env$vocabulary == "<bos>")
        
        indices <- 1:nrow(env$ngmats[[n]])
        for (k in 1:(n - 1)) {
            int <- findInterval(c(bos - 1, bos), env$ngmats[[n]][indices, paste0("word", k)])
            if (int[1] < int[2]) {
                indices <- indices[(int[1] + 1):int[2]]
            } else {
                indices <- integer()
                break
            }
        }
        
        freqs[indices] <- sum(env$ngmats[[n]][indices, paste0("freq", n)])
    }
    
    (env$ngmats[[n]][, paste0("freq", n)] + alpha) / (freqs + alpha * (length(env$vocabulary) - 1))
}

create_svecs_for_stupid_backoff <- function(env, alpha = .4) {
    svecs = list()
    
    for (i in 1:length(env$ngmats)) {
        tic(paste("Create score vector", i))
        
        svecs[[paste0("svec", i)]] <- alpha ^ (length(env$ngmats) - i) * calculate_probs_for_ngmat(env, i)
        
        toc()
    }
    
    svecs
}

env$svecs <- create_svecs_for_stupid_backoff(env)

# Prediction of Next Word with Stupid Backoff

encode_sentence <- function(env, sentence, n) {
    toks <- create_tokens(sentence)
    toks <- replace_rare_tokens(env, toks)
    toks <- as.character(toks)
    if (length(toks) < n) toks <- c(rep("<bos>", n - length(toks)), toks)
    if (length(toks) > n) toks <- toks[(length(toks) - n + 1):length(toks)]
    match(toks, env$vocabulary)
}

predict_next_word_for_encoded_ngram <- function(env, ngram, max_preds = 3) {
    pred_words <- tibble(
        word <- integer(),
        score <- double(),
        n <- integer()
    )
    
    for (i in length(env$ngmats):1) {
        indices <- 1:nrow(env$ngmats[[i]])
        
        # Remove indices of i-grams not starting with the n-gram.
        if (i > 1) {
            for (j in 1:(i - 1)) {
                int <- findInterval(c(ngram[j] - 1, ngram[j]), env$ngmats[[i]][indices, paste0("word", j)])
                if (int[1] < int[2]) {
                    indices <- indices[(int[1] + 1):int[2]]
                } else {
                    indices <- integer()
                    break
                }
            }
        }
        
        # Remove indices of i-grams ending with an already predicted word.
        if (nrow(pred_words) > 0) {
            indices <- indices[which(!(env$ngmats[[i]][indices, paste0("word", i)] %in% pull(pred_words, "word")))]
        }
        
        # Add new predictions to predicted words.
        if (length(indices) > 0) {
            # Sort matching n-grams in descending order of their scores.
            indices <- indices[order(env$svecs[[i]][indices], decreasing = TRUE)]
            
            pred_words <- rbind(
                pred_words,
                tibble(
                    word = env$ngmats[[i]][indices[1:min(max_preds - nrow(pred_words), length(indices))], paste0("word", i)],
                    score = env$svecs[[i]][indices[1:min(max_preds - nrow(pred_words), length(indices))]],
                    n = i
                )
            )
        }
        
        # Stop or continue with a shorter n-gram.
        if (nrow(pred_words) == max_preds) {
            break
        } else {
            ngram <- ngram[-1]
        }
    }
    
    pred_words
}

predict_next_word_for_plain_text <- function(env, text, max_preds = 3) {
    last_sentence <- stri_extract_last_boundaries(paste0(text, " A"), type = "sentence")
    last_sentence <- substr(last_sentence, 1, nchar(last_sentence) - 2)
    encoded_ngram <- encode_sentence(env, last_sentence, length(env$ngmats) - 1)
    pred_words <- predict_next_word_for_encoded_ngram(
        env,
        encoded_ngram,
        max_preds
    )
    pred_words[, "word"] <- env$vocabulary[pull(pred_words, "word")]
    pred_words
}

# Validate the Model

valid_blogs_file_path <- file.path("final", "en_US", "valid_en_US.blogs.txt")
valid_news_file_path <- file.path("final", "en_US", "valid_en_US.news.txt")
valid_twitter_file_path <- file.path("final", "en_US", "valid_en_US.twitter.txt")

valid_corp <- create_corpus(
    valid_blogs_file_path,
    valid_news_file_path,
    valid_twitter_file_path
)

rm(valid_blogs_file_path, valid_news_file_path, valid_twitter_file_path)

calculate_probs_for_unseen_ngmat <- function(env, ngmat, alpha = 0) {
    n <- ncol(ngmat)
    
    freqs1 <- rep(0L, nrow(ngmat))
    
    # Sort n-gram matrix (for efficient look up).
    ord <- 1:nrow(ngmat)
    for (i in n:1) ord <- ord[order(ngmat[ord, paste0("word", i)])]
    ngmat <- ngmat[ord, , drop = FALSE]
    
    # Look up frequencies of n-grams.
    i <- 1
    j <- 1
    while (i <= nrow(ngmat) && j <= nrow(env$ngmats[[n]])) {
        # Compare i-th row of n-gram matrix with j-th row of n-gram frequency matrix
        comp <- 0L
        for (k in 1:n) {
            if (ngmat[i, paste0("word", k)] < env$ngmats[[n]][j, paste0("word", k)]) {
                comp <- -1L
                break
            } else if (ngmat[i, paste0("word", k)] > env$ngmats[[n]][j, paste0("word", k)]) {
                comp <- 1L
                break
            }
        }
        if (comp == -1L) {
            i <- i + 1
        } else if (comp == 1L) {
            j <- j + 1
        } else {
            freqs1[i] <- env$ngmats[[n]][j, paste0("freq", n)]
            i <- i + 1
        }
    }
    
    # Look up frequencies of (n-1)-grams.
    if (n == 1) {
        freqs2 <- sum(env$ngmats[[1]][, "freq1"])
    } else {
        freqs2 <- rep(0L, nrow(ngmat))
        
        # Look up frequencies of w_1 ... w_(n-1) != <bos> ... <bos>.
        i <- 1
        j <- 1
        while (i <= nrow(ngmat) && j <= nrow(env$ngmats[[n - 1]])) {
            # Compare i-th row of n-gram matrix with j-th row of (n-1)-gram frequency matrix
            comp <- 0L
            for (k in 1:(n - 1)) {
                if (ngmat[i, paste0("word", k)] < env$ngmats[[n - 1]][j, paste0("word", k)]) {
                    comp <- -1L
                    break
                } else if (ngmat[i, paste0("word", k)] > env$ngmats[[n - 1]][j, paste0("word", k)]) {
                    comp <- 1L
                    break
                }
            }
            if (comp == -1L) {
                i <- i + 1
            } else if (comp == 1L) {
                j <- j + 1
            } else {
                freqs2[i] <- env$ngmats[[n - 1]][j, paste0("freq", n - 1)]
                i <- i + 1
            }
        }
        
        # Calculate frequencies of w_1 ... w_(n-1) = <bos> ... <bos>.
        bos <- which(env$vocabulary == "<bos>")
        
        indices <- 1:nrow(ngmat)
        for (j in 1:(n - 1)) {
            int <- findInterval(c(bos - 1, bos), ngmat[indices, paste0("word", j)])
            if (int[1] < int[2]) {
                indices <- indices[(int[1] + 1):int[2]]
            } else {
                indices <- integer()
                break
            }
        }
        
        indices2 <- 1:nrow(env$ngmats[[n]])
        for (i in 1:(n - 1)) {
            int <- findInterval(c(bos - 1, bos), env$ngmats[[n]][indices2, paste0("word", i)])
            if (int[1] < int[2]) {
                indices2 <- indices2[(int[1] + 1):int[2]]
            } else {
                indices2 <- integer()
                break
            }
        }
        
        freqs2[indices] <- sum(env$ngmats[[n]][indices2, paste0("freq", n)])
    }
    
    # Revert sorting
    freqs1[ord] <- freqs1
    freqs2[ord] <- freqs2
    
    (freqs1 + alpha) / (freqs2 + alpha * (length(env$vocabulary) - 1))
}

calculate_perplexity <- function(env, n, corp, alpha = 0) {
    toks <- create_tokens(corp)
    toks <- replace_rare_tokens(env, toks)
    ngrams <- as.character(
        tokens_ngrams(
            tokens(lapply(toks, function(x) c(rep("<bos>", n - 1), x, "<eos>"))),
            n,
            concatenator = " "
        )
    )
    ngmat <- matrix(
        match(unlist(strsplit(ngrams, " ")), env$vocabulary),
        ncol = n,
        byrow = TRUE,
        dimnames = list(NULL, paste0("word", 1:n))
    )
    probs <- calculate_probs_for_unseen_ngmat(env, ngmat, alpha)
    logprobs <- log(probs)
    exp(-sum(logprobs / length(logprobs)))
}

create_summary_of_ngmats <- function(env, corp, alpha = 0) {
    tic("Create summary of n-gram frequency matrices")
    
    summary <- tibble(
        name = character(),
        entries = integer(),
        size = character(),
        min.freq = integer(),
        mean.freq = double(),
        max.freq = integer(),
        perplexity = double()
    )
    
    for (i in 1:length(env$ngmats)) {
        summary <- rbind(summary, tibble(
            name = paste0(i, "-grams"),
            entries = nrow(env$ngmats[[i]]),
            size = format(object.size(env$ngmats[[i]]), units = "auto", standard = "SI"),
            min.freq = min(env$ngmats[[i]][, paste0("freq", i)]),
            mean.freq = round(mean(env$ngmats[[i]][, paste0("freq", i)]), digits = 1),
            max.freq = max(env$ngmats[[i]][, paste0("freq", i)]),
            perplexity = round(calculate_perplexity(env, i, corp, alpha), digits = 1)
        ))
    }
    
    toc()
    
    summary
}

calculate_accuracies <- function(env, corp, max_preds = 3) {
    n <- length(env$ngmats)
    toks <- create_tokens(corp)
    toks <- replace_rare_tokens(env, toks)
    ngrams <- tokens_ngrams(
        tokens(lapply(toks, function(x) c(rep("<bos>", n - 1), x, "<eos>"))),
        n,
        concatenator = " "
    )
    freq <- featfreq(dfm(ngrams))
    ngmat <- matrix(
        match(unlist(strsplit(names(freq), " ")), env$vocabulary),
        ncol = n,
        byrow = TRUE,
        dimnames = list(NULL, paste0("word", 1:n))
    )
    freq <- as.integer(freq)
    accuracies <- rep(0L, max_preds)
    for (i in 1:nrow(ngmat)) {
        pred <- predict_next_word_for_encoded_ngram(
            env,
            ngmat[i, 1:(n - 1)],
            max_preds
        )
        pred <- pull(pred, "word")
        for (j in 1:max_preds) {
            if (ngmat[i, n] %in% pred[1:j]) {
                accuracies[j] <- accuracies[j] + freq[i]
            }
        }
        if (i %% 1000 == 0) print(paste0(i, "/", nrow(ngmat), " done."))
    }
    accuracies / sum(freq)
}

create_summary_of_algorithm <- function(env, corp, max_preds = 3) {
    tic("Create summary of prediction algorithm")
    
    overall_summary <- tibble(
        vocabulary.size = format(object.size(env$vocabulary), units = "auto", standard = "SI"),
        ngmats.size = format(object.size(env$ngmats), units = "auto", standard = "SI"),
        svecs.size = format(object.size(env$svecs), units = "auto", standard = "SI"),
        overall.size = format(object.size(env$vocabulary) + object.size(env$ngmats) + object.size(env$svecs),
                              units = "auto",
                              standard = "SI")
    )
    
    accuracy <- calculate_accuracies(env, corp, max_preds)
    for (i in 1:max_preds) overall_summary <- cbind(overall_summary, accuracy[i])
    colnames(overall_summary)[(4 + 1):(4 + max_preds)] <- paste0("accuracy.", 1:max_preds)
    
    toc()
    
    overall_summary
}

create_summary_of_ngmats(env, valid_corp, 1)
create_summary_of_algorithm(env, valid_corp)

# Optimize the Model

list_of_indices <- create_list_of_indices_of_common_toks(env, 3)
for (i in 1:length(env$ngmats)) {
    env$ngmats[[i]] <- env$ngmats[[i]][list_of_indices[[i]], , drop = FALSE]
    env$svecs[[i]] <- env$svecs[[i]][list_of_indices[[i]]]
}
rm(list_of_indices, i)

create_summary_of_ngmats(env, valid_corp, 1)
create_summary_of_algorithm(env, valid_corp)

list_of_indices <- create_list_of_indices_of_common_toks(env, 4)
for (i in 1:length(env$ngmats)) {
    env$ngmats[[i]] <- env$ngmats[[i]][list_of_indices[[i]], , drop = FALSE]
    env$svecs[[i]] <- env$svecs[[i]][list_of_indices[[i]]]
}
rm(list_of_indices, i)

create_summary_of_ngmats(env, valid_corp, 1)
create_summary_of_algorithm(env, valid_corp)

rm(valid_corp)

# Save and Load the Model

save_model <- function(env) {
    save(vocabulary, file = "vocabulary.rds", envir = env)
    save(ngmats, file = "ngmats.rds", envir = env)
    save(svecs, file = "svecs.rds", envir = env)
}
save_model(env)

rm(vocabulary, envir = env)
rm(ngmats, envir = env)
rm(svecs, envir = env)

load_model <- function(env) {
    load(file = "vocabulary.rds", envir = env)
    load(file = "ngmats.rds", envir = env)
    load(file = "svecs.rds", envir = env)
}
load_model(env)

# Test the Model

test_blogs_file_path <- file.path("final", "en_US", "test_en_US.blogs.txt")
test_news_file_path <- file.path("final", "en_US", "test_en_US.news.txt")
test_twitter_file_path <- file.path("final", "en_US", "test_en_US.twitter.txt")

test_corp <- create_corpus(
    test_blogs_file_path,
    test_news_file_path,
    test_twitter_file_path
)

rm(test_blogs_file_path, test_news_file_path, test_twitter_file_path)

create_summary_of_ngmats(env, test_corp, 1)
create_summary_of_algorithm(env, test_corp)

rm(test_corp)

# First Application

find_score_of_encoded_ngram <- function(env, ngram) {
    for (i in length(env$ngmats):1) {
        indices <- 1:nrow(env$ngmats[[i]])
        
        # Remove indices of i-grams not starting with the n-gram.
        for (j in 1:i) {
            int <- findInterval(c(ngram[j] - 1, ngram[j]), env$ngmats[[i]][indices, paste0("word", j)])
            if (int[1] < int[2]) {
                indices <- indices[(int[1] + 1):int[2]]
            } else {
                indices <- integer()
                break
            }
        }
        
        # Stop or continue with a shorter n-gram.
        if (length(indices) == 1L) {
            return(env$svecs[[i]][indices])
        } else {
            ngram <- ngram[-1]
        }
    }
    
    0
}

questions_1 <- c(
    "The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
    "You're the reason why I smile everyday. Can you follow me please? It would mean the",
    "Hey sunshine, can you follow me and make me the",
    "Very early observations on the Bills game: Offense still struggling but the",
    "Go on a romantic date at the",
    "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
    "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
    "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
    "Be grateful for the good times and keep the faith during the",
    "If this isn't the cutest thing you've ever seen, then you must be"
)
answers_1 <- list(
    c("beer", "pretzels", "soda", "cheese"), # correct answer: beer
    c("universe", "most", "world", "best"), # correct answer: world
    c("bluest", "happiest", "saddest", "smelliest"), # correct answer: happiest
    c("players", "referees", "defense", "crowd"), # correct answer: defense
    c("beach", "mall", "grocery", "movies"), # correct answer: beach
    c("motorcycle", "horse", "way", "phone"), # correct answer: way
    c("years", "time", "thing", "weeks"), # correct answer: time
    c("fingers", "toes", "eyes", "ears"), # correct answer: fingers
    c("sad", "hard", "bad", "worse"), # correct answer: bad
    c("asleep", "insane", "insensitive", "callous") # correct answer: insane
)
for (i in 1:10) {
    print(paste0(format(i, width = 2L), ". Question:     ", questions_1[i]))
    pred <- predict_next_word_for_plain_text(env, questions_1[i], 4)
    for (j in 1:nrow(pred)) {
        print(
            paste0(
                format(i, width = 2L), ". Prediction ", j, ": ",
                format(pred[j, "word", drop = TRUE], width = 15L),
                " INV",
                " (found ", pred[j, "n", drop = TRUE], "-gram with score ",
                format(pred[j, "score", drop = TRUE],
                       digits = 1L, nsmall = 9L, width = 11L, scientific = FALSE,
                       small.mark = " ", small.interval = 3L),
                ")"
            )
        )
    }
    for (j in 1:length(answers_1[[i]])) {
        score <- find_score_of_encoded_ngram(
            env,
            encode_sentence(
                env,
                paste(questions_1[i], answers_1[[i]][j]),
                length(env$ngmats)
            )
        )
        print(
            paste0(
                format(i, width = 2L), ". Answer ", j, ":     ",
                format(answers_1[[i]][j], width = 15L),
                if_else(answers_1[[i]][j] %in% env$vocabulary, " INV", " OOV"),
                " (found n-gram with score ",
                format(score,
                       digits = 1L, nsmall = 9L, width = 11L, scientific = FALSE,
                       small.mark = " ", small.interval = 3L),
                ")"
            )
        )
    }
}
rm(questions_1, answers_1, i, pred, j, score)

# Second Application

questions_2 <- c(
    "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
    "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
    "I'd give anything to see arctic monkeys this",
    "Talking to your mom has the same effect as a hug and helps reduce your",
    "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
    "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
    "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
    "Every inch of you is perfect from the bottom to the",
    "I’m thankful my childhood was filled with imagination and bruises from playing",
    "I like how the same people are in almost all of Adam Sandler's"
)
answers_2 <- list(
    c("eat", "give", "die", "sleep"), # correct answer: die
    c("financial", "spiritual", "marital", "horticultural"), # correct answer: marital
    c("decade", "weekend", "month", "morning"), # correct answer: weekend
    c("stress", "sleepiness", "happiness", "hunger"), # correct answer: stress
    c("minute", "look", "walk", "picture"), # correct answer: picture
    c("matter", "case", "incident", "account"), # correct answer: matter
    c("finger", "hand", "arm", "toe"), # correct answer: hand
    c("center", "top", "middle", "side"), # correct answer: top
    c("inside", "daily", "outside", "weekly"), # correct answer: outside
    c("novels", "stories", "movies", "pictures") # correct answer: movies
)
for (i in 1:10) {
    print(paste0(format(i, width = 2L), ". Question:     ", questions_2[i]))
    pred <- predict_next_word_for_plain_text(env, questions_2[i], 4)
    for (j in 1:nrow(pred)) {
        print(
            paste0(
                format(i, width = 2L), ". Prediction ", j, ": ",
                format(pred[j, "word", drop = TRUE], width = 15L),
                " INV",
                " (found ", pred[j, "n", drop = TRUE], "-gram with score ",
                format(pred[j, "score", drop = TRUE],
                       digits = 1L, nsmall = 9L, width = 11L, scientific = FALSE,
                       small.mark = " ", small.interval = 3L),
                ")"
            )
        )
    }
    for (j in 1:length(answers_2[[i]])) {
        score <- find_score_of_encoded_ngram(
            env,
            encode_sentence(
                env,
                paste(questions_2[i], answers_2[[i]][j]),
                length(env$ngmats)
            )
        )
        print(
            paste0(
                format(i, width = 2L), ". Answer ", j, ":     ",
                format(answers_2[[i]][j], width = 15L),
                if_else(answers_2[[i]][j] %in% env$vocabulary, " INV", " OOV"),
                " (found n-gram with score ",
                format(score,
                       digits = 1L, nsmall = 9L, width = 11L, scientific = FALSE,
                       small.mark = " ", small.interval = 3L),
                ")"
            )
        )
    }
}
rm(questions_2, answers_2, i, pred, j, score)

# Final Pruning

paste("Overall size (before limitation to best n-grams):",
      format(object.size(env$vocabulary) + object.size(env$ngmats) + object.size(env$svecs),
             units = "auto",
             standard = "SI"))

create_list_of_indices_of_top_preds <- function(env, max_preds = 3) {
    list_of_indices <- list()
    
    counter <- 0L
    for (i in 1:length(env$ngmats)) {
        ngdf <- as_tibble(env$ngmats[[i]]) %>%
            mutate(index = 1:nrow(env$ngmats[[i]])) %>%
            mutate(score = env$svecs[[i]]) %>%
            arrange(desc(score))
        if (i > 1) {
            words <- paste0("word", 1:(i - 1))
            ngdf <- ngdf %>%
                group_by_at(vars(all_of(words)))
        }
        ngdf <- ngdf %>%
            slice_head(n = max_preds)
        list_of_indices[[i]] <- sort(pull(ngdf, "index"))
        counter <- counter + nrow(env$ngmats[[i]]) - length(list_of_indices[[i]])
    }
    print(paste(counter, "entries flagged for deletion!"))
    
    list_of_indices
}

list_of_indices <- create_list_of_indices_of_top_preds(env, 10)
for (i in 1:length(env$ngmats)) {
    env$ngmats[[i]] <- env$ngmats[[i]][list_of_indices[[i]], , drop = FALSE]
    env$svecs[[i]] <- env$svecs[[i]][list_of_indices[[i]]]
}
rm(list_of_indices, i)

paste("Overall size (after limitation to best n-grams):",
      format(object.size(env$vocabulary) + object.size(env$ngmats) + object.size(env$svecs),
             units = "auto",
             standard = "SI"))

# Final Cleanup of Frequencies

paste("Overall size (before deleting frequencies):",
      format(object.size(env$vocabulary) + object.size(env$ngmats) + object.size(env$svecs),
             units = "auto",
             standard = "SI"))

for (i in 1:length(env$ngmats)) {
    env$ngmats[[i]] <- env$ngmats[[i]][, paste0("word", 1:i), drop = FALSE]
}
rm(i)

paste("Overall size (after deleting frequencies):",
      format(object.size(env$vocabulary) + object.size(env$ngmats) + object.size(env$svecs),
             units = "auto",
             standard = "SI"))

# Save and Load the Model

save_final_model <- function(env) {
    save(vocabulary, file = "final_vocabulary.rds", envir = env)
    save(ngmats, file = "final_ngmats.rds", envir = env)
    save(svecs, file = "final_svecs.rds", envir = env)
}
save_final_model(env)

rm(vocabulary, envir = env)
rm(ngmats, envir = env)
rm(svecs, envir = env)

load_final_model <- function(env) {
    load(file = "final_vocabulary.rds", envir = env)
    load(file = "final_ngmats.rds", envir = env)
    load(file = "final_svecs.rds", envir = env)
}
load_final_model(env)

# Unit Tests

check_env <- new.env()
check_train_docs <- c(
    "ä a.",
    "a b.",
    "a c."
)
check_train_corp <- corpus(check_train_docs) %>% corpus_reshape(to = "sentences")
rm(check_train_docs)
check_env$toks <- create_tokens(check_train_corp)
check_env$vocabulary <- create_vocabulary(check_env, 3)
check_env$toks <- replace_rare_tokens(check_env, check_env$toks)
check_env$ngmats <- create_ngmats(check_env, 3)
rm(toks, envir = check_env)
list_of_indices <- create_list_of_indices_for_sorting(check_env)
for (i in 1:length(check_env$ngmats)) {
    check_env$ngmats[[i]] <- check_env$ngmats[[i]][list_of_indices[[i]], , drop = FALSE]
}
rm(list_of_indices, i)
check_env$svecs <- create_svecs_for_stupid_backoff(check_env)

all(calculate_probs_for_ngmat(check_env, 1) == c(4 / 9, 1 / 9, 1 / 9, 3 / 9))
all(calculate_probs_for_ngmat(check_env, 2) == c(1 / 4, 1 / 4, 1 / 4, 1 / 4, 1 / 1, 1 / 1, 3 / 3))
all(calculate_probs_for_ngmat(check_env, 3) == c(1 / 1, 1 / 1, 1 / 1, 1 / 3, 1 / 3, 1 / 3, 3 / 3))

calculate_perplexity(check_env, 1, check_train_corp) ==
    exp(-sum(c(log(4 / 9) / 9, log(4 / 9) / 9, log(3 / 9) / 9, log(4 / 9) / 9, log(1 / 9) / 9, log(3 / 9) / 9, log(4 / 9) / 9, log(1 / 9) / 9, log(3 / 9) / 9)))
calculate_perplexity(check_env, 2, check_train_corp) ==
    exp(-sum(c(log(3 / 3) / 9, log(1 / 4) / 9, log(1 / 4) / 9, log(3 / 3) / 9, log(1 / 4) / 9, log(1 / 1) / 9, log(3 / 3) / 9, log(1 / 4) / 9, log(1 / 1) / 9)))
calculate_perplexity(check_env, 3, check_train_corp) ==
    exp(-sum(c(log(3 / 3) / 9, log(1 / 3) / 9, log(1 / 1) / 9, log(3 / 3) / 9, log(1 / 3) / 9, log(1 / 1) / 9, log(3 / 3) / 9, log(1 / 3) / 9, log(1 / 1) / 9)))

check_valid_docs <- c(
    "a b c."
)
check_valid_corp <- corpus(check_valid_docs) %>% corpus_reshape(to = "sentences")
rm(check_valid_docs)

calculate_perplexity(check_env, 3, check_valid_corp, 1) ==
    exp(-sum(c(log((3 + 1) / (3 + 5)) / 4, log((1 + 1) / (3 + 5)) / 4, log((0 + 1) / (1 + 5)) / 4, log((0 + 1) / (0 + 5)) / 4)))

all(calculate_accuracies(check_env, check_valid_corp, 1) == c(2 / 4))
all(calculate_accuracies(check_env, check_valid_corp, 2) == c(2 / 4, 3 / 4))
all(calculate_accuracies(check_env, check_valid_corp, 3) == c(2 / 4, 3 / 4, 3 / 4))
all(calculate_accuracies(check_env, check_valid_corp, 4) == c(2 / 4, 3 / 4, 3 / 4, 4 / 4))

s1 <- create_summary_of_ngmats(check_env, check_valid_corp, 1)
all(dim(s1) == c(3, 7))
all(pull(s1, "entries") == c(4, 7, 7))
s2 <- create_summary_of_algorithm(check_env, check_valid_corp)
all(dim(s2) == c(1, 7))
rm(s1, s2)

list_of_indices <- create_list_of_indices_of_common_toks(check_env)
for (i in 1:length(check_env$ngmats)) {
    check_env$ngmats[[i]] <- check_env$ngmats[[i]][list_of_indices[[i]], , drop = FALSE]
    check_env$svecs[[i]] <- check_env$svecs[[i]][list_of_indices[[i]]]
}
rm(list_of_indices, i)

s1 <- create_summary_of_ngmats(check_env, check_valid_corp, 1)
all(dim(s1) == c(3, 7))
all(pull(s1, "entries") == c(2, 1, 1))
s2 <- create_summary_of_algorithm(check_env, check_valid_corp)
all(dim(s2) == c(1, 7))
rm(s1, s2)

list_of_indices <- create_list_of_indices_of_top_preds(check_env, 2)
for (i in 1:length(check_env$ngmats)) {
    check_env$ngmats[[i]] <- check_env$ngmats[[i]][list_of_indices[[i]], , drop = FALSE]
    check_env$svecs[[i]] <- check_env$svecs[[i]][list_of_indices[[i]]]
}
rm(list_of_indices, i)

s1 <- create_summary_of_ngmats(check_env, check_valid_corp, 1)
all(dim(s1) == c(3, 7))
all(pull(s1, "entries") == c(2, 1, 1))
s2 <- create_summary_of_algorithm(check_env, check_valid_corp)
all(dim(s2) == c(1, 7))
rm(s1, s2)
