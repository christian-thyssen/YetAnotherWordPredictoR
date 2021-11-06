library(quanteda)
library(dplyr)
library(shiny)
library(stringi)

env <- new.env()
load_final_model <- function(env) {
    load(file = "final_vocabulary.rds", envir = env)
    load(file = "final_ngmats.rds", envir = env)
    load(file = "final_svecs.rds", envir = env)
}
load_final_model(env)

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

replace_rare_tokens <- function(env, toks) {
    ts <- types(toks)
    tokens_replace(toks, ts, if_else(ts %in% env$vocabulary, ts, "<unk>"))
}

encode_sentence <- function(env, sentence, n) {
    toks <- create_tokens(sentence)
    toks <- replace_rare_tokens(env, toks)
    toks <- as.character(toks)
    if (length(toks) < n) toks <- c(rep("<bos>", n - length(toks)), toks)
    if (length(toks) > n) toks <- toks[(length(toks) - n + 1):length(toks)]
    match(toks, env$vocabulary)
}

predict_next_word_for_encoded_ngram <- function(env, ngram, max_preds, max_length) {
    pred_words <- tibble(
        word <- integer(),
        score <- double(),
        n <- integer(),
        ngram <- character()
    )
    
    for (i in max_length:1) {
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
                    n = i,
                    ngram = apply(
                        env$ngmats[[i]][indices[1:min(max_preds - nrow(pred_words), length(indices))], paste0("word", 1:i), drop = FALSE],
                        1,
                        function(x) paste0(x, collapse = " ")
                    )
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

predict_next_word_for_plain_text <- function(env, text, max_preds, max_length) {
    last_sentence <- stri_extract_last_boundaries(paste0(text, " A"), type = "sentence")
    last_sentence <- substr(last_sentence, 1, nchar(last_sentence) - 2)
    encoded_ngram <- encode_sentence(env, last_sentence, max_length - 1)
    pred_words <- predict_next_word_for_encoded_ngram(
        env,
        encoded_ngram,
        max_preds,
        max_length
    )
    pred_words[, "word"] <- env$vocabulary[pull(pred_words, "word")]
    pred_words[, "ngram"] <- sapply(
        strsplit(pull(pred_words, "ngram"), " "),
        function(x) paste0(env$vocabulary[as.integer(x)], collapse = " ")
    )
    pred_words
}

shinyServer(function(input, output) {
    thematic::thematic_shiny()
    
    prediction <- reactive({
        preds_time <- list()
        tic <- Sys.time()
        preds_time[["preds"]] <- predict_next_word_for_plain_text(env, input$text, input$max_preds, input$max_length)
        toc <- Sys.time()
        preds_time[["time"]] <- toc - tic
        preds_time
    })
    
    output$prediction <- renderTable({
        prediction()[["preds"]]
    })
    
    output$runtime <- renderText({
        paste(format(as.numeric(prediction()[["time"]]), digits = 1L, nsmall = 3L, width = 5L), "s")
    })
    
    output$vocabulary <- renderTable({
        tibble(
            entries = format(length(env$vocabulary), big.mark = ","),
            size = format(object.size(env$vocabulary), units = "auto", standard = "SI")
        )
    })
    
    output$ngrams <- renderTable({
        model_ngrams <- tibble(
            ngrams = character(),
            entries = character(),
            ngrams.size = character(),
            scores.size = character(),
            overall.size = character()
        )
        for (i in 1:length(env$ngmats)) {
            model_ngrams <- rbind(model_ngrams, tibble(
                ngrams = paste0(i, "-grams"),
                entries = format(nrow(env$ngmats[[i]]), big.mark = ","),
                ngrams.size = format(object.size(env$ngmats[[i]]), units = "auto", standard = "SI"),
                scores.size = format(object.size(env$svecs[[i]]), units = "auto", standard = "SI"),
                overall.size = format(object.size(env$ngmats[[i]]) + object.size(env$svecs[[i]]), units = "auto", standard = "SI")
            ))
        }
        model_ngrams <- rbind(model_ngrams, tibble(
            ngrams = paste0("all n-grams"),
            entries = format(sum(sapply(env$ngmats, nrow)), big.mark = ","),
            ngrams.size = format(object.size(env$ngmats), units = "auto", standard = "SI"),
            scores.size = format(object.size(env$svecs), units = "auto", standard = "SI"),
            overall.size = format(object.size(env$ngmats) + object.size(env$svecs), units = "auto", standard = "SI")
        ))
        model_ngrams
    })
})
