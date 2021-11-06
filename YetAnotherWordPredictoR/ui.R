library(shiny)

shinyUI(fluidPage(
    theme = bslib::bs_theme(bootswatch = "sandstone"),
    titlePanel("Yet Another Word PredictoR"),
    sidebarLayout(
        sidebarPanel(
            h4("Configuration"),
            sliderInput(
                "max_preds",
                "Number of predictions:",
                min = 1,
                max = 10,
                value = 3
            ),
            sliderInput(
                "max_length",
                "Start with n-grams of length:",
                min = 1,
                max = 5,
                value = 5
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Word Predictor",
                    h5("Text"),
                    textInput("text", NULL, ""),
                    h5("Next Word"),
                    tableOutput("prediction"),
                    h5("Runtime of Last Prediction"),
                    textOutput("runtime")
                ),
                tabPanel(
                    "Model",
                    h5("Vocabulary"),
                    tableOutput("vocabulary"),
                    h5("N-Grams"),
                    tableOutput("ngrams")
                ),
                tabPanel(
                    "Help",
                    h5("Configuration"),
                    "Please use the slider 'Number of predictions' to configure the number of next words the algorithm predicts.",
                    "Please use the slider 'Start with n-grams of length' to configure the length of the n-grams the algorithm considers first.",
                    "The higher the length of the n-grams, the more context is included.",
                    h5("Word Predictor"),
                    "This is the main part of the application.",
                    "Please enter a text (which may consist of several sentences) into the text input field in the section 'Text'.",
                    "The algorithm's predictions of the next word are automatically shown in the section 'Next Word'.",
                    "Besides the words, the score and the length of the n-gram and the n-gram are shown.",
                    "The section 'Runtime of Last Prediction' shows the runtime of the last prediction in seconds.",
                    h5("Model"),
                    "In the section 'Vocabulary' the number of words in the vocabulary and the size of the vocabulary are shown.",
                    "In the section 'N-Grams' the number of n-grams, the size of the n-gram matrix, the size of the score vector, and the size of matrix and vector are shown (for n = 1, ..., 5).",
                    "Additionally, the overall number of n-grams, the overall size of all n-gram matrices, the overall size of all score vectors, and the overall size of all matrices and vectors are shown.",
                    h5("Help"),
                    "The usage of the application 'Yet Another Word PredictoR' is explained here."
                )
            )
        )
    )
))
