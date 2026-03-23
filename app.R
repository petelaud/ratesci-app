library(shiny)
library(ratesci)

# Define UI for ratesci application
ui <- pageWithSidebar(

  # Application title
  headerPanel("Skewness-Corrected Asymptotic Score confidence intervals for comparison of rates"),

  # Sidebar with controls to select the distribution and contrast etc
  sidebarPanel(

    numericInput("level",
              "Confidence level (%):",
              95,
              min = 1,
              max = 100),

    radioButtons("dist", "Distribution:",
                 list("Binomial" = "bin",
                      "Poisson" = "poi"),
                 selected = "bin",
                 inline = TRUE),

    conditionalPanel('input.dist == "bin"',
      radioButtons(inputId = "contrastbin",
                   label = "Contrast:",
                   list("Rate Difference" = "RD",
                        "Rate Ratio" = "RR",
                        "Odds Ratio" = "OR",
                        "Proportion" = "p"),
                   selected = "RD",
                   inline = TRUE),
    ),

    conditionalPanel('input.dist == "poi"',
      radioButtons(inputId = "contrastpoi",
                  label = "Contrast:",
                  list("Rate Difference" = "RD",
                       "Rate Ratio" = "RR",
                      "Event rate" = "p"),
                  selected = "RD"),
    ),

    textInput("x1", "x1:", "1"),
    textInput("n1", "n1:", "10"),
#    conditionalPanel('input.dist == "bin"',
      conditionalPanel('(input.dist == "bin" & input.contrastbin != "p") |
                       (input.dist == "poi" & input.contrastpoi != "p")',
        textInput("x2", "x2:", "1"),
        textInput("n2", "n2:", "10"),
      ),
#      conditionalPanel('(input.dist == "bin" & input.contrastbin == "p") |
#                       (input.dist == "poi" & input.contrastpoi == "p")',
#                 textInput("x", "x:", "1"),
#                 textInput("n", "n:", "10"),
#      ),
#    ),

#    conditionalPanel('input.dist == "bin" & input.contrastbin == "p" ||
#                     input.dist == "poi" & input.contrastpoi == "p"',
#                     textInput("x", "x:", "1"),
#                     textInput("n", "n:", "10"),
#    ),

    conditionalPanel('(input.dist == "bin" & input.contrastbin != "p") |
                       (input.dist == "poi" & input.contrastpoi != "p")',
                 strong("Skewness correction (omit for Miettinen-Nurminen method):")
    ),
    conditionalPanel('(input.dist == "bin" & input.contrastbin == "p")',
                 strong("Skewness correction (omit for Wilson Score method):")
    ),
    conditionalPanel('(input.dist == "poi" & input.contrastpoi == "p")',
                 strong("Skewness correction (omit for Rao Score method):")
    ),
    radioButtons("skew",
                     NULL,
                     list("Yes" = "TRUE",
                          "No" = "FALSE"),
                      selected = "TRUE",
                      inline = TRUE),

    conditionalPanel('(input.dist == "bin" & input.contrastbin != "p")',

      conditionalPanel('input.skew == "TRUE"',
                        strong("'N-1' correction (omit for Gart-Nam method):")
      ),
      conditionalPanel('(input.skew == "FALSE" & input.contrastbin == "RD")',
                       strong("'N-1' correction (omit for Mee method):")
      ),
      conditionalPanel('(input.skew == "FALSE" & input.contrastbin == "RR")',
                       strong("'N-1' correction (omit for Koopman method):")
      ),
      conditionalPanel('(input.skew == "FALSE" & input.contrastbin == "OR")',
                       strong("'N-1' correction (omit for uncorrected Asymptotic Score method):")
      ),
      radioButtons("bcf",
                     label = NULL,
                     list("Yes" = "TRUE",
                          "No" = "FALSE"),
                      selected = "TRUE",
                      inline = TRUE
                     ),
    ),

#        textInput("level", "Confidence level (%):", "95"),

    textInput("precis", "Decimal precision:", "4"),
    width = 3


  ),


  mainPanel(
    h3(textOutput("caption1")),
    h3(textOutput("caption2")),
    plotOutput("scorePlot")
  )

)


  server <- function(input, output) {



    formulaText1 <- reactive({

      if (input$dist == "bin") {
#        x1 <- ifelse(input$contrastbin == "p",
#                     input$x,
#                     input$x1)
#        n1 <- ifelse(input$contrastbin == "p",
#                     input$n,
#                     input$n1)
        paste0(as.numeric(input$level), "% CI for binomial ",
             input$contrastbin, ": ", input$x1,"/", input$n1,
             ifelse(input$contrastbin == "p", ":  ",
                    paste0(" vs ", input$x2,"/", input$n2, ":  ")))
      } else if (input$dist == "poi") {
#        x1 <- ifelse(input$contrastpoi == "p",
#                     input$x,
#                     input$x1)
#        n1 <- ifelse(input$contrastpoi == "p",
#                     input$n,
#                     input$n1)
        paste0(as.numeric(input$level), "% CI for Poisson ",
               input$contrastpoi, ": ", input$x1,"/", input$n1,
               ifelse(input$contrastpoi == "p", ":  ",
                      paste0(" vs ", input$x2,"/", input$n2, ":  ")))
      }

    })

    formulaText2 <- reactive({
      options(digits = 4)
      contrast <- ifelse(input$dist == "bin",
                         input$contrastbin,
                         input$contrastpoi)
#      x1 <- ifelse(input$contrastbin == "p", input$x, input$x1)
#      n1 <- ifelse(input$contrastbin == "p", input$n, input$n1)
      out <- scoreci(x1 = as.numeric(input$x1),
                     x2 = as.numeric(input$x2),
                     n1 = as.numeric(input$n1),
                     n2 = as.numeric(input$n2),
                     level = as.numeric(input$level) / 100,
                     dist = input$dist,
                     contrast = contrast,
                     skew = input$skew,
                     or_bias = input$skew,
                     bcf = input$bcf,
                     #                   theta0 = as.numeric(input$theta0),
                     plot = FALSE,
                     precis = as.numeric(input$precis))
      myci <- paste0("(",
                     formatC(out$estimates[1],
                             format = ifelse(contrast %in% c("p", "RD"), "f", "fg"),
                             as.numeric(input$precis),
                             flag = "#"),
                     ", ",
                     formatC(out$estimates[3],
                             format = ifelse(contrast %in% c("p", "RD"), "f", "fg"),
                             digits = as.numeric(input$precis),
                             flag = "#"),
                     ")")
      paste0(myci)
    })


    # Return the formula text for printing as a caption
    output$caption1 <- renderText({
      formulaText1()
    })

    output$caption2 <- renderText({
      formulaText2()
    })

    output$scorePlot <- renderPlot({
      contrast <- ifelse(input$dist == "bin",
                         input$contrastbin,
                         input$contrastpoi)
      scoreci(x1 = as.numeric(input$x1),
              x2 = as.numeric(input$x2),
              n1 = as.numeric(input$n1),
              n2 = as.numeric(input$n2),
              level = as.numeric(input$level) / 100,
              dist = input$dist,
              contrast = contrast,
              skew = input$skew,
              or_bias = input$skew,
              bcf = input$bcf,
              #            theta0 = as.numeric(input$theta0),
              plot = TRUE,
              precis = as.numeric(input$precis))
    })

  }


  shinyApp(ui = ui, server = server)



