library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(poisbinom)

ui <- fluidPage(
    headerPanel("Hot Desks Estimator"),
    sidebarLayout(
        sidebarPanel(
            tags$head(
                tags$style(type="text/css", "select { max-width: 440px; }"),
                tags$style(type="text/css", ".span4 { max-width: 490px; }"),
                tags$style(type="text/css", ".well { max-width: 480px; }")
            ),
      # width = 5,
        tags$head(
            tags$style(
                type = "text/css",
                "label{ keep-all;padding:10px; display: table-cell; text-align: center;vertical-align: middle; } .form-group { display: table-row;}"
            )
        ),
        # div(style = "font-size:24px;font-weight: bold", "Requirements"),
        div(
            style = "font-size:16px;font-weight: bold",
            "Add the number of workers who need hot desks and how many days per week are coming to the office:"
        ),
        
        br(),
        tags$table(
            tags$tr(
                width = "100%",
                tags$td(
                    width = "16%",
                    tags$div(style = "font-size:12pX;font-weight: bold;", "I expect ")
                ),
                tags$td(width = "14%", textInput(inputId = "opt1_workers", label = NULL, 20)),
                tags$td(
                    width = "28%",
                    div(style = "font-size:12pX;font-weight: bold;", HTML('&nbsp;'), " workers coming ")
                ),
                tags$td(width = "14%", textInput(inputId = "opt1_days", label = NULL, 2)),
                tags$td(
                    width = "28%",
                    div(style = "font-size:12pX;font-weight: bold; ", HTML('&nbsp;'), "days per week")
                )
            ),
            tags$tr(
                width = "100%",
                tags$td(
                    width = "16%",
                    tags$div(style = "font-size:12pX;font-weight: bold; ", "I expect ")
                ),
                tags$td(width = "14%", textInput(inputId = "opt2_workers", label = NULL, 10)),
                tags$td(
                    width = "28%",
                    div(style = "font-size:12pX;font-weight: bold;", HTML('&nbsp;'), " workers coming ")
                ),
                tags$td(width = "14%", textInput(inputId = "opt2_days", label = NULL, 3)),
                tags$td(
                    width = "28%",
                    div(style = "font-size:12pX;font-weight: bold; ", HTML('&nbsp;'), "days per week")
                )
            ),
            tags$tr(
                width = "100%",
                tags$td(
                    width = "16%",
                    tags$div(style = "font-size:12pX;font-weight: bold; ", "I expect ")
                ),
                tags$td(width = "14%", textInput(inputId = "opt3_workers", label = NULL, 0)),
                tags$td(
                    width = "28%",
                    div(style = "font-size:12pX;font-weight: bold;", HTML('&nbsp;'), " workers coming ")
                ),
                tags$td(width = "14%", textInput(inputId = "opt3_days", label = NULL, 4)),
                tags$td(
                    width = "28%",
                    div(style = "font-size:12pX;font-weight: bold; ", HTML('&nbsp;'), "days per week")
                )
            ),
            tags$tr(
                width = "100%",
                tags$td(
                    width = "16%",
                    tags$div(style = "font-size:12pX;font-weight: bold; ", "I expect ")
                ),
                tags$td(width = "14%", textInput(inputId = "opt4_workers", label = NULL, 10)),
                tags$td(
                    width = "28%",
                    div(style = "font-size:12pX;font-weight: bold; ", HTML('&nbsp;'), " workers coming ")
                ),
                tags$td(width = "14%", textInput(inputId = "opt4_days", label = NULL, 5)),
                tags$td(
                    width = "28%",
                    div(style = "font-size:12pX;font-weight: bold; ", HTML('&nbsp;'), "days per week")
                )
            ),
            tags$tr(
                width = "100%",
                tags$td(
                    width = "16%",
                    tags$div(style = "font-size:12pX;font-weight: bold; ", "I expect ")
                ),
                tags$td(width = "14%", textInput(inputId = "opt5_workers", label = NULL, 2)),
                tags$td(
                    width = "28%",
                    div(style = "font-size:12pX;font-weight: bold; ", HTML('&nbsp;'), " workers coming ")
                ),
                tags$td(width = "14%", textInput(
                    inputId = "opt5_days", label = NULL, 0.5
                )),
                tags$td(
                    width = "28%",
                    div(style = "font-size:12pX;font-weight: bold; ", HTML('&nbsp;'), "days per week")
                )
            )
        )
        ,
        br(),
        div(
            style = "font-size:10px",
            "(If some workers need to come less than one day per week, you can specify it with decimals. Eg, if coming once every two weeks, specify '0.5 times per week'.)"
        ),
        br(),
        textInput(
            inputId = "error",
            label = div(style = "font-size:12pX", "I'll need having enough desks at least in this % of days") ,
            95
        )
    ),
    mainPanel(plotlyOutput("fig"),
              br(),
              div(style = "font-size:10px",
                  p(HTML(
                      paste0(
                          "The estimation is based on a Poisson binomial distribution and is performed using ",
                          a(href = 'https://cran.r-project.org/web/packages/poisbinom/index.html', 'poisbinom'),
                          " package. More details about how Poission binomial distribution can be used to estimate the number of necessary hot desks is on this ",
                          a(href = 'https://stackoverflow.com/', 'link'),
                          '.'
                      )
                  ))))
    
))

server <- function(input, output, session) {
    prob_list <- reactive ({
        workers <- c(
            input$opt1_workers,
            input$opt2_workers,
            input$opt3_workers,
            input$opt4_workers,
            input$opt5_workers
            
        )
        days <-
            c(
                input$opt1_days,
                input$opt2_days,
                input$opt3_days,
                input$opt4_days,
                input$opt5_days
            )
        
        df <- data.frame(workers = workers,
                         days = days) %>%
            mutate_all(funs(as.integer)) %>%
            mutate(workers = replace_na(workers, 0)) %>%
            filter(workers > 0) %>%
            na.omit() %>%
            mutate(prob = days / 5)
        
        probs <- c()
        for (i in 1:nrow(df)) {
            probs = c(probs, rep(df$prob[i], df$workers[i]))
        }
        
        min_desks <-
            qpoisbinom(as.numeric(input$error) / 100, probs)
        
        density <- sapply(seq(length(probs)), function(x)
            dpoisbinom(x, probs))
        
        plot_df <- data.frame(workers = seq(length(probs)),
                              density = density)
        
        list(plot_df = plot_df, min_desks = min_desks)
    })
    
    output$fig <- renderPlotly({
        p <- prob_list()$plot_df %>%
            ggplot(aes(x = workers, y = density)) +
            geom_line() +
            theme_minimal() +
            geom_vline(
                xintercept = prob_list()$min_desks,
                col = 'red',
                linetype = "dashed"
            ) +
            labs(title = "Distribution of expected workers every day") +
            annotate(
                "text",
                x = prob_list()$min_desks + 5,
                y = 0.5 * max(prob_list()$plot_df$density),
                label = paste0("You need at least ", prob_list()$min_desks, " hot desks"),
                color = 'red'
            )
        
        p %>% plotly::ggplotly()
        
    })
    
    
}
shinyApp(ui, server)


# ADDITIONAL CODE FOR ARTICLE -----
# # Create a sample of 50 numbers which are incremented by 1.
# data.frame(x= seq(0,6,by = 1)) %>% 
#     mutate(rate = dbinom(x,6,1/6)) %>% 
#     ggplot(aes(x,rate))+
#     geom_line()+
#     theme_minimal()+
#     scale_y_continuous(labels = scales::percent)+
#     scale_x_continuous(breaks = seq(0,6))+
#     labs(title="5s you get in 6 dice rolls",x='5s')+
#     theme(panel.grid.minor.x = element_blank())
# 
# ggsave("Downloads/dice.png",  dpi = 300,width=20,height = 10,units = "cm")
# 
# df <- data.frame(x= seq(0,30,by = 1)) %>% 
#     mutate(rate = dbinom(x,30,0.2))
# 
# 
# 
# df %>% 
#     ggplot(aes(x,rate))+
#     geom_line()+
#     theme_minimal()+
#     geom_vline(
#         xintercept = qbinom(0.95,30,0.2),
#         col = 'red',
#         linetype = "dashed"
#     ) +
#     annotate(
#         "text",
#         x = qbinom(0.95,30,0.2) + 6,
#         y = max(df$rate)/2,
#         label = paste0("You need at least ", qbinom(0.95,30,0.2), " hot desks"),
#         color = 'red'
#     )+
#     scale_y_continuous(labels = scales::percent)+
#     labs(title="How many workers will come?",x='Workers')
# 
# qbinom(0.95,30,0.2)
