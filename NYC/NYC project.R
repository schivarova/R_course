
install.packages('shinythemes')
install.packages('dplyr')
install.packages('tydyverse')
install.packages('shinyWidgets')
install.packages('shiny')

library(shinythemes)
library(tidyverse)
library(shiny)
library(dplyr)
library(shinyWidgets)

nyc_df <- read.csv("/Users/msgrivachev/Downloads/AB_NYC_2019.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("spacelab"),
    #setBackgroundImage(src = "/Users/msgrivachev/Downloads/new-york-city-airbnb-open-data/nn.png"),
    # Application title
    titlePanel(h1("New-York City apartment offer on Airbnb ")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(position = "left",
        sidebarPanel(width = 3, 
            selectInput("area",
                        "Neighbourhood:",
                        choices = c('Manhattan', 'Brooklyn', 'Bronx', 'Queens', 'Staten Island')),
            uiOutput("special"),
            selectInput("room",
                        "Room Type:",
                        choices = unique(nyc_df$room_type)),
            radioButtons("graph",
                         "Graph type:",
                         choices = c("Plot of reviews", "Plot of bookings"),
                         selected = "Plot of reviews")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h4("AAAAA 6:37 утра, я дала ебу но"),
            p('сюда можно вставить красивый текст на английском, который буду писать не я или удалить эту строчку'),
            tags$a(href = 'https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data', 'Open Data Source (Kaggle)'),
            fluidRow(plotOutput("distPlot1"), style = "padding-top:80px"),
            p('HW #9, made by  Mstislav Grivachev')
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$special <- renderUI({
        switch(input$area,
               "Brooklyn" = selectInput("nhd", "Neighbourhood:",
                                        choices = unique(nyc_df[nyc_df$neighbourhood_group == 'Brooklyn', 6])),
               "Manhattan" = selectInput("nhd", "Neighbourhood:",
                                         choices = unique(nyc_df[nyc_df$neighbourhood_group == 'Manhattan', 6])),
               "Queens" = selectInput("nhd", "Neighbourhood:",
                                      choices = unique(nyc_df[nyc_df$neighbourhood_group == 'Queens', 6])), 
               "Staten Island" = selectInput("nhd", "Neighbourhood:",
                                             choices = unique(nyc_df[nyc_df$neighbourhood_group == 'Staten Island', 6])),
               "Bronx" = selectInput("nhd", "Neighbourhood:",
                                     choices = unique(nyc_df[nyc_df$neighbourhood_group == 'Bronx', 6])))
        
    })
    
    
 
    output$distPlot1 <- renderPlot({
        
    area_data <- nyc_df %>% filter(neighbourhood == req(input$nhd),
                                room_type == req(input$room)) %>% select(-c(name, id, host_id, host_name))
        
        
    if (input$graph == 'Plot of reviews'){
        
        ggplot(data = area_data, aes(x = price, y = number_of_reviews)) + geom_point(color = "#CE4257", size = 0.7) +
            labs(title = "Dependence between price for property and popularity of announcements",
                 x = "Price ($)", y = "Reviews")+ xlim(0, 1000)+
        geom_vline(xintercept = median(area_data$price),
                   color = "#4A5859",
                   lty = 2)  + theme(plot.title = element_text(size=16, colour = '#CE4257', family = 'Open Sans', face = 'bold'))+
            theme(axis.line = element_line(colour = "black"))

        
        }else{
            ggplot(data  = area_data, aes(x = price, y = calculated_host_listings_count)) + 
                geom_point(color = '#0F7173', size = 0.7)+labs(title = "Dependence between price and bookings", 
                                  x = "Price ($)", y = "Number of bookings") + xlim(0, 1000) +
                geom_vline(xintercept = median(area_data$price),
                           color = "#4A5859",
                           lty = 2) + theme(plot.title = element_text(size=16, colour = '#0F7173', family = 'Open Sans', face = 'bold')) +
                theme(axis.line = element_line(colour = "black"))
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

