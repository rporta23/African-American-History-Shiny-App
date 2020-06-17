census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
library(shiny)
library(tidyverse)
library(scales)
library(rsconnect)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Composition of U.S. Population 1790-1870"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            "The purpose of this app is to represent the proportions of whites, free blacks, and enslaved
            blacks in each region of the United States over the period 1790-1870. This period represents essentially the birth of
            the United States as an Independent Nation (Post-Revolutionary War) up until the end of the Civil War and
            the abolition of slavery (1865). I acknowledge that the composition of the nation was far more complex than what
            is represented by this data, and several racial/ethnic groups are not represented. And yet this visualization highlights whites and
            blacks in order to acknowledge the enormous numbers of enslaved persons during this time period and to represent the changes in these
            numbers in different regions over time. This app was created using U.S. census data, source found here: https://www.census.gov/content/dam/Census/library/working-papers/2002/demo/POP-twps0056.pdf.",
            sliderInput("year",
                        "Filter by year:",
                        min = 1790,
                        max = 1870,
                        value = 1790,
                        step = 10
                        ),
            radioButtons("region",
                          "Filter by Region",
                          choices = c("USA Total",
                                      "Northeast",
                                      "Midwest",
                                      "South",
                                      "West"
                                      ),
                         selected = "USA Total"
                         )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("pop_plot")
        )
    )
)

census_tidy <- census %>%
    pivot_longer(names_to = "race_status",
                 values_to = "number",
                 cols = c("white", "black_free", "black_slaves")) %>%
    mutate(race_status = ifelse(race_status == "black_free", "Free Blacks",
                                ifelse(race_status == "black_slaves", "Enslaved Blacks", "Whites")))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$pop_plot <- renderPlot({
        census_tidy2 <- census_tidy %>%
            filter(year == input$year, region == input$region)

        ggplot(census_tidy2, aes(x = race_status,
                                 y = number,
                                 fill = race_status)) +
            geom_col() +
            scale_y_continuous(labels = comma) +
            labs(title = "Composition of U.S. Population",
                 x = "Group",
                 y = "Number of People",
                 fill = "Group")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
