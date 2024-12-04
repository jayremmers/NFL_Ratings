#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readxl)
WeekRanks <- read_excel("WeekRanks.xlsx")

library(png)
library(ggplot2)
library(dplyr)
library(ggimage)
library(magick)
library(DT)
library(plotly)
library(tidyr)

# colors
custom_colors <- c("Bills" = "#002c96",
                   "Dolphins" = "#008e97",
                   "Jets" = "#10573f",
                   "Patriots" = "#0b2342",
                   "Chiefs" = "#e31837",
                   "Chargers" = "#ffc20d",
                   "Broncos" = "#ff4e02",
                   "Raiders" = "#cbcdd3",
                   "Ravens" = "#231076",
                   "Bengals" = "#fb4f14",
                   "Steelers" = "#eead1d",
                   "Browns" = "#ff3b03",
                   "Texans" = "#a71930",
                   "Titans" = "#4c92db",
                   "Colts" = "#013369",
                   "Jaguars" = "#036676",
                   "Eagles" = "#004c54",
                   "Cowboys" = "#002244",
                   "Giants" = "#0b2265",
                   "Commanders" = "#591414",
                   "49ers" = "#b3995d",
                   "Cardinals" = "#af063b",
                   "Seahawks" = "#54b956",
                   "Rams" = "#003299",
                   "Vikings" = "#462678",
                   "Packers" = "#203731",
                   "Lions" = "#0076b8",
                   "Bears" = "#e64201",
                   "Falcons" = "#c82540",
                   "Saints" = "#d3bc8d",
                   "Panthers" = "#0285ca",
                   "Buccaneers" = "#d50b0d"
)

# logos
WeekRanks <- WeekRanks %>%
  mutate(Image = case_when(
    Team == "Bills" ~ "bills.png",
    Team == "Dolphins" ~ "dolphins.png",
    Team == "Patriots" ~ "patriots.png",
    Team == "Jets" ~ "jets1.png",
    Team == "Chiefs" ~ "chiefs.png",
    Team == "Chargers" ~ "chargers.png",
    Team == "Broncos" ~ "broncos.png",
    Team == "Raiders" ~ "raiders.png",
    Team == "Ravens" ~ "ravens.png",
    Team == "Bengals" ~ "bengals.png",
    Team == "Steelers" ~ "steelers1.png",
    Team == "Browns" ~ "browns.png",
    Team == "Texans" ~ "texans.png",
    Team == "Titans" ~ "titans.png",
    Team == "Colts" ~ "colts.png",
    Team == "Jaguars" ~ "jaguars.png",
    Team == "Eagles" ~ "eagles.png",
    Team == "Cowboys" ~ "cowboys.png",
    Team == "Giants" ~ "giants.png",
    Team == "Commanders" ~ "commanders.png",
    Team == "49ers" ~ "49ers1.png",
    Team == "Cardinals" ~ "cardinals.png",
    Team == "Seahawks" ~ "seahawks.png",
    Team == "Rams" ~ "rams.png",
    Team == "Vikings" ~ "vikings.png",
    Team == "Packers" ~ "packers1.png",
    Team == "Lions" ~ "lions.png",
    Team == "Bears" ~ "bears.png",
    Team == "Falcons" ~ "falcons.png",
    Team == "Saints" ~ "saints.png",
    Team == "Panthers" ~ "panthers.png",
    Team == "Buccaneers" ~ "bucs.png"
  ))

resize_images <- function(image_paths, width = 50, height = 50) {
  sapply(image_paths, function(img) {
    image <- image_read(img)
    image <- image_resize(image, paste0(width, "x", height))
    image_write(image, img)
  })
}

resize_images(WeekRanks$Image)

WeekRanks <- WeekRanks %>%
  mutate(Rating = round(Rating, 1))

# Define UI for application that draws a histogram
ui <- fluidPage(

  theme = shinythemes::shinytheme("flatly"),
  
  # Application title
  titlePanel(" "),
  
  navbarPage("Navigation",
             
    tabPanel("Chart",
  
      fluidRow(
        column(12, plotOutput("rank"))
      ),
  
      # Create input panels below the plot
      fluidRow(
        column(1),
        column(5, selectizeInput("team", "Select Team to Highlight:", 
                                 choices = WeekRanks$Team, multiple = TRUE, selected = NULL)),
        column(1),
        column(6, selectInput("type", "Show Rank or Rating", choices = c("Rank", "Rating"),selected = "Rank"))
      )
    ),
    
    tabPanel("Table",
             
             DTOutput("accidentTable")
    ),
    
    tabPanel("Description",
             
             verbatimTextOutput("textbox")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$rank <- renderPlot({
      
      switch(input$type,
             
             # by rank
             "Rank" = ggplot(WeekRanks, aes(x=Week, y=Rank, group=Team, color=Team)) +
                          geom_line(aes(size = ifelse(Team %in% input$team, 1.5, 0.3))) +
                          scale_size_identity() +
                          geom_image(data = WeekRanks[WeekRanks$Week == 13, ], aes(image = Image), size = 0.04) +
                          scale_y_reverse(
                            breaks = c(32, 24, 16, 8, 1)  # Specify which values to label
                          ) +
                          theme_minimal() +
                          scale_color_manual(values = custom_colors) +
                          ggtitle("Rank by Week") +
                          theme(
                            legend.position = "none",
                            plot.title = element_text(hjust = 0.5, size = 18, family = "mono", face = 'bold'),
                            axis.title = element_text(size = 12, family = "mono", face = 'bold')
                          ) +
                          labs(x="Week", y="Rank"),
             
             # by rating
             "Rating" = ggplot(WeekRanks, aes(x=Week, y=Rating, group=Team, color=Team)) +
                          geom_line(aes(size = ifelse(Team %in% input$team, 1.5, 0.3))) +
                          scale_size_identity() +
                          geom_image(data = WeekRanks[WeekRanks$Week == 13, ], aes(image = Image), size = 0.04) +
                          theme_minimal() +
                          scale_color_manual(values = custom_colors) +
                          ggtitle("Rating by Week") +
                          theme(
                            legend.position = "none",
                            plot.title = element_text(hjust = 0.5, size = 18, family = "mono", face = 'bold'),
                            axis.title = element_text(size = 12, family = "mono", face = 'bold')
                          ) +
                          labs(x="Week", y="Rating"),
      )
      
      
    })
    
    output$accidentTable <- renderDT({
      
      wide <- WeekRanks %>%
        select(Team, Week, input$type) %>%
        pivot_wider(names_from = Week, values_from = input$type, names_prefix = "Week ")
      wide <- wide %>%
        select_if(~ !any(is.na(.)))
      
      datatable(wide)
    })
    
    output$textbox <- renderText({
      "My NFL Performance Ratings:
The rating for each team is calculated by averaging the team's point differential and the average point differential of its opponents.
For example, as of Week 12, the 49ers have a point differential of 0, and their opponents have an average point differential of -2. This results in a rating of -1 for the 49ers.
Thus, the worse an opponent is, the more a team needs to win by to prevent its rating from dropping.
Conversely, if a bad team performs well against a good team, the bad team's rating will improve.
For instance, during Week 12, the Chiefs narrowly defeated the lowest-rated team, the Panthers, by only 3 points. This caused the Chiefs to drop from 3rd to 8th in the rankings.

Pros and Cons of the my NFL Performance Ratings:
  Pros:
    1. Does not rely on just wins and losses.
    2. Accounts for point differentials (how much teams win or lose by).
    3. Considers the difficulty of opponents.
    4. Eliminates recency bias (not always good).
    5. Relies on data, not subjective opinions.
  Cons:
    1. Doesn't account for injuries: 
        Example I: In Week 12, the Packers beat the average-rated 49ers by 28 points. However, the 49ers were without their star QB and DE. This led to the Packers climbing eight spots in the rankings.
        Example II: The Dolphins have performed better with Tua healthy, but their poor performances when he was injured still negatively affect their rating. As a result, teams that lose to the Dolphins are penalized more than they perhaps should be.
    2. Garbage-time points can hurt or help a team's rating
    3. No recency bias: While eliminating recency bias is often a strength, it can be a drawback as the system does not account for teams that are currently \"hot\" or \"cold\"."
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
