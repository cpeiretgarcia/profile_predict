# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))


# Load the necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(fmsb)
library(scales)
library(tibble)
library(ggradar)
#install.packages('brms')
#options(pkgType = "binary")
library(brms)
library(marginaleffects)
library(tidyr)
library(ggdist)
library(collapse)



# Load data efficiently
m_balanced <- readRDS('m_balanced_6clust_cat.rds')
multi_df <- readRDS('multi_df.RData')

# Define the desired order for the income levels and ages
ordered_income_levels <- c("First 10% group", 
                           "Second 10% group", 
                           "Third 10% group", 
                           "Fourth 10% group",
                           "Fifth 10% group", 
                           "Sixth 10% group", 
                           "Seventh 10% group", 
                           "Eighth 10% group",
                           "Ninth 10% group", 
                           "Tenth 10% group", 
                           "Income unknown")
ordered_ages <- 6:99
multi_df$Urbanity <- as.numeric(multi_df$Urbanity)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Predicting mobility behaviours"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("age", "Age:", choices = ordered_ages),
      selectInput("gender", "Gender:", choices = unique(multi_df$gender)),
      selectInput("ethnicity", "Ethnicity:", choices = unique(multi_df$ethnicity)),
      selectInput("occupation", "Occupation:", choices = unique(multi_df$occupation)),
      selectInput("education", "Education:", choices = unique(multi_df$education)),
      selectInput("income", "Income:", choices = ordered_income_levels),
      selectInput("hh_composition", "Household Composition:", choices = unique(multi_df$hh_composition)),
      selectInput("urbanity", "Urban density:", choices = unique(multi_df$urban_category)),
      selectInput("entropy_car", "Car entropy:", choices = unique(multi_df$entropy_car_category)),
      selectInput("entropy_pt", "Public transport entropy:", choices = unique(multi_df$entropy_pt_category)),
      selectInput("access_category", "Accessibility:", choices = unique(multi_df$access_category))
    ),
    
    mainPanel(
      plotOutput("barplot"),
      imageOutput("radar_image")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Render the bar plot (prediction plot)
  output$barplot <- renderPlot({
    
    
    new_data <- data.frame(
      age = as.numeric(input$age),
      gender = input$gender,
      ethnicity = input$ethnicity,
      occupation = input$occupation,
      education = input$education,
      income = input$income,
      hh_composition = input$hh_composition,
      urban_category = input$urbanity,
      entropy_car_category = input$entropy_car,
      entropy_pt_category = input$entropy_pt,
      access_category = input$access_category
    )
    
    pred <- predictions(m_balanced, newdata = new_data, vcov = FALSE) %>%
      as_tibble() %>%
      posterior_draws() %>%
      arrange(estimate) %>%
      mutate(group = replace_na(as.character(group), "5")) %>%
      mutate(group = factor(group))
    
    ggplot(pred, aes(x = draw, y = group, fill = group)) +
      stat_halfeye(normalize = 'xy') +
      labs(title = "Probability of belonging", x = element_blank(), y = element_blank()) +
      theme_minimal(base_family = "Clear Sans", base_size = 16) +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(family = "Clear Sans", face = "bold"),
            axis.title.x = element_text(face = 'bold'),
            axis.title.y = element_text(face = 'bold'),
            strip.text = element_text(family = "Clear Sans", face = "bold",
                                      size = rel(0.75), hjust = 0),
            strip.background = element_rect(fill = "grey90", color = NA),
            axis.text.y = element_blank()) +
      scale_fill_manual(
        values = c("#001219", "#0a9396", "#94d2bd", "#ee9b00", "#bb3e03", "#9b2226"),  # Customize colors
        labels = c('Cluster 1: Cycling enthusiasts',
                   'Cluster 2: Public transport commuters',
                   'Cluster 3: Focused drivers',
                   'Cluster 4: Multiactivity',
                   'Cluster 5: Complex chain carers',
                   'Cluster 6: Leisure-oriented walkers'
        ),  # Customize labels
        name = 'Cluster'
      )
    
  })
  # Render the radar plot image
  # output$radar_image <- renderImage({
  #   list(src = "radar_plots_6clust.png",  
  #        contentType = "image/png",
  #        width = 900,  
  #        height = 500,  
  #        alt = "Cluster Radar Plot")
  # }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)