library(haven)
library(dplyr)
library(readr)
library(wesanderson)
library(psych)
library(ggplot2)
library(ggcorrplot)
library(tidyr)

data <- read_csv("C:/Users/Solange/Dropbox/REMS/Data 824/Final project/pisa_latam1.csv")

data <- data %>%
  group_by(CNT, CNTSCHID) %>%
  mutate(schescs = weighted.mean(ESCS, W_FSTUWT, na.rm = TRUE)) %>%
  ungroup()


data <- data %>%
  rename(
    Expected_occupation_sts = BSMJ,
    SCHLTYPE1 = SC013Q01TA,
    Expected_occupation = OCOD3,
    GROWTH_MINDSET = GROSAGR,
    CURIOSITY = CURIOAGR
  )

library(shiny)


gender_labels <- c("1" = "Female", "2" = "Male")
data <- data %>%
  mutate(SCHLTYPE1_label = case_when(
    SCHLTYPE == 1 ~ "Private independent",
    SCHLTYPE == 2 ~ "Private Government-dependent",
    SCHLTYPE == 3 ~ "Public",
    TRUE ~ NA_character_
  ))

data <- data %>%
  mutate(Gender = recode(as.character(ST004D01T), !!!gender_labels))

#shiny 
ui <- fluidPage(
  titlePanel("PISA Latin America: Expected Occupation Status and School Socioeconomic Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", 
                  choices = sort(unique(data$CNT)), 
                  selected = "ARG"),
      selectInput("school_type", "Select School Type:",
                  choices = c("All", unique(data$SCHLTYPE1_label)),
                  selected = "All"),
      selectInput("gender", "Select Gender:", 
                  choices = c("All", unique(data$Gender)), 
                  selected = "All"),
      downloadButton("downloadData", "Download Filtered Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Distribution",
                 plotOutput("boxPlot")),
        tabPanel("EOS vs SES",
                 plotOutput("scatterPlot")),
        tabPanel("Correlation Matrix",
                 plotOutput("corrMatrixPlot"))
      )
    )
  )
)
server <- function(input, output, session) {
  
  # Reactive data filtered by inputs
  filtered_data <- reactive({
    df <- data
    if (input$country != "") {
      df <- df %>% filter(CNT == input$country)
    }
    if (input$school_type != "All") {
      df <- df %>% filter(SCHLTYPE1_label == input$school_type)
    }
    if (input$gender != "All") {
      df <- df %>% filter(Gender == input$gender)
    }
    df
  })
  
  # Boxplot of EOS distribution
  output$boxPlot <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes(x = reorder(CNT, Expected_occupation_sts, median, na.rm = TRUE), 
                   y = Expected_occupation_sts)) +
      geom_violin(fill = "lightblue", trim = FALSE) +
      geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
      labs(title = paste("EOS Distribution -", input$country),
           x = "Country",
           y = "Expected Occupational Status") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Scatterplot EOS vs SES with regression line
  output$scatterPlot <- renderPlot({
    df <- filtered_data()
    ggplot(df, aes(x = schescs, y = Expected_occupation_sts)) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "lm", color = "firebrick") +
      labs(title = paste("Expected Occ. Status vs School Socioeconomic Status -", input$country),
           x = "School Average Socioeconomic Status",
           y = "Expected Occupational Status") +
      theme_minimal()
  })
  
  # Correlation plot
  output$corrMatrixPlot <- renderPlot({
    df <- filtered_data()
    numeric_df <- df %>%
      select(where(is.numeric)) %>%
      drop_na()
    
    if (ncol(numeric_df) < 2) {
      plot.new()
      title("Not enough numeric variables to show correlation matrix")
      return()
    }
    
    cor_matrix <- cor(numeric_df, use = "pairwise.complete.obs")
    cor_melt <- as.data.frame(cor_matrix) %>%
      rownames_to_column("Var1") %>%
      pivot_longer(-Var1, names_to = "Var2", values_to = "value")
    
    ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "tomato", high = "steelblue", mid = "white",
                           midpoint = 0, limit = c(-1,1), space = "Lab",
                           name = "Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            panel.grid = element_blank(),
            axis.title = element_blank()) +
      labs(title = paste("Correlation Matrix -", input$country))
  })
  
  
  # Download filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("pisa_data_filtered_", input$country, ".csv", sep = "")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
}
shinyApp(ui, server)
