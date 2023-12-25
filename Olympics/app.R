############### PLEASE WAIT 20-30 SECONDS FOR THE ANIMATION AND PLOTS TO LOAD!!!!!!!!


# importing libraries
library(shiny)
library(tidyverse)
library(dplyr)
library(ggflowchart)
library(shinydashboard)
library(leaflet)
library(maps)
library(leaflet.extras)
library(shinydashboardPlus)
library(ggthemes)
library(gganimate)
library(gifski)

# reading data
olympics_data <- read.csv("olympics_cleaned.csv", stringsAsFactors = FALSE)

# UI SDefinition
ui <- fluidPage( 
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css",
      integrity = "sha384-ABCsM2Kl0P9zQzp5zqkR5iq13bP4PwVcZc1/8+3+o4IH3Z0vaC5Au8NH3Vn5dZlp",
      crossorigin = "anonymous"
    )
  ),
  
  # dashboard layout to organise boxes
  dashboardPage( skin ="midnight",
                 dashboardHeader(title = span("THE NEXT OLYMPIAN", 
                                              style = "text-shadow: 2px 2px 4px #888888; color: white; font-size: 30px; font: Courier New (monospace); font-weight: bold; font-style: italic"), titleWidth = "100%"),
                 dashboardSidebar(disable = FALSE,
                                  width = 300,
                                  box(width = 12, 
                                      p(span("Welcome, Parents/Guardians!", 
                                             style = "color: white; font-size: 22px; font: Courier New (monospace); 
                   font-weight: bold; font-style: italic")),
                                      p(span("Dreaming of Olympic glory for your daughter? \"The Next Olympian\" Dashboard is your ultimate guide. 
                                             Discover the ideal sport for Olympic success based on your age preference, and make informed decisions within minutes!", 
                                             style = "color: white; font-size: 20px; font: Courier New (monospace); 
                    font-style: italic")),
                                      
                                      p(span("What\'s more? Find out how your home country stacks up against the best in the business for the sport of your choosing.  
                                Let's embark on this transformative journey together, paving the way for her triumph! 🏅🌟", 
                                             style = "color: white; font-size: 20px; font: Courier New (monospace); 
                    font-style: italic"))
                                  ),
                                  fluidRow(
                                    div(style = "background-color: #0085C7; height: 1700px; width: 50px; margin: 5px; float: left;"),
                                    div(style = "background-color: #F4C300; height: 1700px; width: 50px; margin: 5px; float: left;"),
                                    div(style = "background-color: #58595B; height: 1700px; width: 50px; margin: 5px; float: left;"),
                                    div(style = "background-color: #FF5800; height: 1700px; width: 50px; margin: 5px; float: left;"),
                                    div(style = "background-color: #009F3D; height: 1700px; width: 50px; margin: 5px; float: left;")
                                  )
                                  
                 ),
                 
                 dashboardBody(
                   
                   # bringing the summary to above the image in position
                   tags$div(
                     style = "width: 100%; text-align: center; position: relative; z-index: 9999; 
                     background: linear-gradient(to right, rgba(0,0,255,0.5), rgba(0,255,0,0.5));",
                     verbatimTextOutput("increaseLikelihood")
                   ),
                   
                   
                   # olympic rings image
                   fluidRow(
                     box(width = 12, imageOutput("home_img"), background = "black", style = "padding: 0px;")
                   ),

                   # animation plot
                   fluidRow(
                     box(width = 12, imageOutput("overviewPlot"), style = "padding: 0px;")
                   ),
                   
                   # instruction plot
                   fluidRow(
                     box(width = 12, div(
                       style = "background-color: gray; color: white; padding: 20px;",
                       p(HTML("<span style='color: black;'>Complete the left section entirely and then come back to the right </span>")), style = "text-align: center; color: white; font-size: 22px; font: Courier New (monospace)")
                   )),
                   
                   # side descriptions
                   fluidRow(
                     box(width = 6, p(HTML("<span style='color: white;'>Step 1: Sports Picks </span>"), 
                                      style = "text-align: center; color: white; font-size: 20px; font: Courier New (monospace); 
        font-weight: bold; font-style: italic"),
                         p("The foundation of success begins with the right sport choice, doesn't it? 
         Choose your preferences below and we'll unveil your best sport options.",
                           style = "text-align: justify; color: white; font-size: 16px; font: Courier New (monospace); 
        font-style: italic"
                         )),
                     box(width = 6, p(HTML("<span style=' color: white;'>Step 2: Country Picks </span>"), 
                                      style = "text-align: center; color: white; font-size: 20px; font: Courier New (monospace); 
        font-weight: bold; font-style: italic"),
                         p("One down, one to go! 
                           Should you stay in your home country or train elsewhere? Tell us your nationality and we'll help you make that decision.",
                           style = "text-align: justify; color: white; font-size: 16px; font: Courier New (monospace); 
        font-style: italic"
                         ))
                   ),
                   
                   
                   # filters
                   fluidRow(
                     box(height = "200px", style = "overflow-y: auto; height: 175px !important;",  width = 3,  style = "padding: 7px;", sliderInput("ageRange", p("She should win between the ages of", 
                                                                                                                                                                  style = "font-style: italic; color: white; font-size: 16px; font: Courier New (monospace)"),
                                                                                                                                                    min = 0,
                                                                                                                                                    max = 100,
                                                                                                                                                  value = c(1,100))),
                     box(height = "200px", style = "overflow-y: auto; height: 175px !important;",width = 3,  style = "padding: 7px;", numericInput("threshold", p("Atleast these many women should have participated (< 300)", 
                                                                                                                                                                  style = "font-style: italic; color: white; font-size: 16px; font: Courier New (monospace)"), value = 100, min = 1, step = 1)), 
                     box(height = "400px", style = "overflow-y: auto; height: 175px !important;", width = 6,  style = "padding: 7px;", selectInput("countryFilter", p("I call this place my home", 
                                                                                                                                                                      style = "text-align: center; font-style: italic; color: white; font-size: 16px; font: Courier New"), 
                                                                                                                                                   choices = unique(olympics_data$Country),
                                                                                                                                                   selected = unique(olympics_data$Country)[1]))
                   ),
                   
                   # Chart titles
                   fluidRow(
                     column(6, 
                            div(
                              style = "border: 5px solid red; background-color: black; color: white; padding: 10px; text-align: center; font: 'Courier New'; font-size: 16px;",
                              "Top Sports by Winning Percentage among Women Participants"
                            )
                     ),
                     column(6, 
                            div(
                              style = "border: 5px solid red; background-color: black; color: white; padding: 10px; text-align: center; font: 'Courier New'; font-size: 16px;",
                              "Home Vs Country with most Olympians for Selected Sport (Click on Leaf for details)"
                            )
                     )
                   ),
                   
                   # flow chart and map
                   fluidRow(
                     box(width = 6, plotOutput("flow"),  style = "padding: 0px;"),
                     box(width = 6, leafletOutput("worldMap"),  style = "padding: 0px;")
                   ),
                   
                   # chart descriptions
                   fluidRow(
                     box(width = 6,
                         p("Take your pick of the sports using the buttons below. Once chosen, scroll up to step 2 (on the right) and discover the country that will provide her the highest chance of winning.",
                           style = "height: 100px !important; text-align: center; color: white; font-size: 16px; font: Courier New (monospace); 
        font-style: italic"
                         )),
                     box(width = 6, 
                         p("The map above shows the count of winners in your home country (red) and in the best (green). 
                           This should help you in making any relocation decisions!",
                           style = "height: 100px !important; text-align: center; color: white; font-size: 16px; font: Courier New (monospace); 
        font-style: italic"
                         ))
                   ),
                   
                   # radio button and city map
                   fluidRow(
                     box(width = 6, uiOutput("radioButtonsOutput"), style = "padding: 0px;"),
                     box(
                       width = 6,
                       h2("Good Luck"),
                       style = "background-color: #4CAF50; color: white; border: 2px solid #388E3C; padding: 0px; border-radius: 10px; text-align: center;"
                     )
                     
                   )
                   
                 )
  )
)



# Server Definition
server <- function(input, output, session) {
  olympics_data <- read.csv("olympics_cleaned.csv", stringsAsFactors = FALSE)
  
  olympics_data <- reactive({
    read.csv("olympics_cleaned.csv", stringsAsFactors = FALSE)
  })
  
  # filtering women winners between these age ranges
  age_fitlered_data <- reactive({
    age_range <- input$ageRange
    filtered <- olympics_data() %>%
      filter(Age >= age_range[1] & Age <= age_range[2] & Medal %in% c("Gold", "Silver", "Bronze")) %>%
      mutate(AgeRange = paste0(age_range[1], "-", age_range[2])) %>%
      group_by(AgeRange, Sport) %>%
      mutate(TotalWinners = n(),
             PercentageWomen = sum(Sex == "F"),
             PercentageMen = sum(Sex == "M"),
             nw = (PercentageWomen/TotalWinners) * 100,
             nm = (PercentageMen/TotalWinners) * 100) %>%
      distinct(AgeRange, Sport,TotalWinners,PercentageWomen, PercentageMen, nw, nm)
    return(filtered)
  })
  
  
  #################### likelihood statement
  
  observe({
    # Find the first and last years in the data
    first_year <- min(olympics_data()$Year, na.rm = TRUE)
    last_year <- max(olympics_data()$Year, na.rm = TRUE)
    
    # Calculate the percentage of women who won medals in the first year
    women_medals_first_year <- olympics_data()[olympics_data()$Sex == "F" & 
                                                 olympics_data()$Year == first_year & 
                                                 olympics_data()$Medal %in% c("Gold", "Silver", "Bronze"), ]
    women_participated_first_year <- olympics_data()[olympics_data()$Sex == "F" & 
                                                       olympics_data()$Year == first_year, ]
    # If there are no women who won medals in the first year, set the percentage to 0
    if (nrow(women_participated_first_year) == 0) {
      percentage_women_medals_first_year <- 0
    } else {
      percentage_women_medals_first_year <- (nrow(women_medals_first_year) / nrow(women_participated_first_year)) * 100
    }
    
    # Calculate the percentage of women who won medals in the last year
    women_medals_last_year <- olympics_data()[olympics_data()$Sex == "F" & 
                                                olympics_data()$Year == last_year & 
                                                olympics_data()$Medal %in% c("Gold", "Silver", "Bronze"), ]
    women_participated_last_year <- olympics_data()[olympics_data()$Sex == "F" & 
                                                      olympics_data()$Year == last_year, ]
    percentage_women_medals_last_year <- (nrow(women_medals_last_year) / nrow(women_participated_last_year)) * 100
    
    # Calculate the percentage change
    likelihood_increase <- round(percentage_women_medals_last_year - percentage_women_medals_first_year)
    
    
    
    output$increaseLikelihood <- renderPrint({
      cat("Women are", likelihood_increase, "times more likely to win in", last_year, "compared to", first_year, " Find out how below!")
    })
    
    ############################## animated overview plot
    
    output$overviewPlot <- renderImage({
      
      # grouping data by gender
      overview <- olympics_data() %>%
        group_by(Year, Sex) %>%
        mutate(TotalWinners = n()) %>%
        select(Year, Sex, TotalWinners)
      
      # animated plot
      p <- overview %>%
        ggplot(aes(x = Year, y = TotalWinners, color = Sex)) +
        geom_line() +
        theme_solarized_2(light = FALSE) +
        theme(
          text = element_text(
            color = "white",
            size = 16,
            family = "Courier New",
            face = "italic"
          ),
          axis.text.x = element_text(color = "white", size = 12),  # Overrides x-axis text formatting
          axis.text.y = element_text(color = "white", size = 12), # Overrides y-axis text formatting
          axis.title.x = element_blank(),   # Overrides axis title formatting
          axis.title.y = element_blank(),   
          plot.title = element_text(color = "white", size = 16, hjust = 0.5, family = "Courier New (monospace)" )
        )+
        transition_reveal(Year) +
        labs(title = "Trends in the number of male and female winners at the Olympics")
      
      # Save the plot as a temporary file
      temp_file <- tempfile(fileext = ".gif")
      anim_save(temp_file, animate(p, renderer = gifski_renderer(), fps = 10, width = 1000, height = 400))
      
      list(src = temp_file, contentType = "image/gif", width = "100%", height = "90%")
      
    }, deleteFile = F)
    
    ############################## image
    
    # printing image from file
    output$home_img <- renderImage({
      
      list(src = "olympicrings.jpg", width = "100%", height = "120%")
      
    }, deleteFile = F)
    
    
    
    #################### flow chart
    
    #filtering data by threshold and age
    output$flow <- renderPlot({
      threshold <- input$threshold
      
      sorted_data <- age_fitlered_data() %>%
        filter(PercentageWomen >= threshold) %>%
        arrange(desc(nw)) %>% 
        select('AgeRange', 'Sport', 'nw') %>%
        head(5)
      
      # generating data for chart
      data <- tibble::tibble(from = rep(paste("Age Range", sorted_data$AgeRange[1], sep = "\n"), each = nrow(sorted_data)),
                             to = paste(sorted_data$Sport, paste("(", round(sorted_data$nw), "%)"), sep = "\n"))
      
      # chart
      ggflowchart(data, x_nudge = 0.50, y_nudge = 0.15, arrow_size = 0.3, fill = "#8dbfa8", text_colour = "#f2e4c1",
                  arrow_colour = "#f2e4c1") + 
                  theme(
                    legend.position = "none",
                    plot.background = element_rect(
                      fill = "#1B1212",
                      color = "#1B1212"
                      
                    ))
      
      
    })
    
    
    
    ####################### radio button
    
    output$radioButtonsOutput <- renderUI({
      
      threshold <- input$threshold
      
      sorted_data <- age_fitlered_data() %>%
        filter(PercentageWomen >= threshold) %>%
        arrange(desc(nw)) %>% 
        select('AgeRange', 'Sport', 'nw')
      head(5)
      
      
      # Get unique sport values from sorted_data
      sport_choices <- unique(sorted_data$Sport)
      
      if (length(sport_choices) == 0) {
        # If there are no sport choices, display a message
        return(tags$p("No sports available. Please select valid age and minimum female participants."))
      } else {
        # Render radio buttons with available sport choices
        radioButtons("sportRadio", span("Your sport preference", style = "text-align: center; important;font-style: italic; color: white; font-size: 16px; font: Courier New (monospace)"), choices = sport_choices, inline = TRUE)
      }
    })
    
    ################## country map
    
    # Function to convert country names to country codes

    # file that contains the latitude and longitude information
    codes <- read.csv("map.csv")
    
    output$worldMap  <- renderLeaflet({
      sport <- input$sportRadio
      selectedCountry <- input$countryFilter
      
      sport_filtered_data <- olympics_data() %>%
        filter(Sport == sport & Medal %in% c("Gold", "Silver", "Bronze") & Sex == 'F') %>%
        group_by(Country) %>%
        mutate(TotalWinners = n()) %>%
        arrange(desc(TotalWinners)) %>%
        distinct(Country, TotalWinners)
      
      # Find row with maximum TotalWinners
      max_total_winner_row <- sport_filtered_data[which.max(sport_filtered_data$TotalWinners), ]
      
      # Find row where Country matches input$countryFilter
      selected_country_row <- sport_filtered_data[sport_filtered_data$Country == selectedCountry, ]
      
      # Check if no row is found, add a new row
      if (nrow(selected_country_row) == 0) {
        selected_country_row <- data.frame(
          Country = selectedCountry,
          TotalWinners = 0
        )
      }
      
      # Create a data frame with two rows (maximum TotalWinners and selected country)
      combined_data <- rbind(max_total_winner_row, selected_country_row)
      
      # Merge with coordinates data
      country_data <- merge(codes, combined_data, by.x = "country", by.y = "Country") %>%
        select("country", "latitude", "longitude", "TotalWinners")

      
      # Create a leaflet map
      leaflet <- leaflet() %>%
        addTiles() %>%
        setView(lng = 0, lat = 20, zoom = 1) %>%  # Set initial view
        addMarkers(data = country_data, lng = ~longitude, lat = ~latitude,
                   popup = ~paste("Country: ", country, "<br>Total Winners: ", TotalWinners),
                   icon = ~icons(
                     iconUrl = ifelse(country_data$country == selectedCountry,
                                      "https://leafletjs.com/examples/custom-icons/leaf-red.png",
                                      "https://leafletjs.com/examples/custom-icons/leaf-green.png"
                     ),
                     iconWidth = 38, iconHeight = 95,
                     iconAnchorX = 22, iconAnchorY = 94,
                     shadowUrl = "https://leafletjs.com/examples/custom-icons/leaf-shadow.png",
                     shadowWidth = 50, shadowHeight = 64,
                     shadowAnchorX = 4, shadowAnchorY = 62
                   ))
      
       leaflet
      
    })
    
    output$check <-renderTable({
      sport <- input$sportRadio
      home <- input$countryFilter
      
      # filtering for sport
      sport_filtered_data <- olympics_data() %>%
        filter(Sport == sport & Medal %in% c("Gold", "Silver", "Bronze") & Sex == 'F') %>%
        group_by(Country) %>%
        mutate(TotalWinners = n())%>%
        arrange(desc(TotalWinners)) %>%
        distinct(Country, TotalWinners)
      
      # Find row with maximum TotalWinners
      max_total_winner_row <- sport_filtered_data[which.max(sport_filtered_data$TotalWinners), ]
      
      # Find row where Country matches input$countryFilter
      selected_country_row <- sport_filtered_data[sport_filtered_data$Country == home, ]
      
      # Check if no row is found, add a new row
      if (nrow(selected_country_row) == 0) {
        selected_country_row <- data.frame(
          Country = home,
          TotalWinners = 0
        )
      }
      
      # Combine the rows with maximum TotalWinners and selected country
      combined_data <- rbind(max_total_winner_row, selected_country_row)
      
      # Merge with coordinates data
      country_data <- merge(codes, combined_data, by.x = "country", by.y = "Country") %>%
        select("country", "latitude", "longitude", "TotalWinners")
      
      country_data
      
    })

  })
}


# Run the application 
shinyApp(ui = ui, server = server)
