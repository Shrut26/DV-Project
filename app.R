

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(igraph)
library(leaflet)
library(maps)

tracks <- read.csv("tracks1.csv")
artists <- read.csv("artists.csv")
artists1 <- read.csv("artists1.csv")
top_spotify_songs <- read.csv("universal_top_spotify_songs.csv")

# Define UI
ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "Spotify Songs Analysis"),
  skin = "green",
  
  # Sidebar with tabs
  dashboardSidebar(
    sidebarMenu(
      menuItem(" Overview", tabName = "tab1"),
      menuItem(" Artists", tabName = "tab2"),
      menuItem(" Songs", tabName = "tab3"),
      menuItem(" Top Songs", tabName = "tab4")
    )
  ),
  
  # Body content
  dashboardBody(
    # Tabs content
    tabItems(
      # Tab 1 content
      tabItem(tabName = "tab1",
              fluidRow(
                div(class = "col-sm-3",
                    box(width = 12, height = 140, solidHeader = TRUE,
                        div(class = "row",
                            div(class = "col-xs-3 icon",
                                div(class = "fa fa-list-alt fa-3x")
                            ),
                            div(class = "col-xs-9",
                                div(class = "title", "Total Songs", style = "font-size: 24px;"),
                                div(class = "value",style = "font-size: 36px;",
                                    textOutput("total_songs")
                                )
                            )
                        )
                    )
                ),
                div(class = "col-sm-3",
                    box(width = 12, height = 140,solidHeader = TRUE,
                        div(class = "row",
                            div(class = "col-xs-3 icon",
                                div(class = "fa fa-users fa-3x")
                            ),
                            div(class = "col-xs-9",
                                div(class = "title", "Total Artists", style = "font-size: 24px;"),
                                div(class = "value",style = "font-size: 36px;",
                                    textOutput("total_artists")
                                )
                            )
                        )
                    )
                ),
                div(class = "col-sm-3",
                    box(width = 12, solidHeader = TRUE,
                        div(class = "row",
                            div(class = "col-xs-3 icon",
                                div(class = "fa fa-star fa-3x")
                            ),
                            div(class = "col-xs-9",
                                div(class = "title", "Average Popularity", style = "font-size: 24px;"),
                                div(class = "value", style = "font-size: 36px;",
                                    textOutput("average_popularity")
                                )
                            )
                        )
                    )
                ),
                div(class = "col-sm-3",
                    box(width = 12, solidHeader = TRUE,
                        div(class = "row",
                            div(class = "col-xs-3 icon",
                                div(class = "fa fa-clock-o fa-3x")
                            ),
                            div(class = "col-xs-9",
                                div(class = "title", "Average Duration (minutes)", style = "font-size: 24px;"),
                                div(class = "value", style = "font-size: 36px;",
                                    textOutput("average_duration")
                                )
                            )
                        )
                    )
                )
              ),
              fluidRow(
                # Histogram of song durations
                box(title = "Distribution of Song Durations", solidHeader = TRUE,
                    plotOutput("duration_histogram")
                ),
                box(title = "Average Popularity Over Time", solidHeader = TRUE,
                    plotOutput("popularity_over_time")
                ),
                # Other content of Tab 1 (e.g., total songs, total artists, etc.)
              )
      ),
      # Tab 2 content
      tabItem(tabName = "tab2",
              fluidRow(

                div(class = "col-sm-12",
                div(class = "col-sm-2",
                    textInput("artist_name", "Enter Artist's Name:")
                ),
                div(class = "col-sm-2",
                    box(width = 12, height = 70,solidHeader = TRUE,
                        div(class = "row",
                            div(class = "col-xs-9",
                                # div(class = "title", "No. of followers", style = "font-size: 24px;"),
                                div(class = "value",style = "font-size: 16px;",
                                    textOutput("followers_info")
                                )
                            )
                        )
                    )
                ),
                div(class = "col-sm-2",
                    box(width = 12, height = 70, solidHeader = TRUE,
                        div(class = "row",
                            div(class = "col-xs-9",
                                div(class = "value", style = "font-size: 16px;",
                                    textOutput("popularity_info")
                                )
                            )
                        )
                    )
                ),
                div(class = "col-sm-6",
                    box(width = 12, height = 70, solidHeader = TRUE,
                        div(class = "row",
                            div(class = "col-xs-9",
                                div(class = "value", style = "font-size: 16px;",
                                    textOutput("genres_info")
                                )
                            )
                        )
                    )
                )

                # div(class = "col-sm-12",
                # div(class = "row",
                #     div(class = "col-sm-4",
                #         textInput("artist_name", "Enter Artist's Name:")
                #     )
                # ),
                # div(class = "row",
                #     div(class = "col-sm-4",
                #         textOutput("followers_info")
                #     ),
                #     div(class = "col-sm-4",
                #         textOutput("popularity_info")
                #     ),
                #     div(class = "col-sm-4",
                #         textOutput("genres_info")
                #     )
                # )
          ),
                # box(title = "Information for Artist", solidHeader = TRUE,
                #     textInput("artist_name", "Enter Artist's Name:"),
                #     textOutput("followers_info"),
                #     textOutput("popularity_info"),
                #     textOutput("genres_info")
                # ),
                box(title = "Total Number of Songs by Artist (top 10)", solidHeader = TRUE,
                    plotOutput("artist_song_counts_plot")
                ),
                box(title = "Number of Artists by Key", solidHeader = TRUE,
                    plotOutput("key_artist_counts_plot")
                ),
                # box(title = "Top 10 Artists by Followers", solidHeader = TRUE,
                #     plotOutput("top_artists_plot")
                # ),
                box(title = "Top 10 Artists", solidHeader = TRUE,
                    selectInput("metric_selector", "Select Metric:",
                                choices = c("followers", "popularity")),
                    plotOutput("top_artists_plot")
                ),
                box(title = "Distribution of Artists Among Top 10 Genres", solidHeader = TRUE,
                    plotOutput("pie_chart")
                )
              )
      ),
      # Tab 3 content
      tabItem(tabName = "tab3",
              fluidRow(
                box(title = "Average measures of songs for each key", solidHeader = TRUE,
                    selectInput("y_variable", "Select variable for Y-axis:",
                                choices = c("speechiness", "danceability", "acousticness")),
                    plotOutput("box_plot")
                ),
                box(title = "Popularity of keys over year", solidHeader = TRUE,
                    sliderInput("year_range", "Select Year Range:",
                                min = min(tracks$year), max = max(tracks$year),
                                value = c(min(tracks$year), max(tracks$year)),
                                step = 1),
                    checkboxGroupInput("keys", "Select Keys:",
                                       choices = c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"),  # Display all keys as options
                                       inline = TRUE),
                    
                    plotOutput("key_popularity")
                ),
              )
      ),
      # Tab 4 content
      tabItem(tabName = "tab4",
              fluidRow(
                box(title = "Top 10 Songs on Selected Date and Country", solidHeader = TRUE,
                    selectInput("month_selector", "Select Month:", choices = formatC(1:12, width = 2, flag = "0")),
                    selectInput("date_selector", "Select Date:", choices = formatC(1:31, width = 2, flag = "0")),
                    selectInput("year_selector", "Select Year:", choices = c(2023, 2024)),
                    selectInput("country_selector", "Select Country:", choices = c("ZA", "VN", "VE", "UY", "US", "UA", "TW", "TR", "TH", "SV", "SK", "SG", "SE", "SA", "RO", "PY", "PT", "PH", "PE", "PA", "NZ", "NO", "NL", "NI", "NG", "MY", "MX", "MA", "LV", "LU", "LT", "KZ", "KR", "JP", "IT", "IS", "IN", "IL", "IE", "ID", "HU", "HN", "HK", "GR", "GB", "FR", "FI", "ES", "EG", "EE", "EC", "DO", "DK", "DE", "CZ", "CR", "CO", "CL", "CH", "CA", "BY", "BR", "BO", "BG", "BE", "AU", "AT")
),
                    plotOutput("top_songs_plot")
                ),
                box(title = "Music paramters V/s Popularity", solidHeader = TRUE,
                    sliderInput("year_range1", "Select Year Range:",
                                min = min(tracks$year), max = max(tracks$year),
                                value = c(min(tracks$year), max(tracks$year)),
                                step = 1),
                    selectInput("param", "Choose a variable to plot:",
                                choices = c("Danceability", "Speechiness")),
                    checkboxInput("explicit", "Include explicit tracks", TRUE),
                    plotOutput("trackPlot")
                    
                    )
              )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {

  total_songs <- nrow(tracks)
  total_artists <- nrow(artists)
  average_popularity <- mean(tracks$popularity, na.rm = TRUE)
  average_duration <- mean(tracks$duration_min, na.rm = TRUE)
  
  
  output$total_songs <- renderText({
    paste("", total_songs)
  })
  
  output$total_artists <- renderText({
    paste("", total_artists)
  })
  
  output$average_popularity <- renderText({
    paste("", round(average_popularity, 2))
  })
  
  output$average_duration <- renderText({
    paste("", round(average_duration, 2))
  })
  
  output$duration_histogram <- renderPlot({
    ggplot(tracks, aes(x = duration_min)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      coord_cartesian(xlim = c(0, 10)) +
      labs(title = "",
           x = "Duration (minutes)",
           y = "Frequency")
  })
  
  output$popularity_over_time <- renderPlot({
    ggplot(tracks, aes(x = year, y = popularity)) +
      stat_summary(fun = "mean", geom = "line", color = "darkblue") +
      labs(title = "Average Popularity Over Time",
           x = "Year",
           y = "Average Popularity") +
      scale_x_continuous(breaks = seq(min(tracks$year), max(tracks$year), by = 10))
  })
  
  artist_data <- reactive({
    req(input$artist_name)
    filter(artists1, grepl(input$artist_name, name, ignore.case = TRUE))
  })
  
  # Render followers information
  output$followers_info <- renderText({
    artist <- artist_data()
    paste("Followers:", artist$followers)
  })
  
  # Render popularity information
  output$popularity_info <- renderText({
    artist <- artist_data()
    paste("Popularity:", artist$popularity)
  })
  
  # Render genres information
  output$genres_info <- renderText({
    artist <- artist_data()
    cleaned_genres <- gsub("[c()\\\"']", "", artist$genres)
    # print(artist$genres)
    # print(cleaned_genres)
    paste("Genres:", paste(cleaned_genres, collapse = ", "))
})

  
  tracks_unnested <- tracks %>%
    separate_rows(artists, sep = ", ") %>%
    mutate(artists = trimws(artists))  # Remove leading/trailing whitespaces
  
  # Count the number of songs for each artist
  artist_song_counts <- tracks_unnested %>%
    group_by(artists) %>%
    summarise(song_count = n())
  
  # Get the top 10 artists with the highest counts
  top_10_artists <- artist_song_counts %>%
    top_n(10, song_count)
  
  # Sort the top_10_artists dataframe by song_count in descending order
  top_10_artists <- top_10_artists[order(-top_10_artists$song_count), ]
  top_10_artists$artists <- gsub("\\[|\\]", "", top_10_artists$artists)
  
  # Render the vertical bar chart of number of songs by artist
  output$artist_song_counts_plot <- renderPlot({
    ggplot(top_10_artists, aes(x = song_count, y = reorder(artists, song_count))) +
      geom_bar(stat = "identity", fill = "skyblue") +
      geom_text(aes(label = song_count), vjust = 0, color = "black") +
      labs(title = "",
           x = "Number of Songs",
           y = "Artist")
  })
  
  key_artist_counts <- tracks_unnested %>%
    group_by(key) %>%
    summarise(artist_count = n_distinct(artists))
  
  # Map numeric key values to key names
  key_names <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  key_artist_counts$key <- key_names[key_artist_counts$key + 1]  # Adjusting for 0-based indexing
  
  # Render the bar chart of number of artists by key
  output$key_artist_counts_plot <- renderPlot({
    ggplot(key_artist_counts, aes(x = key, y = artist_count)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Number of Artists by Key",
           x = "Key",
           y = "Number of Artists")
  })
  
  # top_artists <- artists %>%
  #   arrange(desc(followers)) %>%
  #   head(10)
  
  # # Render the bar chart of top 10 artists by followers
  # output$top_artists_plot <- renderPlot({
  #   ggplot(top_artists, aes(x = reorder(name, followers), y = followers)) +
  #     geom_bar(stat = "identity", fill = "skyblue") +
  #     geom_text(aes(label = followers), vjust = 0, color = "black") +
  #     labs(title = "Top 10 Artists by Followers",
  #          x = "Artist",
  #          y = "Number of Followers") + coord_flip() +
  #     theme(axis.text.x = element_blank())
  # })

  observe({
    top_artists <- artists %>%
      arrange(desc(!!sym(input$metric_selector))) %>%
      head(10)
    # print(input$metric_selector)
    # print(top_artists)
    
    output$top_artists_plot <- renderPlot({
      ggplot(top_artists, aes(x = reorder(name, !!sym(input$metric_selector)), y = !!sym(input$metric_selector))) +
        geom_bar(stat = "identity", fill = "skyblue") +
        geom_text(aes(label = !!sym(input$metric_selector)), vjust = 0, color = "black") +
        labs(title = paste("Top 10 Artists by", input$metric_selector),
             x = "Artist",
             y = input$metric_selector) + coord_flip() +
        theme(axis.text.x = element_blank())
    })
  })
  
  artists1$genres <- strsplit(gsub("[\\[\\]\\' ]", "", artists1$genres), ",")
  
  # Explode the 'genres' lists into individual rows
  artists_exploded <- tidyr::unnest(artists1, genres)
  
  # Filter out rows with empty genre
  artists_exploded <- artists_exploded[artists_exploded$genres != "", ]
  
  # Extract and count genres
  genre_counts <- table(artists_exploded$genres)
  
  # Convert to data frame
  genre_df <- data.frame(genre = names(genre_counts),
                         count = as.numeric(genre_counts),
                         stringsAsFactors = FALSE)
  
  # Sort by count in descending order and select top 10 genres
  top_10_genres <- head(genre_df[order(-genre_df$count), ], 10)
  top_10_genres$genre <- gsub("[^[:alnum:]]", "", top_10_genres$genre)
  
  # Create a pie chart
  output$pie_chart <- renderPlot({
    pie(top_10_genres$count, labels = paste(top_10_genres$genre, ": ", top_10_genres$count), main = "Distribution of Artists Among Top 10 Genres")
  })
  
  output$box_plot <- renderPlot({
    # Convert key to categorical variable
    tracks$key <- factor(tracks$key, levels = 0:11, labels = c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"))
    
    ggplot(tracks, aes(x = key, y = !!sym(input$y_variable))) +
      geom_boxplot(fill = "skyblue", color = "blue") +
      labs(title = "Average", input$y_variable,  "by Key",
           x = "Key",
           y = input$y_variable) +
      theme_minimal()
  })
  
  
  
  output$key_popularity <- renderPlot({
    tracks$key <- factor(tracks$key, levels = 0:11, labels = c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"))
    
    # Manually specify colors for each key
    key_colors <- c("C" = "red", "C#" = "blue", "D" = "green", "D#" = "orange", "E" = "purple", 
                    "F" = "yellow", "F#" = "cyan", "G" = "magenta", "G#" = "brown", "A" = "pink", 
                    "A#" = "darkgreen", "B" = "darkblue")
    
    avg_popularity <- tracks %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
      group_by(year, key) %>%
      summarise(avg_popularity = mean(popularity)) %>%
      filter(key %in% input$keys)
    
    ggplot(avg_popularity, aes(x = year, y = avg_popularity, color = key, group = key)) +
      geom_line() +
      scale_color_manual(values = key_colors) +  # Use manually specified colors
      labs(title = "Popularity of Different Keys Over the Years",
           x = "Year",
           y = "Average Popularity",
           color = "Key") +
      theme_minimal()
  })
  
  output$top_songs_plot <- renderPlot({
    # Convert input date to a suitable format
    selected_date <- paste(input$year_selector, input$month_selector, input$date_selector, sep = "-")
    
    # Filter the dataset based on selected date and country
    filtered_data <- top_spotify_songs %>%
      filter(snapshot_date == selected_date,
             country == input$country_selector) %>%
      arrange(daily_rank) %>%
      head(10)  # Select top 10 songs
      print(filtered_data)

    filtered_data$name <- substr(filtered_data$name, 1, 50)
      # Truncate long song names

    # Plot the top 10 songs on a vertical bar chart
    ggplot(filtered_data, aes(x = reorder(name,-daily_rank), y = popularity)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      geom_text(aes(label = popularity), vjust = 0, color = "black") +  # Adjust text aesthetics
      labs(title = "Top 10 Songs",
           x = "Song Name",
           y = "Popularity") + coord_flip() +
        theme(axis.text.x = element_blank())
  })
  
  filteredTracks <- reactive({
    tracks_filtered <- tracks %>%
      filter(year >= input$year_range1[1],year <= input$year_range1[2])
    
    if(input$explicit) {
      tracks_filtered
    } else {
      filter(tracks_filtered, explicit == 0)
    }
    
  })
  
  
  output$trackPlot <- renderPlot({
    x <- switch(input$param,
                "Danceability" = filteredTracks()$danceability,
                "Speechiness" = filteredTracks()$speechiness)
    
    y <- filteredTracks()$popularity
    
    ggplot(data = filteredTracks(), aes(x = x, y = y, color = factor(explicit))) +
      geom_point() +
      labs(title = paste(input$param, "vs Popularity"),
           x = input$param, y = "Popularity") +
      theme_minimal()
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
