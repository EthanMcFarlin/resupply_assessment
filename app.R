# Shiny Application for reSupply Dashboard

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(usmap)
library(maps)

# Load data
donation_data <- read_csv("exportedData.csv")

# Preprocess data
donation_data <- donation_data %>%
  mutate(
    donation_date = as.Date(donation_date),
    submitted_at = ymd_hms(submitted_at),
    lastEventTime = ymd_hms(lastEventTime),
    payment_completed = as.factor(payment_completed),
    pickup_address_state = as.factor(pickup_address_state),
    market_name = as.factor(market_name),
    hauler_status = as.factor(hauler_status)
  ) %>%
  filter(!is.na(totalEvents))

# Ensure state abbreviations are converted to full names
state_abbreviations <- data.frame(
  state = state.name,
  abbrev = state.abb,
  stringsAsFactors = FALSE
)

donation_data <- donation_data %>%
  left_join(state_abbreviations, by = c("pickup_address_state" = "abbrev"))

# Create UI
ui <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      body {
        font-family: 'Montserrat', sans-serif;
      }
      .plot-container {
        padding: 16px;
        border-radius: 5px;
      }
      .title-container {
        display: flex;
        align-items: center;
      }
      .logo {
        width: 70px;
        margin-right: 10px;
      }
      /* h1 {
        font-size: 2em;
        margin: 0;
      } */
      .axis-title {
        font-size: 14px;
      }
      .axis-text {
        font-size: 12px;
      }
    "))
  ),
  titlePanel(
    div(class = "title-container",
        img(src = "logo.png", alt = "reSupply Logo", class = "logo")
        # h1("reSupply Dashboard")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Select Chart Type"),
      selectInput("chartType", "Chart Type:",
                  choices = list("Total vs Completed Donations" = "bar",
                                 "Distribution by State" = "map",
                                 "Donation Completion Status" = "pie",
                                 "Number of Events per Donation" = "hist",
                                 "Top Haulers" = "haulers",
                                 "Top Charities" = "charities",
                                 "Completion Percentage Over Time" = "completion_time",
                                 "Top 10 Markets by Completion Percentage" = "completion_market_top",
                                 "Bottom 10 Markets by Completion Percentage" = "completion_market_bottom")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Chart", div(class = "plot-container", plotOutput("plot"))),
        tabPanel("Background", div(class = "plot-container", p("ReSupply is a veteran-founded service that facilitates home donation pickups for individuals who need to donate items like furniture, appliances, and other household goods. They offer expedited pickup options, typically within 24-48 hours, to accommodate busy schedules. The service includes the removal of items from any location within the home, disassembly of items as needed, and they accept donations in any condition. This makes it convenient for donors who are moving, performing cleanouts, or getting new furniture."), 
                                   p("This visualization dashboard explores completion percentage metrics, in an effort to drive increased impact. It is designed by Ethan McFarlin for the data scientist interview assessment.")))
      )
    )
  )
)

# Create Server
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    if(input$chartType == "bar") {
      # Bar Chart for Total vs Completed Donations
      donation_status <- donation_data %>%
        group_by(donation_date, isCompleted) %>%
        summarize(count = n(), .groups = 'drop')
      
      ggplot(donation_status, aes(x = donation_date, y = count, fill = isCompleted)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("Yes" = "#1169B2", "No" = "#E12723")) +
        labs(title = "Total vs Completed reSupply Donations", x = "Date", y = "Number of Donations", fill = "Completed") +
        theme_minimal(base_family = "Montserrat") + 
        theme(
          plot.title = element_text(color = "#1D244B", face = "bold"),
          axis.title = element_text(color = "#1D244B", size = 14),
          axis.text = element_text(color = "#1D244B", size = 12)
        )
      
    } else if(input$chartType == "map") {
      # Choropleth Map for Distribution by State
      donation_by_state <- donation_data %>%
        rename(state = state) %>%
        group_by(state) %>%
        summarize(total_donations = n(), .groups = 'drop')
      
      plot_usmap(data = donation_by_state, values = "total_donations") +
        scale_fill_continuous(low = "white", high = "#1169B2", name = "Total Donations", label = scales::comma) +
        labs(title = "Distribution of reSupply Donations by State") +
        theme(legend.position = "right", plot.title = element_text(color = "#1D244B", face = "bold")) +
        theme_minimal(base_family = "Montserrat") +
        theme(
          plot.title = element_text(color = "#1D244B", face = "bold"),
          axis.title = element_text(color = "#1D244B", size = 14),
          axis.text = element_text(color = "#1D244B", size = 12)
        )
      
    } else if(input$chartType == "pie") {
      # Pie Chart for Donation Completion Status
      completion_status <- donation_data %>%
        group_by(isCompleted) %>%
        summarize(count = n(), .groups = 'drop')
      
      ggplot(completion_status, aes(x = "", y = count, fill = isCompleted)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        scale_fill_manual(values = c("Yes" = "#1169B2", "No" = "#E12723")) +
        labs(title = "reSupply Donation Completion Status", fill = "Completed") +
        theme_minimal(base_family = "Montserrat") + 
        theme(
          plot.title = element_text(color = "#1D244B", face = "bold"),
          legend.title = element_text(color = "#1D244B"),
          legend.text = element_text(color = "#1D244B"),
          axis.title = element_text(color = "#1D244B", size = 14),
          axis.text = element_text(color = "#1D244B", size = 12)
        )
      
    } else if(input$chartType == "hist") {
      # Histogram for Number of Events per Donation
      filtered_data <- donation_data %>% filter(totalEvents <= 30)
      
      ggplot(filtered_data, aes(x = totalEvents)) +
        geom_histogram(binwidth = 1, fill = "#1169B2", color = "white") +
        labs(title = "Number of Events per reSupply Donation (<= 30)", x = "Total Events", y = "Frequency") +
        theme_minimal(base_family = "Montserrat") + 
        theme(
          plot.title = element_text(color = "#1D244B", face = "bold"),
          axis.title = element_text(color = "#1D244B", size = 14),
          axis.text = element_text(color = "#1D244B", size = 12)
        )
      
    } else if(input$chartType == "haulers") {
      # Bar Chart for Top Haulers
      top_haulers <- donation_data %>%
        group_by(assigned_hauler_name) %>%
        summarize(total_donations = n(), .groups = 'drop') %>%
        arrange(desc(total_donations)) %>%
        head(10)
      
      ggplot(top_haulers, aes(x = reorder(assigned_hauler_name, -total_donations), y = total_donations)) +
        geom_bar(stat = "identity", fill = "#1169B2") +
        coord_flip() +
        labs(title = "Top 10 Haulers by Number of reSupply Donations", x = "Hauler Name", y = "Total Donations") +
        theme_minimal(base_family = "Montserrat") + 
        theme(
          plot.title = element_text(color = "#1D244B", face = "bold"),
          axis.title = element_text(color = "#1D244B", size = 14),
          axis.text = element_text(color = "#1D244B", size = 12)
        )
      
    } else if(input$chartType == "charities") {
      # Bar Chart for Top Charities
      top_charities <- donation_data %>%
        group_by(assigned_charity_name) %>%
        summarize(total_donations = n(), .groups = 'drop') %>%
        arrange(desc(total_donations)) %>%
        head(10)
      
      ggplot(top_charities, aes(x = reorder(assigned_charity_name, -total_donations), y = total_donations)) +
        geom_bar(stat = "identity", fill = "#1169B2") +
        coord_flip() +
        labs(title = "Top 10 Charities by Number of reSupply Donations", x = "Charity Name", y = "Total Donations") +
        theme_minimal(base_family = "Montserrat") + 
        theme(
          plot.title = element_text(color = "#1D244B", face = "bold"),
          axis.title = element_text(color = "#1D244B", size = 14),
          axis.text = element_text(color = "#1D244B", size = 12)
        )
      
    } else if(input$chartType == "completion_time") {
      # Line Chart for Completion Percentage Over Time
      completion_data <- donation_data %>%
        group_by(donation_date) %>%
        summarize(
          total_donations = n(),
          completed_donations = sum(hauler_status == 'completed', na.rm = TRUE)
        ) %>%
        mutate(completion_percentage = (completed_donations / total_donations) * 100)
      
      ggplot(completion_data, aes(x = donation_date, y = completion_percentage)) +
        geom_line(color = "#1169B2", size = 1) +
        geom_point(color = "#E12723", size = 2) +
        labs(title = "Completion Percentage Over Time", x = "Date", y = "Completion Percentage (%)") +
        theme_minimal(base_family = "Montserrat") + 
        theme(
          plot.title = element_text(color = "#1D244B", face = "bold"),
          axis.title = element_text(color = "#1D244B", size = 14),
          axis.text = element_text(color = "#1D244B", size = 12)
        )
      
    } else if(input$chartType == "completion_market_top") {
      # Bar Chart for Top 10 Markets by Completion Percentage
      completion_data_market_top <- donation_data %>%
        group_by(market_name) %>%
        summarize(
          total_donations = n(),
          completed_donations = sum(hauler_status == 'completed', na.rm = TRUE)
        ) %>%
        mutate(completion_percentage = (completed_donations / total_donations) * 100) %>%
        arrange(desc(completion_percentage)) %>%
        head(10)
      
      ggplot(completion_data_market_top, aes(x = reorder(market_name, -completion_percentage), y = completion_percentage)) +
        geom_bar(stat = "identity", fill = "#1169B2") +
        coord_flip() +
        labs(title = "Top 10 Markets by Completion Percentage", x = "Market", y = "Completion Percentage (%)") +
        theme_minimal(base_family = "Montserrat") + 
        theme(
          plot.title = element_text(color = "#1D244B", face = "bold"),
          axis.title = element_text(color = "#1D244B", size = 14),
          axis.text = element_text(color = "#1D244B", size = 12)
        )
      
    } else if(input$chartType == "completion_market_bottom") {
      # Bar Chart for Bottom 10 Markets by Completion Percentage
      completion_data_market_bottom <- donation_data %>%
        group_by(market_name) %>%
        summarize(
          total_donations = n(),
          completed_donations = sum(hauler_status == 'completed', na.rm = TRUE)
        ) %>%
        mutate(completion_percentage = (completed_donations / total_donations) * 100) %>%
        filter(completion_percentage > 0) %>%
        arrange(completion_percentage) %>%
        head(10)
      
      ggplot(completion_data_market_bottom, aes(x = reorder(market_name, completion_percentage), y = completion_percentage)) +
        geom_bar(stat = "identity", fill = "#E12723") +
        coord_flip() +
        labs(title = "Bottom 10 Markets by Completion Percentage", x = "Market", y = "Completion Percentage (%)") +
        theme_minimal(base_family = "Montserrat") + 
        theme(
          plot.title = element_text(color = "#1D244B", face = "bold"),
          axis.title = element_text(color = "#1D244B", size = 14),
          axis.text = element_text(color = "#1D244B", size = 12)
        )
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)