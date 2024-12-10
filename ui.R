ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #BF5700; /* Burnt orange background */
        color: #FFFFFF; /* White text */
        font-family: 'Helvetica', sans-serif; /* Clean font */
      }
      .container-fluid {
        background-color: #FFFFFF; /* White card background */
        color: #333333; /* Neutral text for readability */
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.2);
      }
      .btn {
        background-color: #BF5700; /* Burnt orange buttons */
        color: #FFFFFF;
        border: none;
        border-radius: 5px;
        margin-bottom: 10px; /* Add space between buttons */
      }
      .btn:hover {
        background-color: #A94400; /* Slightly darker burnt orange on hover */
        color: #FFFFFF;
      }
      .btn:active {
        background-color: #8A3700; /* Even darker burnt orange on click */
      }
      h1, h2, h3, h4, h5, h6 {
        font-weight: bold;
      }
      h1 {
        color: #000000; /* Black text for the title */
      }
      .form-control {
        border-radius: 5px;
        border: 1px solid #CCCCCC; /* Subtle border for inputs */
      }
      .selectize-input {
        background-color: #FFFFFF; /* White dropdown */
        color: #333333; /* Neutral text */
      }
      .group-box {
        background-color: #FFFFFF; /* White background for group boxes */
        border: 2px solid #BF5700; /* Burnt orange border */
        border-radius: 10px;
        padding: 15px;
        margin-bottom: 20px;
        color: #333333; /* Neutral text for readability */
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.2);
      }
      .output-box {
        background-color: #FFFFFF; /* White background for output */
        border: 2px solid #BF5700; /* Burnt orange border */
        border-radius: 10px;
        padding: 15px;
        color: #333333; /* Neutral text */
        font-family: 'Courier New', monospace; /* Console-like font for output */
      }
      .table {
        width: 100%;
        margin-top: 20px;
        border-collapse: collapse;
      }
      .table th, .table td {
        padding: 10px;
        border: 1px solid #ddd;
      }
      .table th {
        background-color: #BF5700;
        color: white;
      }
    "))
  ),
  
  titlePanel(
    h1("A Linear Mixed Effects Model for Evaluating Synthetic Gene Circuits", align = "center")
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #FFFFFF; border-radius: 10px; padding: 15px;",
      
      # Inputs for number of samples and logical operation
      selectInput("logic", "Choose Logical Operation", 
                  choices = c("OR", "NOR", "AND", "NAND","XOR","XNOR","IMPLY", "NIMPLY")),
      numericInput("n_samples", "Number of Samples per Group", value = 5, min = 1),
      actionButton("submit", "Generate Data Input Table", class = "btn"),
      actionButton("run_analysis", "Run Analysis", class = "btn")
    ),
    
    mainPanel(
      style = "background-color: #FFFFFF; border-radius: 10px; padding: 15px;",
      
      # Display dynamic table for results input
      uiOutput("dynamic_table"),
      br(),
      
      # Display static table after analysis
      tableOutput("display_table"),
      
      # Output box for the statistical analysis results
      tags$div(
        class = "output-box",
        verbatimTextOutput("analysis_results")
      ),
      
      # Cohesiveness score display
      tags$div(
        class = "output-box",
        verbatimTextOutput("cohesiveness_score")
      ),
      
      # Bar graph output
      plotOutput("bar_plot", height = "400px"),
      
      # New plot for cohesiveness score vs dynamic range
      plotOutput("cohesiveness_vs_dynamic", height = "400px")
    )
  )
)
