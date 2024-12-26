ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #1E90FF; /* Dodgerblue background */
        color: #FFFFFF; /* White text */
        font-family: 'Helvetica', sans-serif; /* Clean font */
      }
      .container-fluid {
        background-color: #F0F0F0; /* Light gray card background */
        color: #333333; /* Dark gray text for readability */
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.2);
      }
      .btn {
        background-color: #1E90FF; /* Dodgerblue buttons */
        color: #FFFFFF;
        border: none;
        border-radius: 5px;
        margin-bottom: 10px; /* Add space between buttons */
      }
      .btn:hover {
        background-color: #4682B4; /* Steelblue on hover */
        color: #FFFFFF;
      }
      .btn:active {
        background-color: #5B8FB9; /* Slightly darker blue on click */
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
        color: #333333; /* Dark gray text */
      }
      .group-box {
        background-color: #F0F0F0; /* Light gray background for group boxes */
        border: 2px solid #1E90FF; /* Dodgerblue border */
        border-radius: 10px;
        padding: 15px;
        margin-bottom: 20px;
        color: #333333; /* Dark gray text for readability */
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.2);
      }
      .output-box {
        background-color: #F0F0F0; /* Light gray background for output */
        border: 2px solid #1E90FF; /* Dodgerblue border */
        border-radius: 10px;
        padding: 15px;
        color: #333333; /* Dark gray text */
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
        background-color: #1E90FF;
        color: white;
      }
      .citation {
        font-size: 12px;
        color: #333333;
        text-align: center;
        margin-top: 30px;
      }
    "))
  ),
  
  titlePanel(
    div(
      h1(
        "A Linear Mixed Effects Model for Evaluating Synthetic Gene Circuits", 
        align = "center"
      ),
      tags$p(
        style = "text-align: center; font-size: 16px; color: #000000; margin-top: 10px; font-weight: normal;",
        HTML(
          "Gina Partipilo*, Sarah M. Coleman*, Yang Gao, Ismar E. Miniel Mahfoud, Claus O. Wilke, 
        <u>Hal S. Alper</u>, <u>Benjamin K. Keitz</u>"
        )
      ),
      tags$p(
        style = "text-align: center; font-size: 12px; color: #000000; margin-top: 5px;",
        "* Denotes co-author contribution"
      ),
      tags$p(
        style = "text-align: center; font-size: 12px; color: #000000; margin-top: -5px;",
        "Corresponding authors underlined"
      ),
      div(
        style = "
        background-color: #FFFFFF; 
        color: #333333; 
        border: 2px solid #1E90FF; 
        border-radius: 10px; 
        padding: 10px; 
        margin-top: 20px; 
        width: 60%; 
        margin-left: auto; 
        margin-right: auto;
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.2);",
        tags$p(
          style = "text-align: center; font-size: 14px; font-weight: normal; margin: 0;",
          "Select your logic type and number of replicates used. Generate the data input table and enter your experimental values. Run analysis to run the linear mixed effects model, graph your output, and visualize your gate's performance in relation to 144 others."
        )
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #FFFFFF; border-radius: 10px; padding: 15px;",
      
      # Inputs for number of samples and logical operation
      selectInput("logic", "Choose Logical Operation", 
                  choices = c("OR", "NOR", "AND", "NAND","XOR","XNOR","IMPLY", "NIMPLY")),
      numericInput("n_samples", "Number of Samples per Group", value = 5, min = 1),
      actionButton("submit", "Generate Data Input Table", class = "btn")
    ),
    
    mainPanel(
      style = "background-color: #FFFFFF; border-radius: 10px; padding: 15px;",
      
      # Display dynamic table for results input
      uiOutput("dynamic_table"),
      br(),
      
      # Move "Run Analysis" button beneath the data entry table
      actionButton("run_analysis", "Run Analysis", class = "btn"),
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
      plotOutput("cohesiveness_vs_dynamic", height = "400px"),
      
    )
  ),
  
  # Citation at the bottom of the page
  tags$div(
    class = "citation",
    HTML("Bates, D. M. Lme4: Mixed-Effects Modeling with R. 2010.")
  )
)
