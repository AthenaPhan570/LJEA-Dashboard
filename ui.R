#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

    # Application title

library(shiny)
library(shinydashboard)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "LJEA Dashboard", tags$li(class = "dropdown",
                                                    tags$style(".main-header {max-height: 50px}")
  )),
  # Setup Dashboard Sidebar.
  # Found icon codes, (https://rstudio.github.io/shinydashboard/appear
  #ance.html#icons)
  # Aim: Setting up left side bar for each dataset w/ labels to be used in
  #server code
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("About", tabName = "about", icon = icon("info-circle")),
                menuItem("Biological Data", tabName = "bio", icon = icon("dna")),
                menuItem("Habitat Data", tabName = "habitat", icon = icon("tree")),
                menuItem("Chemical Data", tabName = "chemical", icon = icon("flask"))
    ), 
    tags$style(".left-side, .main-sidebar {padding-top: 50px}")
  ),
  
  # Setup Dashboard Body.
  # Aim: Define each tab and what will be shown on each tab (name each tab)
  # Challenges: Formatting the user interface and making sure to keep track of all of the added code as
  # both of us work on the same app on different computers. Solved through our google doc trackers & communication
  
  # Dashboard Body (Setting up the tabs for our 3 dimensions of data and extra pages for later)
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              fluidRow(
                column(12,
                       box(width = 12, title = h1(strong("Welcome to the LJEA Data Visualization Tool")), status = "primary", solidHeader = TRUE,
                           h3(strong("About the Data")),
                           h4("The data presented here is collected by the Lake James Environmental Association’s water quality monitoring program."),
                           p("Additional infromation about their programs can be found on their website:"),
                           actionButton(
                             inputId = "visitWebsiteButton",
                             label = "Go to Website",
                             onclick = "window.open('https://www.ljea.org/', '_blank')" # Opens in a new tab
                           )
                       ))),
              
              fluidRow(
                
                column(6,
                       box(width = 12, title = h3(strong("Biological Data")), status = "primary",
                           h4("What indexes does the LJEA use to measure biological variables?"),
                           p("This tab displays data related to biological indicators of stream health, notably the NCBI index and bioclassification. The NCBI(North Carolina Biotic Index) is a numerical value used to assess water quality based on the macroinvertebrates present in the stream. Generally, lower NCBI scores indicate better stream quality. Bioclassifications are qualitative ratings (like Excellent, Good, Fair, Poor) that summarize overall stream health based on macroinvertebrate data. For the purpose of our app, we have turned these ratings into quantitative values between 1-5.")
                       )
                ),
                
                column(6,
                       box(width = 12, title = h3(strong("Chemical Data")), status = "primary",
                           h4("Where does the LJEA's chemical data come from?"),
                           p("This tab displays data on Lake James collected by the Environmental Quality Institute (EQI), a non-profit institute responsible for collecting chemical data on over 200 lake sites across Western North Carolina. Trained volunteers are responsible for collecting data for the Volunteer Water Information Network (VWIN) each month. The non-profit institute has been operating for over 35 years and conducts 20,000 tests annually, all for the purpose of maintaining regional water quality for North Carolina. ")))
                
              ),
              
              fluidRow(
                column(6,
                       box(width = 12, title = h3(strong("Habitat Assessment Data")), status = "primary",
                           h4("What do each of the categories of the habitat assessment represent?"),
                           p("This tab displays data collected through a habitat assessment form for the mountain or piedmont zones of Western North Carolina."),
                           p(""),
                           p(strong("Habitat Assessment Variables:")),
                           p(strong("channel.modification:"), " Degree to which the natural stream channel has been straightened/altered.
"),
                           p(strong("instream.habitat:"), " Variety of habitat features like logs and rocks"),
                           p(strong("bottom.substrate:"), " Composition of the stream bottom"),
                           p(strong("pool.variety:"), " Number and diversity of pools on the sides."),
                           p(strong("riffle.habitats:"), " Presence of shallow, fast riffles that oxygenate water."),
                           p(strong("bank.stability.and.vegetation:"), "Stability of streambanks and presence of vegetation."),
                           p(strong("light.penetration:"), " Amount of sunlight on the stream surface."),
                           p(strong("riparian.vegetative.zone.width:"), " Width of natural vegetation along the streambanks."),
                           p(strong("total:"), " Overall stream habitat quality score.")
                       )),
                column(6,
                       box(width = 12, status = "primary",
                       tags$img(src = "stream_monitoring_3.max-1280x720.jpg", alt = "Image of stream monitoring", width = "100%")),
                       box(width = 12, status = "primary", solidHeader = TRUE, 
                           h3(strong("Instructions For Use:")),
                           p("To navigate the LJEA data portal plots, refer to the guide below. The guide will explain how to interpret each of the visualizations in the data tabs and help with understanding the biological, habitat, and chemical health of the Lake James watershed.") )) 
                ),
              
              fluidRow(
                column(6,
                       box(width = 12, status = "primary",
                           tags$img(src = "lineplot.png", alt = "Image of Line Plots", width = "100%"))
                       ),
                column(6,
                       box(width = 12, status = "primary",
                           h4(strong("Time-Series Line Plots:")),
                           p("The line plots show how a particular variable changes over time at one or more selected sites. Each point on the plot represents the value of a variable (e.g., pH, NCBI score) recorded during a specific month at a given LJEA site. A line connects these points in chronological order.
"),
                           p(strong("How to read it:")),
                           p("X-axis: Date of sampling (displayed by month and year)"),
                           p("Y-axis: Measured value of the selected variable"),
                           p("Line color: Represents different monitoring sites"),
                           p("A vertical line is drawn in September 2024 to highlight Hurricane Helene’s occurrence — this helps users spot shifts that may be storm-related."),
                           p("Two horizontal lines in the chemical data are drawn to represent the VWIN estimated regional and pristine average for each of the chemicals. The pristine average representing higher quality of water.")
                )
              )),
              fluidRow(
                column(6,
                       box(width = 12, status = "primary",
                           h4(strong("Box & Whisker Plot:")),
                           p("The box plot displays the full distribution of a variable's values across multiple sites. Boxplots summarize the average values (center), variability, and potential outliers in the data for each site.
"),
                           p(strong("How to read it:")),
                           p("Box: Shows the middle 50% of the data (the interquartile range or IQR)"),
                           p("Top line: 3rd Quartile(75th percentile)"),
                           p("Middle Line: The median of the data"),
                           p("Bottom line: 1st Quartile(25th percentile)"),
                           p("Whiskers: Extend to the minimum and maximum non-outlier values"),
                           p("Dots beyond the whiskers: Outliers"),
                           p("Each box represents a site; colors represent different variables"),
                           p(strong("Boxplots provide a quick snapshot of data variability. For instance, if a site shows high variability or many outliers, it may indicate instability in stream conditions or inconsistent sampling."))
                       ) 
                ),
                column(6,
                       box(width = 12, status = "primary",
                           tags$img(src = "boxplot.png", alt = "Image of Box and Whisker Plots", width = "100%"))
                           
                )),
              
              fluidRow(
                column(6,
                       box(width = 12, status = "primary",
                           tags$img(src = "barplot.png", alt = "Image of BarPlots", width = "100%"))
                ),
                column(6,
                       box(width = 12, status = "primary",
                           h4(strong("Bar (Average) Plots:")),
                           p("The bar plot compares the average value of a selected variable across sites. Each bar represents the mean value of a variable at a particular site over time.
"),
                           p(strong("How to read it:")),
                           p("X-axis: Sites"),
                           p("Y-axis: Average value of the selected variable"),
                           p("Error Bars: tbd"),
                           p("Bar height: Indicates the overall magnitude of the variable"),
                           p("Color: Indicates which variable is being displayed"),
                           p("This is a good tool for fast comparison of sites. The error bars also provide an easy to understand visual of the distribution of the data")
                       )
                )),
              fluidRow(
                column(6,
                       box(width = 12, status = "primary",
                           h4(strong("Hurricane Helene's Impact Plot:")),
                           p("These comparison plots evaluate the before vs. after effects of Hurricane Helene on environmental variables. A bar chart comparing the median values of a selected variable at each site, split between two time periods:"),
                           p("Before September 2024 (pre-Hurricane Helene = yellow)"),
                           p("After September 2024 (post-Hurricane Helene = blue)"),
                           p(strong("How to read it:")),
                           p("Each site has two bars (Before and After)"),
                           p("Color distinguishes the two periods"),
                           p("Height represents the median value"),
                           p(strong("This allows users to assess whether and how stream health was impacted by a major disturbance event like Hurricane Helene. A noticeable change in values may suggest storm-related damage or recovery."))
                       ) 
                ),
                column(6,
                       box(width = 12, status = "primary",
                           tags$img(src = "hhplot.png", alt = "Image of Hurricane Helene Comparison Plot", width = "100%"))
                       
                )),
              fluidRow(
                column(6,
                       box(width = 12, status = "primary",
                           tags$img(src = "scattplot.png", alt = "Image of Chemical Scatterplot", width = "100%"))
                ),
                column(6,
                       box(width = 12, status = "primary",
                           h4(strong("Chemical Variable Scatterplots with Regression:")),
                           p("The scatterplot investigates the relationship between two chemical variables (e.g., Nitrate vs. Turbidity). Each point represents a pair of measurements from the same sample date, with one plotted on the x-axis and one on the y-axis.
"),
                           p(strong("How to read it:")),
                           p("X-axis: One selected chemical variable"),
                           p("Y-axis: Another selected chemical variable"),
                           p("Each point: A data pair from a specific sampling time"),
                           p("Regression line: Find correlations and understand possible associated relationships.  "),
                           p("Equation: Defines a line of best fit equation and confirms direction of the relationship"))
                )),
              
              fluidRow(
                column(6,
                       box(width = 12, status = "primary",
                           h4(strong("Correlation Plots:")),
                           p("The correlation plot shows the strength of linear relationships between all pairs of habitat or chemical variables. The plot is a grid (matrix) of correlation coefficients.
"),
                           p(strong("What it shows:")),
                           p("Values close to +1 indicate a strong positive relationship (e.g., both increase together)"),
                           p("Values near -1 suggest a strong negative relationship (e.g., as one increases, the other decreases)"),
                           p("Values around 0 imply no meaningful relationship"),
                           p(strong("How to read it:")),
                           p("Each cell represents the correlation between two variables"),
                           p("Color and number in each cell indicate the strength and direction of the correlation"),
                           p(strong("This helps detect variables that move together and may be redundant or mechanistically linked. For example, if turbidity and TSS (Total Suspended Solids) are highly correlated, it suggests they may be influenced by similar processes like erosion or stormwater runoff."))
                       ) 
                ),
                column(6,
                       box(width = 12, status = "primary",
                           tags$img(src = "corrplot.png", alt = "Image of Correlation Plot", width = "100%"))
                       
                ))
              ),
      
      
      # h1(strong("Welcome to the LJEA Data Visualization Tool")),
      #  h3("About the Data"),
      #   p("(history of data)"),
      #   h4("Biological Data -"),
      # ),
      
      # Bio Tab; formatted with fluid row and defined user inputs
      tabItem(tabName = "bio",
              fluidRow(
                column(3,
                       box(width = 12, title = "Filters", solidHeader = TRUE, status = "primary",
                           selectizeInput("site_name", "Choose One or More Sites to View",
                                          choices = NULL, multiple = TRUE,
                                          options = list(placeholder = "Select 1 or more sites")),
                           selectInput("variable", "Choose Biological Variable:", choices = NULL),
                           selectInput("data_view", "Choose Data Display Type:",
                                       choices = c("All", "Averages", "Hurricane Helene")))
                ),
                
                column(9,
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Biological Data Summary", status = "info", solidHeader = TRUE,
                                    conditionalPanel(condition = "input.data_view == 'All'",
                                                     plotOutput("bioLinePlot"))))
                       ),
                       
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Biological Plot(s)",status = "info", solidHeader = TRUE,
                                    plotOutput("bioPlot"))
                         )),
                       #fluidRow(
                       #  column(6,
                       #         box(width = 12, title = "Summary Table",status = "info", solidHeader = TRUE,
                       #             conditionalPanel(condition = "input.data_view == 'All' || input.data_view == 'Averages'",
                       #                              DT::dataTableOutput("bioSummary")))))
                         ))
      ),
      
      # Habitat tab; formatted (site column title is "name" not "site.name")
      tabItem(tabName = "habitat",
              fluidRow(
                column(3,
                       box(width = 12, title = "Filters", solidHeader = TRUE, status = "primary",
                           selectizeInput("site_name_hab", "Choose One or More Sites to View", choices = NULL, multiple = TRUE,options = list(placeholder = "Select 1 or more sites")),
                           selectInput("variable_hab", "Choose Habitat Variable:", choices = NULL),
                           selectInput("data_view_hab", "Choose Data Display Type:",
                                       choices = c("All", "Averages", "Hurricane Helene")))
                ),
                
                column(9,
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Habitat Data Summary", status = "info", solidHeader = TRUE,
                                    conditionalPanel(condition = "input.data_view_hab == 'All'",
                                                     plotOutput("habLinePlot")))
                         )),
                       
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Habitat Plot(s)", status = "info", solidHeader = TRUE,
                                    plotOutput("habitatPlot"))
                         )),
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Correlation Matrix", status = "info", solidHeader = TRUE,
                                    plotOutput("habCorrPlot"))
                         )),
                       #fluidRow(
                       #  column(6,
                       #         box(width = 12, title = "Summary Table",  status = "info", solidHeader = TRUE,
                        #           conditionalPanel(condition = "input.data_view_hab == 'All' || input.data_view_hab == 'Averages'",
                        #                             DT::dataTableOutput("habitatSummary")))))
                         ))
      ),
      
      # Chemical tab; nothing different from bio
      tabItem(tabName = "chemical",
              fluidRow(
                column(3,
                       box(width = 12, title = "General Filters", solidHeader = TRUE, status = "primary",
                           selectizeInput("site_name_chem", "Choose One or More Sites to View", choices = NULL, multiple = TRUE,options = list(placeholder = "Select 1 or more sites")),
                           selectInput("variable_chem", "Choose Chemical Variable:", choices = NULL),
                           selectInput("data_view_chem", "Choose Data Display Type:",
                                       choices = c("All", "Averages", "Hurricane Helene")),
                           checkboxInput("show_avg", "Show VWIN Averages", value = TRUE)
                       ),
                       box(width = 12, title = "Scatterplot Filters", solidHeader = TRUE, status = "primary",
                           selectInput("x_var_chem", "Choose X Variable for Scatterplot:", choices = NULL),
                           selectInput("y_var_chem", "Choose Y Variable for Scatterplot:", choices = NULL),
                           checkboxInput("add_regression", "Add Linear Regression Line", value = FALSE)
                       ),
                       box(width = 12, title = "Chemical Variable Key", status = "warning", solidHeader = TRUE,
                           p(strong("NH3.N:"), " Ammonia"),
                           p(strong("NO3.N:"), " Nitrate"),
                           p(strong("Total.P:"), " Total Phosphorus"),
                           p(strong("PO4:"), " Orthophosphate"),
                           p(strong("Turb:"), " Turbidity"),
                           p(strong("TSS:"), " Total Suspended Solids"),
                           p(strong("Cond:"), " Conductivity"),
                           p(strong("Alk:"), " Alkalinity"),
                           p(strong("pH:"), " Acidity level")
                       )
                ),
                
                column(9,
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Chemical Data Summary", status = "info", solidHeader = TRUE,
                                    conditionalPanel(condition = "input.data_view_chem == 'All'",
                                                     plotOutput("chemLinePlot")))
                         )),
                       
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Chemical Data Plot(s)",status = "info", solidHeader = TRUE,
                                    plotOutput("chemPlot"))
                         )),
                       
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Scatterplot of Selected Variables", status = "info", solidHeader = TRUE,
                                    plotOutput("chemScatterPlot"))
                         )),
                       
                       fluidRow(
                         column(12,
                                box(width = 12, title = "Correlation Matrix", status = "info", solidHeader = TRUE,
                                    plotOutput("chemCorrPlot"))
                         )),
                       
                       #fluidRow(
                       #  column(12,
                       #         box(width = 12, title = "Summary Table",status = "info", solidHeader = TRUE,
                        #            conditionalPanel(condition = "input.data_view_chem == 'All' || input.data_view_chem == 'Averages'",
                        #                             DT::dataTableOutput("chemSummary")))))
                         ))
      
      )
    )
  )
)

