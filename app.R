library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(arules)
library(DT)
library(shinycssloaders)

# Load and preprocess the data
course_data <- read_excel("survey_right_version.xlsx", sheet = "Sheet1") %>%
  mutate(Recommended_Courses = strsplit(as.character(Recommended_Courses), ", ")) %>%
  unnest(Recommended_Courses) %>%
  mutate(Recommended_Courses = trimws(Recommended_Courses)) %>%
  distinct()

# Custom CSS
custom_css <- "
  .skin-blue .main-header .navbar { background-color: #003366; }
  .skin-blue .main-header .logo { background-color: #003366; color: #FFD700; }
  .skin-blue .main-header .logo:hover { background-color: #003366; }
  .skin-blue .main-sidebar { background-color: #003366; }
  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a { 
    background-color: #FFD700; color: #003366; }
  .skin-blue .main-sidebar .sidebar .sidebar-menu a { color: white; }
  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
    background-color: #FFD700; color: #003366; }
  .box.box-primary { border-top-color: #003366; }
  .btn-primary { background-color: #003366; border-color: #003366; }
  .btn-primary:hover { background-color: #002244; border-color: #002244; }
  .survey-link { 
    background-color: #FFD700;
    color: #003366 !important;
    padding: 10px;
    margin: 10px;
    border-radius: 5px;
    display: block;
    text-align: center;
    font-weight: bold;
  }
  .survey-link:hover {
    background-color: #FFC000;
    text-decoration: none;
  }
"

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(tagList(icon("graduation-cap"), "Course Recommendation System")),
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Recommendations", tabName = "recommend", icon = icon("lightbulb")),
      menuItem("Course Explorer", tabName = "explore", icon = icon("search")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    div(class = "survey-link-container",
        tags$a(href = "https://docs.google.com/forms/d/e/1FAIpQLSfWrElNR3oOG1yQr6DAFIwHyNnvB5DjUvFNGqHSi-xvLeU9LA/viewform?usp=header",
               target = "_blank",
               class = "survey-link",
               icon("pencil-alt"),
               "Help Improve Our Algorithm")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    tabItems(
      # Recommendations Tab
      tabItem(
        tabName = "recommend",
        fluidRow(
          box(
            width = 4, 
            status = "primary",
            selectInput(
              "major", 
              "Select Major:", 
              choices = sort(unique(course_data$Student_Major))
            ),
            selectInput(
              "level", 
              "Select Academic Level:", 
              choices = c("Freshman", "Sophomore", "Junior", "Senior")
            ),
            actionButton(
              "recommend", 
              "Generate Recommendations", 
              icon = icon("lightbulb"), 
              class = "btn-primary"
            )
          ),
          box(
            width = 8, 
            title = "Recommended Courses", 
            status = "primary",
            withSpinner(DT::dataTableOutput("recommendations"), type = 4)
          )
        ),
        fluidRow(
          box(
            width = 12, 
            title = "All Available Courses for This Level",
            collapsible = TRUE, 
            collapsed = TRUE,
            withSpinner(DT::dataTableOutput("allCourses"))
          )
        )
      ),
      
      # Course Explorer Tab
      tabItem(
        tabName = "explore",
        fluidRow(
          box(
            width = 12, 
            status = "primary",
            selectizeInput(
              "searchCourse", 
              "Search for Courses:",
              choices = NULL, 
              multiple = TRUE,
              options = list(
                placeholder = 'Type to search courses',
                maxItems = 5
              )
            )
          ),
          box(
            width = 12, 
            title = "Course Information", 
            status = "primary",
            withSpinner(DT::dataTableOutput("courseInfo"))
          )
        )
      ),
      
      # About Tab
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            width = 12, 
            status = "primary",
            h2("About This Application"),
            p("This Course Recommendation System helps students discover relevant courses based on their major and academic level."),
            p("The system uses association rule mining (Apriori algorithm) to identify courses that are frequently taken together."),
            hr(),
            h3("How to Use"),
            tags$ul(
              tags$li("Select your major and academic level from the dropdown menus"),
              tags$li("Click 'Generate Recommendations' to see suggested courses"),
              tags$li("Explore all available courses for your level by expanding the bottom panel"),
              tags$li("Use the Course Explorer tab to search for specific courses")
            ),
            hr(),
            h4("We Value Your Feedback"),
            p("Please consider taking our ", 
              tags$a(
                href = "https://docs.google.com/forms/d/e/1FAIpQLSfWrElNR3oOG1yQr6DAFIwHyNnvB5DjUvFNGqHSi-xvLeU9LA/viewform?usp=header",
                target = "_blank", 
                "short survey"),
              " to help improve our recommendation algorithm."),
            hr(),
            h4("Version 2.2"),
            p("Last updated: ", Sys.Date())
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Update course search choices
  updateSelectizeInput(
    session, 
    'searchCourse', 
    choices = sort(unique(course_data$Recommended_Courses)), 
    server = TRUE
  )
  
  # Generate recommendations using Apriori algorithm
  recommendations <- eventReactive(input$recommend, {
    req(input$major, input$level)
    
    filtered_data <- course_data %>%
      filter(Student_Major == input$major, Academic_Level == input$level)
    
    if(nrow(filtered_data) == 0) {
      return(data.frame(Message = "No course data available for this major and level combination."))
    }
    
    suppressWarnings({
      transactions <- as(split(filtered_data$Recommended_Courses, 
                               filtered_data$Student_Major), "transactions")
      
      rules <- apriori(
        transactions, 
        parameter = list(support = 0.1, confidence = 0.6, minlen = 2, maxlen = 5),
        control = list(verbose = FALSE)
      )
    })
    
    if(length(rules) == 0) {
      return(data.frame(
        Message = "No association rules found. Showing top courses instead.",
        get_top_courses(filtered_data)
      ))
    }
    
    rules_df <- as(rules, "data.frame") %>%
      separate(rules, into = c("lhs", "rhs"), sep = "=>") %>%
      mutate(
        lhs = gsub("[\\{\\}]", "", lhs),
        rhs = gsub("[\\{\\}]", "", rhs),
        support = round(support, 3),
        confidence = round(confidence, 3),
        lift = round(lift, 3)
      ) %>%
      arrange(desc(confidence)) %>%
      head(5) %>%
      select("Recommended Course" = rhs, "Frequently Taken With" = lhs)
    
    return(rules_df)
  })
  
  # Helper function to get top courses if no rules found
  get_top_courses <- function(data) {
    data %>%
      count(Recommended_Courses, sort = TRUE) %>%
      head(5) %>%
      select("Recommended Course" = Recommended_Courses)
  }
  
  # Show all available courses for the selected major and level
  all_courses <- eventReactive(input$recommend, {
    req(input$major, input$level)
    
    course_data %>%
      filter(Student_Major == input$major, Academic_Level == input$level) %>%
      select(Recommended_Courses) %>%
      distinct() %>%
      arrange(Recommended_Courses)
  })
  
  # Course search functionality
  course_info <- reactive({
    req(input$searchCourse)
    
    course_data %>%
      filter(Recommended_Courses %in% input$searchCourse) %>%
      group_by(Recommended_Courses) %>%
      summarise(
        Majors = paste(unique(Student_Major), collapse = ", "),
        Levels = paste(unique(Academic_Level), collapse = ", "),
        .groups = 'drop'
      ) %>%
      arrange(Recommended_Courses)
  })
  
  # Output tables
  output$recommendations <- DT::renderDataTable({
    datatable(
      recommendations(), 
      options = list(
        pageLength = 5,
        dom = 't',
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      ),
      rownames = FALSE
    )
  })
  
  output$allCourses <- DT::renderDataTable({
    datatable(
      all_courses(), 
      options = list(pageLength = 10, dom = 'tip'),
      rownames = FALSE,
      colnames = "Available Courses"
    )
  })
  
  output$courseInfo <- DT::renderDataTable({
    datatable(
      course_info(), 
      options = list(pageLength = 5, dom = 'tip'),
      rownames = FALSE
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
