library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(shinyjs)
library(plotly)

# --- SETUP ---
if (!dir.exists("students")) dir.create("students")
addResourcePath("student_photos", "students")

# --- HELPER: GET ROSTER ---
get_roster_data <- function() {
  db_path <- "student_db.csv"
  if(file.exists(db_path)) {
    df <- read.csv(db_path, stringsAsFactors = FALSE, colClasses = "character", na.strings=c("","NA"))
    cols <- colnames(df)
    name_col <- 1; id_col <- 2;
    for(i in 1:ncol(df)) { 
      if(grepl("ID", cols[i], ignore.case=TRUE)) { id_col <- i }
    }
    clean <- data.frame(
      Name = trimws(df[,name_col]), 
      ID = as.character(df[,id_col]), 
      stringsAsFactors=FALSE
    )
    return(clean)
  }
  return(data.frame())
}

# --- HELPER: DEFINE WEEKLY SCHEDULE (Added Math) ---
get_weekly_schedule <- function() {
  data.frame(
    Day = c("Saturday", "Saturday", 
            "Sunday", "Sunday", "Sunday", # Added Math here
            "Monday", "Monday", 
            "Tuesday", "Tuesday", # Added Math here
            "Wednesday", "Wednesday",
            "Thursday"),
    Time = c("09:00 AM", "12:30 PM", 
             "08:30 AM", "11:00 AM", "02:00 PM",
             "10:00 AM", "01:00 PM", 
             "09:00 AM", "11:00 AM",
             "09:00 AM", "12:00 PM",
             "10:00 AM"),
    Course = c("Intro to AI", "Fundamentals of Robotics",
               "Operating system", "Mathematical foundation", "Advanced Statistics",
               "Intro to AI", "Operating system",
               "Mathematical foundation", "Fundamentals of Robotics",
               "Advanced Statistics", "Intro to AI",
               "Operating system"),
    Room = c("Lab 3", "Room 101",
             "Hall B", "Room 404", "Room 205",
             "Lab 1", "Lab 2",
             "Room 404", "Room 303",
             "Hall A", "Lab 3",
             "Lab 1"),
    stringsAsFactors = FALSE
  )
}

# --- HELPER: GET STUDENT HISTORY ---
get_student_history <- function(course_name, student_name, current_week = 15) {
  clean_name <- gsub("[^[:alnum:]]", "_", course_name)
  file_path <- paste0("data/", clean_name, ".csv")
  
  history <- data.frame(Week = 1:current_week, Status = "Pending", Date = NA, ArrivalTime = NA)
  
  if(file.exists(file_path)) {
    df <- read.csv(file_path, stringsAsFactors = FALSE)
    student_records <- df %>% filter(trimws(toupper(Name)) == trimws(toupper(student_name)))
    
    if(nrow(student_records) > 0) {
      for(w in 1:current_week) {
        rec <- student_records %>% filter(Week == w)
        if(nrow(rec) > 0) {
          history$Status[w] <- "Present"
          history$Date[w] <- rec$Date[1]
          history$ArrivalTime[w] <- rec$ArrivalTime[1]
        } else {
          history$Status[w] <- "Absent"
        }
      }
    } else {
      history$Status <- "Absent"
    }
  }
  
  present_cnt <- sum(history$Status == "Present")
  absent_cnt <- sum(history$Status == "Absent")
  late_cnt <- sum(history$ArrivalTime > "13:00:00", na.rm = TRUE)
  att_mark <- case_when(absent_cnt == 0 ~ 5, absent_cnt == 1 ~ 4, absent_cnt == 2 ~ 2.5, TRUE ~ 0)
  arr_mark <- max(0, 5 - (late_cnt * 0.5))
  total_mark <- att_mark + arr_mark
  status_flag <- case_when(absent_cnt >= 4 ~ "WITHDRAWN", absent_cnt == 3 ~ "WARNING", TRUE ~ "Active")
  
  list(history = history, present = present_cnt, absent = absent_cnt, late = late_cnt,
       att_mark = att_mark, arr_mark = arr_mark, total_mark = total_mark, status = status_flag)
}

# --- CSS STYLING ---
custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600;700&display=swap');
body, h1, h2, h3, h4, h5, h6, p, div, span, a { font-family: 'Poppins', sans-serif !important; }

/* LOGIN */
#login-screen { position: fixed; top: 0; left: 0; width: 100%; height: 100vh; display: flex; flex-direction: row; z-index: 9999; background: white; }
.login-left { flex: 6; background: url('back.jpg') no-repeat center center; background-size: cover; position: relative; background-color: #2c3e50; }
.left-overlay { position: absolute; bottom: 0; left: 0; width: 100%; background: linear-gradient(to top, rgba(0,0,0,0.8), transparent); padding: 40px; color: white; }
.login-right { flex: 4; background: white; padding: 50px; display: flex; flex-direction: column; justify-content: center; box-shadow: -5px 0 15px rgba(0,0,0,0.1); }
.login-logo { display: flex; align-items: center; margin-bottom: 30px; }
.login-logo img { height: 50px; margin-right: 15px; }
.form-control { height: 45px; background-color: #f9f9f9; border: 1px solid #ccc; border-radius: 4px; }
#login_btn { background-color: #337ab7; color: white; width: 100%; height: 45px; font-weight: 600; margin-top: 10px; border-radius: 4px; border: none; }
#login_btn:hover { background-color: #286090; }

/* DASHBOARD ITEMS */
.skin-blue .main-header .logo, .skin-blue .main-header .navbar, .skin-blue .main-sidebar { background-color: #fff !important; border-bottom: 1px solid #eee; border-right: 1px solid #eee; color: #333 !important; }
.skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a { border-left-color: #667eea; background: #f4f6f9; color: #333; }
.skin-blue .sidebar-menu>li>a { color: #555; }
.content-wrapper { background-color: #f4f6f9; }

/* SCHEDULE TABLES */
.schedule-table { width: 100%; border-collapse: collapse; margin-top: 10px; }
.schedule-table th { text-align: left; color: #7f8c8d; font-size: 12px; border-bottom: 2px solid #eee; padding: 8px; }
.schedule-table td { padding: 12px 8px; border-bottom: 1px solid #f0f0f0; font-size: 14px; color: #2c3e50; }
.schedule-time { font-weight: 700; color: #667eea; }
.schedule-room { font-size: 11px; background: #eee; padding: 3px 8px; border-radius: 10px; color: #666; }

/* CARDS */
.portal-card { background: white; border-radius: 12px; overflow: hidden; box-shadow: 0 3px 10px rgba(0,0,0,0.08); margin-bottom: 20px; transition: all 0.3s ease; cursor: pointer; border: 1px solid #e0e0e0; }
.portal-card:hover { transform: translateY(-5px); box-shadow: 0 8px 20px rgba(0,0,0,0.15); border-color: #667eea; }
.card-img-container { height: 130px; overflow: hidden; position: relative; }
.card-img-top { width: 100%; height: 100%; object-fit: cover; }
.card-footer-custom { padding: 15px; font-weight: 600; font-size: 13px; color: #333; border-top: 1px solid #f0f0f0; background: #fafafa; }

/* PROFILE */
.user-panel-custom { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); height: 140px; display: flex; flex-direction: column; align-items: center; justify-content: center; border-radius: 0 0 15px 15px; margin-bottom: 15px; }
.user-avatar { width: 70px; height: 70px; border-radius: 50%; border: 3px solid white; margin-bottom: 8px; object-fit: cover; }
.user-text { color: white; text-align: center; }
.user-name { font-weight: 700; font-size: 15px; } 
.user-id { font-size: 11px; opacity: 0.9; }
"

# --- UI ---
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(custom_css))),
  
  # LOGIN SCREEN
  div(id = "login-screen",
      div(class = "login-left",
          div(class = "left-overlay",
              h1("Student Portal", style="margin:0; font-weight:700; font-size: 36px;"),
              p("Access your attendance records, grades, and academic progress.")
          )
      ),
      div(class = "login-right",
          div(class = "login-logo", img(src="logo.jpg"), div(style="font-size:22px; font-weight:600;", "AASTMT")),
          h4("Student Login", style="margin-bottom:20px; font-weight:600; color:#555;"),
          textInput("student_id", NULL, placeholder="Enter Your Student ID"),
          actionButton("login_btn", "Log In", style="width:100%;"),
          uiOutput("login_msg"),
          div(class="login-footer", style="margin-top:auto; font-size:11px; color:#888; text-align:center;", "Copyright © 2025 AASTMT")
      )
  ),
  
  # MAIN APP
  div(id = "main-app", style="display:none;",
      dashboardPage(skin = "blue",
                    dashboardHeader(title = "My Portal", 
                                    tags$li(class = "dropdown", actionLink("logout_btn", "Logout", icon=icon("sign-out-alt")))),
                    dashboardSidebar(width = 250,
                                     uiOutput("sidebar_profile_ui"),
                                     sidebarMenu(id = "tabs",
                                                 menuItem("Home & Schedule", tabName = "home", icon = icon("home")),
                                                 menuItem("Attendance Details", tabName = "details", icon = icon("calendar-check")),
                                                 menuItem("Performance", tabName = "performance", icon = icon("chart-line")),
                                                 menuItem("Grades Summary", tabName = "grades", icon = icon("graduation-cap"))
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        # --- HOME TAB ---
                        tabItem(tabName = "home",
                                fluidRow(
                                  # LEFT COLUMN: COURSES + FULL SCHEDULE
                                  column(8, 
                                         h2(uiOutput("welcome_msg"), style="margin-top:0; color:#2c3e50; font-weight:700;"),
                                         p("Here are your enrolled courses.", style="color:#7f8c8d; margin-bottom:20px;"),
                                         
                                         # ROW 1: COURSES
                                         fluidRow(
                                           column(4, div(class="portal-card", onclick="Shiny.setInputValue('sel_course', 'Advanced Statistics', {priority: 'event'})",
                                                         div(class="card-img-container", img(src="https://img.freepik.com/free-vector/gradient-stock-market-concept_23-2149166910.jpg", class="card-img-top")),
                                                         div(class="card-footer-custom", "📊 Statistics"))),
                                           
                                           column(4, div(class="portal-card", onclick="Shiny.setInputValue('sel_course', 'Intro to AI', {priority: 'event'})",
                                                         div(class="card-img-container", img(src="https://img.freepik.com/free-photo/ai-technology-brain-background-digital-transformation-concept_53876-124672.jpg", class="card-img-top")),
                                                         div(class="card-footer-custom", "🤖 AI"))),
                                           
                                           # NEW MATH CARD
                                           column(4, div(class="portal-card", onclick="Shiny.setInputValue('sel_course', 'Mathematical foundation', {priority: 'event'})",
                                                         div(class="card-img-container", img(src="https://img.freepik.com/free-vector/mathematical-geometry-background_23-2148812678.jpg", class="card-img-top")),
                                                         div(class="card-footer-custom", "📐 Mathematics")))
                                         ),
                                         
                                         # ROW 2: COURSES
                                         fluidRow(
                                           column(4, div(class="portal-card", onclick="Shiny.setInputValue('sel_course', 'Fundamentals of Robotics', {priority: 'event'})",
                                                         div(class="card-img-container", img(src="https://img.freepik.com/free-photo/robot-handshake-human-background-futuristic-digital-age_53876-129770.jpg", class="card-img-top")),
                                                         div(class="card-footer-custom", "🦾 Robotics"))),
                                           
                                           column(4, div(class="portal-card", onclick="Shiny.setInputValue('sel_course', 'Operating system', {priority: 'event'})",
                                                         div(class="card-img-container", img(src="https://img.freepik.com/free-vector/matrix-style-binary-code-digital-background_1017-25336.jpg", class="card-img-top")),
                                                         div(class="card-footer-custom", "💻 OS")))
                                         ),
                                         
                                         # FULL WEEKLY SCHEDULE BOX
                                         box(width = 12, title = icon("calendar-alt", " Full Weekly Timetable"), 
                                             status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                                             DTOutput("full_schedule_table")
                                         )
                                  ),
                                  
                                  # RIGHT COLUMN: TODAY'S SCHEDULE (Quick View)
                                  column(4,
                                         box(width = 12, status = "primary", solidHeader = TRUE,
                                             title = uiOutput("schedule_date_title"),
                                             uiOutput("daily_schedule_ui")
                                         ),
                                         div(style="background:#fff3cd; padding:15px; border-radius:8px; border-left:4px solid #f1c40f;",
                                             h5(icon("bell"), " Announcements", style="margin-top:0; font-weight:600;"),
                                             p("Midterm exams start next week. Please check your emails.", style="font-size:12px; margin:0;")
                                         )
                                  )
                                )
                        ),
                        
                        # --- OTHER TABS ---
                        tabItem(tabName = "details",
                                uiOutput("course_header"),
                                fluidRow(
                                  valueBoxOutput("box_att", width=3), valueBoxOutput("box_abs", width=3),
                                  valueBoxOutput("box_late", width=3), valueBoxOutput("box_status", width=3)
                                ),
                                box(title = "Weekly Attendance Log", width = 12, status = "primary", DTOutput("attendance_table"))
                        ),
                        
                        tabItem(tabName = "performance",
                                h3("Analytics"), uiOutput("performance_course_selector"),
                                fluidRow(
                                  box(title="Trend", width=6, plotlyOutput("trend_plot", height="200px")),
                                  box(title="Score", width=6, plotlyOutput("score_breakdown_plot", height="200px"))
                                ),
                                uiOutput("performance_stats_ui")
                        ),
                        
                        tabItem(tabName = "grades",
                                h2("Grades Summary"),
                                box(width=12, status="info", DTOutput("all_grades_table"))
                        )
                      )
                    )
      )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  user_info <- reactiveValues(logged_in = FALSE, name = "", id = "", photo = "")
  
  observeEvent(input$login_btn, {
    roster <- get_roster_data()
    req_id <- trimws(input$student_id)
    match_row <- roster %>% filter(ID == req_id)
    
    if(nrow(match_row) > 0) {
      user_info$logged_in <- TRUE
      user_info$name <- match_row$Name[1]
      user_info$id <- match_row$ID[1]
      
      clean_search <- gsub("[^[:alnum:]]", "", tolower(user_info$name))
      files <- list.files("students")
      f <- NA
      for(file in files) if(gsub("[^[:alnum:]]", "", tolower(tools::file_path_sans_ext(file))) == clean_search) { f <- file; break }
      user_info$photo <- if(!is.na(f)) paste0("student_photos/", URLencode(f)) else "https://ui-avatars.com/api/?name=Student&background=667eea&color=fff&size=150"
      
      shinyjs::hide("login-screen"); shinyjs::show("main-app")
    } else {
      output$login_msg <- renderUI({ div(style="color:red; margin-top:10px;", "ID not found.") })
    }
  })
  
  observeEvent(input$logout_btn, {
    shinyjs::hide("main-app"); shinyjs::show("login-screen")
    user_info$logged_in <- FALSE; updateTextInput(session, "student_id", value = "")
  })
  
  output$sidebar_profile_ui <- renderUI({
    req(user_info$logged_in)
    div(class = "user-panel-custom", tags$img(src = user_info$photo, class = "user-avatar"),
        div(class = "user-text", div(class = "user-name", user_info$name), div(class = "user-id", paste("ID:", user_info$id))))
  })
  
  # --- HOME: WELCOME & SCHEDULES ---
  output$welcome_msg <- renderUI({ paste("Welcome,", user_info$name) })
  
  output$schedule_date_title <- renderText({ paste("📅 Today:", format(Sys.Date(), "%A")) })
  
  # 1. Today's Schedule (Small Box)
  output$daily_schedule_ui <- renderUI({
    today_day <- format(Sys.Date(), "%A") 
    # today_day <- "Sunday" # Uncomment to test specific day
    
    full_sched <- get_weekly_schedule()
    todays_classes <- full_sched %>% filter(Day == today_day) %>% arrange(Time)
    
    if(nrow(todays_classes) == 0) {
      div(style="text-align:center; padding:20px; color:#7f8c8d;", icon("coffee", "fa-3x"), h4("No classes today!"))
    } else {
      rows <- lapply(1:nrow(todays_classes), function(i) {
        tags$tr(
          tags$td(class="schedule-time", todays_classes$Time[i]),
          tags$td(div(style="font-weight:600;", todays_classes$Course[i]), span(class="schedule-room", icon("map-marker-alt"), todays_classes$Room[i]))
        )
      })
      tags$table(class="schedule-table", tags$thead(tags$tr(tags$th("Time"), tags$th("Course Info"))), tags$tbody(rows))
    }
  })
  
  # 2. Full Weekly Schedule (Large Table)
  output$full_schedule_table <- renderDT({
    df <- get_weekly_schedule()
    # Define order of days
    days_order <- c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
    df$Day <- factor(df$Day, levels = days_order)
    df <- df %>% arrange(Day, Time)
    
    datatable(df, rownames=FALSE, options=list(pageLength=10, dom='t'), class='cell-border stripe')
  })
  
  # --- COURSE DETAILS LOGIC ---
  observeEvent(input$sel_course, { updateTabItems(session, "tabs", "details") })
  selected_course <- reactive({ if(is.null(input$sel_course) || input$sel_course == "") "Advanced Statistics" else input$sel_course })
  output$course_header <- renderUI({ h2(paste("📚", selected_course()), style="margin-top:0; color:#2c3e50; font-weight:600;") })
  
  course_stats <- reactive({ req(user_info$logged_in); get_student_history(selected_course(), user_info$name) })
  output$box_att <- renderValueBox({ valueBox(course_stats()$present, "Attended", icon=icon("check-circle"), color="green") })
  output$box_abs <- renderValueBox({ valueBox(course_stats()$absent, "Absences", icon=icon("times-circle"), color="red") })
  output$box_late <- renderValueBox({ valueBox(course_stats()$late, "Late Arrivals", icon=icon("clock"), color="orange") })
  output$box_status <- renderValueBox({ s <- course_stats()$status; col <- if(s=="Active") "blue" else if(s=="WARNING") "yellow" else "red"; valueBox(s, "Status", icon=icon("info-circle"), color=col) })
  
  output$attendance_table <- renderDT({
    datatable(course_stats()$history, rownames=FALSE, options=list(pageLength=5, dom='tp'), class = 'cell-border stripe') %>%
      formatStyle("Status", color = styleEqual(c("Present", "Absent", "Pending"), c("#27ae60", "#e74c3c", "#95a5a6")), fontWeight = "600")
  })
  
  # --- PERFORMANCE LOGIC ---
  output$performance_course_selector <- renderUI({
    # Added Mathematical foundation to list
    selectInput("perf_course", "Select Course:", 
                choices = c("Advanced Statistics", "Intro to AI", "Fundamentals of Robotics", "Operating system", "Mathematical foundation"), 
                selected = selected_course())
  })
  perf_course_stats <- reactive({ req(input$perf_course, user_info$logged_in); get_student_history(input$perf_course, user_info$name) })
  
  output$trend_plot <- renderPlotly({
    stats <- perf_course_stats(); df <- stats$history %>% mutate(Present = ifelse(Status == "Present", 1, 0)) %>% mutate(Cumulative = cumsum(Present))
    plot_ly(df, x = ~Week, y = ~Cumulative, type = 'scatter', mode = 'lines+markers', line = list(color = '#667eea', width = 3)) %>% layout(yaxis=list(title="Total Attended"))
  })
  output$score_breakdown_plot <- renderPlotly({
    stats <- perf_course_stats(); data <- data.frame(Cat=c("Attendance","Punctuality"), Sc=c(stats$att_mark, stats$arr_mark), Mx=c(5,5))
    plot_ly(data, x=~Cat, y=~Sc, type='bar', marker=list(color=c('#27ae60','#f39c12'))) %>% add_trace(y=~Mx, marker=list(color='rgba(200,200,200,0.3)'), showlegend=F) %>% layout(barmode='overlay')
  })
  
  output$performance_stats_ui <- renderUI({
    stats <- perf_course_stats()
    fluidRow(
      column(3, div(class="stat-mini-card", style="border-left-color:#3498db;", div(class="stat-value", stats$present), div(class="stat-label", "Attended"))),
      column(3, div(class="stat-mini-card", style="border-left-color:#e74c3c;", div(class="stat-value", stats$absent), div(class="stat-label", "Absences"))),
      column(3, div(class="stat-mini-card", style="border-left-color:#f39c12;", div(class="stat-value", stats$late), div(class="stat-label", "Lates"))),
      column(3, div(class="stat-mini-card", style="border-left-color:#27ae60;", div(class="stat-value", sprintf("%.1f/10", stats$total_mark)), div(class="stat-label", "Score")))
    )
  })
  
  output$all_grades_table <- renderDT({
    req(user_info$logged_in)
    # Added Mathematical foundation to list
    courses <- c("Advanced Statistics", "Intro to AI", "Fundamentals of Robotics", "Operating system", "Mathematical foundation")
    summary_data <- data.frame(Course=character(), Absences=integer(), `Total Marks`=numeric(), Status=character(), stringsAsFactors=FALSE, check.names=FALSE)
    for(c in courses) { st <- get_student_history(c, user_info$name); summary_data <- rbind(summary_data, data.frame(Course=c, Absences=st$absent, `Total Marks`=st$total_mark, Status=st$status, check.names=FALSE)) }
    datatable(summary_data, rownames=FALSE, options=list(dom='t')) %>% formatStyle("Status", backgroundColor=styleEqual(c("WARNING","WITHDRAWN","Active"), c("#fff3cd","#f8d7da","#d4edda")))
  })
}

shinyApp(ui, server)