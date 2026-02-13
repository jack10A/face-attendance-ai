# app.R
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(ggplot2)
library(shinyWidgets)
library(blastula)
library(shinyjs)
library(rmarkdown)
library(knitr)
library(dplyr)
library(kableExtra)
library(twilio)

# --- SETUP ---
if (!dir.exists("data")) dir.create("data")
addResourcePath("student_photos", "students")

# --- CREDENTIALS ---
ADMIN_USER <- "admin"
ADMIN_PASS <- "admin123"
MY_EMAIL_ADDR <- Sys.getenv("SMTP_EMAIL")
MY_EMAIL_PASS <- Sys.getenv("SMTP_PASSWORD")
# !!! PASTE YOUR REAL KEYS HERE INSIDE THE QUOTES !!!

# --- HELPER FUNCTIONS ---
get_roster_data <- function() {
  db_path <- "student_db.csv"
  if(file.exists(db_path)) {
    # --- FIX 1: Read everything as TEXT (character) to stop rounding numbers ---
    df <- read.csv(db_path, stringsAsFactors = FALSE, colClasses = "character", na.strings=c("","NA"))
    
    cols <- colnames(df)
    name_col <- 1; id_col <- 2; email_col <- 0; phone_col <- 0
    
    # Auto-detect columns
    for(i in 1:ncol(df)) { 
      if(any(grepl("@", df[1:min(5,nrow(df)), i]))) { email_col <- i }
      if(grepl("ID", cols[i], ignore.case=TRUE)) { id_col <- i }
      if(grepl("Phone", cols[i], ignore.case=TRUE) || grepl("Mobile", cols[i], ignore.case=TRUE)) { phone_col <- i }
    }
    
    if(email_col > 0) {
      # Handle Phone Column
      phone_values <- NA
      if(phone_col > 0) {
        # Clean phone numbers: Remove spaces, dashes, parentheses
        phone_values <- gsub("[^0-9]", "", df[,phone_col]) 
        phone_values[phone_values == ""] <- NA
      }
      
      clean <- data.frame(
        Name = trimws(df[,name_col]), 
        ID = as.character(df[,id_col]), 
        Email = trimws(df[,email_col]),
        ParentPhone = phone_values,
        stringsAsFactors=FALSE
      )
      return(clean[order(clean$Name),])
    }
  }
  
  if(dir.exists("students")) {
    names <- sort(tools::file_path_sans_ext(list.files("students")))
    return(data.frame(Name=trimws(names), ID="N/A", Email=NA, ParentPhone=NA, stringsAsFactors=FALSE))
  }
  return(data.frame(Name=character()))
}

update_manual_attendance <- function(course, week, student_name, student_id, is_present) {
  clean_name <- gsub("[^[:alnum:]]", "_", course)
  file_path <- paste0("data/", clean_name, ".csv")
  cols <- c("ID", "Name", "Date", "ArrivalTime", "Week", "Confidence", "Status")
  
  if(file.exists(file_path)) {
    df <- read.csv(file_path, stringsAsFactors=FALSE)
    if(!"ID" %in% colnames(df)) df$ID <- "N/A"
  } else {
    df <- setNames(data.frame(matrix(ncol=7, nrow=0)), cols)
  }
  
  student_name <- trimws(toupper(student_name))
  match_idx <- which(trimws(toupper(df$Name)) == student_name & df$Week == week)
  already_here <- length(match_idx) > 0
  
  if (is_present && !already_here) {
    new_row <- data.frame(
      ID = as.character(student_id), Name = student_name, Date = format(Sys.Date(),"%Y-%m-%d"), 
      ArrivalTime = "12:30:00", Week = week, Confidence = 100, Status = "Manual Entry"
    )
    # Ensure matching columns if df empty
    if(nrow(df) == 0) df <- new_row else df <- rbind(df, new_row)
    write.csv(df, file_path, row.names=FALSE)
  } else if (!is_present && already_here) {
    df <- df[-match_idx, ]
    write.csv(df, file_path, row.names=FALSE)
  }
}

send_alert_email <- function(name, email_addr, type, course) {
  if (is.na(email_addr) || !grepl("@", email_addr) || grepl("http", email_addr)) {
    message(sprintf("⚠ SKIP: %s - Invalid Email (%s)", name, email_addr))
    return(FALSE)
  }
  if (identical(type, "WARNING")) {
    subj <- paste("⚠ Attendance Warning:", course)
    body_txt <- paste0("Dear ", name, ",\n\nYou have 3 absences in ", course, ".\n\nRegards,\nAttendance Team")
  } else {
    subj <- paste("⛔ WITHDRAWAL NOTICE:", course)
    body_txt <- paste0("Dear ", name, ",\n\nYou have exceeded 4 absences in ", course, ".\n\nRegards,\nAttendance Team")
  }
  message(sprintf("⏳ Sending email to: %s", email_addr))
  my_creds <- tryCatch({
    creds_envvar(
      user = MY_EMAIL_ADDR,
      pass_envvar = "SMTP_PASSWORD",
      host = "smtp.gmail.com",
      port = 465,
      use_ssl = TRUE
    )
  }, error = function(e) {
    message("❌ Could not build credentials: ", e$message)
    return(NULL)
  })
  if (is.null(my_creds)) return(FALSE)
  email_obj <- tryCatch({ compose_email(body = md(body_txt)) }, error = function(e) { message("❌ compose failed: ", e$message); return(NULL) })
  if (is.null(email_obj)) return(FALSE)
  res <- tryCatch({
    email_obj %>% smtp_send(
      to = email_addr,
      from = MY_EMAIL_ADDR,
      subject = subj,
      credentials = my_creds
    )
    TRUE
  }, error = function(e) {
    message("❌ smtp_send failed for ", email_addr, ": ", e$message)
    FALSE
  })
  if (isTRUE(res)) message("✔ SUCCESS! Email queued/sent to: ", email_addr)
  return(isTRUE(res))
}
# --- HELPER: SYSTEM LOG ---
log_action <- function(action_text) {
  entry <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", action_text)
  cat(entry, file = "system_audit.log", sep = "\n", append = TRUE)
}
###sms parent###
# Note the added 'course_name' argument



# --- CSS (AASTMT PORTAL STYLE) ---
custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600;700&display=swap');
body, h1, h2, h3, h4, h5, h6, p, div { font-family: 'Poppins', sans-serif !important; }

/* RESET LOGIN SCREEN TO SPLIT LAYOUT */
#login-screen { 
  position: fixed; top: 0; left: 0; width: 100%; height: 100vh; 
  display: flex; flex-direction: row; 
  z-index: 9999; background: white;
}

/* LEFT SIDE: IMAGE */
.login-left {
  flex: 6; /* 60% width */
  background: url('https://raw.githubusercontent.com/yousef-elshamy/AAST-Photos/main/campus.jpg') no-repeat center center;
  background-size: cover;
  position: relative;
}
/* Fallback color */
.login-left { background-color: #2c3e50; } 

.left-overlay {
  position: absolute; bottom: 0; left: 0; width: 100%;
  background: linear-gradient(to top, rgba(0,0,0,0.8), transparent);
  padding: 40px; color: white;
}

/* RIGHT SIDE: FORM */
.login-right {
  flex: 4; /* 40% width */
  background: white;
  padding: 50px;
  display: flex; flex-direction: column; justify-content: center;
  box-shadow: -5px 0 15px rgba(0,0,0,0.1);
}

/* LOGO & HEADER */
.login-logo { display: flex; align-items: center; margin-bottom: 30px; }
.login-logo img { height: 50px; margin-right: 15px; }
.login-header { font-size: 22px; font-weight: 600; color: #333; }

/* INPUTS */
.form-control { 
  height: 45px; border-radius: 4px; border: 1px solid #ccc; 
  box-shadow: none; margin-bottom: 15px; background-color: #f9f9f9;
}
.form-control:focus { border-color: #3c8dbc; background-color: #fff; }

/* BUTTONS */
#login_btn { 
  background-color: #337ab7; border-color: #2e6da4; color: white; 
  width: 100%; height: 45px; font-weight: 600; font-size: 16px; 
  margin-top: 10px; border-radius: 4px;
}
#login_btn:hover { background-color: #286090; }

#face_login_btn {
  background-color: #fff; color: #444; border: 1px solid #ddd;
  width: 100%; margin-top: 10px; height: 40px;
}

/* FOOTER */
.login-footer { margin-top: auto; font-size: 11px; color: #888; text-align: center; padding-top: 20px;}

/* KEEP APP STYLES */
.student-card { background: #fff; border-radius: 10px; padding: 10px; margin-bottom: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); border-top: 5px solid #bdc3c7; display: flex; flex-direction: column; align-items: center; justify-content: space-between; height: 260px; transition: border-color 0.3s ease; }
.student-img { width: 90px; height: 90px; border-radius: 50%; object-fit: cover; margin-bottom: 10px; border: 3px solid #eee; }
.student-name { font-weight: bold; font-size: 14px; text-align: center; height: 40px; overflow: hidden; }
"

# --- UI ---
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(custom_css))),
  
  # LOGIN
  # --- UPDATED LOGIN SCREEN ---
  div(id = "login-screen",
      # Left Side: Campus Image
      div(class = "login-left", style="background-image: url('back.jpg');",
          div(class = "left-overlay",
              h1("AASTMT  Portal", style="margin: 0; font-weight: 700;"),
              p("AASTMT  Portal is an online gateway where dos can log in to access important program information.", style="font-size: 14px; opacity: 0.9;")
          )
      ),
      
      # Right Side: Login Form
      div(class = "login-right",
          # Header with Logo
          div(class = "login-logo",
              img(src="logo.jpg"),
              div(class="login-header", "Student Portal")
          ),
          
          h4("Login", style="margin-bottom: 20px; font-weight: 600; color:#555;"),
          
          # Inputs (IDs match your server code)
          textInput("user_name", NULL, placeholder="Registration Number"),
          passwordInput("user_pass", NULL, placeholder="Pin Code"),
          
          # Checkbox (Visual)
          div(style="margin-bottom: 15px;",
              checkboxInput("remember", "Remember Me", FALSE)
          ),
          
          # Login Button
          actionButton("login_btn", "Login"),
          
          # Face ID Button
          actionButton("face_login_btn", "Or Login via Face ID", icon=icon("camera")),
          
          br(),
          uiOutput("login_msg"),
          
          # Footer
          div(class="login-footer",
              "Copyright © 2025 Information and Documentation Center - AASTMT"
          )
      )
  ),
  
  # MAIN APP
  div(id = "main-app", style="display:none;",
      dashboardPage(skin = "blue",
                    dashboardHeader(title = "Smart Attendance", 
                                    tags$li(class = "dropdown", actionLink("logout_btn", "Logout", icon=icon("sign-out")))),
                    dashboardSidebar(
                      sidebarUserPanel("dr mohamed fathy", image = "dr.jpg"),
                      selectInput("global_course", "Course:", choices = c("Select..." = "", "Advanced Statistics", "Intro to AI", "Mathematical foundation", "Operating system", "Fundamentals of Robotics", "Professional training")),
                      sliderInput("global_week", "Week:", 1, 15, 1, step=1),
                      sidebarMenu(
                        menuItem("Camera", tabName = "take_att", icon = icon("camera")),
                        menuItem("Manual Roll Call", tabName = "manual_att", icon = icon("clipboard-check")),
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-pie")),
                        menuItem("Grades", tabName = "grades", icon = icon("graduation-cap")),
                        menuItem("Security Logs", tabName = "logs", icon = icon("shield-alt"))
                        
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        # --- TAB 1: CAMERA ---
                        tabItem(tabName = "take_att",
                                h3("Camera Session"),
                                fluidRow(
                                  # The standard button
                                  column(4, uiOutput("camera_btn_ui")), 
                                  
                                  # --- NEW VOICE BUTTON ---
                                  column(4, actionButton("btn_voice_activate", "Voice Command", icon=icon("microphone"), 
                                                         style="width:100%; background:#8e44ad; color:white; font-weight:bold;")),
                                  
                                  # The refresh button
                                  column(4, actionButton("refresh_cam", "Refresh Log", icon=icon("sync"), class="btn-primary", style="width:100%"))
                                ),
                                hr(),
                                h5(id="voice_status", "Click 'Voice Command' and say 'Open Camera'", style="color:#7f8c8d; text-align:center;"),
                                DTOutput("sessionTable")
                        ),
                        
                        # --- TAB 2: MANUAL ---
                        tabItem(tabName = "manual_att",
                                div(class="small-btn-container", h3("Manual Attendance", style="margin:0;"), 
                                    actionButton("refresh_manual", "Sync Data", icon=icon("sync"), class="btn-success btn-mini")),
                                
                                uiOutput("manual_grid_ui")
                        ),
                        
                        # --- TAB 3: DASHBOARD (ANALYTICS CENTER) ---
                        tabItem(
                          tabName = "dashboard",
                          
                          # Header + Buttons
                          div(class="small-btn-container", 
                              h3("Class Analytics", style="margin:0;"), 
                              div(
                                downloadButton("download_report", "PDF Report", class="btn-warning btn-mini"),
                                actionButton("refresh_dash", "Refresh", icon=icon("sync"), class="btn-primary btn-mini")
                              )
                          ),
                          
                          # Summary Boxes
                          fluidRow(
                            valueBoxOutput("box_total", width = 4),
                            valueBoxOutput("box_present", width = 4),
                            valueBoxOutput("box_risk", width = 4)
                          ),
                          
                          # Row 1: Basic Charts
                          fluidRow(
                            box(title="Weekly Trend", width=6, status="info", plotOutput("trendPlot", height="250px")),
                            box(title="Attendance Overview", width=6, status="info", plotOutput("piePlot", height="250px"))
                          ),
                          
                          # Row 2: Heatmap (Visual Pattern Recognition)
                          fluidRow(
                            box(title="Semester Attendance Heatmap", width=12, status="primary", solidHeader=TRUE,
                                plotOutput("heatmapPlot", height="300px"),
                                p("Green: Present | Gray: Absent | Visualizes chronic absenteeism patterns.")
                            )
                          ),
                          
                          # Row 3: AI Prediction (Data Science)
                          hr(),
                          h2("🤖 AI Performance Prediction"),
                          fluidRow(
                            box(title = "Attendance vs Grades Correlation", status = "warning", solidHeader=TRUE, width = 8, 
                                plotOutput("predictPlot", height="250px")),
                            box(title = "Prediction Logic", status = "warning", width = 4, 
                                p("This model uses Linear Regression to predict final success probability based on current attendance patterns."),
                                p(strong("Hypothesis:"), "Higher attendance correlates with higher grades."),
                                p(strong("Model:"), "Grade ~ Days_Present"))
                          ),
                          
                          # Row 4: Raw Logs
                          fluidRow(
                            box(title = "Recent Camera Records", width = 12, collapsible = TRUE, collapsed = TRUE, 
                                DTOutput("camera_table"))
                          )
                        ),
                        
                        # --- TAB 4: GRADES ---
                        tabItem(tabName = "grades",
                                div(class="small-btn-container", h3("Grades"), 
                                    actionButton("refresh_grades", "Refresh", icon=icon("sync"), class="btn-primary btn-mini")),
                                fluidRow(
                                  box(width=12, status="danger", title="Automated Actions",
                                      p("Click below to scan for students with 3 (Warning) or 4 (Withdrawal) absences and send emails."),
                                      actionButton("btn_email", "📧 Check & Send Warnings", class="btn-danger")
                                  )
                                ),
                                hr(), 
                                DTOutput("gradesTable")
                        ),
                        
                        # --- TAB 5: SECURITY LOGS (NEW) ---
                        tabItem(tabName = "logs",
                                h2("🛡 System Audit Log"),
                                p("Immutable record of all actions (Login, Manual Changes, Camera Access) for security auditing."),
                                actionButton("refresh_logs", "Refresh Logs", icon=icon("sync")),
                                hr(),
                                verbatimTextOutput("log_viewer")
                        )
                      )
                    )
      )
  )
)
# --- SERVER ---
server <- function(input, output, session) {
  
  # Check for tinytex (lightweight LaTeX)
  if (!tinytex::is_tinytex()) {
    showNotification(
      "PDF generation requires LaTeX. Install with: tinytex::install_tinytex()",
      type = "warning",
      duration = NULL
    )
  }
  
  # ... rest of your server code
  # AUTH
  observeEvent(input$login_btn, {
    if(input$user_name == ADMIN_USER && input$user_pass == ADMIN_PASS) {
      log_action(paste("User logged in:", input$user_name)) # <--- ADD THIS
      shinyjs::hide("login-screen"); shinyjs::show("main-app")
    } else {
      log_action(paste("Failed login attempt:", input$user_name)) # <--- ADD THIS
      output$login_msg <- renderUI({ p("Incorrect Credentials", style="color:red") })
    }
  })
  observeEvent(input$face_login_btn, {
    showNotification("Scanning...", type="message")
    system("python engine.py auth", wait=TRUE)
    if(file.exists("auth_token.txt")) { shinyjs::hide("login-screen"); shinyjs::show("main-app"); file.remove("auth_token.txt") }
  })
  observeEvent(input$logout_btn, { shinyjs::hide("main-app"); shinyjs::show("login-screen") })
  
  # masterData as reactiveVal (central single source of truth)
  masterData <- reactiveVal(data.frame())
  load_master_data <- function() {
    req(input$global_course)
    clean <- gsub("[^[:alnum:]]", "_", input$global_course)
    path <- paste0("data/", clean, ".csv")
    if(file.exists(path)) {
      df <- read.csv(path, stringsAsFactors=FALSE)
      masterData(df)
    } else {
      masterData(data.frame(Name=character(), Week=integer()))
    }
  }
  
  # Load when course selected and when refresh buttons pressed
  observeEvent(input$global_course, { load_master_data() })
  observeEvent(input$refresh_manual, { load_master_data() })
  observeEvent(input$refresh_cam, { load_master_data() })
  observeEvent(input$refresh_dash, { load_master_data() })
  observeEvent(input$refresh_grades, { load_master_data() })
  
  # --- MANUAL GRID UI ---
  # --- MANUAL GRID UI (With WhatsApp Button) ---
  output$manual_grid_ui <- renderUI({
    req(input$global_course)
    roster <- get_roster_data()
    if(nrow(roster) == 0) return(h4("No Students Found"))
    files <- if(dir.exists("students")) list.files("students") else character(0)
    
    div(style="display:flex; flex-wrap:wrap; margin: -10px;",
        lapply(1:nrow(roster), function(i) {
          nm <- roster$Name[i]
          # Ensure phone has no spaces and no '+' for the URL link
          raw_phone <- roster$ParentPhone[i]
          clean_phone <- gsub("[^0-9]", "", raw_phone) 
          
          # Create the WhatsApp Link
          # https://wa.me/201222...?text=Your+son...
          msg <- URLencode(paste0("🔔 Attendance Alert: ", nm, " is absent today in ", input$global_course))
          wa_link <- paste0("https://wa.me/", clean_phone, "?text=", msg)
          
          # Image Logic
          clean_search <- gsub("[^[:alnum:]]", "", tolower(nm))
          f <- NA
          for(file in files) if(gsub("[^[:alnum:]]", "", tolower(tools::file_path_sans_ext(file))) == clean_search) { f <- file; break }
          img <- if(!is.na(f)) paste0("student_photos/", URLencode(f)) else "https://via.placeholder.com/150?text=No+Img"
          
          div(style="width: 20%; padding: 10px; min-width:200px;",
              div(class = "student-card", id = paste0("card_", i),
                  tags$img(src = img, class = "student-img"), 
                  div(class = "student-name", nm),
                  
                  # --- The Toggle Switch ---
                  materialSwitch(inputId = paste0("manual_", i), label = "Present", value = FALSE, status = "success"),
                  
                  # --- The New WhatsApp Button ---
                  # Only show if phone number exists
                  if(!is.na(clean_phone) && clean_phone != "") {
                    tags$a(href = wa_link, target = "_blank", class = "btn btn-success btn-xs", 
                           style = "width:100%; margin-top:5px; background-color:#25D366; border:none;",
                           icon("whatsapp"), " Alert Parent")
                  } else {
                    tags$span("No Phone", style="font-size:10px; color:red;")
                  }
              )
          )
        })
    )
  })
  # Update switches when data changes (watch course, week, refresh_manual and masterData)
  observe({
    req(input$global_course)
    req(input$global_week)
    input$refresh_manual    # explicit dependency so pressing the button triggers refresh
    # ensure we react to masterData changes too
    df <- masterData()
    roster <- get_roster_data()
    pres_names <- character(0)
    if(nrow(df) > 0) {
      pres_names <- trimws(toupper(df[df$Week == input$global_week, "Name"]))
    }
    if(nrow(roster) > 0) {
      for(i in 1:nrow(roster)) {
        should_be_present <- trimws(toupper(roster$Name[i])) %in% pres_names
        current_state <- isolate(input[[paste0("manual_", i)]])
        if (!is.null(current_state) && current_state != should_be_present) {
          updateMaterialSwitch(session, paste0("manual_", i), value = should_be_present)
          color <- if(should_be_present) "5px solid #2ecc71" else "5px solid #bdc3c7"
          runjs(paste0("$('#card_", i, "').css('border-top', '", color, "');"))
        }
      }
    }
  })
  
  # Listen to user toggles and update CSV
  observe({
    roster <- get_roster_data(); req(nrow(roster) > 0)
    lapply(1:nrow(roster), function(i) {
      observeEvent(input[[paste0("manual_", i)]], {
        update_manual_attendance(input$global_course, input$global_week, roster$Name[i], roster$ID[i], input[[paste0("manual_", i)]])
        
        # <--- ADD THIS BLOCK --->
        status <- if(input[[paste0("manual_", i)]]) "Present" else "Absent"
        log_msg <- paste("Manual Change:", roster$Name[i], "marked", status, "in", input$global_course)
        log_action(log_msg)
        # <----------------------->
        
        is_p <- input[[paste0("manual_", i)]]
        color <- if(is_p) "5px solid #2ecc71" else "5px solid #bdc3c7"
        runjs(paste0("$('#card_", i, "').css('border-top', '", color, "');"))
        load_master_data()
      }, ignoreInit = TRUE)
    })
  })
  # Camera UI
  output$camera_btn_ui <- renderUI({
    if(input$global_course == "") actionButton("dummy", "Select Course First", disabled=TRUE)
    else actionButton("btn_snap", "LAUNCH CAMERA", icon=icon("play"), style="background:#e74c3c; color:white;")
  })
  
  # When camera runs, run synchronously, then reload data and auto-trigger manual refresh
  observeEvent(input$btn_snap, {
    req(input$global_course)
    log_action(paste("Camera started for course:", input$global_course, "Week:", input$global_week))
    # run engine and wait to finish so CSV is fully written
    system(paste("python engine.py", shQuote(input$global_course), input$global_week), wait=TRUE)
    # reload data
    load_master_data()
    # programmatically "click" Sync Data so UI updates immediately
    shinyjs::click("refresh_manual")
  })
  
  # Session table and dashboard visuals use masterData()
  output$sessionTable <- renderDT({
    df <- masterData()
    if(nrow(df)>0) df %>% filter(Week == input$global_week) %>% select(Name, ArrivalTime, Status) else NULL
  })
  output$box_present <- renderValueBox({
    df <- masterData(); p <- if(nrow(df)>0) sum(df$Week == input$global_week) else 0
    valueBox(p, "Present", icon=icon("check"), color="green")
  })
  output$box_total <- renderValueBox({ valueBox(nrow(get_roster_data()), "Students", icon=icon("users"), color="navy") })
  output$box_risk <- renderValueBox({ valueBox("Check", "At Risk", icon=icon("bell"), color="red") })
  output$trendPlot <- renderPlot({ df <- masterData(); if(nrow(df)>0) ggplot(df, aes(x=factor(Week))) + geom_bar() + theme_minimal() })
  output$piePlot <- renderPlot({
    df <- masterData(); roster <- get_roster_data()
    p <- if(nrow(df)>0) sum(df$Week==input$global_week) else 0
    ggplot(data.frame(L=c("Present","Absent"), V=c(p, max(0, nrow(roster)-p))), aes(x="",y=V,fill=L)) + geom_bar(stat="identity")+coord_polar("y")+theme_void()
  })
  
  # --- STATS / GRADES ---
  get_stats <- reactive({
    df <- masterData()
    roster <- get_roster_data()
    curr_week <- as.integer(input$global_week)
    
    if(nrow(roster) == 0) return(NULL)
    if(nrow(df) > 0 && !"NameUpper" %in% colnames(df)) df$NameUpper <- toupper(df$Name)
    
    roster %>% rowwise() %>% mutate(
      NameUpper = toupper(Name),
      Present_Count = if(nrow(df) > 0) sum(df$NameUpper == NameUpper) else 0,
      Late_Count = if(nrow(df) > 0 && "ArrivalTime" %in% names(df)) sum(df$NameUpper == NameUpper & df$ArrivalTime > "13:00:00") else 0,
      Absences = max(0, curr_week - Present_Count),
      Att_Mark = case_when(Absences == 0 ~ 5, Absences == 1 ~ 4, Absences == 2 ~ 2.5, TRUE ~ 0),
      Arr_Mark_Raw = 5 - (Late_Count * 0.5),
      Arr_Mark = max(0, Arr_Mark_Raw),
      Status = case_when(Absences >= 4 ~ "WITHDRAWN", Absences == 3 ~ "WARNING", TRUE ~ "Active"),
      Total_Marks = ifelse(Status == "WITHDRAWN", 0, Att_Mark + Arr_Mark)
    ) %>% select(Name, ID, Present_Count, Late_Count, Absences, Absent_Marks = Att_Mark, Late_Marks = Arr_Mark, Total_Marks, Status)
  })
  
  output$gradesTable <- renderDT({
    stats <- get_stats()
    if(is.null(stats)) return(NULL)
    datatable(stats, options = list(pageLength = 10)) %>%
      formatStyle("Status", backgroundColor = styleEqual(c("WITHDRAWN","WARNING"), c("#ffcccc","#ffffcc")))
  })
  
  # Email warnings using the stats
  observeEvent(input$btn_email, {
    req(input$global_course); roster <- get_roster_data(); df <- masterData(); curr_week <- input$global_week
    if(nrow(roster) == 0) { showNotification("No students in database", type="warning"); return() }
    cnt <- 0
    withProgress(message = "Checking Attendance...", value = 0, {
      for(i in 1:nrow(roster)) {
        name <- roster$Name[i]; email <- roster$Email[i]
        pres_count <- if(nrow(df) > 0) sum(trimws(toupper(df$Name)) == trimws(toupper(name))) else 0
        absences <- max(0, curr_week - pres_count)
        type <- NULL
        if(absences == 3) type <- "WARNING"
        if(absences >= 4) type <- "WITHDRAWN"
        if(!is.null(type)) { if(send_alert_email(name, email, type, input$global_course)) cnt <- cnt + 1 
        log_action(paste("Email sent to:", name, "Type:", type))}
        incProgress(1/nrow(roster))
      }
    })
    showNotification(paste("Job Done. Sent", cnt, "emails."), type="message")
  })
  ######3333
  # reactive dataset for camera attendance
  cameraData <- reactiveVal(data.frame())
  
  loadCameraData <- function() {
    clean <- gsub("[^[:alnum:]]", "_", input$global_course)
    path <- paste0("data/", clean, ".csv")
    
    if (file.exists(path)) {
      df <- read.csv(path, stringsAsFactors = FALSE)
      cameraData(df)
    }
  }
  observeEvent(c(input$global_course, input$global_week), {
    loadCameraData()
  })
  observeEvent(input$btn_snap, {
    system(
      paste("python engine.py", shQuote(input$global_course), input$global_week),
      wait = TRUE
    )
    loadCameraData()
    shinyjs::click("refresh_manual")
  })
  output$camera_table <- renderDT({
    df <- cameraData()
    if (nrow(df) == 0) return(NULL)
    
    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  
  # PDF report (uses get_stats())
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Attendance_Report_", 
             gsub("[^[:alnum:]]", "_", input$global_course), 
             "Week", input$global_week, "", 
             format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Show progress
      showNotification("Generating PDF report...", type = "message", duration = 3)
      
      # Check if report.Rmd exists
      if (!file.exists("report.Rmd")) {
        showNotification("Error: report.Rmd file not found in app directory!", 
                         type = "error", duration = 10)
        return(NULL)
      }
      
      # Copy the report template to a temporary directory
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Prepare data
      stats_data <- get_stats()
      risk_data <- if(!is.null(stats_data) && nrow(stats_data) > 0) {
        stats_data %>% filter(Status %in% c("WARNING", "WITHDRAWN"))
      } else {
        data.frame()
      }
      
      # Render the document
      tryCatch({
        rmarkdown::render(
          input = tempReport,
          output_file = file,
          output_format = "pdf_document",
          params = list(
            course = input$global_course,
            week = input$global_week,
            data = stats_data,
            risk_students = risk_data
          ),
          envir = new.env(parent = globalenv()),
          quiet = FALSE  # Set to FALSE to see errors
        )
        showNotification("✅ PDF generated successfully!", type = "message")
      }, error = function(e) {
        showNotification(
          paste("❌ PDF generation failed:", e$message), 
          type = "error", 
          duration = 10
        )
        print(e)  # Print full error to console
      })
    },
    contentType = "application/pdf"
  )
  # --- AI PREDICTION PLOT ---
  output$predictPlot <- renderPlot({
    # 1. Gather Data (Re-using the logic from Grades)
    roster <- get_roster_data()
    df <- masterData()
    curr <- input$global_week
    
    if(nrow(roster) == 0) return(NULL)
    if(nrow(df) > 0) df$NameUpper <- toupper(df$Name)
    
    # 2. Calculate Stats for plotting
    plot_data <- roster %>% rowwise() %>% mutate(
      P = if(nrow(df)>0) sum(trimws(toupper(df$NameUpper)) == trimws(toupper(Name))) else 0,
      L = if(nrow(df)>0) sum(trimws(toupper(df$NameUpper)) == trimws(toupper(Name)) & df$ArrivalTime > "13:00:00") else 0,
      A = max(0, curr - P),
      # Calculate Grade (0-10)
      AttM = case_when(A==0~5, A==1~4, A==2~2.5, TRUE~0),
      ArrM = max(0, 5-(L*0.5)),
      Status = case_when(A>=4~"WITHDRAWN", A==3~"WARNING", TRUE~"Active"),
      Grade = ifelse(Status=="WITHDRAWN", 0, AttM+ArrM)
    )
    
    # 3. Draw Linear Regression Graph
    # X axis = Days Present, Y axis = Grade
    ggplot(plot_data, aes(x = P, y = Grade)) +
      geom_point(aes(color = Status), size = 5, alpha=0.7) + # Dots
      geom_smooth(method = "lm", color = "#2c3e50", fill = "#bdc3c7", size=1.5) + # The AI Prediction Line
      labs(x = "Lectures Attended", y = "Predicted Grade (Out of 10)") +
      scale_color_manual(values = c("Active"="green", "WARNING"="orange", "WITHDRAWN"="red")) +
      theme_minimal() +
      theme(text = element_text(size=14, face="bold"))
  })
  
  
  # --- 4. RENDER HEATMAP ---
  output$heatmapPlot <- renderPlot({
    # Load ALL data files for this course to build history
    clean_name <- gsub("[^[:alnum:]]", "_", input$global_course)
    file_path <- paste0("data/", clean_name, ".csv")
    
    if(!file.exists(file_path)) return(NULL)
    
    df <- read.csv(file_path, stringsAsFactors = FALSE)
    roster <- get_roster_data()
    
    # Create a full grid (All Students x All Weeks)
    all_weeks <- 1:15
    grid <- expand.grid(Name = roster$Name, Week = all_weeks)
    
    # Merge with actual attendance
    df$Name <- trimws(toupper(df$Name))
    grid$NameUpper <- trimws(toupper(grid$Name))
    
    merged <- left_join(grid, df, by = c("NameUpper" = "Name", "Week" = "Week"))
    
    # Determine Status for Plotting
    merged$Status_Plot <- ifelse(!is.na(merged$ArrivalTime), "Present", "Absent")
    
    ggplot(merged, aes(x = factor(Week), y = Name, fill = Status_Plot)) +
      geom_tile(color = "white") +
      scale_fill_manual(values = c("Present" = "#2ecc71", "Absent" = "#e74c3c")) +
      labs(x = "Week Number", y = "Student Name", fill = "Status") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))
  }) 
  # --- TAB 5: LOG VIEWER LOGIC ---
  output$log_viewer <- renderText({
    # Re-run this whenever the refresh button is clicked
    input$refresh_logs 
    
    log_file <- "system_audit.log"
    
    if (file.exists(log_file)) {
      # Read the file and join lines with newlines
      logs <- readLines(log_file)
      # Reverse order to show newest first (optional)
      paste(rev(logs), collapse = "\n")
    } else {
      "No system logs found yet."
    }
  })
  # --- VOICE TRIGGER LOGIC ---
  observeEvent(input$btn_voice_activate, {
    req(input$global_course) # Ensure course is selected
    
    # 1. Update UI to show listening status
    shinyjs::html("voice_status", "🎤 LISTENING... Say 'Open Camera' or 'Close Camera'")
    shinyjs::runjs("$('#voice_status').css('color', 'red');")
    
    # 2. Run the python trigger script (This freezes R for ~4 seconds while listening)
    result <- tryCatch({
      system("python trigger.py", wait = TRUE)
    }, warning = function(w) {
      return(1) # Return failure on warning
    })
    
    # 3. Check result and handle different commands
    if (result == 0) {
      # OPEN CAMERA (exit code 0)
      shinyjs::html("voice_status", "✅ Opening Camera...")
      shinyjs::runjs("$('#voice_status').css('color', 'green');")
      
      shinyjs::delay(1000, {
        shinyjs::click("btn_snap") 
        shinyjs::html("voice_status", "Click 'Voice Command' and say 'Open Camera' or 'Close Camera'")
        shinyjs::runjs("$('#voice_status').css('color', '#7f8c8d');")
      })
      
    } else if (result == 2) {
      # CLOSE CAMERA (exit code 2)
      shinyjs::html("voice_status", "✅ Closing Camera...")
      shinyjs::runjs("$('#voice_status').css('color', 'orange');")
      
      # --- FIX: Write to the file that engine.py is watching ---
      writeLines("CLOSE", "voice_command.txt") 
      # -------------------------------------------------------
      
      shinyjs::delay(1500, {
        shinyjs::html("voice_status", "Camera closed. Click 'Voice Command' to reopen.")
        shinyjs::runjs("$('#voice_status').css('color', '#7f8c8d');")
      })
      
    } else {
      # ERROR or unrecognized command (exit code 1)
      shinyjs::html("voice_status", "❌ Command failed or timeout. Try again.")
      shinyjs::runjs("$('#voice_status').css('color', 'red');")
    }
  })
  
}

shinyApp(ui, server)