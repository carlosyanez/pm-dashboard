
library(tidyverse)

library(semantic.dashboard)
library(htmltools)


#########################

source("./r/variables.R")
saveRDS(app_vars, file="files/app_vars.rds") 
rendered_data <-reactiveValues()
source("./r/render.R", echo = F, prompt.echo = "", spaced = F)


###UI Tabs

tab_home_1.0 <- fluidRow(h1("Project's Dashboard"))
tab_home_1.1 <- fluidRow(h2("version 1"))
tab_home_1.2 <- fluidRow(textOutput("update_msg"))
tab_home_1.3 <- fluidRow(actionButton("refresh_data", "Refresh Data"))

tab_summary_1.0 <-fluidRow(
    h1("Programme's Synoptic View")
)
tab_summary_1.1 <-fluidRow(
    semantic.dashboard::valueBox("Backlog", textOutput("summary1.Backlog"), icon("hourglass outline"), color = "blue", width = 2,size="tiny"),
    semantic.dashboard::valueBox("Planning", textOutput("summary1.Planning"), icon("hourglass start"), color = "blue", width = 2,size="tiny"),
    semantic.dashboard::valueBox("In Progress", textOutput("summary1.InProgress"), icon("hourglass half"), color = "blue", width = 2,size="tiny"),
    semantic.dashboard::valueBox("Complete", textOutput("summary1.Complete"), icon("hourglass start"), color = "blue", width = 2,size="tiny"),
    semantic.dashboard::valueBox("R", textOutput("summary1.R"), icon("exclamation triangle"), color = "red", width = 2,size="tiny"),
    semantic.dashboard::valueBox("A", textOutput("summary1.A"), icon("exclamation circle"), color = "orange", width = 2,size="tiny"),
    semantic.dashboard::valueBox("G", textOutput("summary1.G"), icon("exclamation"), color = "green", width = 2,size="tiny")  
)

tab_summary_1.2 <-fluidRow(
    h3("Programme Kanban")
)

tab_summary_1.3 <- fluidRow(
    
    semantic.dashboard::column(4,style=paste("background-color:",app_vars$c.background1,";",sep=""),                      
                               semantic.dashboard::box(title="Backlog",color = app_vars$kanban_backlog,
                                                       ribbon = TRUE,
                                                       title_side = "top left",
                                                       collapsible = FALSE,
                                                       tableOutput("project_list_backlog"))),
    semantic.dashboard::column(4,style=paste("background-color:",app_vars$c.background1,";",sep=""),                      
                               semantic.dashboard::box(title="Planning",color = app_vars$kanban_planning,
                                                       ribbon = TRUE,
                                                       title_side = "top left",
                                                       collapsible = FALSE,
                                                       width=4,tableOutput("project_list_planning"))),
    semantic.dashboard::column(4,style=paste("background-color:",app_vars$c.background1,";",sep=""),                      
                               semantic.dashboard::box(title="In Progress",color = app_vars$kanban_progress,
                                                       ribbon = TRUE,
                                                       title_side = "top left",
                                                       collapsible = FALSE,
                                                       width=4,tableOutput("project_list_active"))),
    semantic.dashboard::column(4,style=paste("background-color:",app_vars$c.background1,";",sep=""),                      
                               semantic.dashboard::box(title="Complete",color = app_vars$kanban_complete,
                                                       ribbon = TRUE,
                                                       title_side = "top left",
                                                       collapsible = FALSE,
                                                       width=4,tableOutput("project_list_complete")))
)

tab_summary_1.4 <-fluidRow(
    h3("Roadmap")
)

tab_summary_1.5 <-fluidRow(
    semantic.dashboard::box(title="Date Selection",
                            width=16,collapsible = TRUE,
                            color=app_vars$c.background2,
    uiOutput("date_range"))
)

tab_summary_1.6 <-  fluidRow(
    semantic.dashboard::box(title="Roadmap",
                            width=16,collapsible = FALSE,
                            color=app_vars$c.background2,
                           
                            plotly::plotlyOutput("projects_roadmap"))
)

tab_summary_2.0 <- fluidRow(
    h1("High Level Report")
)

tab_summary_2.1  <-fluidRow(
semantic.dashboard::valueBox("Backlog", textOutput("summary2.Backlog"), icon("hourglass outline"), color = "blue", width = 2,size="tiny"),
semantic.dashboard::valueBox("Planning", textOutput("summary2.Planning"), icon("hourglass start"), color = "blue", width = 2,size="tiny"),
semantic.dashboard::valueBox("In Progress", textOutput("summary2.InProgress"), icon("hourglass half"), color = "blue", width = 2,size="tiny"),
semantic.dashboard::valueBox("Complete", textOutput("summary2.Complete"), icon("hourglass start"), color = "blue", width = 2,size="tiny"),
semantic.dashboard::valueBox("R", textOutput("summary2.R"), icon("exclamation triangle"), color = "red", width = 2,size="tiny"),
semantic.dashboard::valueBox("A", textOutput("summary2.A"), icon("exclamation circle"), color = "orange", width = 2,size="tiny"),
semantic.dashboard::valueBox("G", textOutput("summary2.G"), icon("exclamation"), color = "green", width = 2,size="tiny")  
)

tab_summary_2.2 <- fluidRow(
    semantic.dashboard::valueBox("Late Start", textOutput("summary2.late.start"), icon("exclamation"), color = "violet", width = 2,size="tiny"),
    semantic.dashboard::valueBox("Overdue", textOutput("summary2.overdue"), icon("exclamation"), color = "violet", width = 2,size="tiny"),
    semantic.dashboard::valueBox("Late Tasks",textOutput("summary2.late.tasks"), icon("hourglass outline"), color = "violet", width = 2,size="tiny"),
    semantic.dashboard::valueBox("R Issues", textOutput("summary2.issue.R"), icon("exclamation"), color = "red", width = 2,size="tiny"),
    semantic.dashboard::valueBox("A Issues", textOutput("summary2.issue.A"), icon("exclamation"), color = "orange", width = 2,size="tiny"),
    semantic.dashboard::valueBox("Op Actions", textOutput("summary2.actions"), icon("hourglass outline"), color = "violet", width = 2,size="tiny")
)

tab_summary_2.3 <- fluidRow(
    h3("Projects")
)
tab_summary_2.4 <- fluidRow(
    semantic.dashboard::box(title="Projects",width=16,color="blue",
                            tableOutput("projects"))
)
tab_summary_2.5 <- fluidRow(
    h3("Issues")
)

tab_summary_2.6 <- fluidRow(
    semantic.dashboard::box(title="Issues",width=16,color="red",
                            tableOutput("issues_summary"))
)

tab_summary_2.7 <- fluidRow(
    h3("Actions")
)

tab_summary_2.8 <- fluidRow(
    semantic.dashboard::box(title="Upcoming Tasks",width=12,color="violet",
                            tableOutput("actions_summary"))
)

tab_daily_summary_1.0 <- fluidRow(
    h1("Daily Tracker")
)

tab_daily_summary_1.1 <- column(10,
                                semantic.dashboard::box(title="Item Tracker",color="blue",
                                                        tableOutput("consolidated_tasks"))
)

tab_daily_summary_1.2 <- column(6,
                                h4("RAG Distribution"),
                                semantic.dashboard::valueBox("R", textOutput("stats_dailyR"), icon("exclamation"), color = "red", width = 2,size="tiny"),
                                br(),
                                semantic.dashboard::valueBox("A", textOutput("stats_dailyA"), icon("exclamation"), color = "orange", width = 2,size="tiny"),
                                br(),
                                semantic.dashboard::valueBox("G", textOutput("stats_dailyG"), icon("exclamation"), color = "green", width = 2,size="tiny"),
                                br(),
                                semantic.dashboard::box(title="Project Distribution",color="blue",
                                                        tableOutput("project_freq")),
                                br(),
                                semantic.dashboard::box(title="People's Workload",color="blue",
                                                        tableOutput("people_freq"))
                                
)

tab_pdetails_1.0 <- fluidRow(
  column(4,value_box_output("RAG_value_box")
  ),
                            column(1,"Select Project:"),
                            column(2,
                            
                            semantic.dashboard::dropdownMenu(icon=icon("cog"), 
                                                             selectInput(inputId = 'project_selection',
                                                                         label = 'Select Project',
                                                                         choices = c("None - Loading"))))
                            
                           
                               
  )

tab_pdetails_1.1 <- fluidRow(
  column(16,
  semantic.dashboard::box(title="Project",color="blue",
                         h1(htmlOutput("project.project_name")))
  )
)

tab_pdetails_1.2 <- fluidRow(
    
    semantic.dashboard::valueBox("Progress", textOutput("project_taskprogress"), icon("battery three quarters"), color = "green", width = 2,size="tiny"),
    semantic.dashboard::valueBox("Tasks - R", textOutput("project_taskR"), icon("exclamation triangle"), color = "red", width = 2,size="tiny"),
    semantic.dashboard::valueBox("Tasks - A", textOutput("project_taskA"), icon("exclamation circle"), color = "orange", width = 2,size="tiny"),
    semantic.dashboard::valueBox("Tasks - G", textOutput("project_taskG"), icon("thumbs up outline"), color = "green", width = 2,size="tiny"),
    semantic.dashboard::valueBox("Complete", textOutput("project_taskg"), icon("check"), color = "grey", width = 2,size="tiny"),
    semantic.dashboard::valueBox("Op Issues", textOutput("project_issueopen"), icon("bullhorn"), color = "violet", width = 2,size="tiny"),
    semantic.dashboard::valueBox("Op Actions", textOutput("project_actionopen"), icon("tasks"), color = "violet", width = 2,size="tiny")
    
)

tab_pdetails_1.3 <- fluidRow(
    semantic.dashboard::box(title="Scope",color = "blue",
                            ribbon = TRUE,
                            title_side = "top left",
                            collapsible = FALSE,
                            width=4,textOutput("project.project_scope")),
    semantic.dashboard::box(title="Objectives",color = "blue",
                            ribbon = TRUE,
                            title_side = "top left",
                            collapsible = FALSE,
                            width=4,textOutput("project.project_objectives")),
    semantic.dashboard::box(title="Key People",color = "blue",
                            ribbon = TRUE,
                            title_side = "top left",
                            collapsible = FALSE,
                            width=4,tableOutput("project.project_people")),
    semantic.dashboard::box(title="Key Dates",color = "blue",
                            ribbon = TRUE,
                            title_side = "top left",
                            collapsible = FALSE,
                            width=4,tableOutput("project.project_dates"))
    
)

tab_pdetails_1.4 <- fluidRow(
  semantic.dashboard::box(title="Issues",width=16,color="red",
                          tableOutput("project.project_issues"))
  )

tab_pdetails_1.5 <- fluidRow(#comments and actions

  semantic.dashboard::box(title="Actions",width=8,color="blue",
                          tableOutput("project.project_actions")),
  
  semantic.dashboard::box(title="Updates",width=8,color="blue",
                          tableOutput("project.project_updates"))
  
  )

tab_pdetails_1.6 <- fluidRow(#tasks
  semantic.dashboard::box(title="Tasks",width=8,color="violet",
                          tableOutput("project.project_tasks")),
  semantic.dashboard::box(title="Timeline",width=8,
                          collapsible = TRUE,
                          color=app_vars$c.background2,
                           plotly::plotlyOutput("project.project_plan"))
  
  )

tab_dashboard_1.0 <- fluidRow(
  h1("Reports")
  
 )

tab_dashboard_1.1 <- fluidRow(
                        column(4,h4("Download Snapshot :::")), 
                        column(4,downloadButton("downloadData", "Download Report"))
  
)

tab_dashboard_1.2 <- fluidRow(
  h2("Save Reports")
  
)
tab_dashboard_1.3 <- fluidRow(

                         column(4,textInput("version_name", "Type Version Name", "Project Name")),
       
                         column(4,actionButton("save_version", "Save Version")),
            
                         column(4,uiOutput("dashboard_saved_report")),
                 
                         column(4,uiOutput("clip"))
              
)

tab_dashboard_1.4 <- fluidRow(
  h2("Load Saved Report")
  
)

tab_dashboard_1.5 <- fluidRow(
  
  column(6,selectInput(inputId = 'report_selection',
                       label = 'Select Version',
                       choices = c("current"))),
  column(10,"Results",
         tableOutput("past_report"))
  
  
)

# Define UI for application that draws a histogram

header <- semantic.dashboard::dashboardHeader(title = "Projects Dashboard", inverted=TRUE)

sidebar <- semantic.dashboard::dashboardSidebar(sidebarMenu(
    menuItem(tabName = "home", text = "Home", icon = icon("home")),
    menuItem(tabName = "summary_1", text = "Synoptic", icon = icon("compass")) ,
    menuItem(tabName = "summary_2", text = "Report", icon = icon("clipboard outline")),
    menuItem(tabName = "project_details", text = "Project Details", icon = icon("file alternate outline")),
    menuItem(tabName = "daily_view", text = "Daily Tracker", icon = icon("calendar check outline")) ,  
    menuItem(tabName = "flexdashboard", text = "Download Report", icon = icon("download")),
    menuItem(tabName = "about", text = "About", icon = icon("copyright outline icon"))
    
),inverted=TRUE)

body <-   semantic.dashboard::dashboardBody(
    tabItems(
        tabItem("home",
                tab_home_1.0,
                tab_home_1.1,
                tab_home_1.2,
                tab_home_1.3),
        tabItem("summary_1",
                tab_summary_1.0,
                tab_summary_1.1,
                tab_summary_1.2,
                tab_summary_1.3,
                tab_summary_1.4,
       #         tab_summary_1.5,
                 tab_summary_1.6),
        tabItem("summary_2",
                tab_summary_2.0,
                tab_summary_2.1,
                tab_summary_2.2,
                tab_summary_2.3,
                tab_summary_2.4,
                tab_summary_2.5,
                tab_summary_2.6,
                tab_summary_2.7,
                tab_summary_2.8),
        tabItem("project_details",
                tab_pdetails_1.0,
                tab_pdetails_1.1,
                tab_pdetails_1.2,
                tab_pdetails_1.3,
                tab_pdetails_1.4,
                tab_pdetails_1.5,
                tab_pdetails_1.6
                ),
        tabItem("daily_view",
                tab_daily_summary_1.0,
                fluidRow(
                    tab_daily_summary_1.1,
                    tab_daily_summary_1.2)),
       tabItem("flexdashboard",
                tab_dashboard_1.0,
                tab_dashboard_1.1,
               tab_dashboard_1.2,
               tab_dashboard_1.3,
               tab_dashboard_1.4,
               tab_dashboard_1.5),
        tabItem("about",
                includeHTML("about.html"))
    )
)

ui <-  semantic.dashboard::dashboardPage(header,sidebar,body,theme = app_vars$theme)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
   if(file.exists(app_vars$Vdata_file)){
     snapshots_version <- readRDS(file=app_vars$Vdata_file)
   }else{
     snapshots_version <- tibble(version=c("current"),timestamp=c(1))
     saveRDS(snapshots_version, file=app_vars$Vdata_file) 
   }

   v_FlexLoc <- reactiveValues()
   v_FlexLoc$text <- ""

    message("Initial Load")
    
    observeEvent(input$date_from,
                 {
                     if(input$date_from==""){
                         date_from <- lubridate::as_date(cut(app_vars$today, "month"))
                     }else
                     {
                         date_from <- lubridate::as_date(input$date_from)
                     }
                     
                     if(input$date_to==""){
                         date_to <- lubridate::as_date(cut(app_vars$today, "month") + lubridate::dweeks(24))
                     }else
                     {
                         date_to <- lubridate::as_date(input$date_to)
                     }
                     
                     rendered_data$render.roadmap <- projects_roadmap(presentation_data,app_vars,
                                                                      date_from,
                                                                      date_to)
                     
                 })
    
    observeEvent(input$date_to,
                 {
                     if(input$date_from==""){
                         date_from <- lubridate::as_date(cut(app_vars$today, "month"))
                     }else
                     {
                         date_from <- lubridate::as_date(input$date_from)
                     }
                     
                     if(input$date_to==""){
                         date_to <- lubridate::as_date(cut(app_vars$today, "month") + lubridate::dweeks(24))
                     }else
                     {
                         date_to <- lubridate::as_date(input$date_to)
                     }
                     
                     rendered_data$render.roadmap <- projects_roadmap(presentation_data,app_vars,
                                                                      date_from,
                                                                      date_to)
                     
                 })
    
    observeEvent(input$refresh_data,
                 {
                     message(paste("Starting Refresh"))
                     
                     source("./r/trello.R", echo = F, prompt.echo = "", spaced = F)
                     normalised_data$data_retrieved <- lubridate::as_date(lubridate::now())
                     saveRDS(normalised_data, file=app_vars$Ndata_file) 
                     normalised_data <-readRDS(file=app_vars$Ndata_file)

                    
                     message("Data Sourced:")
                     message(nrow(normalised_data$projects))
                     
                     source("./r/eval.R", echo = F, prompt.echo = "", spaced = F)
                     
                     message("Data Evaluated")
                     message(nrow(presentation_data$projects))


                     presentation_data$data_retrieved <- lubridate::now()
                     rendered_data$data_retrieved <- lubridate::now()
                     
                     saveRDS(normalised_data, file=app_vars$Ndata_file) 
                     saveRDS(presentation_data, file=app_vars$Pdata_file) 
                     
                     rendered_data$render.kanban <- project_kanban_tables(presentation_data,app_vars)
                     rendered_data$render.roadmap <- projects_roadmap(presentation_data,app_vars,lubridate::as_date(cut(app_vars$today, "month")),
                                                                     (lubridate::as_date(cut(app_vars$today, "month"))+lubridate::dweeks(24)))
                     rendered_data$render.projects <- projects_summary(presentation_data,app_vars)
                     rendered_data$render.issues <- issues_summary(presentation_data,app_vars)
                     rendered_data$render.actions <- actions_summary(presentation_data,app_vars)
                     rendered_data$render.tasks  <- consolidated_tasks(presentation_data,app_vars)
                     rendered_data$render.people_freq <- people_freq(presentation_data,app_vars)
                     rendered_data$render.project_freq <- project_freq(presentation_data,app_vars)
                     rendered_data$render.stats_summary1 <- stats_summary1(presentation_data)
                     rendered_data$render.stats_summary2 <- stats_summary2(presentation_data)
                     rendered_data$render.stats_daily <- stats_daily(presentation_data)
                     rendered_data$render.update_msg <- last_update_date(rendered_data)
                     
                     rendered_data$render.project_data <- project_data(presentation_data)
                     rendered_data$render.project_tasks <- project_tasks(presentation_data,app_vars)
                     rendered_data$render.project_plan <- project_plan(presentation_data)
                     rendered_data$render.project_issues <- issues_summary(presentation_data,app_vars,project_name = "-1")
                     rendered_data$render.project_actions <- actions_summary(presentation_data,app_vars,project_name = "-1",
                                                                             min_action_width="1em")
                     rendered_data$render.project_updates <-  project_updates(presentation_data,app_vars)
                     
                    # #rendered_data$render.past_report<- past_reports(version_parameter="current",
                    #                                                 #app_vars,snapshots_version)
                     
                     saveRDS(rendered_data, file=app_vars$Rdata_file) 
                     updateSelectInput(session, "project_selection",
                                       choices =  presentation_data$projects %>% pull(Name))
                     
                     
                     message(paste("Refresh completed",rendered_data$data_retrieved))
                 })
    
    
    observeEvent(input$project_selection,
                 {
                     rendered_data$render.project_data <- project_data(presentation_data,
                                                                       selected_project=input$project_selection)
                     rendered_data$render.project_tasks <- project_tasks(presentation_data,app_vars,
                                                                         project_name=input$project_selection)
                     rendered_data$render.project_plan <- project_plan(presentation_data,
                                                                       project_name=input$project_selection)
                     rendered_data$render.project_issues <- issues_summary(presentation_data,app_vars,
                                                                           project_name=input$project_selection)
                     rendered_data$render.project_actions <- actions_summary(presentation_data,app_vars,
                                                                             project_name=input$project_selection,
                                                                             min_action_width="1em")
                     rendered_data$render.project_updates <-  project_updates(presentation_data,app_vars,
                                                                              project_name=input$project_selection)
                     
                     
                 })
    
    ##Save report version
    observeEvent(input$save_version,
                 {
                   ts_value <- as.numeric(lubridate::now())
                   snapshots_version <- add_row(snapshots_version,
                                                version=isolate(input$version_name),timestamp=ts_value)
                   saveRDS(snapshots_version, file=app_vars$Vdata_file) 
                   updateSelectInput(session, "report_selection",
                                     choices =  snapshots_version %>% pull(version))
                   
                   v_RData <- paste("./files/rendered_data_",ts_value,".rds",sep="")
                   v_PData <- paste("./files/presentation_data_",ts_value,".rds",sep="")
                   v_FlexName <- paste(app_vars$html_loc,"FlexDashboard_",ts_value,".html", sep = "") 
                  
                   outfile <- rmarkdown::render(app_vars$ReportFlex,
                                                output_format = flexdashboard::flex_dashboard(theme = app_vars$theme_flex,
                                                                                              orientation="rows",
                                                                                              social="menu"
                                                ))
                   file.copy(outfile, v_FlexName)
                   file.remove(outfile) 
                   
                   saveRDS(presentation_data, file=v_PData) 
                   saveRDS(rendered_data, file=v_RData)
                   
                   v_FlexLoc$text <- paste(app_vars$URL,"reports/","FlexDashboard_",ts_value,".html", sep = "") 
                   updateSelectInput(session, "report_selection",
                                     choices =  snapshots_version %>% pull(version))
                   
                 })
    
    
    observeEvent(input$report_selection,
                 {
                   if(length(input$report_selection)==0){
                     version_value <- "current"
                   }else{
                     version_value <- input$report_selection
                   }
                   
                   rendered_data$render.past_report<- past_reports(version_parameter=version_value,
                                                                   app_vars,snapshots_version)    
                   
                 })
    
    
    ### Load Files
    if(file.exists(app_vars$Rdata_file)){
        presentation_data <-readRDS(file=app_vars$Pdata_file)
        rendered_data <-readRDS(file=app_vars$Rdata_file)
        message(paste("Rendered data loaded",isolate(rendered_data$data_retrieved)))
        if(!("data_retrieved" %in% "data_retrieved")){
            isolate(rendered_data$data_retrieved) <- lubridate::now() - lubridate::ddays(100000)
            
        }
        
        ageing_check <- ( (isolate(rendered_data$data_retrieved) + app_vars$auto_refresh) > lubridate::as_date(lubridate::now()) )
        
        if(ageing_check){
            rendered_data <-readRDS(file=app_vars$Rdata_file)
            
            #rendered_data$render.past_report<- past_reports(version_parameter="current",
                                                            #app_vars,snapshots_version) 
            updateSelectInput(session, "project_selection",
                              choices =  presentation_data$projects %>% pull(Name))
            
            updateSelectInput(session, "report_selection",
                              choices =  snapshots_version %>% pull(version))
            
        }else
        {
            source("./r/trello.R", echo = F, prompt.echo = "", spaced = F)
            source("./r/eval.R", echo = F, prompt.echo = "", spaced = F)
            
            
            normalised_data$data_retrieved <- lubridate::now()
            presentation_data$data_retrieved <- lubridate::now()
            
            saveRDS(normalised_data, file=app_vars$Ndata_file)  
            saveRDS(presentation_data, file=app_vars$Pdata_file) 
            
            
            rendered_data$data_retrieved <- lubridate::now()
            
            rendered_data$render.kanban = project_kanban_tables(presentation_data,app_vars)
            rendered_data$render.roadmap <- projects_roadmap(presentation_data,app_vars,lubridate::as_date(cut(app_vars$today, "month")),
                                                             (lubridate::as_date(cut(app_vars$today, "month"))+lubridate::dweeks(24)))
            rendered_data$render.projects <- projects_summary(presentation_data,app_vars)
            rendered_data$render.issues <- issues_summary(presentation_data,app_vars)
            rendered_data$render.actions <- actions_summary(presentation_data,app_vars)
            rendered_data$render.tasks <- consolidated_tasks(presentation_data,app_vars)
            rendered_data$render.people_freq <- people_freq(presentation_data,app_vars)
            rendered_data$render.project_freq <- project_freq(presentation_data,app_vars)
            rendered_data$render.stats_summary1 <- stats_summary1(presentation_data)
            rendered_data$render.stats_summary2 <- stats_summary2(presentation_data)
            rendered_data$render.stats_daily <- stats_daily(presentation_data)
            rendered_data$render.update_msg <- last_update_date(rendered_data)
            
            rendered_data$render.project_data <- project_data(presentation_data)
            rendered_data$render.project_tasks <- project_tasks(presentation_data,app_vars)
            rendered_data$render.project_plan <- project_plan(presentation_data)
            rendered_data$render.project_issues <- issues_summary(presentation_data,app_vars,project_name = "-1")
            rendered_data$render.project_actions <- actions_summary(presentation_data,app_vars,project_name = "-1",
                                                                    min_action_width="1em")
            rendered_data$render.project_updates <-  project_updates(presentation_data,app_vars)
            
            #rendered_data$render.past_report<- past_reports(version_parameter="current",
                                                            #app_vars,snapshots_version) 
            
            saveRDS(rendered_data, file=app_vars$Rdata_file) 
            updateSelectInput(session, "project_selection",
                              choices =  presentation_data$projects %>% pull(Name))
            updateSelectInput(session, "report_selection",
                              choices =  snapshots_version %>% pull(version))
            
            
        }
        
        
    
        
        }else{
            source("./r/trello.R", echo = F, prompt.echo = "", spaced = F)
            source("./r/eval.R", echo = F, prompt.echo = "", spaced = F)
            
            
            normalised_data$data_retrieved <- lubridate::now()
            presentation_data$data_retrieved <- lubridate::now()
            
            saveRDS(normalised_data, file=app_vars$Ndata_file)  
            saveRDS(presentation_data, file=app_vars$Pdata_file) 
            
            
            rendered_data$data_retrieved <- lubridate::now()
            
            rendered_data$render.kanban = project_kanban_tables(presentation_data,app_vars)
            rendered_data$render.roadmap <- projects_roadmap(presentation_data,app_vars,lubridate::as_date(cut(app_vars$today, "month")),
                                                             (lubridate::as_date(cut(app_vars$today, "month"))+lubridate::dweeks(24)))
            rendered_data$render.projects <- projects_summary(presentation_data,app_vars)
            rendered_data$render.issues <- issues_summary(presentation_data,app_vars)
            rendered_data$render.actions <- actions_summary(presentation_data,app_vars)
            rendered_data$render.tasks <- consolidated_tasks(presentation_data,app_vars)
            rendered_data$render.people_freq <- people_freq(presentation_data,app_vars)
            rendered_data$render.project_freq <- project_freq(presentation_data,app_vars)
            rendered_data$render.stats_summary1 <- stats_summary1(presentation_data)
            rendered_data$render.stats_summary2 <- stats_summary2(presentation_data)
            rendered_data$render.stats_daily <- stats_daily(presentation_data)
            rendered_data$render.update_msg <- last_update_date(rendered_data)

            rendered_data$render.project_data <- project_data(presentation_data)
            rendered_data$render.project_tasks <- project_tasks(presentation_data,app_vars)
            rendered_data$render.project_plan <- project_plan(presentation_data)
            rendered_data$render.project_issues <- issues_summary(presentation_data,app_vars,project_name = "-1")
            rendered_data$render.project_actions <- actions_summary(presentation_data,app_vars,project_name = "-1",
                                                                    min_action_width="1em")
            rendered_data$render.project_updates <-  project_updates(presentation_data,app_vars)
            #rendered_data$render.past_report<- past_reports(version_parameter="current",app_vars,
            #                                                snapshots_version)            
                       
            saveRDS(rendered_data, file=app_vars$Rdata_file) 
            updateSelectInput(session, "project_selection",
                              choices =  presentation_data$projects %>% pull(Name))
            updateSelectInput(session, "report_selection",
                              choices =  snapshots_version %>% pull(version))
            
        
    }   
    

    output$summary1.Backlog <- renderText ({
        rendered_data$render.stats_summary1$Backlog
    })
    
    output$summary1.Planning <- renderText ({
        rendered_data$render.stats_summary1$Planning
    })
    
    output$summary1.InProgress <- renderText ({
        rendered_data$render.stats_summary1$InProgress
    })
    
    output$summary1.Complete <- renderText ({
        rendered_data$render.stats_summary1$Complete
    })
    
    output$summary1.R <- renderText ({
        rendered_data$render.stats_summary1$R
    })
    
    output$summary1.A <- renderText ({
        rendered_data$render.stats_summary1$A
    })
    
    output$summary1.G <- renderText ({
        rendered_data$render.stats_summary1$G
    })
    
    
    output$project_list_backlog <- function(){
        rendered_data$render.kanban$project_list_backlog
    }  
    
    output$project_list_planning <- function(){
        rendered_data$render.kanban$project_list_planning
    }  
    
    output$project_list_active <- function(){
        rendered_data$render.kanban$project_list_active
    } 
    
    output$project_list_complete <- function(){
        rendered_data$render.kanban$project_list_complete
    }  
    
    output$projects_roadmap <- plotly::renderPlotly({
    rendered_data$render.roadmap
    
    })

 
    output$summary2.Backlog <- renderText ({
        rendered_data$render.stats_summary1$Backlog
    })
    
    output$summary2.Planning <- renderText ({
        rendered_data$render.stats_summary1$Planning
    })
    
    output$summary2.InProgress <- renderText ({
        rendered_data$render.stats_summary1$InProgress
    })
    
    output$summary2.Complete <- renderText ({
        rendered_data$render.stats_summary1$Complete
    })
    
    output$summary2.R <- renderText ({
        rendered_data$render.stats_summary1$R
    })
    
    output$summary2.A <- renderText ({
        rendered_data$render.stats_summary1$A
    })
    
    output$summary2.G <- renderText ({
        rendered_data$render.stats_summary1$G
    })
    
    
    output$summary2.late.start <- renderText ({
        rendered_data$render.stats_summary2$late.start
    })    

    output$summary2.overdue <- renderText ({
        rendered_data$render.stats_summary2$overdue
    })    
    
    output$summary2.late.tasks <- renderText ({
        rendered_data$render.stats_summary2$late.tasks
    })    
    
    output$summary2.issue.R <- renderText ({
        rendered_data$render.stats_summary2$issue.R
    })    
    
    output$summary2.issue.A <- renderText ({
        rendered_data$render.stats_summary2$issue.A
    })    
    
    output$summary2.actions <- renderText ({
        rendered_data$render.stats_summary2$open.actions
    })   
    
    

    output$projects <- function(){
        rendered_data$render.projects
    }  
    
    output$issues_summary <- function(){
        rendered_data$render.issues
    } 
    
    output$actions_summary <- function(){
        rendered_data$render.actions
    } 
    
    
    output$consolidated_tasks <- renderText ({
        rendered_data$render.tasks
    })  
    output$stats_dailyR <- renderText ({
        rendered_data$render.stats_daily$R
    })   
    
    output$stats_dailyA <- renderText ({
        rendered_data$render.stats_daily$A
    })   
    
    output$stats_dailyG <- renderText ({
        rendered_data$render.stats_daily$G
    })   
    
    output$people_freq <- function(){
        rendered_data$render.people_freq
    } 
    
    output$project_freq <- function(){
        rendered_data$render.project_freq
    } 
    
    
output$update_msg <- renderText ({
    rendered_data$render.update_msg
})


output$project.project_name <- renderUI({
        HTML(rendered_data$render.project_data[1,]$Project)
  })
 


output$RAG_value_box <- renderValueBox({
    valueBox(
        value = rendered_data$render.project_data[1,]$RAG,
        subtitle="Overall Status",
        icon = icon(rendered_data$render.project_data[1,]$icon),
        color = rendered_data$render.project_data[1,]$RAG_colour,
        width = 1,
        size="tiny")
})

output$project.project_scope <- renderText ({
    rendered_data$render.project_data[1,]$Scope
})

output$project.project_objective <- renderText ({
    rendered_data$render.project_data[1,]$Objectives
})

output$project.project_people <- function(){
    tibble(Role=c("Project Manager","Project Lead"),
           Person=c(rendered_data$render.project_data[1,]$t.Project_Manager,
                    rendered_data$render.project_data[1,]$t.Project_Lead)) %>%
        knitr::kable(format = "html", escape = F) %>%
        kableExtra::kable_styling(full_width = T)
}

output$project.project_dates <- function(){
    tibble(Role=c("Start","End"),
           Person=c(rendered_data$render.project_data[1,]$t.start,
                    rendered_data$render.project_data[1,]$t.end)) %>%
        knitr::kable(format = "html", escape = F) %>%
        kableExtra::kable_styling(full_width = T)
}



output$project_taskR <- renderText ({
    rendered_data$render.project_data[1,]$task.R
}) 
output$project_taskA <- renderText ({
    rendered_data$render.project_data[1,]$task.A
}) 
output$project_taskG <- renderText ({
    rendered_data$render.project_data[1,]$task.G
}) 
output$project_taskg <- renderText ({
    rendered_data$render.project_data[1,]$task.g
}) 
output$project_taskprogress <- renderText ({
    paste(rendered_data$render.project_data[1,]$task.progress,"%")
}) 
output$project_actionopen <- renderText ({
    rendered_data$render.project_data[1,]$action.open
}) 
output$project_issueopen <- renderText ({
    rendered_data$render.project_data[1,]$issue.open
}) 

output$project.project_tasks <- function(){
  rendered_data$render.project_tasks 
}

output$project.project_plan <- plotly::renderPlotly({
  
  rendered_data$render.project_plan
  
})

output$project.project_issues <- function(){
  rendered_data$render.project_issues 
  
}

output$project.project_updates <- function(){
  rendered_data$render.project_updates
  
}

output$project.project_actions <- function(){
    rendered_data$render.project_actions 
  
}



output$dashboard_saved_report <- renderUI({
  if(v_FlexLoc$text==""){""
    }else{
  url <- a(v_FlexLoc$text, href=v_FlexLoc$text)
  HTML(paste("Report:", url))
  }
 
})


output$past_report <- function(){
  rendered_data$render.past_report
  
}






output$date_range <- renderUI({
    tagList(
        tags$div(tags$div(HTML("From")),
                 date_input("date_from", value = lubridate::as_date(cut(app_vars$today, "month")) , style = "width: 30%;")),
        tags$div(tags$div(HTML("To")),
                 date_input("date_to", value = lubridate::as_date(cut(app_vars$today, "month"))+lubridate::dweeks(24), style = "width: 30%;"))
    )
})




output$downloadData <- downloadHandler(
    filename = function() {
        paste("FlexDashboard_",as.numeric(lubridate::now()),".html", sep = "")
    },
    content = function(file) {
       outfile <- rmarkdown::render(app_vars$ReportFlex,
                                    output_format = flexdashboard::flex_dashboard(theme = app_vars$theme_flex,
                                                                                  orientation="rows",
                                                                                  social="menu"
                                                                                  ))
       file.copy(outfile, file)
       file.remove(outfile)
    },

)


message("Initial Load Complete")
}


# Run the application 
shinyApp(ui = ui, server = server)
