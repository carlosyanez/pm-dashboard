if(!require(semantic.dashboard)) install.packages("semantic.dashboard", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(vistime)) install.packages("vistime", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(trelloR)) install.packages("trelloR", repos = "http://cran.us.r-project.org")


#library(shiny)
library(semantic.dashboard) 
library(tidyverse)
library(knitr)
library(kableExtra)
library(vistime)
library(lubridate)
library(plotly)
library(trelloR)

#########################

source("./r/variables.R")
#normalised_data <-reactiveValues()
#presentation_data <-reactiveValues()
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
    valueBox("Backlog", textOutput("summary1.Backlog"), icon("hourglass outline"), color = "blue", width = 2,size="tiny"),
    valueBox("Planning", textOutput("summary1.Planning"), icon("hourglass start"), color = "blue", width = 2,size="tiny"),
    valueBox("In Progress", textOutput("summary1.InProgress"), icon("hourglass half"), color = "blue", width = 2,size="tiny"),
    valueBox("Complete", textOutput("summary1.Complete"), icon("hourglass start"), color = "blue", width = 2,size="tiny"),
    valueBox("R", textOutput("summary1.R"), icon("exclamation triangle"), color = "red", width = 2,size="tiny"),
    valueBox("A", textOutput("summary1.A"), icon("exclamation circle"), color = "orange", width = 2,size="tiny"),
    valueBox("G", textOutput("summary1.G"), icon("exclamation"), color = "green", width = 2,size="tiny")  
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

tab_summary_1.5 <-  fluidRow(
    dateRangeInput("daterange_roadmap", "Date range:",
                   start = ymd(today()),
                   end   = ymd(today()+dweeks(24)))
    
)

tab_summary_1.6 <-  fluidRow(
    semantic.dashboard::box(title="Roadmap",
                            width=16,collapsible = FALSE,
                            color=app_vars$c.background2,
                            plotlyOutput("projects_roadmap"))
)

tab_summary_2.0 <- fluidRow(
    h1("High Level Report")
)

tab_summary_2.1  <-fluidRow(
valueBox("Backlog", textOutput("summary2.Backlog"), icon("hourglass outline"), color = "blue", width = 2,size="tiny"),
valueBox("Planning", textOutput("summary2.Planning"), icon("hourglass start"), color = "blue", width = 2,size="tiny"),
valueBox("In Progress", textOutput("summary2.InProgress"), icon("hourglass half"), color = "blue", width = 2,size="tiny"),
valueBox("Complete", textOutput("summary2.Complete"), icon("hourglass start"), color = "blue", width = 2,size="tiny"),
valueBox("R", textOutput("summary2.R"), icon("exclamation triangle"), color = "red", width = 2,size="tiny"),
valueBox("A", textOutput("summary2.A"), icon("exclamation circle"), color = "orange", width = 2,size="tiny"),
valueBox("G", textOutput("summary2.G"), icon("exclamation"), color = "green", width = 2,size="tiny")  
)

tab_summary_2.2 <- fluidRow(
    valueBox("Late Start", textOutput("summary2.late.start"), icon("exclamation"), color = "violet", width = 2,size="tiny"),
    valueBox("Overdue", textOutput("summary2.overdue"), icon("exclamation"), color = "violet", width = 2,size="tiny"),
    valueBox("Late Tasks",textOutput("summary2.late.tasks"), icon("hourglass outline"), color = "violet", width = 2,size="tiny"),
    valueBox("R Issues", textOutput("summary2.issue.R"), icon("exclamation"), color = "red", width = 2,size="tiny"),
    valueBox("A Issues", textOutput("summary2.issue.A"), icon("exclamation"), color = "orange", width = 2,size="tiny"),
    valueBox("Op Actions", textOutput("summary2.actions"), icon("hourglass outline"), color = "violet", width = 2,size="tiny")
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
    h3("Daily Tracker")
)

tab_daily_summary_1.1 <- column(10,
                                semantic.dashboard::box(title="Item Tracker",color="blue",
                                                        tableOutput("consolidated_tasks"))
)

tab_daily_summary_1.2 <- column(6,
                                h4("RAG Distribution"),
                                valueBox("R", textOutput("stats_dailyR"), icon("exclamation"), color = "red", width = 2,size="tiny"),
                                br(),
                                valueBox("A", textOutput("stats_dailyA"), icon("exclamation"), color = "orange", width = 2,size="tiny"),
                                br(),
                                valueBox("G", textOutput("stats_dailyG"), icon("exclamation"), color = "green", width = 2,size="tiny"),
                                br(),
                                semantic.dashboard::box(title="Project Distribution",color="blue",
                                                        tableOutput("project_freq")),
                                br(),
                                semantic.dashboard::box(title="People's Workload",color="blue",
                                                        tableOutput("people_freq"))
                                
)

# Define UI for application that draws a histogram

header <- dashboardHeader(title = "Projects Dashboard")

sidebar <- dashboardSidebar(sidebarMenu(
    menuItem(tabName = "home", text = "Home", icon = icon("home")),
    menuItem(tabName = "summary_1", text = "Projects' Synoptic", icon = icon("compass")) ,
    menuItem(tabName = "summary_2", text = "Projects' Report", icon = icon("clipboard outline")),
 #   menuItem(tabName = "project_details", text = "Project Details", icon = icon("file alternate outline")),
    menuItem(tabName = "daily_view", text = "Daily Tracker", icon = icon("calendar check outline")) ,  
 #   menuItem(tabName = "flexdashboard", text = "Download Report", icon = icon("download")),
    menuItem(tabName = "about", text = "About", icon = icon("copyright outline icon"))
    
))

body <-   dashboardBody(
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
                tab_summary_1.5,
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
#        tabItem("project_details",
#                tab_home_1.0),
        tabItem("daily_view",
                tab_daily_summary_1.0,
                fluidRow(
                    tab_daily_summary_1.1,
                    tab_daily_summary_1.2)),
#        tabItem("flexdashboard",
#                tab_home_1.0),
        tabItem("about",
                tab_home_1.0)
    ) 
)

ui <- dashboardPage(header,sidebar,body,theme = "cerulean")

# Define server logic required to draw a histogram
server <- function(input, output) {
    message("Initial Load")
    observeEvent(input$refresh_data,
                 {
                     message(paste("Starting Refresh"))
                     
                     source("./r/trello.R", echo = F, prompt.echo = "", spaced = F)
                     normalised_data$data_retrieved <- as_date(now())
                     saveRDS(normalised_data, file=app_vars$Ndata_file) 
                     normalised_data <-readRDS(file=app_vars$Ndata_file)

                    
                     message("Data Sourced:")
                     message(nrow(normalised_data$projects))
                     
                     source("./r/eval.R", echo = F, prompt.echo = "", spaced = F)
                     
                     message("Data Evaluated")
                     message(nrow(presentation_data$projects))


                     presentation_data$data_retrieved <- now()
                     rendered_data$data_retrieved <- now()
                     
                     saveRDS(normalised_data, file=app_vars$Ndata_file) 
                     saveRDS(presentation_data, file=app_vars$Pdata_file) 
                     
                    rendered_data$render.kanban = project_kanban_tables(presentation_data,app_vars)
                     rendered_data$render.roadmap <- projects_roadmap(presentation_data,app_vars,as_date(cut(app_vars$today, "month")))
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

                     saveRDS(rendered_data, file=app_vars$Rdata_file)  
                     
                     message(paste("Refresh completed",rendered_data$data_retrieved))
                 })

    ### Load Files
    if(file.exists(app_vars$Rdata_file)){
        rendered_data <-readRDS(file=app_vars$Rdata_file)
        message(paste("Rendered data loaded",isolate(rendered_data$data_retrieved)))
        if(!("data_retrieved" %in% "data_retrieved")){
            isolate(rendered_data$data_retrieved) <- now() - ddays(100000)
            
        }
        
        ageing_check <- ( (isolate(rendered_data$data_retrieved) + app_vars$auto_refresh) > as_date(now()) )
        
        if(ageing_check){
            rendered_data <-readRDS(file=app_vars$Rdata_file)
        }else
        {
            source("./r/trello.R", echo = F, prompt.echo = "", spaced = F)
            source("./r/eval.R", echo = F, prompt.echo = "", spaced = F)
            
            
            normalised_data$data_retrieved <- now()
            presentation_data$data_retrieved <- now()
            
            saveRDS(normalised_data, file=app_vars$Ndata_file)  
            saveRDS(presentation_data, file=app_vars$Pdata_file) 
            
            
            rendered_data$data_retrieved <- now()
            
            rendered_data$render.kanban = project_kanban_tables(presentation_data,app_vars)
            rendered_data$render.roadmap <- projects_roadmap(presentation_data,app_vars,as_date(cut(app_vars$today, "month")))
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
            

            saveRDS(rendered_data, file=app_vars$Rdata_file) 
        }
        
        
    
        }else{
            source("./r/trello.R", echo = F, prompt.echo = "", spaced = F)
            source("./r/eval.R", echo = F, prompt.echo = "", spaced = F)
            
            
            normalised_data$data_retrieved <- now()
            presentation_data$data_retrieved <- now()
            
            saveRDS(normalised_data, file=app_vars$Ndata_file)  
            saveRDS(presentation_data, file=app_vars$Pdata_file) 
            
            
            rendered_data$data_retrieved <- now()
            
            rendered_data$render.kanban = project_kanban_tables(presentation_data,app_vars)
            rendered_data$render.roadmap <- projects_roadmap(presentation_data,app_vars,as_date(cut(app_vars$today, "month")))
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
            
            
            saveRDS(rendered_data, file=app_vars$Rdata_file) 
        
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
    
    output$projects_roadmap <- renderPlotly({
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
        rendered_data$render.issues
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






message("Initial Load Complete")
}


# Run the application 
shinyApp(ui = ui, server = server)
