#########################
library(shiny)
library(semantic.dashboard) 
library(tidyverse)
library(knitr)
library(kableExtra)
library(vistime)
library(lubridate)
library(plotly)
library(trelloR)
#library(wordcloud)
#library(RColorBrewer)
#########################

#########################
#Load Variables
#########################

source("files/variables.R", echo = F, prompt.echo = "", spaced = F)
normalised_data <-vector(mode = "list", length = 0)

#### Get Data 

if(file.exists(Rdata_file)){
  normalised_data<-readRDS(file=Rdata_file)
  
}else{
  source("r/trello.R", echo = F, prompt.echo = "", spaced = F)
  normalised_data$data_retrieved <- now()
  saveRDS(normalised_data, file =Rdata_file)  
}



if(!("data_retrieved" %in% names(normalised_data))){
  normalised_data$data_retrieved <- now() - ddays(100000)
  
}

normalised_data$data_retrieved + auto_refresh

if((normalised_data$data_retrieved + auto_refresh) > now()){
 source("./r/eval.R", echo = F, prompt.echo = "", spaced = F)
  
}else
{
  source("r/trello.R", echo = F, prompt.echo = "", spaced = F)
  normalised_data$data_retrieved <- now()
  saveRDS(normalised_data, file=Rdata_file)
  
#  source("r/eval.R", echo = F, prompt.echo = "", spaced = F)
  
}
rm(list=lsf.str())

#########################
#UI functions: tabs
#########################


tab_home <- fluidRow(h1("Project's Dashboard"),
                     h2(paste(source_system," version")),
                     hr(),
                     br(),)

tab_summary_1.0 <-fluidRow(
  h1("Programme's Synoptic View")
)
tab_summary_1.1 <-fluidRow(
  valueBox("Backlog", programme_stats[1,]$Backlog, icon("hourglass outline"), color = "blue", width = 2,size="tiny"),
  valueBox("Planning", programme_stats[1,]$Planning, icon("hourglass start"), color = "blue", width = 2,size="tiny"),
  valueBox("In Progress", programme_stats[1,]$"In Progress", icon("hourglass half"), color = "blue", width = 2,size="tiny"),
  valueBox("Complete", programme_stats[1,]$Complete, icon("hourglass start"), color = "blue", width = 2,size="tiny"),
  valueBox("R", programme_stats[1,]$R, icon("exclamation triangle"), color = "red", width = 2,size="tiny"),
  valueBox("A", programme_stats[1,]$A, icon("exclamation circle"), color = "orange", width = 2,size="tiny"),
  valueBox("G", programme_stats[1,]$G, icon("exclamation"), color = "green", width = 2,size="tiny")

)

tab_summary_1.2 <-fluidRow(
  h3("Programme Kanban")
  )

tab_summary_1.3 <- fluidRow(
  
  semantic.dashboard::column(4,style=paste("background-color:",c.background1,";",sep=""),                      
  semantic.dashboard::box(title="Backlog",color = kanban_backlog,
                          ribbon = TRUE,
                          title_side = "top left",
                          collapsible = FALSE,
                          tableOutput("project_list_backlog"))),
  semantic.dashboard::column(4,style=paste("background-color:",c.background1,";",sep=""),                      
  semantic.dashboard::box(title="Planning",color = kanban_planning,
                          ribbon = TRUE,
                          title_side = "top left",
                          collapsible = FALSE,
                          width=4,tableOutput("project_list_planning"))),
  semantic.dashboard::column(4,style=paste("background-color:",c.background1,";",sep=""),                      
  semantic.dashboard::box(title="In Progress",color = kanban_progress,
                          ribbon = TRUE,
                          title_side = "top left",
                          collapsible = FALSE,
                          width=4,tableOutput("project_list_active"))),
  semantic.dashboard::column(4,style=paste("background-color:",c.background1,";",sep=""),                      
  semantic.dashboard::box(title="Complete",color = kanban_complete,
                          ribbon = TRUE,
                          title_side = "top left",
                          collapsible = FALSE,
                          width=4,tableOutput("project_list_complete")))
)

tab_summary_1.4 <-fluidRow(
  h3("Roadmap")
)

tab_summary_1.5 <-  dateInput("roadmap_start_date", label = h3("Date input"), value = "2014-01-01")

tab_summary_1.6 <-  fluidRow(
   semantic.dashboard::box(title="Roadmap",
                           width=16,collapsible = FALSE,
                           color=c.background2,
                           plotlyOutput("projects_roadmap"))
)

tab_summary_2.0 <- fluidRow(
  h1("High Level Report")
)

tab_summary_2.1 <- tab_summary_1.1

tab_summary_2.2 <- fluidRow(
  valueBox("Late Start", programme_stats[1,]$late.start, icon("exclamation"), color = "violet", width = 2,size="tiny"),
  valueBox("Overdue", programme_stats[1,]$overdue, icon("exclamation"), color = "violet", width = 2,size="tiny"),
  valueBox("Late Tasks", programme_stats[1,]$late.tasks, icon("hourglass outline"), color = "violet", width = 2,size="tiny"),
  valueBox("R Issues", programme_stats$issue.R, icon("exclamation"), color = "red", width = 2,size="tiny"),
  valueBox("A Issues", programme_stats$issue.A, icon("exclamation"), color = "orange", width = 2,size="tiny"),
  valueBox("Op Actions", programme_stats[1,]$open.actions, icon("hourglass outline"), color = "violet", width = 2,size="tiny")
  
)

tab_summary_2.3 <- fluidRow(
  h3("Projects")
)
tab_summary_2.4 <- fluidRow(
  semantic.dashboard::box(title="Projects",width=16,color="blue",
  tableOutput("projects_summary"))
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
                                h4("RAG distribution"),
                                valueBox("R", consolidated_stats$R, icon("exclamation"), color = "red", width = 2,size="tiny"),
                                br(),
                                valueBox("A", consolidated_stats$A, icon("exclamation"), color = "orange", width = 2,size="tiny"),
                                br(),
                                valueBox("G", consolidated_stats$G, icon("exclamation"), color = "green", width = 2,size="tiny"),
                                br(),
                                h4("Workload"),
                                tableOutput("project_freq"),
                                tableOutput("people_freq")
                                
                                )

#########################
#UI functions
#########################
header <- dashboardHeader(title = "Projects Dashboard")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(tabName = "home", text = "Home", icon = icon("home")),
  menuItem(tabName = "summary_1", text = "Projects' Synoptic", icon = icon("compass")),
  menuItem(tabName = "summary_2", text = "Projects' Report", icon = icon("clipboard outline")),
  menuItem(tabName = "project_details", text = "Project Details", icon = icon("file alternate outline")),
  menuItem(tabName = "daily_view", text = "Daily Tracker", icon = icon("calendar check outline")),  
  menuItem(tabName = "flexdashboard", text = "Download Report", icon = icon("download")),
  menuItem(tabName = "about", text = "About", icon = icon("copyright outline icon"))
  
  ))

body <-   dashboardBody(
  tabItems(
    tabItem("home",
            tab_home),
    tabItem("summary_1",
            tab_summary_1.0,
            tab_summary_1.1,
            tab_summary_1.2,
            tab_summary_1.3,
            tab_summary_1.4,
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
            tab_home),
    tabItem("daily_view",
            tab_daily_summary_1.0,
            fluidRow(
            tab_daily_summary_1.1,
            tab_daily_summary_1.2)),
   # tabItem("flexdashboard",
   #         tab_home),
  #  tabItem("about",
  #          tab_home)
  ) 
)

ui <- dashboardPage(header,sidebar,body,theme = "flatly")

#########################
#server elements
#########################

#########################
## Projects kanban 
#########################

#### generate table function
####Table
 project_kanban_tables <- function(input,output){
        
   projects_kanban <- normalised_data$projects  %>%
     select(labels,State,Name) %>%
     mutate(colour=ifelse(State=="Complete","grey",labels)) %>%
     left_join(status_colours,by="colour") %>%
     mutate(Pres_Name=cell_spec(Name, color="white",background = hex))   
   
   
  output$project_list_backlog <- function() {
    k_state <- "Backlog"
    k_colour <- "white"
    presentation_data$projects %>% 
      mutate(k.Project=ifelse(State==k_state,k.Project,cell_spec("",background = k_colour, color=k_colour)),
             k.Progress=ifelse(State==k_state,k.Progress,""),
             k.Overdue_Tasks=ifelse(State==k_state,k.Overdue_Tasks,""),
             k.Open_Issues=ifelse(State==k_state,k.Open_Issues,"") 
      ) %>% select(k.Project,k.Progress,k.Overdue_Tasks,k.Open_Issues) %>%
      arrange(k.Project) %>%
      kable(format = "html", escape = F,col.names = NULL) %>%
      kable_styling(full_width = F) %>%
      column_spec(2:4, italic = TRUE)
  }
  
  output$project_list_planning <- function() {
    k_state <- "Planning"
   # k_colour <- project_kanban_background %>% filter(State==k_state) %>% pull(hex2)
    k_colour <- "white"
    presentation_data$projects %>% 
      mutate(k.Project=ifelse(State==k_state,k.Project,cell_spec("",background = k_colour, color=k_colour)),
             k.Progress=ifelse(State==k_state,k.Progress,""),
             k.Overdue_Tasks=ifelse(State==k_state,k.Overdue_Tasks,""),
             k.Open_Issues=ifelse(State==k_state,k.Open_Issues,"") 
      ) %>% select(k.Project,k.Progress,k.Overdue_Tasks,k.Open_Issues) %>%
      arrange(k.Project) %>%
      kable(format = "html", escape = F,col.names = NULL) %>%
      kable_styling(full_width = F) %>%
      column_spec(2:4,italic = TRUE)
}
  output$project_list_active <- function() {
    k_state <- "In Progress"
    k_colour <- "white"
    presentation_data$projects %>% 
      mutate(k.Project=ifelse(State==k_state,k.Project,cell_spec("",background = k_colour, color=k_colour)),
             k.Progress=ifelse(State==k_state,k.Progress,""),
             k.Overdue_Tasks=ifelse(State==k_state,k.Overdue_Tasks,""),
             k.Open_Issues=ifelse(State==k_state,k.Open_Issues,"") 
      ) %>% select(k.Project,k.Progress,k.Overdue_Tasks,k.Open_Issues) %>%
      arrange(k.Project) %>%
      kable(format = "html", escape = F,col.names = NULL) %>%
      kable_styling(full_width = F) %>%
      column_spec(2:4,italic = TRUE)
  }
  
output$project_list_complete <- function() {
  k_state <- "Complete"
  k_colour <- "white"
  presentation_data$projects %>% 
    mutate(k.Project=ifelse(State==k_state,k.Project,cell_spec("",background = k_colour, color=k_colour)),
           k.Progress=ifelse(State==k_state,k.Progress,""),
           k.Overdue_Tasks=ifelse(State==k_state,k.Overdue_Tasks,""),
           k.Open_Issues=ifelse(State==k_state,k.Open_Issues,"") 
    ) %>% select(k.Project,k.Progress,k.Overdue_Tasks,k.Open_Issues) %>%
    arrange(k.Project) %>%
    kable(format = "html", escape = F,col.names = NULL) %>%
    kable_styling(full_width = F) %>%
    column_spec(2:4,italic = TRUE)
}
 }

 projects_roadmap <- function(input,output){
  
  roadmap_start_date <- as_date(cut(today, "month"))
  
  roadmap_table<-presentation_data$projects %>%
    mutate(no_start=is.na(strftime(start)),
           no_end=is.na(strftime(end))) %>%
    filter(!(no_start) & !(no_end)) %>%
    mutate(start=as_date(ifelse(no_start,end,start)),
           end = as_date(ifelse(no_end,start,end))) %>%
    mutate(start=as_date(ifelse(start<  roadmap_start_date,  roadmap_start_date,start)),
           end=as_date(ifelse(end<roadmap_start_date,(roadmap_start_date - ddays(1)),end))) %>%
    filter(end>=roadmap_start_date) %>%
    mutate(comments=paste("Start:", start,
                          " - End:",end,
                          " - State :", RAG,
                          " ",RAG_comment,
                          sep = "")) %>%
    select(event=Name,start,end,group=State,color=State_colour,tooltip=comments)   
  
  tl<-vistime(roadmap_table, title = "Roadmap",optimize_y=FALSE)
  
  line <- list(
    type = "line",
    line = list(color = "orange"),
    xref = "x",
    yref = "y",
    x0 = today, 
    x1 = today,
    y0 = 0,
    y1 = nrow(projects_roadmap)+3
    
  )
  
  tl <- layout(tl, title = 'Project Roadmap', shapes = line) 
  
  
  output$projects_roadmap <- renderPlotly({tl})
}

 projects_summary <- function(input,output){
   
   project_list <- presentation_data$projects %>% 
     mutate(condition1=(!(State %in% c("Complete")) & !is.na(strftime(end))),
            condition2=(State %in% c("Planning","In Progress")),
            condition3= (State %in% c("Complete") & (end+ddays(15)>today)),
            select = condition1 | condition2 | condition3
     ) %>%
     filter(select) %>%
     select(" "=t.RAG,State=t.State,Due=t.end,"Project Name"=t.Project,
            "%"=t.Progress,OD=t.Overdue_Tasks,
            Issues=t.Open_Issues,Update=t.Update) 
   p_rows <- nrow(project_list)
   
   output$projects_summary <- function() {

     project_list %>%
       kable(format = "html", escape = F) %>%
       kable_styling(full_width = T)  %>%
       column_spec(2, width_min="7em") %>%
       column_spec(3, width_min="5em") %>%
       column_spec(4, width_min="8em") %>%
       column_spec(8,width_min="10em") %>%
       row_spec(seq(1,p_rows,2),background =c.background3,hline_after=TRUE) %>%
       row_spec(seq(2,p_rows,2),background =c.background4,hline_after=TRUE)
   }
 }
 
 issues_summary <- function(input,output){
   
   issue_list <- presentation_data$issues %>% 
     mutate(condition1=((State %in% c("Open")) ),
            condition2= (State %in% c("Closed") & (due+ddays(15)>today)),
            select = condition1 | condition2
     ) %>%
     filter(select) %>%
     arrange(due) %>%
     select(" "=t.RAG,State=t.State,"Project Name"=t.Project,
            Issue=t.Issue,Assignee=t.Assignee,			
            Impact=t.Impact,Action=t.Action,Due=t.due) 
   p_rows <- nrow(issue_list)
   
   output$issues_summary <- function() {
     issue_list %>%
       kable(format = "html", escape = F) %>%
       kable_styling(full_width = T)  %>%
       row_spec(seq(1,p_rows,2),background =c.background3,hline_after=TRUE) %>%
       row_spec(seq(2,p_rows,2),background =c.background4,hline_after=TRUE) %>%
       column_spec(8,width_min="5em") 
     
   }
 }
 
 actions_summary <- function(input,output){
   
   action_list <- presentation_data$actions %>% 
     mutate(condition1=((State %in% c("Open")) ),
            condition2= (State %in% c("Closed") & (due+ddays(15)>today)),
            selection = condition1 | condition2) %>%
     filter(selection) %>%
     arrange(due) %>%
     select(" "=t.RAG,State=t.State,"Project Name"=t.Project,
            Action=t.Action, Assignee=t.assignee, Due=t.due) 
   p_rows <- nrow(action_list)
   
   output$actions_summary <- function() {
     
     action_list %>%
       kable(format = "html", escape = F) %>%
       kable_styling(full_width = T)  %>%
       row_spec(seq(1,p_rows,2),background =c.background3,hline_after=TRUE) %>%
       row_spec(seq(2,p_rows,2),background =c.background4,hline_after=TRUE) %>%
       column_spec(4,width_min="20em") %>%
       column_spec(6,width_min="5em") 
       
     
   }
 }
 
 
 consolidated_tasks <- function(input,output){
  
   
   X_rows <- which(t.consolidated_tasks$row_colour=="X")
   U_rows <- which(t.consolidated_tasks$row_colour=="U")
   M_rows <- which(t.consolidated_tasks$row_colour=="M")
   L_rows <- which(t.consolidated_tasks$row_colour=="L")

   output$consolidated_tasks <- function() {
     
     t.consolidated_tasks %>%
       select(" "=t.RAG,Type,State=t.State,Due=t.due,Item=t.Item,Assignee=t.assignee)  %>% 
       kable(format = "html", escape = F) %>%
       kable_styling(full_width = F) %>%
       row_spec(X_rows,background=c.X_rows,hline_after=TRUE) %>%
       row_spec(U_rows,background=c.U_rows,hline_after=TRUE) %>%
       row_spec(M_rows,background=c.M_rows,hline_after=TRUE) %>%
       row_spec(L_rows,background=c.L_rows,hline_after=TRUE) %>%
       column_spec(3, width_min="7em") %>%
       column_spec(4, width_min="5em") 
     
     
   }
 }
 people_freq <- function(input,output){
   
   output$people_freq <- function() {
   consolidated_stats$person_frequency %>% 
     left_join(font_sizes,by="nbr") %>%
     mutate(Assignee=cell_spec(assignee,font_size = size, 
                               bold = (nbr %in% c(1,2,3))
                               ,
                               color = ifelse((nbr %in% c(1,2,3)),
                                              "navy",
                                              t.default_colour))) %>%
     mutate(Workload=cell_spec(freq,font_size = size, 
                               bold = (nbr %in% c(1,2,3))
                               ,
                               color = ifelse((nbr %in% c(1,2,3)),
                                              "navy",
                                              t.default_colour))) %>%
     select(Assignee,Workload)%>%
     kable(format = "html", escape = F) %>%
     kable_styling(full_width = F) 
   }
   
 }

 project_freq <- function(input,output){
   
   output$project_freq <- function() {
     consolidated_stats$project_frequency %>% 
       left_join(font_sizes,by="nbr") %>%
       mutate(Project=cell_spec(Project,font_size = size, 
                                 bold = (nbr %in% c(1,2,3))
                                 ,
                                 color = ifelse((nbr %in% c(1,2,3)),
                                                "navy",
                                                t.default_colour))) %>%
       mutate(Workload=cell_spec(freq,font_size = size, 
                                 bold = (nbr %in% c(1,2,3))
                                 ,
                                 color = ifelse((nbr %in% c(1,2,3)),
                                                "navy",
                                                t.default_colour))) %>%
       select(Assignee,Workload)%>%
       kable(format = "html", escape = F) %>%
       kable_styling(full_width = F) 
   }
   
 }
 
#########################
#server function
#########################
server <- function(input, output) {
  


  project_kanban_tables(input,output) 
  #tables: output$project_list_backlog,output$project_list_planning,output$project_list_active,
  #        output$project_list_complete
  
  projects_roadmap(input,output)
  #timeline : output$projects_roadmap
  
  projects_summary(input,output)
  #tables: output$projects_summary
  
  issues_summary(input,output)
  #tables: output$issues_summary
  
  actions_summary(input,output)
  #tables: output$actions_summary
  
  consolidated_tasks(input,output)
  #tables: output$consolidated_tasks
  
  people_freq(input,output)
  project_freq(input,output)
}
 
shinyApp(ui, server)
