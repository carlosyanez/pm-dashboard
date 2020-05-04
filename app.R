#########################
library(semantic.dashboard) # <-- You only need to change this line to: library(semantic.dashboard)
library(tidyverse)
library(knitr)
library(kableExtra)
library(vistime)
library(lubridate)
#########################

#########################
#Colours
#########################
status_colours <- tibble(colour=c("NA","green","amber","red","grey","black"),
                         colour_short=c("NA","G","A","R","g","B"),
                         hex=c("purple","green","orange","red","#dedede","black"))

project_kanban_background <- tibble(State=c("Backlog","Planning","Active","Complete"),
                                    Task=c("To Do","Blocked","In Progress","Complete"),
                                    Action=c("dummy1","dummy2","incomplete","complete"),
                                    Issue=c("dummy1","dummy2","Open","Closed"),
                                    hex=c("#EAE8FF","#D8D5DB","#a8edc9","#dedede"))

t.default_colour <- "black"


#########################
#UI elements
#########################
header <- dashboardHeader(title = "PM's Dashboard")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(tabName = "home", text = "Home", icon = icon("home")),
  menuItem(tabName = "another", text = "Another Tab", icon = icon("heart"))))

body <-   dashboardBody(fluidRow(
  semantic.dashboard::column(4,
         "Backlog",
         semantic.dashboard::column(4,style = "background-color:#EAE8FF;",
           tableOutput("project_list_backlog"))),
  semantic.dashboard::column(4,
         "Planning",
         semantic.dashboard::column(4,style = "background-color:#D8D5DB;",
           tableOutput("project_list_planning"))),
  semantic.dashboard::column(4,
         "In Progress",
         semantic.dashboard::column(4,style = "background-color:#ADACB5;",
           tableOutput("project_list_active"))),
  semantic.dashboard::column(4,
         "Complete",
         semantic.dashboard::column(4,style = "background-color:#2D3142;",
           tableOutput("project_list_complete"))),
  plotlyOutput("projects_roadmap")
)
)

#########################
#UI function
#########################
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
    projects_kanban  %>% mutate(Name=ifelse(State==k_state,Name,""),
                                Pres_Name=ifelse(State==k_state,Pres_Name,
                                                 cell_spec("", color="white",
                                                           background = (project_kanban_background %>%
                                                             filter(State==k_state) %>% 
                                                             pull(hex))))) %>%
      arrange(desc(Name)) %>% 
      select(Pres_Name) %>%
      kable(format = "html", escape = F,col.names = NULL) %>%
      kable_styling("striped", full_width = F)
}
  output$project_list_planning <- function() {
    k_state <- "Planning"
    projects_kanban  %>% mutate(Name=ifelse(State==k_state,Name,""),
                                Pres_Name=ifelse(State==k_state,Pres_Name,
                                                 cell_spec("", color="white",
                                                           background = (project_kanban_background %>%
                                                                           filter(State==k_state) %>% 
                                                                           pull(hex))))) %>%
      arrange(desc(Name)) %>% 
      select(Pres_Name) %>%
      kable(format = "html", escape = F,col.names = NULL) %>%
      kable_styling("striped", full_width = F)
}
  output$project_list_active <- function() {
    k_state <- "Active"
    projects_kanban  %>% mutate(Name=ifelse(State==k_state,Name,""),
                                Pres_Name=ifelse(State==k_state,Pres_Name,
                                                 cell_spec("", color="white",
                                                           background = (project_kanban_background %>%
                                                                           filter(State==k_state) %>% 
                                                                           pull(hex))))) %>%
      arrange(desc(Name)) %>% 
      select(Pres_Name) %>%
      kable(format = "html", escape = F,col.names = NULL) %>%
      kable_styling("striped", full_width = F)
}
output$project_list_complete <- function() {
  k_state <- "Complete"
  projects_kanban  %>% mutate(Name=ifelse(State==k_state,Name,""),
                              Pres_Name=ifelse(State==k_state,Pres_Name,
                                               cell_spec("", color="white",
                                                         background = (project_kanban_background %>%
                                                                         filter(State==k_state) %>% 
                                                                         pull(hex))))) %>%
    arrange(desc(Name)) %>% 
    select(Pres_Name) %>%
    kable(format = "html", escape = F,col.names = NULL) %>%
    kable_styling("striped", full_width = F)
}
 }


#########################
#Projects Roadmap
#########################

projects_roadmap <- function(input,output){
  
   projects_roadmap <- normalised_data$projects %>% select(Name,State,due,labels) %>% 
    mutate(start=Sys.Date(),
           end=as_date(ifelse(is.na(due),Sys.Date(),due))) %>%
    left_join((project_kanban_background %>% select(State,fontcolor=hex)),by="State") %>%
    left_join((status_colours %>% select(labels=colour,color=hex)),by="labels") %>%
    filter(!(State=="Complete")) %>%
    select(event=Name,start,end,fontcolor,color,group=State) 
  
   output$projects_roadmap <- renderPlotly({
     vistime(projects_roadmap, title="Timeline")
})
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
}

shinyApp(ui, server)
