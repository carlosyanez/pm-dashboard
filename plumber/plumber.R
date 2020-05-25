# plumber.R
library(tidyverse)

####What happens if file doesnt exist???? it will load ahead of shiny app
#rds_location <- "/srv/shiny-server/pm_dashboard/files/"

rds_location <- "/Users/carlosyanez/GitHub Projects/Trello_Dashboard/shiny/files/"

rendered_rds <- paste(rds_location,"rendered_data.rds",sep="")

rendered_data_current <- readRDS(pins::pin(rendered_rds))

#* Synoptic - Backlog 
#* @html
#* @get /synoptic_backlog
function(version="current"){
    if(version=="current"){
        rendered_data <- rendered_data_current
    }
    else{
        render_file <- paste(rds_location,"rendered_data_",version,".rds",sep="")
        rendered_data <- readRDS(file=render_file)
    }
     shiny::isolate(rendered_data$render.kanban$project_list_backlog)
}

#* Synoptic - Planning 
#* @html
#* @get /synoptic_planning
function(version="current"){
    if(version=="current"){
        rendered_data <- rendered_data_current
    }
    else{
        render_file <- paste(rds_location,"rendered_data_",version,".rds",sep="")
        rendered_data <- readRDS(file=render_file)
    }
    shiny::isolate(rendered_data$render.kanban$project_list_planning)
}

#* Synoptic - In Progress 
#* @html
#* @get /synoptic_inprogress
function(version="current"){
    if(version=="current"){
        rendered_data <- rendered_data_current
    }
    else{
        render_file <- paste(rds_location,"rendered_data_",version,".rds",sep="")
        rendered_data <- readRDS(file=render_file)
    }
    shiny::isolate(rendered_data$render.kanban$project_list_active)
}

#* Synoptic - Complete 
#* @html
#* @get /synoptic_complete
function(version="current"){
    if(version=="current"){
        rendered_data <- rendered_data_current
    }
    else{
        render_file <- paste(rds_location,"rendered_data_",version,".rds",sep="")
        rendered_data <- readRDS(file=render_file)
    }
    shiny::isolate(rendered_data$render.kanban$project_list_complete)
}

#* Roadmap
#* @html
#* @get /roadmap
function(version="current"){
    if(version=="current"){
        rendered_data <- rendered_data_current
    }
    else{
        render_file <- paste(rds_location,"rendered_data_",version,".rds",sep="")
        rendered_data <- readRDS(file=render_file)
    }
    
    shiny::isolate(rendered_data$render.roadmap)
}

#* Roadmap
#* @html
#* @get /project_summaries
function(version="current"){
    if(version=="current"){
        rendered_data <- rendered_data_current
    }
    else{
        render_file <- paste(rds_location,"rendered_data_",version,".rds",sep="")
        rendered_data <- readRDS(file=render_file)
    }
    
    shiny::isolate(rendered_data$render.projects)
}

#* Roadmap
#* @html
#* @get /issues
function(version="current"){
    if(version=="current"){
        rendered_data <- rendered_data_current
    }
    else{
        render_file <- paste(rds_location,"rendered_data_",version,".rds",sep="")
        rendered_data <- readRDS(file=render_file)
    }
    
    shiny::isolate(rendered_data$render.issues)
}

#* Roadmap
#* @html
#* @get /actions
function(version="current"){
    if(version=="current"){
        rendered_data <- rendered_data_current
    }
    else{
        render_file <- paste(rds_location,"rendered_data_",version,".rds",sep="")
        rendered_data <- readRDS(file=render_file)
    }
    
    shiny::isolate(rendered_data$render.actions)
}


#* Echo back the input
#* @param msg The message to echo
#* @get /retrieved
function(){
    list(msg = shiny::isolate(rendered_data_current$data_retrieved))
}

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg=""){
    list(msg = paste0("The message is: '", msg, "'"))
}
 
#* Plot a histogram
#* @png
#* @get /plot
function(){
    rand <- rnorm(100)
    hist(rand)
}
 
#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b){
    as.numeric(a) + as.numeric(b)
}

