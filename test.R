library(shiny)
library(mwshiny)

# named list of ui pages that are the contain the title and content of each of my windows
ui_win <- list()
serv_calc <- list()
serv_out <- list()

###############################################################
# UI

ui_win[["Input"]] <- fluidPage(
  titlePanel("Query"),
  sidebarLayout(
    sidebarPanel(
           textInput("freetxt", "Enter the text to display:"),
           checkboxInput(inputId = "reverse", label = "Reverse string?", value = FALSE)
    ),
    # just an empty main panel
    mainPanel()
  )
)

# then we add what we want to see in the scatter section
ui_win[["Output"]] <- fluidPage(
  titlePanel("Response"),
  verbatimTextOutput(outputId = "verb"),
  textOutput(outputId = "text")
)

###############################################################
# SERVER CALCULATION

reverse_string <- function(s) {
  paste(rev(strsplit(s, NULL)[[1]]), collapse = "")
}

serv_calc[[1]] <- function(calc, sess){
  observeEvent(list( calc$freetxt, 
                     calc$reverse ), {
                       req(calc$freetxt) # <--- TO AVOID WARNING MESSAGE
                         calc[["inter"]] <- if(calc$reverse) { reverse_string(calc$freetxt) 
                         } else { calc$freetxt }
  })
}

serv_calc[[2]] <- function(calc, sess){
  observeEvent(list( calc$freetxt, 
                     calc$inter ), {
                       req(calc$freetxt) # <--- TO AVOID WARNING MESSAGE
                       calc[["response"]] <- sprintf("%s ---> %s", calc$freetxt, calc$inter)
                     })
}


###############################################################
# SERVER OUTPUT

# note the name is the same as the outputid
serv_out[["verb"]] <- function(calc, sess){
  renderText({ calc$response })
}

serv_out[["text"]] <- function(calc, sess){
  renderText({ calc$response })
}


###############################################################
# RUN APP ---

runApp(mwsApp(ui_win, serv_calc, serv_out),
       launch.browser = TRUE)

