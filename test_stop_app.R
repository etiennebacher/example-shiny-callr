library(shiny)
library(shinyjs)
library(uuid)
library(ggplot2)
library(waiter)


bg_is_running <- function(process) {
  if(process$poll_io(0)["process"] == "timeout")
    return(TRUE)
  else return(FALSE)
}


ui <- fluidPage(
  useWaiter(),
  useShinyjs(),
  titlePanel("Test background job"),
  selectInput("cars", "Cars", names(mtcars)),
  actionButton("start","Start Job"),
  actionButton("stop", "Stop job"),
  plotOutput("plot")
)

# the toy example job
slow_func <- function(var){
  Sys.sleep(5)
  return(var)
}

slow_func_2 <- function(var) {
  library(ggplot2)
  ggplot(mtcars, aes(drat, !!sym(var))) + 
    geom_point() +
    ggtitle(var)
}

server <- function(input, output, session) {
  
  w <- Waiter$new(id = "plot")
  shinyjs::disable("stop")

  token <- reactiveValues(var = NULL, id = NULL, last_id = NULL)
  jobs <- reactiveValues()
  jobs_2 <- reactiveValues()
  
  
  # When I press "start", run the slow function and append the output to
  # the list of jobs. To render the plot, check if the background process is
  #  finished. If it's not, re-check one second later.
  
  long_run <- eventReactive(input$start, {
    token$id <- c(token$id, UUIDgenerate())
    token$last_id <- token$id[[length(token$id)]]
    message(paste0("running task with id: ", token$last_id))
    jobs[[token$last_id]] <- callr::r_bg(
      slow_func,
      args = list(var = input$cars)
    )
    return(jobs[[token$last_id]])
  })
  
  long_run_2 <- reactive({
    if (bg_is_running(long_run())) {
      invalidateLater(1000)
    } else {
      jobs_2[[token$last_id]] <- callr::r_bg(
        slow_func_2,
        args = list(var = jobs[[token$last_id]]$get_result())
      )
    }
  })
  
  observeEvent(input$start, {
    output$plot <- renderPlot({
      if (bg_is_running(long_run())) {
        w$show()
        w$update(tagList(spin_ring(), br(), h4("Treating the data...")))
        shinyjs::enable("stop")
        invalidateLater(1000)
        req(FALSE, cancelOutput = TRUE)
      } else {
        if (bg_is_running(long_run_2())) {
          w$update(tagList(spin_ring(), br(), h4("Making plot...")))
          shinyjs::enable("stop")
          invalidateLater(1000)
          req(FALSE, cancelOutput = TRUE)
        } else {
          shinyjs::disable("stop")
          jobs_2[[token$last_id]]$get_result()
        }
      }
    })
  })
  
  # There are two background processes that run consecutively: long_run and 
  # long_run_2. When I press "stop", I need to check which process is concerned.
  # 
  # If the first process is still running, I need to kill it. If the second
  # process is running, I need to kill it. In both cases, I remove the last
  # id from the list, which makes it inaccessible for both jobs.
  # 
  # Finally, I display the last process (which by definition is the last plot
  # produced).
  
  observeEvent(input$stop, {

    if (!length(token$id) > 0) 
      return(NULL)
    
    if (bg_is_running(long_run())) {
      jobs[[token$last_id]]$kill()
      message(paste0("task ", token$last_id, " stopped"))
      token$id <- token$id[-length(token$id)]
    } else {
      if (bg_is_running(long_run_2())) {
        jobs_2[[token$last_id]]$kill()
        message(paste0("task ", token$last_id, " stopped"))
        token$id <- token$id[-length(token$id)]
      }
    }
    
    if (!length(token$id) > 0) {
      output$plot <- renderPlot(NULL)
      return(NULL)
    }
      
    token$last_id <- token$id[[length(token$id)]]

    output$plot <- renderPlot({
      jobs_2[[token$last_id]]$get_result()
    })
  })
  
}

shinyApp(ui = ui, server = server)
