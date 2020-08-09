
#invoke shiny libraries
library(shiny)
library(shinydashboard)
library(ROI.plugin.glpk)

ui <- dashboardPage(skin = "blue", title = "Shift scheduler",
      dashboardHeader(title = h4("Shift scheduler"), 
                      
                      tags$li(a(href = 'https://github.com/SidharthMacherla/schedulerShinyApp/blob/master/schedulerShinyApp.R',
                                icon("github"), title = "Go to source code"),
                              class = "dropdown"),
                      tags$li(a(href = 'https://www.foyi.co.nz/posts/howtoguides/staffscheduler/',
                                icon("book"), title = "Go to documentation"),
                              class = "dropdown"),
                      tags$li(a(href = 'https://github.com/SidharthMacherla/schedulerShinyApp/blob/master/LICENSE',
                                icon("certificate"), title = "View license"),
                              class = "dropdown")
      ),#ends dashboard header  
      
      dashboardSidebar(
        sidebarMenu(
          menuItem("Build optimization model", tabName = "tab1", icon = icon("cog")),
          menuItem("View schedule", tabName = "tab2", icon = icon("table"))
        )#ends sidebarMenu
        
      ),#ends dashboardSidebar
      dashboardBody(
        tabItems(
          #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
          #UI Side: first tab
          #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
          tabItem(tabName = "tab1",
                  fluidRow(
                    box(title = "Select shift preference data", status = "primary", 
                        solidHeader = TRUE, width = 12,
                        
                        tags$i("Note: Skip this step to use the default data. 
                               Use these templates, to load your own data."),
                        downloadLink('shiftPreferenceTemplate', 'Shift Preference Template, '),
                        downloadLink('skillTemplate', 'Skill Template'),
                        
                        fileInput(inputId = "shiftPrefRaw", label = "Import shift preference data", 
                                  width = '20%', 
                                  accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
                        
                        #h5("Import staff skill data"),
                        fileInput(inputId = "staffSkillRaw", label = "Import staff skill data", width = '20%',
                                  accept = c("text/csv","text/comma-separated-values,text/plain", ".csv"))
                        ),#ends select input box
                    
                    box(title = "View data", status = "primary", 
                        solidHeader = TRUE, width = 12,
                        div(style = 'overflow-x: scroll; overflow-y: scroll; height:300px;',  DT::dataTableOutput('viewShiftPrefData'))
                        ),#ends console box
                    
                    box(title = "Set constraints", status = "primary", 
                        solidHeader = TRUE, width = 12,
                        fluidRow(
                          box(title = NULL, status = "primary", solidHeader = TRUE, width = 6,
                              h5(strong("How many days are the staff allowed 
                                        to work per week?")),
                              numericInput(inputId = "maxDaysPerWeek", label = NULL, 
                                           value = 5, step = 1, width = '25%'),
                              #error message
                              textOutput('errorMaxDaysPerWeek'),
                              hr(),
                              
                              h5(strong("What is the minimum staffing requirement per shift?")),
                              numericInput(inputId = "minStaffPerShift", label = NULL, 
                                           value = 1, step = 1, width = '25%'),
                              #error message
                              textOutput('errorMinStaffPerShift')
                              ),#ends 1st constraints box
                          
                          box(title = NULL, status = "primary", solidHeader = TRUE, width = 6,
                              h5(strong("How many shifts are staff allowed to work per day?")),
                              numericInput(inputId = "maxShiftsPerDay", label = NULL, 
                                           value = 1, step = 1, width = '25%'),
                              #error message
                              textOutput('errorMaxShiftsPerDay'),
                              hr(),
                              h5(strong("Which skills must be present across all shifts?")),
                              checkboxGroupInput(inputId = "skillsPerShift", label = NULL, choices = c("skill1", "skill2", "skill3"),
                                                 inline = TRUE, width = '100%')
                              )#ends 2nd constraints box
                          ),#ends fluid row of constraints box
                        actionButton(inputId = "runOptimizer", label = "Run scheduler", 
                                     icon("paper-plane"), 
                                     style="color: white; background-color: steelblue;  
                                                   border-color: steelblue"), br(),
                        uiOutput("successOptimizer")
                        )#ends step3 box
                        )#ends fluid row
                        ),#ends firsttab
          
          #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
          #UI Side: second tab
          #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
          tabItem(tabName = "tab2",
                  fluidRow(
                    box(
                      title = "View and download staff schedule", status = "primary", 
                      solidHeader = TRUE, width = 12,
                      div(style = 'overflow-x: scroll',  DT::dataTableOutput('finalData'))
                    )),
                  downloadLink('downloadSchedule', 'Download data')
                  
                  )#ends second tab
                        )#ends tabItems
        )#ends dashboard body
      )#ends UI


server <- function(input, output, session) {
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  #Server Side: Global variables
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
  #set file size limit
  options(shiny.maxRequestSize=100*1024^2)
  
  #instantite global variables as NULL. These will be updated as the user sets their values
  globals <- reactiveValues(shiftPrefRaw = NULL, staffSkillRaw = NULL, numOfStaff = NULL, numOfDays = NULL,
                            numOfShifts = NULL, staffSchedule = NULL)
  
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  #Server Side: Set default global variables
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& 
  globals$shiftPrefRaw <- data.frame(
                          "StaffId" = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6),
                          "Shift" = c(1, 2, 3, 1, 2, 3, 
                                      1, 2, 3, 1, 2, 3, 
                                      1, 2, 3, 1, 2, 3),
                          "Monday" = c(1,1,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,1),
                          "Tuesday" = c(1,1,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,1),
                          "Wednesday" = c(1,1,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,1),
                          "Thursday" = c(1,1,0,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1),
                          "Friday" = c(1,1,0,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1),
                          "Saturday" = c(1,1,0,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1),
                          "Sunday" = c(1,1,0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1)
  )
  
  globals$staffSkillRaw <- data.frame(
                          "staffId" = c(1, 1, 1,
                                        2, 2, 2,
                                        3, 3, 3,
                                        4, 4, 4,
                                        5, 5, 5,
                                        6, 6, 6
                                        ),
                          "skill" = c("skill1", "skill2", "skill3", 
                                      "skill1", "skill2", "skill3", 
                                      "skill1", "skill2", "skill3",
                                      "skill1", "skill2", "skill3",
                                      "skill1", "skill2", "skill3",
                                      "skill1", "skill2", "skill3"
                                      )
  )

  globals$numOfStaff <- 5
  globals$numOfDays <- 7
  globals$numOfShifts <- 3
  
  output$viewShiftPrefData <- DT::renderDataTable(globals$shiftPrefRaw)
  
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  #Server Side: interactivity
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& 
  
  #import shift preference data
  observeEvent(input$shiftPrefRaw, 
               {
                 globals$shiftPrefRaw <- read.csv(input$shiftPrefRaw$datapath, 
                                                  header = TRUE, fileEncoding = "UTF-8-BOM") 
                 globals$numOfStaff <- length(unique(globals$shiftPrefRaw$StaffId))
                 globals$numOfDays <- (ncol(globals$shiftPrefRaw)-2)
                 globals$numOfShifts <- length(unique(globals$shiftPrefRaw$Shift))
               }
               )
  #import staff skill data
  observeEvent(input$staffSkillRaw, 
               {
                 globals$staffSkillRaw <- read.csv(input$staffSkillRaw$datapath, 
                                                  header = TRUE, fileEncoding = "UTF-8-BOM") 
                 #Update the select variable checkbox in tab4 when the wpsvr file is uploaded.
                 updateCheckboxGroupInput(session, "skillsPerShift", choices = unique(globals$staffSkillRaw$skill))
                 #print message for qa purposes
                 message(sprintf("uploaded skill data"))
               }
  )
  
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  #Server Side: functions
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
  #source the piping operator from magrittr
  "%>%" <- magrittr::`%>%`
  
  #function to compute the skill. This is used for the constraint
  checkSkill <- function(staff, skill)
  {
    #if the staff has the skill then, return 1 else 0
    skillPresence <- nrow(globals$staffSkillRaw[globals$staffSkillRaw$staffId == staff & globals$staffSkillRaw$skill == skill,])
    return(skillPresence)
  }
  
  #function to check if staff is available for a given day and shift
  checkPref <- function(staff, day, shift)
  {
    staffShiftSubset <- globals$shiftPrefRaw[globals$shiftPrefRaw$StaffId == staff & globals$shiftPrefRaw$Shift == shift,]
    staffShiftDaySubset <- staffShiftSubset[which(!names(staffShiftSubset) %in% c("StaffId", "Shift"))]
    staffShiftDaySubset <- staffShiftDaySubset[,day]
    isAvail <- staffShiftDaySubset
    #to ensure that non-preferred shifts are given least preference, place a high penalty.
    isPref <- ifelse(isAvail == 0, -10000, isAvail)
    return(isPref)
  }
  
  #function to add skill based constraints iteratively
  skillConstraints <- function(m, skillList)
  {
    constraintAddedModel <- m
    for(s in skillList)
    {
      constraintAddedModel <- ompr::add_constraint(constraintAddedModel, sum_expr(checkSkill(staff = i, skill = s) * x[i,j,k], i = 1:globals$numOfStaff) >= 1, j = 1:globals$numOfDays, k = 1:globals$numOfShifts)
    }
    return(constraintAddedModel)
  }
  
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  #Server Side: model build
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
  observeEvent(input$runOptimizer,
               {
                 #erase any existing model or result.
                 result <- NULL
                 model <- NULL
                 
                 numOfStaff <- globals$numOfStaff
                 #print QA message to console
                 message(sprintf("There are %s staff to schedule", numOfStaff))
                 
                 numOfDays <- globals$numOfDays
                 #print QA message to console
                 message(sprintf("There are %s days to schedule", numOfDays))
                 
                 numOfShifts <- globals$numOfShifts
                 #print QA message to console
                 message(sprintf("There are %s shifts to schedule", numOfShifts))
                 
                 #create a blank model object
                 model <- ompr::MIPModel() %>%
                   
                         #add decision variable
                         ompr::add_variable(x[i,j,k], i = 1:numOfStaff, j = 1:numOfDays, 
                                            k = 1:numOfShifts, type = "binary") %>%
                           
                         #set objective function
                         ompr::set_objective(sum_expr(checkPref(staff = i, day = j, shift = k) * x[i, j, k], i = 1:numOfStaff, 
                                                      j = 1:numOfDays, k = 1:numOfShifts),sense = "max") %>%
                          
                         #each staff can work a maximum of 1 shift a day
                         ompr::add_constraint(sum_expr(x[i,j,k], k = 1:numOfShifts) <= input$maxShiftsPerDay, 
                                              i = 1:numOfStaff, j =  1:numOfDays) %>%
                   
                         #each shift each day must have atleast 1 staff
                         ompr::add_constraint(sum_expr(x[i,j,k], i = 1:numOfStaff) >= input$minStaffPerShift, j =  1:numOfDays, k = 1:numOfShifts) %>%      
                   
                         #each staff can work a maximum of 5 days a week
                         ompr::add_constraint(sum_expr(x[i,j,k], j =  1:numOfDays, k = 1:numOfShifts) <= input$maxDaysPerWeek, 
                                              i = 1:numOfStaff) 
                         
                         #add skill based constraints
                         skillList <- input$skillsPerShift
                         

                         if(length(skillList) >=1)
                         {
                           model <- skillConstraints(model, skillList)
                         }
                         
                         #inspect model
                         model
                         
                         
                         #print message 
                         message(sprintf("solving integer programming model"))
                         
                         #solve integer programming model
                         result <- ompr::solve_model(model, ompr.roi::with_ROI(solver = "glpk", verbose = TRUE))
                         
                         #source solution and build roster
                         roster <- result %>% 
                           ompr::get_solution(x[i,j,k])
                         
                         roster <- roster[,c("i", "j", "k", "value")]
                         
                         #set readable column names
                         colnames(roster) <- c("staff", "day", "shift", "rostered")
                         
                         finalData <- reshape(data = roster, idvar = c("staff", "shift"), timevar = "day", 
                                                direction = "wide", v.names = NULL)
                         
                         names(finalData) <- names(globals$shiftPrefRaw)
                         
                         
                         #print message for qa purposes
                         if(result$status == "optimal")
                         {
                           output$successOptimizer <- renderText(paste(as.character(icon("check")), 
                                                                       "Done. Go to next tab to view and 
                                                                       download schedule", sep = " "))
                           output$finalData <- DT::renderDataTable(finalData)
                           globals$staffSchedule <- finalData
                         }else 
                         {
                           output$successOptimizer <- renderText(paste(as.character(icon("times")), 
                                                                       "Optimal solution not found. Please 
                                                                       change contraints and try again", sep = " "))
                           output$finalData <- NULL
                         }
               })
  
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  #Server Side: Download and error handlers
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
  
  #Download shift preference template
  output$shiftPreferenceTemplate <- downloadHandler(
    filename = function() {
      paste('shiftPreferenceTemplate', '.csv', sep='')
    },
    content = function(con) {
      write.csv(globals$shiftPrefRaw, con, row.names = FALSE)
    }
  )
  
  #Download skill templates
  output$skillTemplate <- downloadHandler(
    filename = function() {
      paste('skillTemplate', '.csv', sep='')
    },
    content = function(con) {
      write.csv(globals$staffSkillRaw, con, row.names = FALSE)
    }
  )
  
  #Download final data
  output$downloadSchedule <- downloadHandler(
    filename = function() {
      paste('staffSchedule', '.csv', sep='')
    },
    content = function(con) {
      write.csv(globals$staffSchedule, con, row.names = FALSE)
    }
  )
  
  #error message for max days per week constraint
  observeEvent(input$maxDaysPerWeek,
               {
                 output$errorMaxDaysPerWeek <- renderText({
                   if(input$maxDaysPerWeek < 1 | input$maxDaysPerWeek > 7)
                   {
                     stop(safeError(paste("Please select between 1 to 7")))
                   }
                 })#ends error
               })#ends observe event
  
  #error message for min staffing constraint
  observeEvent(input$minStaffPerShift,
               {
                 output$errorMinStaffPerShift <- renderText({
                   if(input$minStaffPerShift < 0 | input$minStaffPerShift > globals$numOfStaff)
                   {
                     stop(safeError(paste("Minimum staffing must be atleast 0 and never more than available staff")))
                   }
                 })#ends error
               })#ends observe event
  
  #error message for max shifts constraint
  observeEvent(input$maxShiftsPerDay,
               {
                 output$errorMaxShiftsPerDay <- renderText({
                   if(input$maxShiftsPerDay < 1 | input$maxShiftsPerDay > globals$numOfShifts)
                   {
                     stop(safeError(paste("Maximum number of shifts must be atleast 1 and never more than available shifts")))
                   }
                 })#ends error
               })#ends observe event
  
}

# Run the application 
shinyApp(ui = ui, server = server)

