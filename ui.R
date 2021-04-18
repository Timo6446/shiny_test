#========================================================================================================================================================
###################
# Shiny Insights
###################

#============================================================================
#Packages
library(shiny)
library(shinyWidgets)
library(shinymanager)
library(shinytoastr)
library(shinyFeedback)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyjs)

library(tidyverse)
library(ggplot2)
library(data.table)
library(DT)
library(plotly)
library(formattable)
library(sparkline)
library(htmlwidgets)
library(htmltools)

#browseVignettes(package = "shinyFeedback")

#============================================================================
# Define UI
ui <- fluidPage("", 
                
                #Global Settings
                #==================================
                theme = shinytheme("flatly"),
                shinyBS:::shinyBSDep,
                useShinydashboard(),
                useShinydashboardPlus(),
                useSweetAlert(),
                getDependency('sparkline'),
                useToastr(),
                useShinyFeedback(),
                setBackgroundColor("#F5F9FC"),
                #setShadow(class = "box"),
                setShadow(id = "TestPanel01"),
                setShadow(id = "TestPanel02"),
                setShadow(id = "TestPanel03"),
                setShadow(id = "TestPanel04"),
                setShadow(id = "TestPanel05"),
                setShadow(id = "TestPanel06"),
                setShadow(id = "TestPanel07"),
                setShadow(id = "TestPanel08"),
                setShadow(id = "TestPanel09"),
                setShadow(id = "TestPanel10"),
                setShadow(id = "TestPanel11"),
                setShadow(id = "TestPanel12"),
                setShadow(id = "TabBox01"),
                setShadow(id = "Box01"),
                setShadow(id = "ActionButton01"),
                setShadow(id = "BoxKnob"),
                setShadow(id = "WellPanelTotal"),
                setShadow(id = "BoxTimeBlock"),
                setShadow(id = "BoxYouhou01"),
                setShadow(id = "BoxYouhou02"),
                setShadow(id = "InfoBox01"),
                setShadow(id = "ValueBox01"),

                #==================================
                # tags$script(HTML(
                #   "$(selector).tooltip({title:'somthing~', trigger:'hover', delay:{hide:800}, placement:'bottom'});"
                # )),
                
                #==================================
                fluidRow(
                  column(8,
                         br(),
                         p(strong("Dashboard123", style = "font-size:50px;"), #style="text-align: center;")),
                           span(" - Dashboard456", style = "color:grey80"))),
                  column(3, align = "right",
                         br(),
                         img(src = "LogoTeva.png", height = 60, width = 160, style="padding-bottom:1px; margin-bottom: 1px")),
                  column(1, align = "middle",
                         br(style = "line-height:38px"),
                         actionButton("ActionButton01", "", icon = icon("info")))
                  ),
                
                #==================================
                # fluidRow(
                #   column(10, align = "center", offset = 1,
                #          fluidRow(img(src = "LogoTeva.png", height = 120, width = 210, style="padding-bottom:1px; margin-bottom: 1px"))
                #          #fluidRow(h5("Brand Management"))
                #          ),
                #   column(1, align = "right",
                #          br(),
                #          br(),
                #          actionButton("ActionButton01", "", icon = icon("info")))
                #   ),
                
                #==================================
                bsTooltip(id = "TestPanel02", title = "This is a wellPanel",
                          placement = "bottom", trigger = "hover", options = list(animation = TRUE)),
                
                #==================================
                br(),
                fluidRow(
                  #==================
                  column(4, align = "center",
                         wellPanel(id = "TestPanel01", style = "background: white",
                                   h5("Test01"),
                                   
                                   dropdown(
                                     
                                     tags$h3("List of Input"),
                                     
                                     pickerInput(inputId = 'xcol2',
                                                 label = 'X Variable',
                                                 choices = names(iris),
                                                 options = list(`style` = "btn-info")),
                                     
                                     pickerInput(inputId = 'ycol2',
                                                 label = 'Y Variable',
                                                 choices = names(iris),
                                                 selected = names(iris)[[2]],
                                                 options = list(`style` = "btn-warning")),
                                     
                                     sliderInput(inputId = 'clusters2',
                                                 label = 'Cluster count',
                                                 value = 3,
                                                 min = 1, max = 9),
                                     
                                     style = "unite", icon = icon("gear"),
                                     tooltip = tooltipOptions(title = "Click to see inputs !"),
                                     status = "primary", width = "250px",
                                     animate = animateOptions(
                                       enter = animations$fading_entrances$fadeInLeftBig,
                                       exit = animations$fading_exits$fadeOutRightBig
                                     )
                                   ))),
                  
                  #==================
                  column(4, align = "center",
                         wellPanel(id = "TestPanel02", style = "background: white",
                                   h5("Test02"),
                                   switchInput(
                                     inputId = "Id014",
                                     onStatus = "success", 
                                     offStatus = "danger"
                                   ),
                                   br(),
                                   textInput(
                                     "myInput",
                                     "Warn if >3 characters",
                                     value = ""
                                   )
                         )),

                  #==================
                  column(4, align = "center",
                         box(id = "TestPanel03",
                             title = "TestBox",
                             collapsible = TRUE,
                             solidHeader = TRUE,
                             #background = "olive",
                             status = "primary",
                             width = 12,
                             icon = icon("robot"),
                             h5("Test03"),
                             tags$i(class = "fas fa-robot", style="font-size: 32px; color: gray"))
                         )
                ),
                
                #==================================
                br(),
                wellPanel(id = "TestPanel08", style = "background: white",
                       
                          selectInput("select", "Select an option",
                                      c("This one is okay" = "ok",
                                        "This will give an error" = "error")),
                          
                          # Wrap the button in the function `withBusyIndicatorUI()`
                          withBusyIndicatorUI(
                            actionButton(
                              "uploadFilesBtn",
                              "Test data",
                              class = "btn-primary"
                            )
                          )   
                ),

                #==================================
                br(),
                wellPanel(id = "TestPanel05", style = "background: white",
                          
                          #==================
                          fluidRow(
                            column(6,
                                   navlistPanel(well = FALSE,
                                                "Header A",
                                                tabPanel("Component 1"),
                                                tabPanel("Component 2"),
                                                "Header B",
                                                tabPanel("Component 3"),
                                                tabPanel("Component 4"),
                                                tabPanel("Component 5")
                                   )),
                            column(6,
                                   #ithSpinner(plotlyOutput("Plotly01"), image = "teva_logo_animation.gif", image.height = 300, image.width = 400))),
                                   withSpinner(plotlyOutput("Plotly01"), image = "Spinner_Test.gif"))),
                          
                          #==================
                          fluidRow(
                            br(),
                            br(),
                            column(12,
                                   fluidRow(
                                     box(id = "Box01",
                                         solidHeader = FALSE,
                                         title = "Status summary",
                                         background = NULL,
                                         width = 12,
                                         status = "primary",
                                         footer = fluidRow(
                                           column(
                                             width = 6,
                                             descriptionBlock(
                                               number = "17%", 
                                               numberColor = "green", 
                                               numberIcon = icon("caret-up"),
                                               header = "$35,210.43", 
                                               text = "TOTAL REVENUE", 
                                               rightBorder = TRUE,
                                               marginBottom = FALSE
                                             )
                                           ),
                                           column(
                                             width = 6,
                                             descriptionBlock(
                                               number = "18%", 
                                               numberColor = "red", 
                                               numberIcon = icon("caret-down"),
                                               header = "1200", 
                                               text = "GOAL COMPLETION", 
                                               rightBorder = FALSE,
                                               marginBottom = FALSE
                                             )
                                           )
                                         )
                                     ))
                            ))),
                
                #==================================
                br(),
                wellPanel(id = "TestPanel10", style = "background: white",
                          
                          #===============
                          fluidRow(
                            column(
                              12,
                              actionButton(
                                "counter_button",
                                "+1"
                              )
                            )
                          ),
                          
                          #===============
                          br(),
                          fluidRow(
                            valueBoxModuleUI(
                              id = "betterBox",
                              subtitle = "Counter",
                              textColor = "#FFF",
                              #width = 4,
                              icon = icon("rocket"),
                              iconColor = "#FFF",
                              backgroundColor = "#107CAB"
                            )
                          )
                ),
                
                #==================================
                br(),
                wellPanel(id = "TestPanel12", style = "background: white",  
                          fluidRow(
                            column(
                              12,
                              br(),
                              loadingButton(
                                "myLoadingButton", 
                                label = "Submit"
                              ),
                              loadingButton(
                                "myCancelButton", 
                                label = "Cancel",
                                class = "btn btn-danger",
                                loadingLabel = "Cancelling...",
                              )
                            )
                          )
                ),

                #==================================
                br(),
                wellPanel(id = "TestPanel06", style = "background: white",
                          fluidRow(
                            column(6, 
                                   h3("TestDT123"),
                                   br(),
                                   withSpinner(DT::dataTableOutput("DT01"))),
                            column(6, 
                                   h3("TestPlot123"),
                                   plotlyOutput("Plotly02"))
                          )      
                ),
                
                #==================================
                br(),
                wellPanel(id = "TestPanel07", style = "background: white",
                          fluidRow(
                            column(6, 
                                   h3("TestDT123"),
                                   br(),
                                   withSpinner(DT::dataTableOutput("DT02"))),
                            column(6,
                                   verbatimTextOutput("print"))
                )),

                #==================================
                br(),
                wellPanel(id = "TestPanel09", style = "background: white",
                          fluidRow(
                            column(12, 
                                   h3("TestDT_Formattable & Sparkline"),
                                   br(),
                                   withSpinner(uiOutput("DT03")))
                          )),
                
                #==================================
                br(),
                wellPanel(id = "TestPanel11", style = "background: white",

                          h3("Iris k-means clustering"),
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                inputId = "xcol",
                                label = "X Variable",
                                choices = names(iris)
                              ),
                              selectInput(
                                inputId = "ycol",
                                label = "Y Variable",
                                choices = names(iris),
                                selected = names(iris)[[2]]
                              ),
                              numericInput(
                                inputId = "clusters",
                                label = "Cluster count",
                                value = 3,
                                min = 1,
                                max = 9
                              )
                            ),
                            mainPanel(
                              plotOutput("PlotKMeans")
                            )
                          )
                          ),
                
                
                #==================================
                br(),
                fluidRow(
                  
                  #==================
                  tabBox(id = "TabBox01", selected = NULL, title = "TestTabBox", width = 4,
                         tabPanel("Test123",
                                  h5("Test123"),
                                  radioGroupButtons(
                                    inputId = "Id071",
                                    label = "Test",
                                    choices = c("A", 
                                                "B", "C", "D"),
                                    status = "primary",
                                    checkIcon = list(
                                      yes = icon("ok", 
                                                 lib = "glyphicon"),
                                      no = icon("remove",
                                                lib = "glyphicon"))#,
                                    #direction = "vertical"
                                  )),
                         tabPanel("Test456",
                                  h5("Test456"),
                                  verbatimTextOutput("auth_output")),
                         navbarMenu("TestNavbarMenu",
                                    tabPanel("Test444",
                                             h5("Test444")),
                                    tabPanel("Test555",
                                             h5("Test555")))),
                  
                  #==================
                  tags$style("
                  .info-box-icon.bg-aqua {
                    background-color: #3B8E6C !important;
                  }"),
                  
                  infoBox("Test123", value = 10, subtitle = "Test",
                          icon = shiny::icon("bar-chart"), color = "aqua", width = 4,
                          href = NULL, fill = FALSE),
                  
                  #==================
                  tags$style(".small-box.bg-yellow { background-color: #3B8E6C !important; color: #fff !important; }"),
                  
                  valueBox(
                    width = 4,
                    color = "yellow",
                    value = "5.00",
                    subtitle = NULL,  
                    icon = tags$i(class = "fas fa-thumbs-up", style="font-size: 32px; color: white") #thumbs-up can be replaced with other icons (e.g. robot)
                  )
                  
                  #infoBoxOutput("InfoBox01"),
                  #valueBoxOutput("ValueBox01")
                ),
                
                #==================================
                br(),
                fluidRow(
                  column(12,  align = "center",
                         wellPanel(id = "TestPanel04", style = "background: white",
                                   h3("Test04"),
                                   starBlock(4, color = "olive")))
                ),
                
                #==================================
                br(),
                tabsetPanel(
                  
                  tabPanel("Test01", icon = icon("robot"),
                           br(),
                           p("Das ist ein Test 01")
                  ),
                  
                  tabPanel("Test02", icon = icon("robot"),
                           br(),
                           p("Das ist ein Test 02")
                  )
                ),
                
                tags$style(HTML("
                  .box.box-solid.box-danger>.box-header {
                    color:#fff;
                    background:#3B8E6C
                  }
                  
                  .box.box-danger>.box-header {
                    color:#000000;
                    background:#fff
                 }
                  
                  .box.box-solid.box-danger{
                    border-bottom-color:#3B8E6C;
                    border-left-color:#3B8E6C;
                    border-right-color:#3B8E6C;
                    border-top-color:#3B8E6C;
                  }
                  
                  .box.box-danger{
                    border-bottom-color:#3B8E6C;
                    border-left-color:#3B8E6C;
                    border-right-color:#3B8E6C;
                    border-top-color:#3B8E6C;
                  }
                  
                  .btn.btn-box-tool.btn-sm.btn-danger {
                    color:#3B8E6C;
                    background:#3B8E6C
                  }
                                    ")),
                
                fluidRow(
                  column(6,
                  hr(),
                  box(id = "BoxYouhou01", width = 12, title = "youhou", status = "danger", solidHeader = FALSE,
                      collapsible = TRUE,
                      "Box content"
                  )),
                  
                  column(6,
                         hr(),
                         box(id = "BoxYouhou02", width = 12, title = "youhou", status = "danger", solidHeader = TRUE,
                             collapsible = TRUE,
                             "Box content"
                         ))
                ),
                
                #Box with boxPad inputs
                #==================================
                hr(),
                box(
                  id = "BoxKnob",
                  title = "Box with boxPad containing inputs",
                  status = "primary",
                  collapsible = TRUE,
                  width = NULL,
                  fluidRow(
                    column(
                      width = 6,
                      knobInput(
                        inputId = "myKnob",
                        skin = "tron",
                        readOnly = FALSE,
                        label = "",
                        value = 50,
                        min = 0,
                        displayPrevious = FALSE,
                        fgColor = "#428BCA",
                        inputColor = "#428BCA"
                      )
                    ),
                    column(
                      width = 6,
                      h5("TestKnob")
                    )
                  )
                ),
                
                #==================================
                #Time Block
                box(
                  id = "BoxTimeBlock",
                  title = "Timeline",
                  collapsible = TRUE,
                  status = "info",
                  timelineBlock(
                    width = 12,
                    timelineEnd(color = "red"),
                    timelineLabel(2018, color = "teal"),
                    timelineItem(
                      title = "Item 1",
                      icon = icon("gears"),
                      color = "olive",
                      time = "now",
                      footer = "Here is the footer",
                      "This is the body"
                    ),
                    timelineItem(
                      title = "Item 2",
                      border = FALSE
                    ),
                    timelineLabel(2015, color = "orange"),
                    timelineItem(
                      title = "Item 3",
                      icon = icon("paint-brush"),
                      color = "maroon"
                    ),
                    timelineStart(color = "purple")
                  )
                )
                
                #OPTIONAL
                #==================================
                # flipBox(
                #   id = "myflipbox", 
                #   #trigger = "click",
                #   width = 12,
                #   front = div(
                #     class = "text-center",
                #     h1("Flip on click"),
                #     img(
                #       src = "https://image.flaticon.com/icons/svg/149/149076.svg",
                #       height = "300px",
                #       width = "100%"
                #     )
                #   ),
                #   back = div(
                #     class = "text-center",
                #     height = "300px",
                #     width = "100%",
                #     h1("Flip on click"),
                #     p("More information....")
                #   )
                # )

)

# Wrap your UI with secure_app
ui <- secure_app(ui)

#============================================================================
#Send E-Mail from R

# library(mailR)
# #system.file("java", package = "mailR")
# send.mail(from = "timo.buckle@gmail.com",
#           to = c("timo.buckle@gmail.com"),
#           subject = "Test123",
#           body = "HelloWorld",
#           smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "timo.buckle@gmail.com", passwd = "pp8er3dd", ssl = TRUE), 
#           authenticate = TRUE, send = TRUE)

#============================================================================
#Stock prices

# library(tidyquant)
# library(quantmod)
# library(recipes)
# library(tidyverse)

# tickers = c("AAPL", "NFLX", "AMZN", "K", "O")
# prices <- tq_get(tickers,
#                  from = "2020-01-01",
#                  to = "2021-03-28",
#                  get = "stock.prices")
#View(prices)

# prices %>%
#   ggplot(aes(x = date, y = adjusted, color = symbol)) +
#   geom_line() +
#   facet_wrap(~symbol,scales = 'free_y') +
#   theme_classic() +
#   labs(x = 'Date',
#        y = "Adjusted Price",
#        title = "Price Chart") +
#   scale_x_date(date_breaks = "month",
#                date_labels = "%b\n%y")
# prices %>%
#   ggplot(aes(x = date, y = adjusted)) +
#   geom_line(aes(color = symbol)) +
#   theme_classic() +
#   labs(x = 'Date',
#        y = "Adjusted Price",
#        title = "Apple price chart") #+
#   #scale_y_continuous(breaks = seq(0,300,10))

#=======================================================================================================================================================================