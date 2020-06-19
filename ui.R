dashboardPagePlus(
    enable_preloader = TRUE,
    header = dashboardHeaderPlus(
        
        title = tagList(img(src = "img/CF_Logo_Final.jpg", width= '230', style= 'margin-top: -5px; margin-left: -20px')),
        
        enable_rightsidebar = FALSE,
        
        # John Deere Logo in the upper right corner
        tags$li(class = "dropdown", img(src='logon.png', style= 'margin-right:10px; margin-top:7px')),
        # tags$li(class = "dropdown",style= 'margin-right:50px; margin-top:21px', userOutput("user_info"))
        
        userOutput("user_info")
    ),
    
    sidebar = dashboardSidebar(
        width = 280,
        sidebarMenu(
            id = "tabs",
            menuItem(text = "Live Monitoring",tabName = "dashboard",icon = icon("calendar")),
            menuItem(text = "SPC Dashboard",tabName = "performance",icon = icon("dashboard")),
            menuItem(text = "Override Performance",tabName = "over_per",icon = icon("expand"))
            # menuItem(text = "Override Performance",tabName = "cap",icon = icon("briefcase")),
            # menuItem(text = "Report Download",tabName="Report",icon = icon("expand"))
        )
    ),
    
    body = dashboardBody(
        useShinyjs(),
        useShinyalert(),
        useSweetAlert(),
        setShadow(class = "box"),
        tags$head(tags$style(HTML("div.col-sm-7 {padding:1px}"))),
        tags$head(tags$style(HTML("div.col-sm-5 {padding:1px}"))),
        tags$head(tags$style(HTML("div.col-sm-6 {padding:1px}"))),
        tags$head(tags$style(HTML("div.col-sm-8 {padding:1px}"))),
        tags$head(tags$style(HTML("div.col-sm-4 {padding:1px}"))),
        tags$head(tags$style(HTML("div.col-sm-3 {padding:1px}"))),
        tags$head(
            tags$link(
                rel = "stylesheet",
                type = "text/css",
                href = "css/jdify.css"
            )
        ),
        tags$div(tags$style(HTML( ".dropdown-menu{z-index:99999 !important;}"))),
        
        tabItems(
            
            ############################################################ Live Monitoring TAB ############################################################    
            ###########################################################################################################################################

            tabItem(tabName = "dashboard",
                    tabsetPanel(type= "tabs",
                                tabPanel("Live",
                                         fluidRow(column(8),
                                                  column(3, downloadBttn(
                                                      outputId = "live_report",
                                                      label = "Get Report",
                                                      style = "material-flat"
                                                  )
                                                  )
                                             
                                         ),
                                         fluidRow(
                                             column(3,
                                                    box(title = "Today's OverRide",width = 12,solidHeader = TRUE,status="primary",
                                                        highchartOutput("oc_count_plot") %>% withSpinner(color="#0dc5c1")
                                                    )
                                             ),
                                             
                                             column(9,
                                                    box(title = " Top 10 Overrideded Parameter",width = 12,solidHeader = TRUE,status="primary",
                                                        plotlyOutput("oc_parameter_plot") %>% withSpinner(color="#0dc5c1")
                                                    )
                                             )
                                         ),
                                         fluidRow(
                                             box(title = "Top 25 Over and Under Specfication ",width = 12,solidHeader = TRUE,status="primary",
                                                 highchartOutput("over_under_specification_plot") %>% withSpinner(color="#0dc5c1")
                                             )
                                         )
                                ),
                                tabPanel("Department Wise",
                                         fluidRow(
                                             column(3, align= "center", selectInput("selected_live_dept","Department",choices = as.character(unique(Dept_Choice)))),
                                             column(2,
                                                    align="center",
                                                    airDatepickerInput(
                                                        inputId = "DW_date_range",
                                                        label = "Select Date",
                                                        range = FALSE, value = Sys.Date(),
                                                        dateFormat = "yyyy-mm-dd"
                                                    )
                                             ),
                                             column(2, align= "center", actionBttn(
                                                 inputId = "get_data_live",
                                                 label = "Get Data",
                                                 style = "fill"
                                             )),
                                             column(2, align= "center", downloadBttn(
                                                 outputId = "dept_report",
                                                 label = "Get Data",
                                                 style = "fill"
                                             ))
                                         ),
                                         fluidRow(
                                             box(title = "Top 10 Overrideded Parameter",width = 12, height = "600px", solidHeader = TRUE,status="primary",
                                                 plotlyOutput("DW_parameter_plot", height = 500) %>% withSpinner(color="#0dc5c1")
                                         )),
                                         fluidRow(
                                             box(title = "Top 25 Over and Under Specfication",width = 12, height = "500px", solidHeader = TRUE,status="primary",
                                                 highchartOutput("over_under_specification_live") %>% withSpinner(color="#0dc5c1")
                                             )
                                         )  
                                )
                    )

            ),
            
############################################################ SPC Dashboard TAB ############################################################    
###########################################################################################################################################

            tabItem(tabName = "performance",
                    fluidRow(
                        boxPlus(title = "Control Panel",width = 12,solidHeader = FALSE,status="primary",
                                column(2, align= "center", selectInput("selected_dept","Department",choices = as.character(unique(Dept_Choice)))),
                                column(2, align= "center", selectInput("selected_station","Station",choices = as.character(unique(ST_Choice)))),
                                column(2, align= "center", selectInput("selected_SPGroup", "Parameter Group", choices =as.character(unique(SPG_Choice)))),
                                column(2, align= "center", selectInput("parameter_Name", "Parameter Name", choices = as.character(unique(ParameterName_choice)))),
                                
                                column(2,
                                       align="center",
                                       airDatepickerInput(
                                           inputId = "selected_date_range",
                                           label = "Time period",
                                           range = TRUE, value = c(Sys.Date()-7, Sys.Date())
                                       )
                                ),
                                column(2, align= "center", fluidRow(
                                    actionBttn(
                                        inputId = "top_get_data",
                                        label = "Get Data",
                                        style = "fill"
                                    )
                                ),
                                fluidRow(
                                    downloadBttn(
                                        outputId = "spc_report",
                                        label = "Get Report",
                                        style = "fill"
                                    )
                                )
                                )
                        )
                    ),

                    
                    ######################## PLots #############################
                    
                    fluidRow(
                     column(9, 
                        boxPlus(title = "Control Chart Torque Values", width = '12',collapsible = FALSE,
                                height = 'auto', status="primary", solidHeader = TRUE,closable = FALSE,
                                plotlyOutput("tourque_performance",height = 700)%>% withSpinner(color="#feda15")
                                )
                        ),
                     column(3, 
                         fluidRow(shinydashboard::valueBoxOutput(outputId = "valuebox1", width = 12)),
                         fluidRow(shinydashboard::valueBoxOutput(outputId = "valuebox2", width = 12)),
                         fluidRow(shinydashboard::valueBoxOutput(outputId = "valuebox3", width = 12)),
                         fluidRow(shinydashboard::valueBoxOutput(outputId = "valuebox4", width = 12)),
                         fluidRow(shinydashboard::valueBoxOutput(outputId = "valuebox5", width = 12)),
                         fluidRow(shinydashboard::valueBoxOutput(outputId = "valuebox6", width = 12)
                         )
                         
                     )
                     ),
                    
                    fluidRow(
                               boxPlus(title = "Torque Normal Distribution Chart",
                                       width = 9, closable = FALSE, collapsible = FALSE,height = '800px', status="primary",
                                       solidHeader = TRUE, 
                                       plotlyOutput("normal_distribution", height = 700) %>% withSpinner(color="#feda15"),
                                       fluidRow(descriptionBlock(
                                           text = textOutput('nd_description'), 
                                           right_border = FALSE,
                                           margin_bottom = FALSE
                                       ))
                               ),
                               
                               boxPlus(title = "Torque Values Interpretations",
                                       solidHeader = TRUE,
                                       closable = FALSE,
                                       width = 3,
                                       height = 800,
                                       status = "primary",
                                       collapsible = FALSE,
                                       tabPanel(icon = icon("file-text-o"),'Data',
                                                column(12,tableOutput("interpretation") %>% withSpinner(color="#feda15"))
                                       )
                               )
                        
                        )
                        
                        
                    ),
            
############################################################################################################################################
# END of SPC Dashboard TA
############################################################################################################################################
            


############################################################ OverRide Performance TAB ######################################################
############################################################################################################################################ 

 tabItem(tabName = "over_per",
        fluidRow(),
         # boxPlus(title = "Control Panel",solidHeader = FALSE,width = '12',collapsible = TRUE,height = 'auto',status="primary",
          fluidRow(
            column(2, align="center",
                  selectInput("selected_year","Year",choices = c(2019:2050),
                  selected = as.numeric(format(Sys.Date(),'%Y')),width = "85%")),
            
            column(2, align="center",
                   materialSwitch("over_per_month_option","By Month",status="info",inline=FALSE)),
            
            column(2, align="center", hidden(div(id="over_per_month_id",
                   selectInput("selected_month","Month",
                   choices = c("January"="01", "February"="02", "March"="03", "April"="04", "May"="05",
                               "June"="06", "July"="07", "August"="08", "September"="09",
                               "October"="10", "November"="11", "December"="12"),
                   selected = format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%m"))))
                   ),
            
            column(2, align="center",
                  hidden(div(id="over_per_week_option_material_id",
                  materialSwitch("over_per_week_option","By Week",status="info",inline=FALSE)))
                  ),
            
            column(2, align="center",
                   hidden(div(id="over_per_week_id",
                         airDatepickerInput(inputId = "selected_date",
                                            label = "Weeks with Dates",
                                            range = TRUE, value = c(Sys.Date()-7, Sys.Date())
                                            )))),
            column(1,align="center", actionButton("get_or_data","Get Data")),
                            ),
        fluidRow(column(9, ""),
                 column(3, align="center",
                        downloadBttn(
                            outputId = "over_report",
                            label = "Get Report",
                            style = "material-flat"
                        ))
                 ),
        fluidRow(),
                    ############  All charts #############          
                    fluidRow(
                        column(6,
                               boxPlus(title = "Top 10 Parameter in OverRide",solidHeader = TRUE,width = '12',collapsible = TRUE,height = 'auto',status="primary",
                                   highchartOutput("parameter_issue") %>% withSpinner(color="#0dc5c1") 
                               )
                        ),
                        column(6,
                               boxPlus(title = "Top 10 Station in OverRide ",solidHeader = TRUE,width = '12',collapsible = TRUE,height = 'auto',status="primary",
                                   highchartOutput("overrides_by_station") %>% withSpinner(color="#0dc5c1") 
                               )
                        )
                        
                    ),
                    fluidRow(
                        column(6,
                               boxPlus(title = "Top 10 Department in OverRide",solidHeader = TRUE,width = '12',collapsible = TRUE,height = 'auto',status="primary",
                                   highchartOutput("overrides_by_department") %>% withSpinner(color="#0dc5c1") 
                               )
                        ),
                        column(6,
                               boxPlus(title = "Top 10 OverRide Code in OverRide",solidHeader = TRUE,width = '12',collapsible = TRUE,height = 'auto',status="primary",
                                   highchartOutput("overrides_by_person") %>% withSpinner(color="#0dc5c1")
                               )
                        )
                        
                    ),
                    fluidRow(
                        column(6,
                               boxPlus(title = "Top 10 OverRide Code in OverRide",solidHeader = TRUE,width = '12',collapsible = TRUE,height = 'auto',status="primary",
                                   highchartOutput("overrides_by_oc_code") %>% withSpinner(color="#0dc5c1") 
                               )
                        ),
                        column(6,
                               boxPlus(title = "Top 10 Comment in OverRide",solidHeader = TRUE,width = '12',collapsible = TRUE,height = 'auto',status="primary",
                                   highchartOutput("overrides_by_comment") %>% withSpinner(color="#0dc5c1")
                               )
                        )
                        
                    )
                    
            )

############################################################################################################################################
# END OverRide Performance TAB
############################################################################################################################################
            ),
            
            ################## All End ########################        
    rightsidebar = rightSidebar(),
    title = "JD Shiny APP",
    footer = dashboardFooter(
        left_text = img(src = "footer.png"),
        right_text = img(src = "img/footer.jpg")
    )     
    )
)
