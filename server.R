function(input, output, session){
  
  #================================================================================= User Information =============================================================================== 
  
  user_details<-'v0mg1uf'
  # user_details<-session$user
  output$user_info<-renderUser({
    dashboardUser(name = user_details, title = "EPPS Dashboard", src = "online.png",
                  HTML('<div>
         <a href="#"><i class="fa fa-circle text-success"></i> Online</a>
         </div>')
    )
  }) 
  
  ############################################################ Live Monitoring TAB ############################################################    
  ########################################################################################################################################### 
  
  oc_count_plot_gen<-reactive({
    invalidateLater(180000)
    qr<-paste0("Select count(distinct overrideid) as cn from OVERRIDE_LOG with (NOLOCK) where convert(date,entrytimestamp)=convert(date,getdate())")
    cn_data<-dbGetQuery(sql_data_con,qr)
    dataLabel<- paste0("<div style=text-align:center><span style=font-size:300%;>", cn_data$cn,
                       "</span><br/><span style='font-size:100%;color:#808080;'></div>")
    hc <- highchart() %>%
      hc_chart(type = "gauge") %>%
      hc_pane(startAngle = -150,
              endAngle = 150) %>%
      hc_yAxis(
        min = 0,
        max = 400
      )
    hc %>%  hc_add_series(data = cn_data$cn, name = "Override Count",dataLabels=list(borderWidth=0, useHTML=TRUE,format=dataLabel))
  })
  
  
  output$oc_count_plot<-renderHighchart({
    oc_count_plot_gen()
  })
  
  
  ############# Top OverRide Parameter ID ###################
  
  oc_parameter_plot_gen<-reactive({
    invalidateLater(180000)
    qr<-paste0("Select top 10
		stations.name as StName, 
        DEPARTMENTS.Name as DeptName,
        ASSEMBLY_PARAMETERS.Name as PName,
		OVERRIDE_LOG.parameterid,
       count(OVERRIDE_LOG.parameterid)as fail_count from stations
			   inner join STATION_PARAMETER_GROUPS on STATIONS.stationid=station_parameter_groups.stationid
        and STATIONS.archived_timestamp is null
		inner join DEPARTMENTS ON DEPARTMENTS.DEPTID=STATIONS.DEPTID
		inner join ASSEMBLY_PARAMETERS on station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
        and station_parameter_groups.archived_timestamp is null
               inner join OVERRIDE_LOG on OVERRIDE_LOG.parameterid=assembly_parameters.ParameterID
               and assembly_parameters.disabled_timestamp is NUll
               where
               convert(date,OVERRIDE_LOG.entrytimestamp)=convert(date,getdate()) 
               group by stations.name, DEPARTMENTS.Name, ASSEMBLY_PARAMETERS.Name, OVERRIDE_LOG.parameterid order by count(OVERRIDE_LOG.parameterid) desc")
    oc_parameter_data<-dbGetQuery(sql_data_con,qr)
    print("oc_parameter_data")
    print(oc_parameter_data)
    oc_parameter_data$Status<- ifelse(oc_parameter_data$fail_count == 0, "Good", 
                                      ifelse(oc_parameter_data$fail_count >= 1 & oc_parameter_data$fail_count <= 2, "Not Good", 
                                             ifelse(oc_parameter_data$fail_count >2 & oc_parameter_data$fail_count <=5, "Bad", 
                                                    ifelse(oc_parameter_data$fail_count >=6 & oc_parameter_data$fail_count <= 10, "Very Bad", 
                                                           ifelse(oc_parameter_data$fail_count > 10, "Worst", "")))) )
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "black"
    )
    a <- list(
      title = "Count",
      titlefont = f1,
      showticklabels = TRUE,
      exponentformat = "E"
    )
    
    ax <- list(
      title = "Departments",
      titlefont = f1,
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    
    ptly <- plot_ly(oc_parameter_data, x = oc_parameter_data$PName, y= oc_parameter_data$fail_count, color= oc_parameter_data$PName, type = "bar",
                 hoverinfo = 'text',
                 text = ~paste('<br>Station Name:<b> ', StName,
                               '<br>Status:<b> ', Status,
                               '<br>Override Count:<b> ', fail_count,
                               '<br> Departname Name:<b> ', DeptName,
                               '<br> Parameter Name:<b> ', PName)) %>% 
      layout(title= "Override Count", xaxis = ax, yaxis= a, showlegend = FALSE) %>%
      plotly::config(displaylogo = FALSE)
    ptly
    # hc <- highchart() %>%
    #   hc_chart(type = "column")%>%
    #   hc_xAxis(
    #     title="Parameter Overrided",
    #     categories = oc_parameter_data$Name) %>%
    #   hc_yAxis(title = list(text = "Override Count"))%>%
    #   hc_add_series(name = "Override Count", data = oc_parameter_data$fail_count,color="#367C2B",
    #                 tooltip = list(pointFormat = "<b>Station Name:</b> {point.StName}  <br/>
    #                                               <b>Departname Name:</b> {point.DeptName}<br/>
    #                                               <b>Parameter Name:</b> {point.PName}<br/>")
    #                 ) %>%
    #  hc_tooltip(headerFormat='',shared=TRUE)
    # hc
    
    
  })
  
  
  output$oc_parameter_plot<-renderPlotly({
    oc_parameter_plot_gen()
  })  
  
  
  ############# Over_Under_Specification_Chart ################ 
  
  over_under_specification_plot_gen<-reactive({
    invalidateLater(300000)
    qr<-paste0("Select top 25 
        stations.name, 
        DEPARTMENTS.Name as DeptName,
        ASSEMBLY_PARAMETERS.Name, 
        ASSEMBLIES.AssemblyName,
        count(case when assembly_results_data.IsAcceptable = 1 then 1 else null end) as ACCEPTED,
        count(case when assembly_results_data.IsAcceptable = 0 then 1 else null end) as REJECTED
        from stations
        inner join STATION_PARAMETER_GROUPS on STATIONS.stationid=station_parameter_groups.stationid
        and STATIONS.archived_timestamp is null
        inner join DEPARTMENTS ON DEPARTMENTS.DEPTID=STATIONS.DEPTID
        inner join ASSEMBLY_PARAMETERS on station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
        and station_parameter_groups.archived_timestamp is null
        inner join ASSEMBLY_RESULTS_DATA on ASSEMBLY_RESULTS_DATA.parameterid=ASSEMBLY_PARAMETERS.parameterid
        and assembly_parameters.disabled_timestamp is NUll
        inner join ASSEMBLIES on ASSEMBLY_RESULTS_DATA.AssemblyID=ASSEMBLIES.AssemblyID
        where convert(date,ASSEMBLY_RESULTS_DATA.entrytimestamp)=convert(date,getdate())
        group by stations.name, DEPARTMENTS.Name, ASSEMBLY_PARAMETERS.Name, ASSEMBLIES.AssemblyName 
        having count(case when ASSEMBLY_RESULTS_DATA.IsAcceptable = 0 then 1 else null end) > 0
        order by count(case when assembly_results_data.IsAcceptable = 0 then 1 else null end) desc")
    
    today_spec_data<-dbGetQuery(sql_data_con,qr)
    print("today_spec_data")
    print(today_spec_data)
    if(nrow(today_spec_data)==0)
    {
      returnValue()
      shinyalert::shinyalert("Harvester Quality System","No Records Present",type = "error")
    }
    else
    {
      today_spec_data$INCIDENT<-c(1:nrow(today_spec_data))
      hc<-highchart() %>%
        hc_add_series(data = today_spec_data, hcaes(INCIDENT, ACCEPTED), name = "ACCEPTED", type = "line",color="#367C2B",
                      tooltip = list(pointFormat = "<b>Station Name:</b> {point.name}<br/>
                                                    <b>Departname Name:</b> {point.DeptName}<br/>
                                                    <b>Parameter Name:</b> {point.Name}<br/>
                                                    <b>Vehicle Number:</b> {point.AssemblyName}<br/>
                                                    <b>Accepted:</b> {point.ACCEPTED}<br/>")) %>%
        hc_add_series(data = today_spec_data, hcaes(INCIDENT, REJECTED), name = "REJECTED", type = "line",color="#FFDE00",
                      tooltip = list(pointFormat = "<b>Rejected:</b> {point.REJECTED}<br/>")) %>%
        hc_yAxis(title = list(text = "Hours")) %>%
        hc_tooltip(crosshairs = TRUE)
      hc
    }
  })
  
  
  output$over_under_specification_plot<-renderHighchart({
    over_under_specification_plot_gen()
  })
  
  output$live_report <- downloadHandler(
    filename = function(){
      paste('Report', format(Sys.time(), paste("_%d%b%Y_%X")), '.html', sep = '')
    },
    content = function(file) {
      params <- list(OCP= oc_count_plot_gen(), OPP = oc_parameter_plot_gen(), OUSP = over_under_specification_plot_gen())
      src <- normalizePath('Live.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'FlexD.Rmd', overwrite = TRUE)
      out <- rmarkdown::render('FlexD.Rmd', output_format = flexdashboard::flex_dashboard())
      file.rename(out, file)
      # Get a nicely formatted date/time string
    }
  )
  
  
  ############################################################ Live Monitoring dEPARTMEN wISE ############################################################
  observeEvent(input$get_data_live,{
    if(is.null(input$get_data_live)||input$get_data_live==0)
    {
      returnValue()
    }
    else
    {
      ############# Top OverRide Parameter ID ###################
      
        DWqr<-paste0("Select top 10
		                  stations.name as StName, 
                      DEPARTMENTS.Name as DeptName,
                      ASSEMBLY_PARAMETERS.Name as PName,
		                  OVERRIDE_LOG.parameterid,
                      count(OVERRIDE_LOG.parameterid)as fail_count from stations
			                inner join STATION_PARAMETER_GROUPS on STATIONS.stationid=station_parameter_groups.stationid and STATIONS.archived_timestamp is null
		                  inner join DEPARTMENTS ON DEPARTMENTS.DEPTID=STATIONS.DEPTID
		                  inner join ASSEMBLY_PARAMETERS on station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid and station_parameter_groups.archived_timestamp is null
                      inner join OVERRIDE_LOG on OVERRIDE_LOG.parameterid=assembly_parameters.ParameterID and assembly_parameters.disabled_timestamp is NUll
                      where
                      convert(date,OVERRIDE_LOG.entrytimestamp)=convert(date,'",input$DW_date_range,"') and
                      DEPARTMENTS.NAME='",input$selected_live_dept,"'
                      group by stations.name, DEPARTMENTS.Name, ASSEMBLY_PARAMETERS.Name, OVERRIDE_LOG.parameterid order by count(OVERRIDE_LOG.parameterid) desc")
        DW_parameter_data<-dbGetQuery(sql_data_con,DWqr)
        if(nrow(DW_parameter_data)==0)
        {
          returnValue()
          shinyalert::shinyalert("Harvester Quality System","No Records Present",type = "error")
        }
        else{
          output$DW_parameter_plot<-renderPlotly({
            DW_parameter_data$Status<- ifelse(DW_parameter_data$fail_count == 0, "Good", 
                                              ifelse(DW_parameter_data$fail_count >= 1 & DW_parameter_data$fail_count <= 2, "Not Good", 
                                                     ifelse(DW_parameter_data$fail_count >2 & DW_parameter_data$fail_count <=5, "Bad", 
                                                            ifelse(DW_parameter_data$fail_count >=6 & DW_parameter_data$fail_count <= 10, "Very Bad", 
                                                                   ifelse(DW_parameter_data$fail_count > 10, "Worst", "")))) )
            print("DW_parameter_data")
            print(DW_parameter_data)
            
            f1 <- list(
              family = "Arial, sans-serif",
              size = 18,
              color = "black"
            )
            a <- list(
              title = "Count",
              titlefont = f1,
              showticklabels = TRUE,
              tickangle = 0,
              exponentformat = "E"
            )
            ax <- list(
              title = "Departments",
              titlefont = f1,
              zeroline = FALSE,
              showline = FALSE,
              showticklabels = FALSE,
              showgrid = FALSE
            )
            
            ptly <<- plot_ly(DW_parameter_data, x= DW_parameter_data$PName, y= DW_parameter_data$fail_count, color= DW_parameter_data$PName, type = "bar",
                            hoverinfo = 'text',
                            text = ~paste('<br>Station Name:<b> ', StName,
                                          '<br>Status:<b> ', Status,
                                          '<br>Override Count:<b> ', fail_count,
                                          '<br> Departname Name:<b> ', DeptName,
                                          '<br> Parameter Name:<b> ', PName)) %>% 
              layout(title= "Override Count", xaxis = ax, yaxis= a, showlegend = FALSE) %>%
              plotly::config(displaylogo = FALSE)
            ptly
          })
        }
       
        
      
      
      
      ############# Over_Under_Specification_Chart ################ 
        live_qr<-paste0("Select top 100
                         stations.name as StName,
                         ASSEMBLY_PARAMETERS.Name as Pname,
                         ASSEMBLIES.AssemblyName,
                         DEPARTMENTS.Name as DeptName,
                         count(case when assembly_results_data.IsAcceptable = 1 then 1 else null end) as ACCEPTED,
                         count(case when assembly_results_data.IsAcceptable = 0 then 1 else null end) as REJECTED
                         from stations
                         inner join STATION_PARAMETER_GROUPS on STATIONS.stationid=station_parameter_groups.stationid and STATIONS.archived_timestamp is null
                         inner join ASSEMBLY_PARAMETERS on station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid and station_parameter_groups.archived_timestamp is null
                         inner join ASSEMBLY_RESULTS_DATA on ASSEMBLY_RESULTS_DATA.parameterid=ASSEMBLY_PARAMETERS.parameterid and assembly_parameters.disabled_timestamp is NUll
                         inner join ASSEMBLIES on ASSEMBLY_RESULTS_DATA.AssemblyID=ASSEMBLIES.AssemblyID
                         INNER JOIN DEPARTMENTS ON DEPARTMENTS.DEPTID=STATIONS.DEPTID
                         where convert(date,ASSEMBLY_RESULTS_DATA.entrytimestamp)=convert(date,'",input$DW_date_range,"') and
                         DEPARTMENTS.NAME='",input$selected_live_dept,"'
                         group by stations.name, ASSEMBLY_PARAMETERS.Name, ASSEMBLIES.AssemblyName, DEPARTMENTS.Name 
                         having count(case when ASSEMBLY_RESULTS_DATA.IsAcceptable = 0 then 1 else null end) > 0
                         order by count(case when assembly_results_data.IsAcceptable = 0 then 1 else null end) desc")
        
        today_spec_data<-dbGetQuery(sql_data_con,live_qr)
        print("today_spec_data")
        print(today_spec_data)
        
          if(nrow(today_spec_data)==0)
          {
            returnValue()
            shinyalert::shinyalert("Harvester Quality System","No Records Present",type = "error")
          }
          else
          {
            today_spec_data$INCIDENT<-c(1:nrow(today_spec_data))
            output$over_under_specification_live<-renderHighchart({
              ousl<<-highchart() %>%
                hc_add_series(data = today_spec_data, hcaes(INCIDENT, ACCEPTED), name = "ACCEPTED", type = "line",color="#367C2B",
                              tooltip = list(pointFormat = "<b>Accepted:</b> {point.ACCEPTED}<br/>
                                                          <b>Station Name:</b> {point.StName}  <br/>
                                                          <b>Parameter Name:</b> {point.Pname}<br/>
                                                          <b>Vehicle Number:</b> {point.AssemblyName}<br/>
                                                          <b>Departname Name:</b> {point.DeptName}<br/>")) %>%
                hc_add_series(data = today_spec_data, hcaes(INCIDENT, REJECTED), name = "REJECTED", type = "line",color="#FFDE00",
                              tooltip = list(pointFormat = "<b>Rejected:</b> {point.REJECTED}<br/>")) %>%
                hc_yAxis(title = list(text = "Hours")) %>%
                hc_tooltip(headerFormat='',shared=TRUE)
              ousl
            })
          }
    }
    output$dept_report <- downloadHandler(
      filename = function(){
        paste('Report Department Wise', format(Sys.time(), paste("_%d%b%Y_%X")), '.html', sep = '')
      },
      content = function(file) {
        params <- list(OCP= ptly,  OUSP = ousl)
        src <- normalizePath('dept.Rmd')
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'dept.Rmd', overwrite = TRUE)
        out <- rmarkdown::render('dept.Rmd', output_format = flexdashboard::flex_dashboard())
        file.rename(out, file)
        # Get a nicely formatted date/time string
      }
    )
  })
  
  
  ############################################################ SPC Dashboard TAB ############################################################    
  ###########################################################################################################################################

  observeEvent(input$selected_dept,{
    # per_list_data_creation()
    updateSelectInput(session, 'selected_station', label = paste0("Station"),
                      choices = unique(FinalDB$StationName[FinalDB$DeptName == input$selected_dept])
                      )
  })


  observeEvent(input$selected_station,{
    updateSelectInput(session, 'selected_SPGroup', label = paste0("Parameter Group"),
                      choices = unique(FinalDB$SPGroupName[FinalDB$StationName == input$selected_station])
                      # choices = unique(parameter_group_list)
                      )
  })

  observeEvent(input$selected_SPGroup,{
    updateSelectInput(session, 'parameter_Name', label = paste0("Parameter Group"),
                      choices = unique(FinalDB$ParameterName[FinalDB$SPGroupName == input$selected_SPGroup])
                      # choices = unique(parameter_name)
                      )
  })
  
  observeEvent(input$top_get_data,{
    if(is.null(input$top_get_data)||input$top_get_data==0)
    {
      returnValue()
    }
    else
    {
      spc_qr<- paste0("Select ARD.EntryTimestamp as DATE_HX,
       ARD.DataValue,
       ARD.MINVALUE as MINVALUE,
       ARD.MAXVALUE as MAXVALUE,
       ARD.AssemblyID,
       ASSEMBLIES.AssemblyName
       from ASSEMBLY_RESULTS_DATA ARD
	   WITH (NOLOCK)
                 INNER JOIN ASSEMBLY_PARAMETERS AP on ARD.ParameterID=AP.ParameterID
                 INNER JOIN ASSEMBLIES on ARD.AssemblyID=ASSEMBLIES.AssemblyID
                 INNER JOIN STATION_PARAMETER_GROUPS SPG ON AP.GROUPID=SPG.GROUPID
                 INNER JOIN STATIONS ON STATIONS.STATIONID=SPG.STATIONID
                 INNER JOIN DEPARTMENTS ON DEPARTMENTS.DEPTID=STATIONS.DEPTID
                 INNER JOIN BUILDINGS ON BUILDINGS.BuildingID=DEPARTMENTS.BuildingID
                 INNER JOIN JD_SITES ON JD_SITES.SiteID=BUILDINGS.SiteID
                 where ARD.entrytimestamp between '",input$selected_date_range[1],"' and '",input$selected_date_range[2],"'
                 and
                 AP.NAME='",input$parameter_Name,"'
                 and
                 SPG.NAME = '",input$selected_SPGroup,"'
                 and
                 STATIONS.Name ='",input$selected_station,"'
                 and
                 DEPARTMENTS.NAME='",input$selected_dept,"' and
                 DEPARTMENTS.archived_timestamp is null
                 and
                 SPG.archived_timestamp is null
                 and
                 AP.archived_timestamp is null and
                 AP.disabled_timestamp is null and
                 ARD.parameterchildname='Torque'")
      
      print("SPC Query")
      print(spc_qr)

      tourque_performance_data<-dbGetQuery(sql_data_con,spc_qr)
      if(nrow(tourque_performance_data)==0)
      {
        returnValue()
        shinyalert::shinyalert("Harvester Quality System","No Records Present",type = "error")
      }
      else
      {
        print("tourque_performance_data")
        print(tourque_performance_data)
        print("unique Assymbly Name")
        print(unique(tourque_performance_data$AssemblyName))
        output$tourque_performance<-renderPlotly({
          i_chart<<- Ichart(tourque_performance_data, x = tourque_performance_data$DATE_HX, y = as.numeric(tourque_performance_data$DataValue), xname = "Time", yname = "Tourque Values", Label = tourque_performance_data$AssemblyName)
          i_chart
          })
        
        data<- tourque_performance_data
        values<- as.numeric(tourque_performance_data$DataValue)
        
        
        Mean <- round(mean(values), 2)
        sigma_overall <- sd(values)
        temp_sigma = c()
        for (value in values){
          if(value >= (Mean - sigma_overall) && value <= (Mean + sigma_overall)){
            temp_sigma <- c(temp_sigma, value)
          }
        }
        sigma_percent <- round((sum(length(temp_sigma))/ nrow(data)) * 100,2 )
        Median<- round(median(values), 2)
        Mode<- round(getmode(valx = values), 2)
        
        #   if(Mean < Median){
        #     nd_title <- paste0('Left - Skewed')
        #   }
        #   else if(Mean > Median){
        #     nd_title <- paste0('Right - Skewed')
        #   }
        #   else if(Mean == Median && sigma_percent >= 68){
        #     nd_title <- paste0('Normal Distribution')
        #   } else {
        #     nd_title <- paste0('Skewed')
        #   }
        # nd_title
        
        output$nd_description <- renderText({
          
          if(Mean < Median){
            text <- paste0('Mean = ',Mean, ' Median = ', Median, '. It is left-skewed.')
          }
          else if(Mean > Median){
            text <- paste0('Mean = ',Mean, ' Median = ', Median, '. It is right-skewed.')
          }
          else if(Mean == Median){
            text <- paste0('Mean = Median = Mode = ', Mean, '. It is symmetric.')
          }
          if(sigma_percent < 68 && Mean != Median){
            text <- paste0(text,' ', paste0(round(sigma_percent, 2), '% of data fits. The standard fit is 68%. The data is not normal. 
                                            Therefore, check for some special or common assignable cause affecting the data.'))
          }
          else{
            text <- paste0(text, ' ', paste0(round(sigma_percent, 2), '% of data fits.'))
          }
          
          ttext<<- text
          ttext
        })
        
        output$interpretation<- renderTable({
          spc_table<<-spcInterpretations(values = tourque_performance_data$DataValue, MAXVALUE = tourque_performance_data$MAXVALUE, MINVALUE= tourque_performance_data$MINVALUE)
          spc_table
          })
        
        output$normal_distribution<-renderPlotly({
          if(Mean < Median){
            nd_title <- paste0('Left - Skewed')
          }
          else if(Mean > Median){
            nd_title <- paste0('Right - Skewed')
          }
          else if(Mean == Median && sigma_percent >= 68){
            nd_title <- paste0('Normal Distribution')
          } else {
            nd_title <- paste0('Skewed')
          }
          print("nd_title")
          print(nd_title)
          norm_chart<<-ND_distribution(data = tourque_performance_data, parameter = as.numeric(tourque_performance_data$DataValue), xname = "Data Points", title = nd_title)
          norm_chart
          })
        
        ################ Valuebox Data Gen ###############################
        
        valuebox_data_gen<-eventReactive(input$top_get_data,{
          if( is.null(input$top_get_data)||input$top_get_data==0)
          {
            returnValue()
          }
          else
          {
            qr<-paste0("Select count(distinct ARD.assemblyid) as VEHICLE,
                   count(case when ARD.IsAcceptable = 1 then 1 else null end) as ACCEPTED,
                   count(case when ARD.IsAcceptable = 0 then 1 else null end) as REJECTED,
                   MAX(ARD.MINVALUE) as MINVALUE,
                   MAX(ARD.MAXVALUE) as MAXVALUE
                   from ASSEMBLY_RESULTS_DATA ARD
                   WITH (NOLOCK) 
                   INNER JOIN ASSEMBLY_PARAMETERS AP on ARD.ParameterID=AP.ParameterID
                   INNER JOIN STATION_PARAMETER_GROUPS SPG ON AP.GROUPID=SPG.GROUPID 
                   INNER JOIN STATIONS ON STATIONS.STATIONID=SPG.STATIONID 
                   INNER JOIN DEPARTMENTS ON DEPARTMENTS.DEPTID=STATIONS.DEPTID
                   INNER JOIN BUILDINGS ON BUILDINGS.BuildingID=DEPARTMENTS.BuildingID
                   INNER JOIN JD_SITES ON JD_SITES.SiteID=BUILDINGS.SiteID 
                   where convert(date,ARD.entrytimestamp) between '",input$selected_date_range[1],"' and '",input$selected_date_range[2],"'
                   and
                   AP.NAME='",input$parameter_Name,"'
                   and
                   SPG.NAME = '",input$selected_SPGroup,"'
                   and
                   STATIONS.Name ='",input$selected_station,"'
                   and
                   DEPARTMENTS.NAME='",input$selected_dept,"'")
            valuebox_data<<-dbGetQuery(sql_data_con,qr)
            print("valuebox_data")
            print(valuebox_data)
            vehicle<<- valuebox_data$VEHICLE
            vaccept<<- valuebox_data$ACCEPTED
            vreject<<- valuebox_data$REJECTED
            vmax<<- valuebox_data$MAXVALUE 
            vmin<<-valuebox_data$MINVALUE
          }
        })
        
        valuebox4_data_gen<-eventReactive(input$top_get_data,{
          if( is.null(input$top_get_data)||input$top_get_data==0)
          {
            returnValue()
          }
          else
          {
            qr<-paste0("Select count(distinct overrideid) as COUNT from override_log OL
                 WITH (NOLOCK) 
                 INNER JOIN ASSEMBLY_PARAMETERS AP on OL.ParameterID=AP.ParameterID
                 INNER JOIN STATION_PARAMETER_GROUPS SPG ON AP.GROUPID=SPG.GROUPID 
                 INNER JOIN STATIONS ON STATIONS.STATIONID=SPG.STATIONID 
                 INNER JOIN DEPARTMENTS ON DEPARTMENTS.DEPTID=STATIONS.DEPTID
                 INNER JOIN BUILDINGS ON BUILDINGS.BuildingID=DEPARTMENTS.BuildingID
                 INNER JOIN JD_SITES ON JD_SITES.SiteID=BUILDINGS.SiteID 
                 where convert(date,OL.entrytimestamp) between '",input$selected_date_range[1],"' and '",input$selected_date_range[2],"'
                 and
                 AP.NAME='",input$parameter_Name,"'
                 and
                 SPG.NAME = '",input$selected_SPGroup,"'
                 and
                 STATIONS.Name ='",input$selected_station,"'
                 and
                 DEPARTMENTS.NAME='",input$selected_dept,"'")
            valuebox4_data<<-dbGetQuery(sql_data_con,qr)
            vcount<<- valuebox4_data$COUNT
          }
        })
        
        
        
        
        output$valuebox1<-renderValueBox({
          if(is.null(input$top_get_data)||input$top_get_data==0)
          {
            shinydashboard::valueBox(0,"Vehicle Assembled", icon = icon("hourglass-half"), color = "olive", width = 12)
          }
          else
          {
            valuebox_data_gen()
            shinydashboard::valueBox(valuebox_data$VEHICLE,"Vehicle Assembled", icon = icon("hourglass-half"),color = "olive", width = 12)
          }
        }) 
        
        output$valuebox2<-renderValueBox({
          if(is.null(input$top_get_data)||input$top_get_data==0)
          {
            shinydashboard::valueBox(0,"JDAAT Accepted", icon = icon("check-circle"),color = "light-blue", width = 12)
          }
          else
          {
            valuebox_data_gen()
            shinydashboard::valueBox(valuebox_data$ACCEPTED,"JDAAT Accepted", icon = icon("check-circle"),color = "light-blue", width = 12)
          }
        })
        
        output$valuebox3<-renderValueBox({
          if(is.null(input$top_get_data)||input$top_get_data==0)
          {
            shinydashboard::valueBox(0,"JDAAT Rejected", icon = icon("window-close"),color = "red", width = 12)
          }
          else
          {
            valuebox_data_gen()
            shinydashboard::valueBox(valuebox_data$REJECTED,"JDAAT Rejected", icon = icon("window-close"),color = "red", width = 12)
          }
        })
        
        
        output$valuebox4<-renderValueBox({
          if(is.null(input$top_get_data)||input$top_get_data==0)
          {
            shinydashboard::valueBox(0,"JDAAT OverRide", icon = icon("list-ol"),color = "purple", width = 12)
          }
          else
          {
            valuebox4_data_gen()
            shinydashboard::valueBox(valuebox4_data$COUNT,"JDAAT OverRide", icon = icon("list-ol"),color = "purple", width = 12)
          }
        })
        
        
        
        output$valuebox5<-renderValueBox({
          if(is.null(input$top_get_data)||input$top_get_data==0)
          {
            shinydashboard::valueBox(0,"Torque UCL", icon = icon("sort-amount-up"),color = "teal", width = 12)
          }
          else
          {
            valuebox_data_gen()
            shinydashboard::valueBox(valuebox_data$MAXVALUE,"Torque UCL", icon = icon("sort-amount-up"),color = "teal", width = 12)
          }
        })
        
        
        output$valuebox6<-renderValueBox({
          if(is.null(input$top_get_data)||input$top_get_data==0)
          {
            shinydashboard::valueBox(0,"Torque LCL", icon = icon("sort-amount-down"),color = "maroon", width = 12)
          }
          else
          {
            valuebox_data_gen()
            shinydashboard::valueBox(valuebox_data$MINVALUE,"Torque LCL", icon = icon("sort-amount-down"),color = "maroon", width = 12)
          }
        })
        
        output$spc_report<- downloadHandler(
          filename = function(){
            paste('Report', format(Sys.time(), paste("_%d%b%Y_%X")), '.html', sep = '')
          },
          content = function(file) {
            params <- list(vbox1= vehicle, vbox2 = vaccept, vbox3 = vreject, vbox4= vcount, vbox5= vmax, vbox6= vmin,
                           nchart= norm_chart, spchart= i_chart, tables= spc_table)
            src <- normalizePath('spc.Rmd')
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'spc.Rmd', overwrite = TRUE)
            out <- rmarkdown::render('spc.Rmd', output_format = flexdashboard::flex_dashboard())
            file.rename(out, file)
            # Get a nicely formatted date/time string
          }
        )
        
      }
    }
  })
  
  ######################################################## END OF SPC Dashboard TAB #########################################################    
  ###########################################################################################################################################
  
  
  ############################################################ OverRide Performance TAB ######################################################
  ############################################################################################################################################ 
  
  observe({
    if(input$over_per_month_option==TRUE)
    {
      shinyjs::show("over_per_month_id")
      shinyjs::show("over_per_week_option_material_id")
    }
    else
    {
      shinyjs::hide("over_per_month_id")
      shinyjs::hide("over_per_week_option_material_id")
      shinyjs::hide("over_per_week_id")
    }
  })
  
  
  observe({
    if(input$over_per_week_option==TRUE)
    {
      shinyjs::show("over_per_week_id")
      shinyjs::disable("selected_month")
      shinyjs::disable("selected_year")
    }
    else
    {
      shinyjs::hide("over_per_week_id")
      shinyjs::enable("selected_month")
      shinyjs::enable("selected_year")
    }
  })
  
  
  
  ################### Top 10 Parameter Issue ################
  
  
  parameter_issue_plot_gen<-eventReactive(input$get_or_data,{
    if(is.null(input$get_or_data)||input$get_or_data==0)
    {
      returnValue()
    }
    else
    {
      if(input$over_per_month_option==FALSE)
      {
        qr<-paste0("
                    Select  top 10
                   OVERRIDE_LOG.parameterid,
                   ASSEMBLY_PARAMETERS.Name,
                   count(OVERRIDE_LOG.parameterid)as fail_count from OVERRIDE_LOG with(NOLOCK)
                   inner join
                   ASSEMBLY_PARAMETERS
                   on
                   OVERRIDE_LOG.parameterid=assembly_parameters.ParameterID
                   and assembly_parameters.disabled_timestamp is NUll
                   where
                   year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                   group by OVERRIDE_LOG.parameterid,ASSEMBLY_PARAMETERS.Name order by count(OVERRIDE_LOG.parameterid) desc")
      }
      else
      {
        if(input$over_per_week_option==TRUE)
        {
          qr<-paste0("Select  top 10
                      OVERRIDE_LOG.parameterid,
                     ASSEMBLY_PARAMETERS.Name,
                     count(OVERRIDE_LOG.parameterid)as fail_count from OVERRIDE_LOG with(NOLOCK)
                     inner join
                     ASSEMBLY_PARAMETERS
                     on
                     OVERRIDE_LOG.parameterid=assembly_parameters.ParameterID
                     and assembly_parameters.disabled_timestamp is NUll
                     where
                     convert(date,entrytimestamp) between '",input$selected_date[1],"' and '",input$selected_date[2],"'
                     group by OVERRIDE_LOG.parameterid,ASSEMBLY_PARAMETERS.Name order by count(OVERRIDE_LOG.parameterid) desc")
        }
        else
        {
          qr<-paste0("Select  top 10
                      OVERRIDE_LOG.parameterid,
                     ASSEMBLY_PARAMETERS.Name,
                     count(OVERRIDE_LOG.parameterid)as fail_count from OVERRIDE_LOG with(NOLOCK)
                     inner join
                     ASSEMBLY_PARAMETERS
                     on
                     OVERRIDE_LOG.parameterid=assembly_parameters.ParameterID
                     and assembly_parameters.disabled_timestamp is NUll
                     where
                     year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                     and month(OVERRIDE_LOG.entrytimestamp)='",input$selected_month,"'
                     group by OVERRIDE_LOG.parameterid,ASSEMBLY_PARAMETERS.Name order by count(OVERRIDE_LOG.parameterid) desc")
        }
      }
      parameter_issue_data<-dbGetQuery(sql_data_con,qr)
      if(nrow(parameter_issue_data)==0)
      {
        returnValue()
      }
      else
      {
        hc<-highchart() %>%
          hc_chart(type = "column") %>%
          hc_xAxis(categories = parameter_issue_data$Name,
                   tickmarkPlacement = "on",
                   title = list(enabled = FALSE),
                   labels=list(
                     style=list(
                       fontSize='10px'
                     ),
                     step=1
                   )
          ) %>%
          hc_yAxis(title = list(text = "Issue Count of Top 10 Parameter")) %>%
          hc_add_series(name = "Count of Issue", data =parameter_issue_data$fail_count,color='#20720D')
        hc
      }
    }
  })
  
  
  output$parameter_issue<-renderHighchart({
    validate(need(input$get_or_data,""))
    input$get_or_data
    parameter_issue_plot_gen()
  })
  
  ################## Top 10 Over Rides by Station #################
  
  overrides_by_station_plt<-eventReactive(input$get_or_data,{
    if(is.null(input$get_or_data)||input$get_or_data==0)
    {
      returnValue()
    }
    else
    {
      if(input$over_per_month_option==FALSE)
      {
        qr<-paste0("Select top 10
                    stations.name,
                   count(OVERRIDE_LOG.parameterid) as fail_count,
                   count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '7' and '15' then 1 else null end) as Shift1,
                   count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '16' and '23' then 1 else null end) as Shift2,
                   count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '0' and '6' then 1 else null end) as Shift3
                   from 
                   stations with(NOLOCK)
                   inner join
                   STATION_PARAMETER_GROUPS with(NOLOCK)
                   on 
                   STATIONS.stationid=station_parameter_groups.stationid
                   and STATIONS.archived_timestamp is null
                   inner join 
                   ASSEMBLY_PARAMETERS with(NOLOCK)
                   on
                   station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
                   and station_parameter_groups.archived_timestamp is null
                   inner join
                   override_log with(NOLOCK)
                   on
                   override_log.parameterid=ASSEMBLY_PARAMETERS.parameterid
                   and assembly_parameters.disabled_timestamp is NUll
                   where 
                   year(override_log.entrytimestamp)='",input$selected_year,"'
                   group by stations.name
                   order by 
                   count(OVERRIDE_LOG.parameterid) desc
                   ")
      }
      else
      {
        if(input$over_per_week_option==TRUE)
        {
          qr<-paste0("Select top 10
                    stations.name,
                     count(OVERRIDE_LOG.parameterid) as fail_count,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '7' and '15' then 1 else null end) as Shift1,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '16' and '23' then 1 else null end) as Shift2,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '0' and '6' then 1 else null end) as Shift3
                     from 
                     stations with(NOLOCK)
                     inner join
                     STATION_PARAMETER_GROUPS with(NOLOCK)
                     on 
                     STATIONS.stationid=station_parameter_groups.stationid
                     and STATIONS.archived_timestamp is null
                     inner join 
                     ASSEMBLY_PARAMETERS with(NOLOCK)
                     on
                     station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
                     and station_parameter_groups.archived_timestamp is null
                     inner join
                     override_log with(NOLOCK)
                     on
                     override_log.parameterid=ASSEMBLY_PARAMETERS.parameterid
                     and assembly_parameters.disabled_timestamp is NUll
                     where 
                     convert(date,override_log.entrytimestamp) between '",input$selected_date[1],"' and '",input$selected_date[2],"'
                     group by stations.name
                     order by 
                     count(OVERRIDE_LOG.parameterid) desc")
          
        }
        else
        {
          qr<-paste0("Select top 10
                    stations.name,
                     count(OVERRIDE_LOG.parameterid) as fail_count,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '7' and '15' then 1 else null end) as Shift1,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '16' and '23' then 1 else null end) as Shift2,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '0' and '6' then 1 else null end) as Shift3
                     from 
                     stations with(NOLOCK)
                     inner join
                     STATION_PARAMETER_GROUPS with(NOLOCK)
                     on 
                     STATIONS.stationid=station_parameter_groups.stationid
                     and STATIONS.archived_timestamp is null
                     inner join 
                     ASSEMBLY_PARAMETERS with(NOLOCK)
                     on
                     station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
                     and station_parameter_groups.archived_timestamp is null
                     inner join
                     override_log with(NOLOCK)
                     on
                     override_log.parameterid=ASSEMBLY_PARAMETERS.parameterid
                     and assembly_parameters.disabled_timestamp is NUll
                     where 
                      year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                     and month(OVERRIDE_LOG.entrytimestamp)='",input$selected_month,"'
                     group by stations.name
                     order by 
                     count(OVERRIDE_LOG.parameterid) desc")
          
        }
      }
      override_station_data<-dbGetQuery(sql_data_con,qr)
      if(nrow(override_station_data)==0)
      {
        returnValue()
      }
      else
      {
        colors=c('#27251F','#367C2B','#FFDE00','#27251F','#367C2B','#FFDE00','#27251F')
        hc<-highchart() %>%
          hc_chart(type = "column") %>%
          hc_yAxis(title = list(text = "Override Counts of Top 10 Station")) %>%
          hc_xAxis(categories = override_station_data$name,
                   tickmarkPlacement = "on",
                   title = list(enabled = FALSE),
                   labels=list(
                     style=list(
                       fontSize='10px'
                     ),
                     step=1
                   )
          ) %>%
          hc_tooltip(formatter = JS("function () {
                            return '<b>' + this.x + '</b><br/>' +
                            this.series.name + ': ' + this.y + '<br/>' +
                            'Total: ' + this.point.stackTotal;
                            }")) %>%
          hc_plotOptions(column=list(stacking="normal"))
        
        for(i in 3:ncol(override_station_data)) 
        {
          hc <- hc_add_series(hc, override_station_data[, i], name = names(override_station_data)[i],color=colors[i]
          )
        }
        hc
      }
    }
  })
  
  
  output$overrides_by_station<-renderHighchart({
    validate(need(input$get_or_data,""))
    input$get_or_data
    overrides_by_station_plt()
  })
  
  ################# Top 10  overrides by Departments ###############  
  
  overrides_by_department_plot<-eventReactive(input$get_or_data,{
    if(is.null(input$get_or_data)||input$get_or_data==0)
    {
      returnValue()
    }
    else
    {
      if(input$over_per_month_option==FALSE)
      {
        qr<-paste0("Select top 10
                    departments.name,
                   count(OVERRIDE_LOG.parameterid) as fail_count,
                   count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '7' and '15' then 1 else null end) as Shift1,
                   count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '16' and '23' then 1 else null end) as Shift2,
                   count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '0' and '6' then 1 else null end) as Shift3
                   from 
                   departments
                   inner join
                   stations with(NOLOCK)
                   on
                   departments.deptid=stations.deptid
                   and departments.archived_timestamp is null
                   inner join
                   STATION_PARAMETER_GROUPS with(NOLOCK)
                   on 
                   STATIONS.stationid=station_parameter_groups.stationid
                   and STATIONS.archived_timestamp is null
                   inner join 
                   ASSEMBLY_PARAMETERS with(NOLOCK)
                   on
                   station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
                   and station_parameter_groups.archived_timestamp is null
                   inner join
                   override_log with(NOLOCK)
                   on
                   override_log.parameterid=ASSEMBLY_PARAMETERS.parameterid
                   and assembly_parameters.disabled_timestamp is NUll
                   where 
                   year(override_log.entrytimestamp)='",input$selected_year,"'
                   group by departments.name
                   order by 
                   count(OVERRIDE_LOG.parameterid) desc")
        
      }
      else
      {
        if(input$over_per_week_option==TRUE)
        {
          qr<-paste0("Select top 10
                      departments.name,
                     count(OVERRIDE_LOG.parameterid) as fail_count,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '7' and '15' then 1 else null end) as Shift1,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '16' and '23' then 1 else null end) as Shift2,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '0' and '6' then 1 else null end) as Shift3
                     from 
                     departments with(NOLOCK)
                     inner join
                     stations with(NOLOCK)
                     on
                     departments.deptid=stations.deptid
                     and departments.archived_timestamp is null
                     inner join
                     STATION_PARAMETER_GROUPS with(NOLOCK)
                     on 
                     STATIONS.stationid=station_parameter_groups.stationid
                     and STATIONS.archived_timestamp is null
                     inner join 
                     ASSEMBLY_PARAMETERS with(NOLOCK)
                     on
                     station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
                     and station_parameter_groups.archived_timestamp is null
                     inner join
                     override_log with(NOLOCK)
                     on
                     override_log.parameterid=ASSEMBLY_PARAMETERS.parameterid
                     and assembly_parameters.disabled_timestamp is NUll
                     where 
                     convert(date,override_log.entrytimestamp) between '",input$selected_date[1],"' and '",input$selected_date[2],"'
                     group by departments.name
                     order by 
                     count(OVERRIDE_LOG.parameterid) desc")
          
          
        }
        else
        {
          qr<-paste0("Select top 10
                      departments.name,
                     count(OVERRIDE_LOG.parameterid) as fail_count,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '7' and '15' then 1 else null end) as Shift1,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '16' and '23' then 1 else null end) as Shift2,
                     count(case when datepart(hour,OVERRIDE_LOG.entrytimestamp) between '0' and '6' then 1 else null end) as Shift3
                     from 
                     departments with(NOLOCK)
                     inner join
                     stations with(NOLOCK)
                     on
                     departments.deptid=stations.deptid
                     and departments.archived_timestamp is null
                     inner join
                     STATION_PARAMETER_GROUPS with(NOLOCK)
                     on 
                     STATIONS.stationid=station_parameter_groups.stationid
                     and STATIONS.archived_timestamp is null
                     inner join 
                     ASSEMBLY_PARAMETERS with(NOLOCK)
                     on
                     station_parameter_groups.groupid=ASSEMBLY_PARAMETERS.groupid
                     and station_parameter_groups.archived_timestamp is null
                     inner join
                     override_log with(NOLOCK)
                     on
                     override_log.parameterid=ASSEMBLY_PARAMETERS.parameterid
                     and assembly_parameters.disabled_timestamp is NUll
                     where 
                      year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                     and month(OVERRIDE_LOG.entrytimestamp)='",input$selected_month,"'
                     group by departments.name
                     order by 
                     count(OVERRIDE_LOG.parameterid) desc")
          
          
        }
      }
      override_dept_data<-dbGetQuery(sql_data_con,qr)
      if(nrow(override_dept_data)==0)
      {
        returnValue()
      }
      else
      {
        colors=c('#27251F','#367C2B','#FFDE00','#27251F','#367C2B','#FFDE00','#27251F')
        hc<-highchart() %>%
          hc_chart(type = "column") %>%
          hc_yAxis(title = list(text = "Override Counts of Top 10 Department")) %>%
          hc_xAxis(categories = override_dept_data$name,
                   tickmarkPlacement = "on",
                   title = list(enabled = FALSE),
                   labels=list(
                     style=list(
                       fontSize='10px'
                     ),
                     step=1
                   )
          ) %>%
          hc_tooltip(formatter = JS("function () {
                                    return '<b>' + this.x + '</b><br/>' +
                                    this.series.name + ': ' + this.y + '<br/>' +
                                    'Total: ' + this.point.stackTotal;
      }")) %>%
          hc_plotOptions(column=list(stacking="normal"))
        
        for(i in 3:ncol(override_dept_data)) 
        {
          hc <- hc_add_series(hc, override_dept_data[, i], name = names(override_dept_data)[i],color=colors[i]
          )
        }
        hc
      }
    }
  })
  
  
  output$overrides_by_department<-renderHighchart({
    validate(need(input$get_or_data,""))
    input$get_or_data
    overrides_by_department_plot()
  })
  
  ################ Top 10 Overrides by Person #############
  
  overrides_by_person_plot<-eventReactive(input$get_or_data,{
    if(is.null(input$get_or_data)||input$get_or_data==0)
    {
      returnValue()
    }
    else
    {
      if(input$over_per_month_option==FALSE)
      {
        qr<-paste0("Select top 10 username,count(username) as person_count from override_log
                   where 
                   year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                   group by 
                   override_log.username
                   order by count(username) desc ")
      }
      else
      {
        if(input$over_per_week_option==TRUE)
        {
          qr<-paste0("Select top 10 username,count(username) as person_count from override_log
                     where 
                     convert(date,override_log.entrytimestamp) between '",input$selected_date[1],"' and '",input$selected_date[2],"'
                     group by 
                     override_log.username
                     order by count(username) desc ")
        }
        else
        {
          qr<-paste0("Select top 10 username,count(username) as person_count from override_log
                     where 
                     year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                     and month(OVERRIDE_LOG.entrytimestamp)='",input$selected_month,"'
                     group by 
                     override_log.username
                     order by count(username) desc ")
        }
      }
      username_data<-dbGetQuery(sql_data_con,qr)
      if(nrow(username_data)==0)
      {
        returnValue()
      }
      else
      {
        hc<-highchart() %>%
          hc_chart(type = "column") %>%
          hc_xAxis(categories = username_data$username,
                   tickmarkPlacement = "on",
                   title = list(enabled = FALSE),
                   labels=list(
                     style=list(
                       fontSize='10px'
                     ),
                     step=1
                   )
          ) %>%
          hc_yAxis(title = list(text = "Top 10 User who Overrides")) %>%
          hc_add_series(name = "OverRide Count", data =username_data$person_count,color='#20720D')
        hc
      }
      
    }
  })
  
  
  output$overrides_by_person<-renderHighchart({
    validate(need(input$get_or_data,""))
    input$get_or_data
    overrides_by_person_plot()
  })
  
  ################ Top 10  Overrides Codes in Override ############
  
  overrides_by_oc_code_plot<-eventReactive(input$get_or_data,{
    if(is.null(input$get_or_data)||input$get_or_data==0)
    {
      returnValue()
    }
    else
    {
      if(input$over_per_month_option==FALSE)
      {
        qr<-paste0("Select top 10  manual_override_dept_codes.codeid,
                    manual_override_dept_codes.description,
                   count(override_log.codeid) as COUNT_FAIL
                   from 
                   manual_override_dept_codes with(NOLOCK)
                   inner join 
                   override_log
                   on
                   override_log.codeid=manual_override_dept_codes.codeid
                   where
                   year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                   group by
                   manual_override_dept_codes.codeid,
                   manual_override_dept_codes.description
                   order by 
                   count(override_log.codeid) desc")
      }
      else
      {
        if(input$over_per_week_option==TRUE)
        {
          qr<-paste0("Select top 10  manual_override_dept_codes.codeid,
                    manual_override_dept_codes.description,
                     count(override_log.codeid) as COUNT_FAIL
                     from 
                     manual_override_dept_codes with(NOLOCK)
                     inner join 
                     override_log
                     on
                     override_log.codeid=manual_override_dept_codes.codeid
                     where
                      convert(date,override_log.entrytimestamp) between '",input$selected_date[1],"' and '",input$selected_date[2],"'
                     group by
                     manual_override_dept_codes.codeid,
                     manual_override_dept_codes.description
                     order by 
                     count(override_log.codeid) desc")
        }
        else
        {
          qr<-paste0("Select top 10  manual_override_dept_codes.codeid,
                      manual_override_dept_codes.description,
                     count(override_log.codeid) as COUNT_FAIL
                     from 
                     manual_override_dept_codes with(NOLOCK)
                     inner join 
                     override_log
                     on
                     override_log.codeid=manual_override_dept_codes.codeid
                     where
                     year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                     and month(OVERRIDE_LOG.entrytimestamp)='",input$selected_month,"'
                     group by
                     manual_override_dept_codes.codeid,
                     manual_override_dept_codes.description
                     order by 
                     count(override_log.codeid) desc")
        }
      }
      oc_data<-dbGetQuery(sql_data_con,qr)
      if(nrow(oc_data)==0)
      {
        returnValue()
      }
      else
      {
        hc<-highchart() %>%
          hc_chart(type = "column") %>%
          hc_xAxis(categories = oc_data$description,
                   tickmarkPlacement = "on",
                   title = list(enabled = FALSE),
                   labels=list(
                     style=list(
                       fontSize='10px'
                     ),
                     step=1
                   )
          ) %>%
          hc_yAxis(title = list(text = "Top 10 OverRide Code Count")) %>%
          hc_add_series(name = "OverRide Code Count", data =oc_data$COUNT_FAIL,color='#20720D')
        hc
      }
    }
  })
  
  
  output$overrides_by_oc_code<-renderHighchart({
    validate(need(input$get_or_data,""))
    input$get_or_data
    overrides_by_oc_code_plot()
  })
  
  ################ Top 10 Comments in Override #############
  
  overrides_by_comment_plot<-eventReactive(input$get_or_data,{
    if(is.null(input$get_or_data)||input$get_or_data==0)
    {
      returnValue()
    }
    else
    {
      if(input$over_per_month_option==FALSE)
      {
        
        qr<-paste0("Select top 10 comments, count(comments) as COUNT from override_log 
                    where 
                    year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                    group by
                   comments
                   order by 
                   count(comments) desc")
      }
      else
      {
        if(input$over_per_week_option==TRUE)
        {
          qr<-paste0("Select top 10 comments, count(comments) as COUNT from override_log 
                      where 
                      convert(date,override_log.entrytimestamp) between '",input$selected_date[1],"' and '",input$selected_date[2],"'
                      group by
                     comments
                     order by 
                     count(comments) desc")
        }
        else
        {
          qr<-paste0("Select top 10 comments, count(comments) as COUNT from override_log 
                      where 
                    year(OVERRIDE_LOG.entrytimestamp)='",input$selected_year,"'
                     and month(OVERRIDE_LOG.entrytimestamp)='",input$selected_month,"'
                     group by
                     comments
                     order by 
                     count(comments) desc")
        }
      }
      comment_data<-dbGetQuery(sql_data_con,qr)
      if(nrow(comment_data)==0)
      {
        returnValue()
      }
      else
      {
        hc<-highchart() %>%
          hc_chart(type = "column") %>%
          hc_xAxis(categories = comment_data$comments,
                   tickmarkPlacement = "on",
                   title = list(enabled = FALSE),
                   labels=list(
                     style=list(
                       fontSize='10px'
                     ),
                     step=1
                   )
          ) %>%
          hc_yAxis(title = list(text = "Top 10 OverRide Comment Count")) %>%
          hc_add_series(name = "OverRide Comment Count", data =comment_data$COUNT,color='#20720D')
        hc
      }
    }
  })
  
  
  output$overrides_by_comment<-renderHighchart({
    validate(need(input$get_or_data,""))
    input$get_or_data
    overrides_by_comment_plot()
  })
  
  output$over_report <- downloadHandler(
    filename = function(){
      paste('Override Parameters Report', format(Sys.time(), paste("%d%b%Y")), '.html', sep = '')
    },
    content = function(file) {
      params <- list(TTPO = parameter_issue_plot_gen(), TTSO = overrides_by_station_plt(), 
                     TTDO = overrides_by_department_plot(),
                     TTOC1 = overrides_by_person_plot(), TTOC2 = overrides_by_oc_code_plot(),
                     TTCO = overrides_by_comment_plot()
                     )
      src <- normalizePath('over.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'over.Rmd', overwrite = TRUE)
      out <- rmarkdown::render('over.Rmd', output_format = flexdashboard::flex_dashboard())
      file.rename(out, file)
      # Get a nicely formatted date/time string
    }
  )
  
}
#=================================================================================== END ========================================================================================