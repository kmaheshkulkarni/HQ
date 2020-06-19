# t <- list(family = "sans serif", size = 20, color = '#feda15')


Ichart <- function(data, x = NULL, y = NULL, xname = NULL, yname = NULL, Label= NULL){
  f1 <- list(family = "Arial, sans-serif", size = 18, color = "darkgrey")
  f2 <- list(family = "Old Standard TT, serif", size = 14, color = "black")
  a <- list(title = paste0(xname), titlefont = f1, showticklabels = TRUE, tickangle = 45, tickfont = f2, exponentformat = "E")
  b <- list(title = paste0(yname), titlefont = f1, showticklabels = TRUE, tickangle = 45, tickfont = f2, exponentformat = "E")
  
  Mean_ichart <- mean(y)
  moving_range <- abs(diff(y))
  moving_range_mean <- mean(moving_range)
  sigma_i_mr_chart <- moving_range_mean/1.128
  ucl_i_chart <- Mean_ichart + (3 * sigma_i_mr_chart)
  lcl_i_chart <- Mean_ichart - (3 * sigma_i_mr_chart)
  data$colors <- "steelblue"
  data$symbols <- "circle"
  data$sizes <- 5
  # data$utc_date_time <- ymd_hms(data$utc_date_time)
  
  data$colors[y > ucl_i_chart] <- "#367c2b"  
  data$symbols[y > ucl_i_chart] <- "circle"
  data$sizes[y > ucl_i_chart] <- 8
  
  data$colors[y < lcl_i_chart] <- "#367c2b"
  data$symbols[y < lcl_i_chart] <- "circle"
  data$sizes[y < lcl_i_chart] <- 8 
  return(plotly::plot_ly(data, x = x, y = y, text = paste("Vehicle Number:", Label)) %>%
           plotly::add_markers(name = paste0('Pararameter value'), marker = list(color = ~I(as.character(colors)), symbol = ~I(as.character(symbols)), size = data$sizes), showlegend = FALSE) %>%
           plotly::add_trace(type = 'scatter', mode = 'lines', color = I('#01579b'), showlegend = FALSE, opacity=1) %>%
           plotly::add_trace(y=ucl_i_chart,name = "UCL",type = 'scatter', mode ='lines', line = list(dash = "dot"), color = I('#e53935'), showlegend = FALSE) %>%
           plotly::add_trace(y=(Mean_ichart + (2*sigma_i_mr_chart)),name = "+ 2 Sigma",type = 'scatter', mode = 'lines', line = list(dash = "dash"), color = I('#ffb74d'), showlegend = FALSE) %>%
           plotly::add_trace(y=(Mean_ichart + sigma_i_mr_chart),name = "+ 1 Sigma",type = 'scatter', mode = 'lines', line = list(dash = "dash"), color = I('#ffcc80'), showlegend = FALSE) %>%
           plotly::add_trace(y=Mean_ichart,name = "Average", mode ='lines',type = 'scatter', line = list(dash = "solid"), color = I('#66bb6a'), showlegend = FALSE) %>%
           plotly::add_trace(y=(Mean_ichart - sigma_i_mr_chart),name = "- 1 Sigma",type = 'scatter', mode = 'lines', line = list(dash = "dash"), color = I('#ffcc80'), showlegend = FALSE) %>%
           plotly::add_trace(y=(Mean_ichart - (2*sigma_i_mr_chart)),name = "- 2 Sigma",type = 'scatter', mode = 'lines', line = list(dash = "dash"), color = I('#ffb74d'), showlegend = FALSE) %>%
           plotly::add_trace(y=lcl_i_chart,name = "LCL",type = 'scatter', mode ='lines', line = list(dash = "dot"), color = I('#e53935'), showlegend = FALSE) %>%
           plotly::layout(xaxis = a, title = paste0(), yaxis = b) %>%
           plotly::layout(legend = list(orientation = "h",   # show entries horizontally
                                        xanchor = "bottom",  # use center of legend as anchor
                                        x = "middle", y = -0.3,
                                        bordercolor = "#333",
                                        borderwidth = 2))%>%
           plotly::config(displaylogo = FALSE))
}



ND_distribution <- function(data = NULL, parameter = NULL, xname = "", title = NULL){
  parameter <- as.numeric(parameter)
  mean_overall <- mean(parameter)
  sigma_overall <- sd(parameter)
  t <- list(family = "arial", size = 14, color = '#333')
  # par(mar=c(1,1,1,1))
  histo <- hist(parameter, breaks = 10, plot = FALSE) 
  xfit <- seq(min(parameter), max(parameter)) 
  yfit <- dnorm(xfit, mean = mean(parameter), sd = sd(parameter)) 
  yfit <- yfit * diff(histo$mids[1:2]) * length(parameter) 
  limit_data <- data.frame(xvalues = c((mean_overall - (3*sigma_overall)),
                                       (mean_overall - (2*sigma_overall)),
                                       (mean_overall - sigma_overall),
                                       (mean_overall),
                                       (mean_overall + sigma_overall),
                                       (mean_overall + (2*sigma_overall)),
                                       (mean_overall + (3*sigma_overall))),
                           yvalues = c(max(yfit), max(yfit), max(yfit), max(yfit), max(yfit), max(yfit), max(yfit)),
                           textvalues = c('-3 Sigma', '-2 Sigma', '-1 Sigma', 'Mean', '+1 Sigma', '+2 Sigma', '+3 Sigma'))
  return(
    plot_ly(x = xfit, y = yfit)%>%
      add_trace(name = 'Distribution',type = 'scatter', mode='lines', color = I('#01579b'), showlegend = FALSE)%>%
      add_trace(x=(mean_overall - (3*sigma_overall)),type = 'scatter', mode = 'lines', line = list(dash = "dash"), color = I('#ffb74d'), name='-2 sigma', showlegend = FALSE)%>%
      add_trace(x=(mean_overall - (2*sigma_overall)),type = 'scatter', mode = 'lines', line = list(dash = "dash"), color = I('#ffb74d'), name='-2 sigma', showlegend = FALSE)%>%
      add_trace(x=(mean_overall - sigma_overall),type = 'scatter', mode = 'lines', line = list(dash = "dash"), color = I('#ffcc80'), name='-1 sigma', showlegend = FALSE)%>%
      add_trace(x=mean_overall,type = 'scatter', mode = 'lines', line = list(dash = "dash"), color = I('#66bb6a'), name='Mean', showlegend = FALSE)%>%
      add_trace(x=(mean_overall + sigma_overall),type = 'scatter', mode = 'lines', line = list(dash = "dash"), color = I('#ffcc80'), name='+1 sigma', showlegend = FALSE)%>%
      add_trace(x=(mean_overall + (2*sigma_overall)),type = 'scatter', mode = 'lines', line = list(dash = "dash"), color = I('#ffb74d'), name='+2 sigma', showlegend = FALSE)%>%
      add_trace(x=(mean_overall + (3*sigma_overall)),type = 'scatter', mode = 'lines', line = list(dash = "dash"), color = I('#ffb74d'), name='+2 sigma', showlegend = FALSE)%>%
      layout(title= paste0(title), xaxis = list(title = paste0(xname)),yaxis = list (title = "Frequency"))%>%
      add_annotations(x = limit_data$xvalues, y = limit_data$yvalues, text = limit_data$textvalues, xref = "x", yref = "y", showarrow = TRUE, arrowhead = 4, arrowsize = 0.5, ax = 10, ay = -25, opacity = 0.7)%>%
      plotly::config(displaylogo = FALSE)
  )
}