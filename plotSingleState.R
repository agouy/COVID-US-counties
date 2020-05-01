### Generate plots for each state

plotStatev1 <- function(data.to.plot, sta) {
  
  datu <- data.to.plot[data.to.plot$state == sta, c("county", "date", "cases")]
  PALETTE <-  sample(colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(length(unique(datu$county))))
  
  ggp <- ggplot() + geom_line(data=datu, aes(x=date, y=cases, colour=county, group=county))+
    scale_color_manual(values = PALETTE) + 
    theme_classic() + 
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
  
  fg <- ggplotly(ggp)  %>% config(displayModeBar = F) %>% 
    layout(xaxis=list(fixedrange=TRUE)) %>%
    layout(yaxis=list(fixedrange=TRUE))
  
  (partial_bundle(fg))
}

plotStatev2 <- function(data.to.plot, sta) {
  
  datu <- data.to.plot[data.to.plot$state == sta, c("county", "date", "cases")]
  PALETTE <-  sample(colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(length(unique(datu$county))))
  
  ggp <- ggplot() + geom_line(data=datu, aes(x=date, y=cases, colour=county, group=county))+
    scale_color_manual(values = PALETTE) + 
    theme_classic() + 
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
  
  fg <- ggplotly(ggp)  %>%
    layout(xaxis=list(fixedrange=TRUE)) %>%
    layout(yaxis=list(fixedrange=TRUE))
  
  (partial_bundle(fg))
}

plotStatev3 <- function(data.to.plot, sta) {
  
  datu <- data.to.plot[data.to.plot$state == sta, c("county", "time", "date", "cases", "cases100k", "ID")]
  PALETTE <-  sample(colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(length(unique(datu$county))))
  sd <- SharedData$new(datu, key = ~ID)

  fig <- plot_ly(sd, x = ~time) %>% 
    layout(
      title = paste("Evolution of COVID cases in", sta),
      yaxis = list(title = "Number of cases"),
      xaxis = list(title = "Date (days since January 21)")
      # yaxis = list(range = c(0, 40000))#,
      # height = 500
    ) %>% 
    
    add_trace(
      y = ~cases,
      color = ~(county),
      mode = "lines+markers",
      colors = PALETTE,
      name= ~county,
      marker = list(size = 4)
    )  %>% 
    
    config(displayModeBar = F)

  toWebGL(bscols(
    list(
      filter_slider("mag", "Days since January 21", sd, column = ~as.numeric(time), step=1, width=200),
      filter_slider("mag2", "Number of cases", sd, column = ~cases, step=1, width=200),
      # filter_select("ctw", "Select county:", sd, ~county),
      filter_checkbox("ctw", "Select county:", sd, ~county, columns = 2)
    ),
    partial_bundle(fig),
    widths = c(4, NA)
  ))
}

# yaxis = list(type = "log")

# Original
plotStatev1(nyt.data, "California")

# Original but with bar (more interactivity)
plotStatev2(nyt.data, "California")

# Original but with bar (more interactivity)
wid <- plotStatev3(nyt.data, "Wisconsin")
?saveWidget
htmltools::save_html(wid, file = "Wisconsin.html")

saveWidget(wid, "Wisconsin.html", selfcontained = TRUE,
           background = "white", title = class(wid)[[1]],
           knitrOptions = list())
