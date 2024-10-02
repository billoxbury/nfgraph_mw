
ui <- list()

####################################################################
ui[["control"]] <- fluidPage(
  
  titlePanel("Controls"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "port", 
                  label = "Server port:",
                  choices = c("Any",
                              as.character(ports)),
                  selected = "Any"),
      sliderInput(inputId = "edgefrac",
                  label = "% edges to draw:", 
                  min = 0.0, max = 5.0, step=0.1,
                  value = 0.0),
      selectInput(inputId = "coordchoice", 
                  label = "Main graph layout:",
                  choices = c("DRL",
                              "LINE 1st-order",
                              "LINE 2nd-order",
                              "LINE mixed",
                              "Fruchterman-Reingold"
                  ),
                  selected = "DRL"),
      selectInput(inputId = "zoomcoords", 
                  label = "Zoom graph layout:",
                  choices = c("Communications layout",
                              "Functional layout",
                              "Auto adjust"),
                  selected = "Auto adjust"),
      checkboxInput(inputId = "drawzoom", 
                    label = "Draw zoom graph? (All edges!)", 
                    value = TRUE),
      sliderInput("proto_temp", 
                  label = "Protocol amplifier:",
                  min = 1, max = 10, value = 1),
      sliderInput("port_temp", 
                  label = "Port amplifier:",
                  min = 1, max = 10, value = 1)
    ),
    mainPanel()
  )
)

####################################################################
ui[["funcview"]] <- fluidPage(
  titlePanel("Functional view"),
  plotOutput("funcplot",
             brush = brushOpts(id = "func_plot_brush"),
             width = "100%",
             height = "800px"
  )
)

####################################################################
ui[["commsview"]] <- fluidPage(
  titlePanel("Communications view"),
  plotOutput("commplot",
             brush = brushOpts(id = "comm_plot_brush"),
             width = "100%",
             height = "800px"
  ),
  # OPTIONAL:
  sliderInput(inputId = "edgefrac",
              label = "% edges to draw:", 
              min = 0.0, max = 5.0, step=0.1,
              value = 0.0),
  selectInput(inputId = "coordchoice", 
              label = "Main graph layout:",
              choices = c("DRL",
                          "LINE 1st-order",
                          "LINE 2nd-order",
                          "LINE mixed",
                          "Fruchterman-Reingold"
              ),
              selected = "DRL")
)

####################################################################
ui[["zoomview"]] <- fluidPage(
  titlePanel("Zoom region"),
  plotOutput("subplot",
             click = clickOpts(id="plot_click"),
             hover = hoverOpts(id="plot_hover", delayType="throttle"),
             width = "100%",
             height = "800px"
  ),
  # OPTIONAL
  selectInput(inputId = "zoomcoords", 
              label = "Zoom graph layout:",
              choices = c("Communications layout",
                          "Functional layout",
                          "Auto adjust"),
              selected = "Auto adjust")
)

####################################################################
ui[["portview"]] <- fluidPage(
  titlePanel("Port profile"),
  plotOutput("plot_hover_port", width="100%"),
  sliderInput("port_temp", 
              label = "Port amplifier:",
              min = 1, max = 10, value = 1)
)

ui[["protoview"]] <- fluidPage(
  titlePanel("Protocol profile"),
  plotOutput("plot_hover_proto", width="100%"),
  sliderInput("proto_temp", 
              label = "Protocol amplifier:",
              min = 1, max = 10, value = 1)
)

####################################################################
ui[["links"]] <- fluidPage(
  titlePanel("Useful links"),
  HTML('
       <ol>
  <li>Alexander D. Kent: <a href="https://csr.lanl.gov/data/cyber1/">Comprehensive multi-source cyber-security events</a>, doi:10.17021/1179829 (2015)</li>
  <li>Laurens van der Maaten, Geoffrey Hinton: <a href="https://www.jmlr.org/papers/volume9/vandermaaten08a/vandermaaten08a.pdf">Visualizing Data using t-SNE</a>, Journal of Machine Learning Research, 1 (2008)</li>
  <li>Laurens van der Maaten: <a href="https://arxiv.org/abs/1301.3342">Barnes-Hut-SNE</a>, arXiv:1301.3342v2 [cs.LG] (2013)</li>
  <li>Bill Oxbury: <a href="https://billoxbury.github.io/data_science/lanl-netflow-1/">Exploring a computer network from its netflow traffic I</a> and
  <a href="https://billoxbury.github.io/data_science/lanl-netflow-2/">II</a> blog posts (2018) 
</ol>
       ')
)


####################################################################
ui[["debug"]] <- fluidPage(
  titlePanel("Testing inputs"),
  verbatimTextOutput(outputId = "verb")
)
