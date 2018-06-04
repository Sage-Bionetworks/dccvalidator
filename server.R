server <- function(input, output) {

  output$manifest_tab <- renderTable({
    req(input$manifest)
    manifest <- read.csv(input$manifest$datapath)
    return(manifest)
  })

  output$indiv_tab <- renderTable({
    req(input$individual)
    individual <- read.csv(input$individual$datapath)
    return(individual)
  })

  output$assay_tab <- renderTable({
    req(input$assay)
    assay <- read.csv(input$assay$datapath)
    return(assay)
  })
}
