library(shiny)
#setwd("~/shiny/zip-1.0")
source("helpers.R")

dat <- read.table("data/zip.fiedler.txt")
x <- zip.train[,-1]
y <- zip.train[,1]
vec <- dat[,1]
fzip <- dat[,-1]

# input <- list(nbins=10, klevel=5, layout="Auto")

shinyServer(

  function(input, output) {
        
    # build clusters:
    cluster.set <- reactive({
      make.clusters(fzip, vec, input$nbins, klevel=input$klevel)
    })
    mg <- reactive({
      mapper.graph(cluster.set())
    })
    comps <- reactive({
      components(mg())
    })
    ncc <- reactive({
      length(comps())
    })
    lout <- reactive({
      lout.fn <- switch(input$layout,
                        
                        "Auto" = layout.auto,
                        "Circle" = layout.circle,
                        "DRL" = layout.drl,
                        "Fruchterman Reingold" = layout.fruchterman.reingold,
                        "Fruchterman Reingold grid" = layout.fruchterman.reingold.grid,
                        "Graphopt" = layout.graphopt,
                        "Grid" = layout.grid,
                        "Grid 3D"  = layout.grid.3d,
                        "Kamada Kawai" = layout.kamada.kawai,
                        "LGL" = layout.lgl,
                        "MDS" = layout.mds,
                        "Random" = layout.random,
                        "Reingold Tilford" = layout.reingold.tilford,
                        "Sphere" = layout.sphere,
                        "Spring" = layout.spring,
                        "SVD" = layout.svd  
                        )
      lout.fn(mg())
    })
    
    # outputs:
    output$clustertext <- renderTable({
      print.clusters(cluster.set())
    })
    output$entropyview <- renderPlot({
      show.entropy(cluster.set(), y)
    })
    output$graphview <- renderPlot({
      show.graph(mg(), cluster.set(), v=input$obsvertex, layout=lout())
    })
    output$digitview <- renderPlot({
      show.digits(cluster.set(), input$obsvertex)
    })
    output$digittable <- renderTable({
      print.digits(cluster.set(), input$obsvertex)
    }) 
  }
)
