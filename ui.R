library(shiny)

shinyUI(fluidPage(
  
  img(src="KummerSurface.jpeg", width=80, align='right'),
  titlePanel("A topological trick for visualisation"),

  # suppresses spurious 
  # 'progress' error messages after all the debugging 
  # is done:
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # the main stuff:
  HTML("
       <br>
       <p>
       The plot below illustrates the 'mapper' construction described in section 3 of the
reference below, here applied to the well known Zip data set of 7,291 hand-written digits.
       </p><p>
The original data lives in 256-dimensional pixel space. The idea of the construction is to 
partition the data points into overlapping bins -- thought of as an 'open cover' -- and 
to perform heirarchical clustering within each bin. A graph is then constructed whose
vertices are the resulting clusters across all bins, and whose edges says that a pair of 
bins has one or more points in common. In the interactive plot you can control the resolution
via two variables: the number of bins and the number of clusters extracted within each bin.
</p><p>
How should you choose these tuning parameters? For each choice plot the distribution of entropy
of the class distributions within the clusters. There is then a trade-off between number of 
clusters (graph vertices) and average entropy. You want both of these to be low.
</p><p>
So where do the bins come from? Following the topological analogy, we pull back open intervals
from the real line under a suitable 'continuous' function. For this function, we take local 
density of the point cloud -- measured by counting neighbouring points within a fixed radius.
</p><p>
There is, however, a caveat... The curse of dimension makes it hard to make all this work
in 256 dimensions. So I've actually performed a dimensional reduction first (down to
6 dimensions in fact). There are various ways one can do this -- I've used a Fiedler
embedding from the nearest-neighbour graph in pixel space (but I'm brushing that under the carpet for now).
</p>
       <h4>Reference</h4>
       <li>
       <ul> Gunnar Carlsson: <a href='http://www.ams.org/journals/bull/2009-46-02/S0273-0979-09-01249-X/'>Topology and Data</a>, 
       <i>Bull Amer Math Soc</i> 46 (2009), 255-308.
       </li>
       <hr>     
       "),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Set clustering parameters for data visualisation:"),
      
      sliderInput("nbins", 
                  label = "Resolution (nr bins):",
                  min = 1, max = 50, value = 10),
      sliderInput("klevel", 
                  label = "Resolution (heirarchical cluster k):",
                  min = 1, max = 20, value = 10),
            
      selectInput("layout", 
                  label = "Graph layout:",
                  choices = c("Auto",
                              "Circle",
                              "DRL",
                              "Fruchterman Reingold", 
                              "Fruchterman Reingold grid",
                              "Graphopt",
                              "Grid",
                              "Grid 3D",
                              "Kamada Kawai",
                              "LGL",
                              "MDS",
                              "Random",
                              "Reingold Tilford",
                              "Sphere",
                              "Spring",
                              "SVD"),
                  selected = "Auto"),
      
      numericInput("obsvertex", 
                   label = "Vertex number:", 
                   value = 14),
      helpText("Each vertex represents a cluster of digits. Here's a sample, plus table of counts:"),
      plotOutput("digitview"),
      tableOutput("digittable")
    ),
    
    mainPanel(  
      #tableOutput("clustertext"),
      helpText("For the given parameter choice, examine the distribution of entropy (or 'impurity') of the resulting clusters:"),
      plotOutput("entropyview"),
      helpText("Here's the graph view:"),
      plotOutput("graphview")
    )
  ),
  HTML("
     <hr>
     &copy Bill Ox 2015 <a href='mailto:box121209@gmail.com'>box121209@gmail.com</a>
     ")
  )
)