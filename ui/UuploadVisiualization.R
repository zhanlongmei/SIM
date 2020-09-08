tabPanel("1.Data upload", fluid = TRUE,
         fluidPage(theme = shinytheme("cerulean"),
           fluidRow(
             h2("File upload"),
             p("Uplaod your data or try our example data. An example data is avaiable at Tutorial panel. 
               An instruction about data preparing is also provided."),
             column(6,
                    fileInput("peakfile", "Choose Sample-by-feature matrix",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                    )
             ),
             column(6,
                    fileInput("listfile", "Choose sample list File",
                              accept = c(
                                "text/txt",
                                "text/comma-separated-values,text/plain",
                                ".txt")
                    )
             )
           ),
           hr(),
             fluidRow(
                    h3("Miss percent"),


                    textOutput("mzmisspercent"),


                    hr(),
                    h3("missing value distribution"),
                    p("In the left figure, the X axis represents the samples and the y axis represents MS features"),
                    p("Red color indicates the missing values, while the white and grey represent observed values"),
                       column(6,
                           p("the distribution of missing value on the whole dataset"),
                           plotOutput("missmatrix")
                    ),
                    column(6,
                           p("missing ratio in each sample"),
                           plotOutput("samplepercent")
                    ),
                    h2("Filtering based on \"80% rules\""),
                    p("Please set the minimum missing ratio for the QC and study samples. 
                      Features from QC sampples that have a higher missing ratio than the threshould will be removed.
                      Features from all study sample that have a higher missing ratio than the threshould will be removed."),
                    
                    column(6,
                           sliderInput("qcpercentst", "Percent of missing in QC sample",
                                       min = 0, max = 1, value = 0.5
                           )
                    ),
                    column(6,
                           sliderInput("samplepercentst", "Percent of missing in study sample",
                                       min = 0, max = 1, value = 0.5
                           )
                    )
                   

             )
           
           
         )
)
