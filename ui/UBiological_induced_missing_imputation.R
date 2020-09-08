tabPanel("2.SIM imputation", fluid = TRUE,
         fluidPage(theme = shinytheme("cerulean"),
                   fluidRow(
                     h2("Introduction:"),
                     h4("TM"),
                     p("Due to the experimental design, some metabolites or chemicals are likely only detectable in one group but completely undetectable in the other.
                       If a MS feature is completely missed in one group but is detected in the other group with 50% detection rate at least, it is classified into TM. 
                       Such missing value is filled with random values with extreme low intensity ranging from 0 to 0.01."),
                     h4("LIM"),
                     p("LIMs are missing values caused by low signals. In the grouped datasets, 
                       if the maximum abundance of a MS feature from certain group of samples is lower than 
                       the 10% quantile of the entire dataset, the missing values in this MS feature are called LIM.
                       The LIMs are imputed with the minimum observed value from this MS feature"),
                     h4("UM"),
                     p("The remaining missing values without clear causes are called UMs, and kNN is recommended for UMs imputation"),
                     hr(),
                     h3("Summary of three types of missing values"),
                     textOutput("miss_summary"),
                     h3("Recognition and imputation to TM"),
                     p("In the figure below, the x axis represents MS features, and y axis represents samples, 
                    the observed values are marked in white, TMs are marked in red and LIMs/UMs are marked in blue. 
                    Only the MS features containing TMs are selected for visualization."),
                     plotOutput("totalgroupmissedmz"),
                     textOutput("total_nu"),
                     hr(),
                     h3("Recognition and imputation to LIM"),
                     p("The cutoffs used to determine the LIM can be modified:"),
                     p("please specifiy the intensity cutoff to recognize the LIMs. For a specific MS feature, if the biological replication samples containing missing values and the largest observed values of these samples is smaller than the cutoff, the missing values would be marked as LIMs."),
                      sliderInput("lowquantile", "set a low intensity limit",
                                   min = 0, max = 0.2, value = 0.1
                      ),
                     p("The figure below shows the distribution of LIMs and UMs after TMs imputation, 
                       the x axis represents MS features, and y axis represents samples, 
                       the intensities of the observed values are represented by the color range. 
                       The LIMs are marked as red and UMs as white. Only 60 MS features are randomly selected for
                       visualization purpose."),
                     plotOutput("lowintensitymissing"),
                     textOutput("small_nu"),
                     h3("Missing value distribution before and after TM & LIM processing"),
                     p("The figure below shows the distribution of missing values before/after processing of TMs and LIMs.
                       The x axis represents samples and y axis represents MS features. The missing values are marked in red."),
                     column(6,
                            p("Before processing TM and LIM"),
                            plotOutput("missmatrixbefore")
                     ),
                     column(6,
                            p("After processing TM and LIM"),
                            plotOutput("missmatrix_low_fliter")
                     ),
                     h3("Imputation to UM"),
                     textOutput("um_count"),
                     radioButtons("um_impute", "Choose imputation methods for UMs:",
                                        c("kNN" = "kNN",
                                          "LLS" = "LLS",
                                          "bPCA" = "bPCA",
                                          "pPCA" = "pPCA",
                                          "SVD" = "SVD",
                                          "Mindet" = "Mindet",
                                          "MissForestImp" = "MissForestImp")),
                     downloadLink('downloadsim', 'Download SIM imputed result')
                  )
         )
)

