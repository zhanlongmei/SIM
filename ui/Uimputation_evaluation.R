tabPanel("4.Influence of imputation on differental analysis", fluid = TRUE,
         fluidPage(theme = shinytheme("cerulean"),
                   fluidRow(
                     h2("Usage"),
                     p("This function could be used to explore the influence of imputation on differential analysis:"),
                     p("If you have performed more than one imputation, please select two imputation results to compare them."),
                     p("You can only compare two imputation each time."),
                     selectizeInput(# Replace with what you want to have in sidebarPanel
                       'inputId' = "impmethodsel"
                       , 'label' = "Please select two applied imputation methods:"
                       , 'choices' = c("SIM","kNN","LLS","bPCA","pPCA","SVD","Mindet","MissForestImp")
                       , 'selected' = ""  # pick first column in dataset as names
                       , multiple = TRUE
                       , options = list(maxItems = 2)
                     ),
                     
                     h2("Visual comparion of the different DAFs"),
                     p("If there is no significant difference in classification accuarcy between two imputed results, 
                       it may indicate that (1) there is no difference between these two results; (2) the classification accuarcy could not tell the real difference."),
                     p("Since the classification accuarcy measures the effects of imputation on whole dataset level, it may fail to demonstrate the difference."),
                     p("The overlap of the differential abundance features (DAFs) can be used as a sensitive measurement to the similarity of different imputed results."),
                     p("The visual comparison of the different DAFs between two imputed results could indicate why there are different DAFs, and which imputed result is more reasonable."),
                     p("To perform this analysis,  firstly, you need to select two biological groups:"),
                     h4("The overlap of DAFs between two imputed results"),
                     uiOutput("selgroup"),
                     textOutput("venndiffmz"),
                     h4("Visual comparison of the different DAFs"),
                     p("In the following plots, each row represents an imputation approach, and each column represents a MS feature,
                       the blue dots are the observed values and the red dots are the imputed values. For the replication samples,
                       the imputed and observed values should be comparable. If the imputed values are biased from the observed values in the replication samples, 
                       these imputed values may be inappropriate."),
                     plotOutput("in1out2"),
                     plotOutput("in2out1"),
                     h2("PCA analysis"),
                     column(6,
                            plotOutput("pca_effect1")
                     ),
                     column(6,
                            plotOutput("pca_effect2")
                     )
                   )
         )
)