tabPanel("3.Other imputation", fluid = TRUE,
         fluidPage(theme = shinytheme("cerulean"),
                   fluidRow(
                     h2("Introduction"),
                      p("In SIM, three kinds of imputations are 
                        avaiable: (1) Local and (2) Global structure based imputation and (3) machine learning based imputation."),
                      p("The local structure based imputation replace the missing value based on 
                        the expression profiles of several other features with similar intensity profiles in the same 
                       dataset.This strategy ,in general, make the assumption that the features are regulated dependently,
                       and the highly correlated profiles are observed with coregulated features.
                       K nearest neighbors (KNN) and local least-squares (LLS) are two most often used methods:"),
                     p("The global-structure-based imputation methods apply dimension reduction techniques to decompose the
                       data matrix and then iteratively reconstruct the missing values.
                       Bayesian PCA (BPCA), Probabilistic PCA (PPCA) and SVD are most often used methods:"),
                     p("The machine learning based imputation employs the models such as the random forest to esitmate the missing values,
                       the MissForest is a typical appraoch."),
                     hr(),
                     h2("Please choose imputation approaches:"),
                     column(6,
                            #h2("Batch effects Removal"),
                            checkboxGroupInput("bm", "Choose imputation methods for all missing values:",
                                               c("kNN" = "kNN",
                                                 "LLS" = "LLS",
                                                 "bPCA" = "bPCA",
                                                 "pPCA" = "pPCA",
                                                 "SVD" = "SVD",
                                                 "Mindet" = "Mindet",
                                                 "MissForestImp" = "MissForestImp")),
                            actionButton("Imputation", "Apply these methods", class = "btn-primary")
                     ),
                     column(6,
                            #h2("5. Choose the best result to download"),
                            radioButtons("dw", "Choose a result to download:",
                                         c("kNN" = "kNN",
                                           "LLS" = "LLS",
                                           "bPCA" = "bPCA",
                                           "pPCA" = "pPCA",
                                           "SVD" = "SVD",
                                           "Mindet" = "Mindet",
                                           "MissForestImp" = "MissForestImp")),
                            downloadLink('downloadDatas', 'Download')
                     )
                     ),
                     p("Note: the selected appraoches will be applied to impute all the missing values in the dataset."),
                     p("The imputation would take a long time to complete (especially the MissForest), which depends on the amount of your data, please wait. and do not clikc one button many times"),
                     h2("Evaluation to different imputations"),
                     column(6,
                     tableOutput("umiatable")
                     ),
                     column(6,
                     plotOutput("umia")
                     )
                     
                   
         )
)