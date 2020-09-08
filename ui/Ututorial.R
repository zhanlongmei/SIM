tabPanel("0.Tutorial", fluid = TRUE,
         fluidPage(theme = shinytheme("cerulean"),
                   fluidRow(
                     h2("What's SIM?"),
                     p("SIM is short for Stepwise Imputation of untargeted Metabolomics data sets. 
                       It's a shiny application designed to handling missing data in a systematic way"),
                     p("The missing values in a dataset are firstly classified into truly missing (TM), 
                       low intensity induced missing (LIM) and missing values with unclear reasones (UM). Different imputation appraoches are applied to these missing values."),
                     tags$img(src = "SIM_pipeline.png", width = "600px", height = "400px"),
                     hr(),
                     h2("Source code and example data"),
                     p("The source code can be found on https://github.com/zhanlongmei/simple-imputation-for-metabolomics-data"),
                     p("An example data for mouse plasma published in Frontiers in Microbiology and stored in Metabolight as MTBLS418 is avaiable:"),
                     downloadButton("downloadData", label = "Download"),
                     hr(),
                     h2("How to use SIM?"),
                     p("The usage of SIM is very simple. Users need to provide a sample-by-feature matrix and sample list file and follow the instructions"),
                     h4("Sample list"),
                     p("The sample list is a txt file containing the sample information separated by the tab, it should contain four colums: \"sample	batch	class	order\". "),
                     p("sample refers to sample name; batch refers to the batch information; class refers to the biological group, the QC group must be named as \"QC\", as shown in our example data; order refers to the injetion order."),
                     tags$img(src = "sample_list.png",width="400px",height="500px"),
                     h4("Sample-by-feature matrix"),
                     p("The sample-by-feature matrix is a csv file containing the signal intensity of MS features, the first columns must be mz and RT, the rest columns are sample names. Each row represents a mz"),
                     p("The sample names in the peak table must be the same with sample names in the sample list file."),
                     tags$img(src = "sample_feature_matrix.png",width="800px",height="500px"),
                     hr(),
                     h2("Contact Us"),
                     p("If you have any questions, suggestions or remarks, please contact meizhanlong@genomics.cn. You can also visti our SIM github page and build a local version on your own computer. https://github.com/zhanlongmei/simple-imputation-for-metabolomics-data.")
                   )
         )
)
