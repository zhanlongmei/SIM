
options(shiny.maxRequestSize=30*1024^2)

peakData <- reactive({
  validate(
    need(input$peakfile != "", "Please upload peak file")
  )
  inFile1 <- input$peakfile
  if (is.null(inFile1))
    return(NULL)
  read_csv(inFile1$datapath)

})

listData<- reactive({
  validate(
    need(input$listfile != "", "Please upload list file")
  )
  inFile2 <- input$listfile
  if (is.null(inFile2))
    return(NULL)
  read.delim(inFile2$datapath,header = T,sep="\t")
})

mergedf <- reactive({
  peak <- peakData()
  list <- listData()
  list$mark <- "sample"
  list$mark[list$class=="QC"] <- "QC"
  peak[peak==0] <- NA
  peakt <- as.data.frame(t(peak[,-c(1,2)]))
  names(peakt) <- peak$mz
  peakt$sample <- row.names(peakt)
  inner_join(list,peakt,by="sample")
})

value <- reactiveValues() 
value$a=data.frame(
  group = c("no_miss","has_miss"),
  value = c("FALSE", "TRUE")
)


output$mzmisspercent <- renderText({
  data <- mergedf()
  mz_miss <- sum(sapply(data[,-c(1,2,3,4,5)], function(x){any(is.na(x))}))
  total_mz <- ncol(data)-5
  mz_ratio <- mz_miss/total_mz
  miss_number <- sum(is.na(data[,-c(1,2,3,4,5)]))
  total_number <- sum(!(is.na(data[,-c(1,2,3,4,5)])))+miss_number
  miss_ratio <- miss_number/total_number
  paste(round(miss_ratio,2)," percent of missing values was found in this dataset, affecting ",round(mz_ratio,2)," percent of MS features.")
  #paste("A total of ", total_mz, " were found in this dataset, among them, ", mz_miss, " containing missing values", sep= "")
})

output$samplepercent <- renderPlot({
  data <- mergedf()
  list <- listData()
  sample_name <- data$sample
  data <- as.data.frame(t(data[,-c(1,2,3,4,5)]))
  names(data) <- sample_name
  md_pattern <- md.pattern(data)

  sample_miss_number <- md_pattern[nrow(md_pattern),][-ncol(md_pattern)]/nrow(data)
  sample_miss_number_plot <- data.frame(
    sample=names(sample_miss_number),
    percents=sample_miss_number
  ) 
    inner_join(list[,c(1,3)],sample_miss_number_plot,by="sample") %>%
    ggplot(aes(class,percents))+
    geom_boxplot()+
    geom_jitter(width = 0.2)
})

output$missmatrix <- renderPlot({
  data <- mergedf()
  list <- listData()
  sample_name <- data$sample
  data <- as.data.frame(t(data[,-c(1,2,3,4,5)]))
  names(data) <- sample_name
  annotation_col = as.data.frame(list[,3,drop=F])
  row.names(annotation_col) <- list$sample
  sorted_name <- row.names(annotation_col[order(annotation_col$class),1,drop=F])

  sorted_order_matrixplot <- sapply(sorted_name,function(x){which(colnames(data)==x)})
  matrixplot(data[,sorted_order_matrixplot], interactive = F)
})


datafiltered <- reactive({
  data <- mergedf()
  qc_ratio <- input$qcpercentst
  sam_ratio <- input$samplepercentst
  miss_ratio_by_class <- aggregate(data[,-c(1,2,3,4,5)],by=list(data$class),ratio_count)   #miss_ratio_by_class[,c(1:5)]
  qc_miss_ratio <- as.data.frame(t(miss_ratio_by_class[miss_ratio_by_class$Group.1=="QC",-1]) >= qc_ratio)    #head(qc_miss_ratio)
  names(qc_miss_ratio) <- c("qc")
  qc_fliter_mz <- row.names(subset(qc_miss_ratio,qc=="TRUE"))
  
  sam_miss_ratio <- miss_ratio_by_class[!(miss_ratio_by_class$Group.1=="QC"),]  #sam_miss_ratio[,c(1:5)]
  sam_miss_judge <- apply(sam_miss_ratio[,-1],2,function(x){all(x>=sam_ratio)})
  sample_fliter_mz <- names(which(sam_miss_judge=="TRUE"))
  
  mz_fliter <- union(qc_fliter_mz,sample_fliter_mz)   #length(mz_fliter)
  data_flitered <- data[,!(names(data) %in% mz_fliter)]   #data_flitered[c(1:5),c(1:10)]
  return(data_flitered)
  
})


