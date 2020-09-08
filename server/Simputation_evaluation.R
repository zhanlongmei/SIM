
output$selgroup <- renderUI({
  list <- listData()
  list <- list[(list$class != "QC"),]
  classchoose <- unique(as.character(list$class))
  
  selectizeInput(
    'inputId' = "classsel"
    , 'label' = "Please select two class labels:"
    , 'choices' = classchoose
    , 'selected' = ""  
    , multiple = TRUE
    , options = list(maxItems = 2)
  )
})

output$venndiffmz <- renderText({
  validate(
    need(length(input$impmethodsel)==2, "Please choose two imputed results")
  )
  validate(
    need(length(input$classsel)==2, "Please choose two biological groups")
  )
  m <- input$impmethodsel
  titles1 <- m[1]
  titles2 <- m[2]

  imputedd <- umimputed()
  eval(parse(text=paste("data1 <- imputedd$",titles1,"$data",sep = "")))
  eval(parse(text=paste("data2 <- imputedd$",titles2,"$data",sep = "")))
  g <- input$classsel
  g1 <- g[1]
  g2 <- g[2]
  method1 <- different_express_single_pair(g1,g2,data1)
  method2 <- different_express_single_pair(g1,g2,data2)
  g1l <- length(method1)
  g2l <- length(method2)
  gt <- length(intersect(method1,method2))
  x1 <- g1l-gt
  x2 <- g2l-gt
  paste(x1," differential abundant MS features were only found in ", titles1, ", ", x2, " were only found in ",titles2,
              " and ", gt, " were found in both groups",sep = "")
  
})


output$in1out2 <- renderPlot({
  validate(
    need(length(input$impmethodsel)==2, "Please choose two imputed results")
  )
  validate(
    need(length(input$classsel)==2, "Please choose two biological groups")
  )
  namem <- input$impmethodsel
  name1 <- namem[1]
  name2 <- namem[2]
  g <- input$classsel
  g1 <- g[1]
  g2 <- g[2]

  imputedd <- umimputed()
  eval(parse(text=paste("data1 <- imputedd$",name1,"$data",sep = "")))
  eval(parse(text=paste("data2 <- imputedd$",name2,"$data",sep = "")))
  peaks <- datafiltered()
  in1out2 <- two_imp_diff_express_comparsion_in1_out2(peaks,data1,data2,name1,name2,g1,g2,1)
  in1out2
})

output$in2out1 <- renderPlot({
  validate(
    need(length(input$impmethodsel)==2, "Please choose two imputed results")
  )
  validate(
    need(length(input$classsel)==2, "Please choose two biological groups")
  )
  namem <- input$impmethodsel
  name1 <- namem[1]
  name2 <- namem[2]
  g <- input$classsel
  g1 <- g[1]
  g2 <- g[2]

  imputedd <- umimputed()
  eval(parse(text=paste("data1 <- imputedd$",name1,"$data",sep = "")))
  eval(parse(text=paste("data2 <- imputedd$",name2,"$data",sep = "")))
  peaks <- datafiltered()
  in2out1 <- two_imp_diff_express_comparsion_in1_out2(peaks,data1,data2,name1,name2,g1,g2,2)
  in2out1
})

output$pca_effect1 <- renderPlot({
  validate(
    need(length(input$impmethodsel)==2, "Please choose two imputed results")
  )
  validate(
    need(length(input$classsel)==2, "Please choose two biological groups")
  )
  m <- input$impmethodsel
  titles1 <- m[1]

  imputedd <- umimputed()
  eval(parse(text=paste("data1 <- imputedd$",titles1,"$data",sep = "")))
  p1 <- single_pca(data1,titles1)
  p1 %>%
    ggplot(aes(Dim.1,Dim.2,color=class))+geom_point()+
    stat_ellipse()+
    xlab("PC1")+
    ylab("PC2")+
    theme_bw()+
    ggtitle(paste("PCA plot of the ",titles1," imputed dataset"))

})

output$pca_effect2 <- renderPlot({
  validate(
    need(length(input$impmethodsel)==2, "Please choose two imputed results")
  )
  validate(
    need(length(input$classsel)==2, "Please choose two biological groups")
  )
  m <- input$impmethodsel

  titles2 <- m[2]
  imputedd <- umimputed()
  eval(parse(text=paste("data2 <- imputedd$",titles2,"$data",sep = "")))
  p2 <- single_pca(data2,titles2)
  p2 %>%
    ggplot(aes(Dim.1,Dim.2,color=class))+geom_point()+
    stat_ellipse()+
    xlab("PC1")+
    ylab("PC2")+
    theme_bw()+
    ggtitle(paste("PCA plot of the ",titles2," imputed dataset"))
  
})