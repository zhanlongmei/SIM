#------------------imputation-------------------------
umimputed <- eventReactive(input$Imputation, {

  data <- datafiltered()
  m <- input$bm
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, 
               expr = {
                 n <- sapply(m,function(x){
                   k <- eval(parse(text=paste(x,"(data)",sep = "")))
                   return(list(name=x,data=k))
                 },simplify = F)
               })
  SIM <- list(name="SIM",data=sim())
  n$SIM <- SIM
  return(n)
})

#------------------file download-------------------------
dwf <- reactive({
  f <- input$dw
  ll <- umimputed()
  data <- eval(parse(text=paste("ll$",f,"$data",sep = "")))
  print(data[1:5,1:10])
  return(data)
})
output$downloadDatas <- downloadHandler(
  filename = function() {
    paste("Imputed", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.table(dwf()[,-5], file,row.names = F,col.names = T)
  }
)

#------------------IA-------------------------------------
ia <- reactive({
  imputations <- umimputed()
  cdnaf <- datafiltered()
  gr <- unique(subset(cdnaf,class != "QC")$class)[1]
  print(gr)
  sa <- apply(cdnaf[which(cdnaf$class==gr),-c(1:5)],2,function(x){sum(is.na(x))/length(x)})
  cong <- names(sa[sa>0 & sa <1])
  ori <- cdnaf[which(cdnaf$class==gr),which(names(cdnaf) %in% cong)] %>% melt()
  ori$label <- ifelse(is.na(ori$value),"imputed","observed")
  
  val <- function(ll){
    d <- ll$data
    n <- ll$name
    imp <- d[which(d$class==gr),which(names(d) %in% cong)] %>%melt()
    imp <- cbind(ori[,3,drop=F],imp)
    ou <- NULL
    for(i in unique(imp$variable)){
      a <- subset(imp,variable==i)
      im <- which(a$label=="imputed")
      ob <- which(a$label=="observed")
      m <- median(a$value[im])/median(a$value[ob])
      o <- data.frame(m=m)
      row.names(o) <- i
      ou <- rbind(ou,o)
    }
    ou$n <- n
    return(ou)
  }
  ias <- sapply(imputations,val,simplify = F)
  iast <- do.call("rbind",ias)
  return(iast)
})

output$umia <- renderPlot({
  dd <- ia()
  iap <- ggplot(dd,aes(n,log2(m)))+geom_boxplot()+
    theme_bw()+
    labs(x=NULL,y="-log2(IAs)")
    return(iap)
})

output$umiatable <- renderTable({
  dd <- ia()
  print(head(dd))
  k <- aggregate(log2(dd$m),by=list(approach=as.factor(dd$n)),median)
  names(k)[2] <- "IA"
  return(k)
})


