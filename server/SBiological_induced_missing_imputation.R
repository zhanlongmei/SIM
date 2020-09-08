#======================================total group missing plotling=======================================================================
output$totalgroupmissedmz <- renderPlot({
  data <- datafiltered()
  miss_ratio_by_class <- aggregate(data[,-c(1,2,3,4,5)],by=list(data$class),ratio_count)  #miss_ratio_by_class[,c(1:15)]
  total_group_missed_mz <- miss_ratio_by_class %>%     #head(total_group_missed_mz)
    melt(id=c("Group.1")) %>%
    filter(value == 1)
  validate(
    need(nrow(total_group_missed_mz) >0, "No total group missed features")
  )

  data2 <- data[,which(names(data) %in% unique(total_group_missed_mz$variable))]
  row.names(data2) <- data$sample
  
  napostion <- is.na(data2)
  data2[napostion] <- 3
  data2[!napostion] <- 2
  for(i in c(1:nrow(total_group_missed_mz))){
    #i <- 1
    nn <- length(data2[grep(total_group_missed_mz$Group.1[i],data$class),grep(total_group_missed_mz$variable[i],colnames(data2))])
    data2[grep(total_group_missed_mz$Group.1[i],data$class),grep(total_group_missed_mz$variable[i],colnames(data2))] <- 1
  }
  data2 <- as.data.frame(data2)
  data2 <- data2[,!(apply(data2,2,function(x){all(x==2)}))]
  if(ncol(data2)>300){
    data2 <- data2[,c(1:300)]
  }
  list <- listData()
  k<-melt(cbind(sample=rownames(data2), data2),id="sample")
  kk<-merge(list,k,by.x = "sample",by.y = "sample")
  tp <- ggplot(kk, 
         aes(x = variable, y = sample, fill = factor(value))) + 
    geom_tile()+
    theme(axis.text.x = element_blank())+
    theme(axis.ticks.x = element_blank())+
    scale_y_discrete(breaks=kk$sample, labels = kk$class)+
    scale_fill_manual(values=c("red", "white", "blue"), 
                      breaks=c("1", "2", "3"),
                      labels=c("TM", "", "LIM/UM"))+
    theme(legend.position="top")+
    guides(fill=guide_legend(title=NULL))
  return(tp)
})

#======================================total group missing handling=======================================================================
totalImputed <- reactive({
  data <- datafiltered()
  
  all_value <- as.numeric(as.matrix(data[,-c(1,2,3,4,5)]))
  low_value <- quantile(all_value,0.01,na.rm = T)
  all_low <- all_value[which(all_value<low_value)]
  mean_l <- mean(all_low,na.rm = T)
  sd_l <- sd(all_low,na.rm = T)
  
  miss_ratio_by_class <- aggregate(data[,-c(1,2,3,4,5)],by=list(data$class),ratio_count)  #miss_ratio_by_class[,c(1:15)]
  total_group_missed_mz <- miss_ratio_by_class %>%     #head(total_group_missed_mz)
    melt(id=c("Group.1")) %>%
    filter(value == 1)
  
  if(nrow(total_group_missed_mz) == 0){
    validate(
      need(nrow(total_group_missed_mz) >0, "No total group missed features"),
      return(data)
    )
  }else{
    data2 <- as.matrix(data[,-c(1,2,3,4,5)])
    for(i in c(1:nrow(total_group_missed_mz))){
      #i <- 1
      nn <- length(data2[grep(total_group_missed_mz$Group.1[i],data$class),grep(total_group_missed_mz$variable[i],colnames(data2))])
      set.seed(1)
      data2[grep(total_group_missed_mz$Group.1[i],data$class),grep(total_group_missed_mz$variable[i],colnames(data2))] <- abs(rnorm(nn))
    }
    data <- cbind(data[,c(1,2,3,4,5)],data2) #data[c(1:5),c(1:10)]

    return(data)
  }
})

output$total_nu <- renderText({
  data1 <- datafiltered()
  data2 <- totalImputed()
  total_nu <- sum(is.na(data1))-sum(is.na(data2))
  paste("A total of ",total_nu,"total group missing has been replaced with small value!") 
})
#======================================low abundance induced missing plotling=======================================================================
output$lowintensitymissing <- renderPlot({
  data <- totalImputed()
  value_ratio <- input$lowquantile
  list <- listData()
  miss_ratio_by_class <- aggregate(data[,-c(1,2,3,4,5)],by=list(data$class),ratio_count) #miss_ratio_by_class[,c(1:15)]
  mean_intensity_by_class <- aggregate(data[,-c(1,2,3,4,5)],by=list(data$class),function(x){mean(as.numeric(as.character(x)),na.rm = T)}) #mean_intensity_by_class[,c(1:15)]
  a <- melt(miss_ratio_by_class,id=c("Group.1"))
  names(a) <- c("group","mz","ratio")
  a$gmz <- paste(a$group,a$mz,sep="_")
  b <- melt(mean_intensity_by_class,id=c("Group.1"))
  names(b) <- c("group","mz","intensity")
  b$gmz <- paste(b$group,b$mz,sep="_")
  ab <- merge(a,b,by.x = "gmz",by.y = "gmz")  #head(ab)
  #----------------
  all_value <- as.numeric(as.matrix(data[,-c(1,2,3,4,5)]))
  low_value <- quantile(all_value,value_ratio,na.rm = T)
  low_mz <- subset(ab,intensity<=low_value)
  validate(
    need(nrow(low_mz) >0, "No low intensity induced missing identified!")
  )
  #----------------
  data2 <- as.matrix(data[,-c(1,2,3,4,5)])
  row.names(data2) <- data$sample
  data4 <- data2
  data5 <- data2
  napo <- is.na(data2)
  
  data4[!(is.na(data4))] <- NA
  for(i in c(1:nrow(low_mz))){
    class_mark <- grep(low_mz$group.x[i],data$class)
    mz_mark <- grep(low_mz$mz.x[i],colnames(data2))
    k<-which(is.na(data2[class_mark,mz_mark]))
    nn <- length(k)
    data4[class_mark[k],mz_mark]<- 1
  }
  data5 <- data5[,apply(data5,2,function(x){any(is.na(x))})]
  data5 <- as.data.frame(data5)
  data6 <- melt(cbind(sample=rownames(data5), data5))
  data6 <- data6[!(is.na(data6$value)),]
  
  data4 <- as.data.frame(data4)
  data8 <- melt(cbind(sample=rownames(data4), data4))
  data8 <- filter(data8,value==1)
  
  fel <- length(unique(data6$variable))
  if(fel>50){
    fe <- unique(data6$variable)[c(1:50)]
  }else{
    fe <- unique(data6$variable)
  }
  data8 <- data8[(data8$variable %in% fe),]
  data6 <- data6[(data6$variable %in% fe),]
  data6<-merge(list,data6,by.x = "sample",by.y = "sample")
  
  
  pp <- ggplot(data6,aes(variable,sample,fill=sqrt(sqrt(value))))+
    geom_tile()+scale_fill_gradientn(colours = terrain.colors(10),name="intensity")+
    geom_tile(aes(variable,sample),data=data8,fill="red")+
    theme(axis.text.x = element_blank())+
    theme(axis.ticks.x = element_blank())+
    scale_y_discrete(breaks=data6$sample, labels = data6$class)#+
    #theme(legend.position="top")
  pp
  
})
#======================================low abundance induced missing handling=======================================================================

minimunImputed <- reactive({
  data <- totalImputed()
  value_ratio <- input$lowquantile
  miss_ratio_by_class <- aggregate(data[,-c(1,2,3,4,5)],by=list(data$class),ratio_count) #miss_ratio_by_class[,c(1:15)]
  mean_intensity_by_class <- aggregate(data[,-c(1,2,3,4,5)],by=list(data$class),function(x){mean(as.numeric(as.character(x)),na.rm = T)}) #mean_intensity_by_class[,c(1:15)]
  a <- melt(miss_ratio_by_class,id=c("Group.1"))
  names(a) <- c("group","mz","ratio")
  a$gmz <- paste(a$group,a$mz,sep="_")
  b <- melt(mean_intensity_by_class,id=c("Group.1"))
  names(b) <- c("group","mz","intensity")
  b$gmz <- paste(b$group,b$mz,sep="_")
  ab <- merge(a,b,by.x = "gmz",by.y = "gmz")  #head(ab)
  #----------------
  all_value <- as.numeric(as.matrix(data[,-c(1,2,3,4,5)]))
  low_value <- quantile(all_value,value_ratio,na.rm = T)
  all_low <- all_value[which(all_value<low_value)]
  mean_l <- mean(all_low,na.rm = T)
  sd_l <- sd(all_low,na.rm = T)
  low_mz <- subset(ab,intensity<=low_value)
  if(nrow(low_mz) == 0){
    validate(
      need(nrow(low_mz) >0, "No low intensity induced missing identified!"),
      return(data)
    )
  }else{
    data2 <- as.matrix(data[,-c(1,2,3,4,5)])
    for(i in c(1:nrow(low_mz))){
      #i <- 3
      class_mark <- grep(low_mz$group.x[i],data$class)
      mz_mark <- grep(low_mz$mz.x[i],colnames(data2))
      dd <- data2[class_mark,mz_mark]
      k<-which(is.na(dd))
      nn <- length(k)
      if(sum(!is.na(dd))>1){
        m <- min(dd,na.rm=T)
        s <- sd(dd,na.rm = T)
      }else{
        m <- dd[!is.na(dd)]
        s <- m
      }
      data2[class_mark[k],mz_mark]<- rnorm_fixed(nn,m,s*0.05)
    }
    data <- cbind(data[,c(1,2,3,4,5)],data2) #data[c(1:5),c(1:10)]
    return(data)
  }
})

output$small_nu <- renderText({
  data1 <- totalImputed()
  data2 <- minimunImputed()
  s_nu <- sum(is.na(data1))-sum(is.na(data2))
  paste("A total of ",s_nu,"low abundance induced missing has been replaced with small value!") 
})

output$missmatrixbefore <- renderPlot({
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

output$missmatrix_low_fliter <- renderPlot({
  data <- minimunImputed()
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

#======================================UM handling=======================
#sim <- reactive({
#  data <- minimunImputed()
#  return(kNN(data))
#})

sim <- reactive({
  data <- minimunImputed()
  m <- input$um_impute
  k <- eval(parse(text=paste(m,"(data)",sep = "")))
  return(k)
})

output$um_count <- renderText({
  data2 <- sum(is.na(minimunImputed()))
  paste("A total of ",data2,"UMs has been imputed with kNN!") 
})

output$downloadsim <- downloadHandler(
  filename = function() {
    paste("SIM_Imputed", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.table(sim(),file,row.names = F,col.names = T,sep = ",")
  }
)

output$miss_summary <- renderText({
  data1 <- datafiltered()
  data2 <- totalImputed()
  data3 <- minimunImputed()
  tm <- sum(is.na(data1))-sum(is.na(data2))
  lim <- sum(is.na(data2))-sum(is.na(data3))
  um <- sum(is.na(data3))
  paste("A total of ",tm,"total group missing,", lim, " low intensity induced missing and ", um, " unclear missing were found in the dataset",sep = "") 
})