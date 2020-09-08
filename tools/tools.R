

ratio_count <- function(x){
  sum(is.na(x))/length(x)
}

rnorm_fixed = function(n,mu, sigma) {
  set.seed(1)
  x = rnorm(n)  
  x = sigma * x / sd(x)  
  x = x - mean(x) + mu  
  x[which(x<0)] <- abs(x[which(x<0)])
  return(x)
}

single_sample_imp_validation <- function(x,pre,imp){
  pre_da <- t(pre[x,-c(1,2,3,4,5)])
  miss_item <- which(is.na(pre_da))
  if(length(miss_item)>0){
    pre_da <- pre_da[-miss_item]
    imp_da <- t(imp[x,-c(1,2,3,4,5)])
    imp_da <- imp_da[miss_item]
    pre_df <- data.frame(
      mark="pre",
      intensity=pre_da
    )
    imp_df <- data.frame(
      mark="imp",
      intensity=imp_da
    )
    plot_D <- rbind(pre_df,imp_df)
  }else{
    plot_D <- data.frame(
      mark="pre",
      intensity=pre_da
    )
  }

  plot_D$sample <- pre[,1][x]
  return(plot_D)
}

sample_wise_imputation_validation <- function(pre,imp){

  sample_xiabiao <- which(apply(pre,1,function(x){any(is.na(x))}))#get the samples that contain missing data
  sample_names <- sample_xiabiao[ceiling(quantile(c(1:length(sample_xiabiao))))]
  names(sample_names) <- pre[,1][sample_names]
  a <- sample_names[1]
  plot_dat_p <- single_sample_imp_validation(a,pre,imp)
  for(i in sample_names[-1]){
    x <- single_sample_imp_validation(i,pre,imp)
    plot_dat_p <- rbind(plot_dat_p,x)

  }


  plot_dat_p$sample <- as.character(plot_dat_p$sample)

  sample_validation_plot <- ggplot(plot_dat_p,aes(sample,log(intensity)))+
    geom_jitter(aes(colour=mark))+
    coord_flip()+
    theme_light()+
    theme(legend.position="top")+
    guides(fill=guide_legend(title=NULL))

  return(sample_validation_plot)
}

single_mz_imp_validation <- function(x,pre,imp){

  pre_da <- pre[,x]
  miss_item <- which(is.na(pre_da))
  if(length(miss_item)>0){
    pre_da <- pre_da[-miss_item]
    imp_da <- imp[,x]
    imp_da <- imp_da[miss_item]
    pre_df <- data.frame(
      mark="pre",
      intensity=pre_da
    )
    imp_df <- data.frame(
      mark="imp",
      intensity=imp_da
    )
    plot_D <- rbind(pre_df,imp_df)
  }else{
    imp_df <- data.frame(
      mark="pre",
      intensity=pre_da
    )
  }
  
  plot_D$mz <- names(pre)[x]
  return(plot_D)
}

mz_wise_imputation_validataion <- function(pre,imp){
  
  mz_xiabiao <- which(apply(pre,2,function(x){any(is.na(x))}))#get the mzs that contain missing data
  mz_names <- mz_xiabiao[ceiling(quantile(c(1:length(mz_xiabiao)),probs = c(1:10)/10))]
  mz_names <- mz_names[-length(mz_names)]
  names(mz_names) <- names(pre)[mz_names]
  plot_dat_p <- single_mz_imp_validation(mz_names[1],pre,imp)
  for(i in mz_names[-1]){
    aa <- single_mz_imp_validation(i,pre,imp)
    plot_dat_p <- rbind(plot_dat_p,aa)
  }
  mz_validation_plot <- ggplot(plot_dat_p,aes(mz,log(intensity)))+
    geom_boxplot()+
    geom_jitter(aes(colour=mark))+
    coord_flip()+
    theme_light()+
    theme(legend.position="top")+
    guides(fill=guide_legend(title=NULL))
  return(mz_validation_plot)
  
}

different_express_single_pair <- function(compare_strs1,compare_strs2,data){
  ratio_p_cal <- function(x){
    fenzi <- x[which(data$class==compare_strs1)]
    fenmu <- x[which(data$class==compare_strs2)]
    ratio <- mean(fenzi)/mean(fenmu)
    p_value <- t.test(fenzi,fenmu)$p.value
    out <- c(ratio,p_value)
    names(out) <- c("ratio","p")
    return(out)
  }
  ratio_p <- as.data.frame(t(apply(data[,-c(1,2,3,4,5)],2,ratio_p_cal)))
  ratio_p$q <- p.adjust(ratio_p$p,"BH")
  sig_ratio_p <- row.names(subset(ratio_p,((ratio>=2 |ratio<=0.5) & p<=0.05)))
  return(sig_ratio_p)
}

two_imp_diff_express_comparsion_in1_out2 <- function(peaks,data1,data2,name1,name2,g1,g2,direction){

  sig_mz_1 <- different_express_single_pair(g1,g2,data1)
  sig_mz_2 <- different_express_single_pair(g1,g2,data2)
  
  #===========================comapre intensity==============================
  #in method 1 not in method 2
  if(direction == 1){
    in_method_1_out_method_2 <- setdiff(sig_mz_1,sig_mz_2)
    titleso<- paste("Different abundance MS features identified in ",name1," but not in ", name2," imputed dataset",sep = "")
  }
  if(direction == 2){
    in_method_1_out_method_2 <- setdiff(sig_mz_2,sig_mz_1)
    titleso<- paste("Different abundant MS features identified in ",name2," but not in ", name1," imputed dataset",sep = "")
  }

  vc <- list(peaks=peaks,d1=data1,d2=data2,n1=name1,n2=name2,in_method_1_out_method_2=in_method_1_out_method_2)
  validate(
    need(length(in_method_1_out_method_2)>0, "No features found!")
  )
  if(length(in_method_1_out_method_2)>0){
    col_x <- c(3,which(names(peaks) %in% in_method_1_out_method_2))
  }
  cdnaf <- peaks
  single_mode <- function(direction){
    x <- in_method_1_out_method_2
    if(length(x)>11){x <- x[c(1:11)]}
    origin <- melt(cdnaf[,c(3,which(names(cdnaf) %in% x))],id="class") %>% mutate(mark=ifelse(is.na(value),"imputed","observed"))
    d1 <-  melt(data1[,which(names(data1) %in% x),drop=F]) %>% mutate(imp_method=name1)
    d2 <- melt(data2[,which(names(data2) %in% x),drop=F]) %>% mutate(imp_method=name2)
    sig_intensity_plot <- rbind(cbind(origin[,c(1,4)],d1),cbind(origin[,c(1,4)],d2)) %>%
      filter(class == g1 | class == g2) %>%
      ggplot(aes(class,log(value),color=mark)) + 
      geom_point() +
      #facet_grid(cols = vars(variable),rows = vars(as.factor(imp_method))) +
      facet_grid(imp_method ~ variable) +
      theme_bw()+
      theme(legend.position="top")+
      labs(title=titleso)
  }
  return(single_mode(1))
}

clu_plot <- function(impd,n){

  row.names(impd) <- impd$sample
  imp.hc <- hclust(dist(scale(as.matrix(impd[,-c(1:5)]))))
  
  ord <- imp.hc$labels[imp.hc$order]
  clust_order <- impd[,c(1,3)]
  clust_order$nu <- factor(impd$class, levels = unique(impd$class), labels = c(1:length(unique(impd$class))))
  ords <- sapply(ord,function(x){which(clust_order$sample %in% x)})
  clust_order <- clust_order[ords,]
  p <- fviz_dend(imp.hc, 
            cex = 0.5, 
            main = n,
            label_cols=clust_order$nu

  )
  return(p)

}

pca_plot <- function(data,titlein){

  impdata <- as.matrix(data[,-c(1,2,3,4,5)])
  row.names(impdata) <- data$sample
  print(impdata[c(1:5),c(1:5)])
  diagnosis <- as.numeric(as.factor(data$class))
  #pca analysis
  imp.pr <- prcomp(impdata, scale = TRUE, center = TRUE)
  plotda <- cbind(as.data.frame(imp.pr$x[, c(1, 2)]),data$class)
  names(plotda)[3] <- "sgroup"
  ggplot(plotda,aes(PC1,PC2))+
    geom_point(aes(colour=sgroup))+
    stat_ellipse(aes(x=PC1, y=PC2,color=sgroup),type = "norm")+
    labs(title=titlein)
}


kNN <- function(datas){

  data <- datas[,-c(1,2,3,4,5)]
  data2 <- impute.knn(as.matrix(data))
  data3 <- data2$data
  data3[data3<0] <- 0.01
  data <- cbind(datas[,c(1,2,3,4,5)],data3)
  return(data)
}
pPCA <- function(datas){

  data <- datas[,-c(1,2,3,4,5)]
  row.names(data) <- datas$sample
  data2 <- t(data)
  pc <- pca(data2,nPcs=2,method="ppca")
  imputed <- completeObs(pc)
  imputed[imputed<0] <- rnorm_fixed(sum(imputed<0),mean(imputed),mean(imputed))
  data <- cbind(datas[,c(1,2,3,4,5)],as.data.frame(t(imputed)))
  return(data)
}
MissForestImp <- function(datas){
  data <- datas[,-c(1,2,3,4,5)]
  data2 <- missForest(data)$ximp
  data2[data2<0] <- 0.01
  data <- cbind(datas[,c(1,2,3,4,5)],data2)
  return(data)
}
bPCA <- function(datas){
  data <- datas[,-c(1,2,3,4,5)]
  row.names(data) <- datas$sample
  data2 <- t(data)
  pc <- pca(data2,nPcs=2,method="bpca")
  imputed <- completeObs(pc)
  imputed[imputed<0] <- 0.01
  data <- cbind(datas[,c(1,2,3,4,5)],as.data.frame(t(imputed)))
  return(data)
}
SVD <- function(datas){
  data <- datas[,-c(1,2,3,4,5)]
  row.names(data) <- datas$sample
  data2 <- t(data)
  pc <- pca(data2,nPcs=2,method="svdImpute")
  imputed <- completeObs(pc)
  imputed[imputed<0] <- 0.01
  data <- cbind(datas[,c(1,2,3,4,5)],as.data.frame(t(imputed)))
  return(data)
}
LLS <- function(datas){
  data <- datas[,-c(1,2,3,4,5)]
  data1 <- t(data)
  set.seed(5)
  result <- llsImpute(data1, k = 10, correlation="pearson", allVariables=TRUE)
  imputed <- completeObs(result)
  imputed[imputed<0] <- 0.01
  data2 <- cbind(datas[,c(1,2,3,4,5)],as.data.frame(t(imputed)))
  return(data2)
}
Mindet <- function(datas){

  data <- datas[,-c(1,2,3,4,5)]
  data2 <- impute.MinDet(t(data))
  data <- cbind(datas[,c(1,2,3,4,5)],t(data2))
  return(data)
}
logTransform=function(x){

  y <- x[,-c(1:5)]
  y[y<0] <- NA
  y <- impute.knn(as.matrix(y))$data
  
  min.val <- min(abs(y[y!=0 & !is.na(y)]))/10;
  y2 <- cbind(x[,c(1:5)],log2((y + sqrt(y^2 + min.val^2))/2))
  row.names(y2) <- y2$sample
  return(y2)
}
single_pca <- function(d,n){

  d <- d[which(d$class != "QC"),]

  row.names(d) <- d$sample
  l <- d[,c(1:5)]
  #---------------pca
  x2 <- d

  p <- logTransform(x2)[,-c(1:5)] #p[c(1:5),c(1:5)]
  
  row.names(p) <- x2$sample
  
  pcamodel <- PCA(p,scale.unit = FALSE,graph = FALSE)
  eig.val <- pcamodel$eig[,2]
  loadings <- pcamodel$var$coord[,c(1,2)]        
  scores <- pcamodel$ind$coord[,c(1,2)]      
  l$batch <- as.factor(l$batch)
  l$class <- as.factor(l$class)
  da <- scores %>%
    as.data.frame() %>%
    mutate(sample=row.names(scores)) %>%
    merge(l,by.x = "sample",by.y = "sample") %>%
    mutate(method=n)
  return(da)
}


