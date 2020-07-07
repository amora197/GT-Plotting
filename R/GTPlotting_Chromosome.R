#' Generate the genotype plots for each chromosome in seprate files.
#'
#' @param vcfTablefile Dataframe which contains the important features extracted from a vcf file using VcfToTable function.
#' @param chrome_length Dataframe which contains the size of each chromosome which is the output of the VcfToTable function.
#' @param ctrl_names String which gives the name of the Controlled sample should be the same as in the vcf file.
#'
#' @return genotype plots for each chromosome
#' @export
#'
#'
#'
GTPlotting_Chromosome<- function(vcfTablefile,chrome_length,ctrl_names){
  length_of_samples <- length(colnames(vcfTablefile))
  colnames_x<- colnames(vcfTablefile)
  print(class(colnames_x))
  #colnames_x[,1]<-colnames(colnames_x)
  #colnames_x <- droplevels(colnames_x)
  o=1

  for (i in colnames_x[6:length_of_samples]){
    print(i)
    #tp=paste("chrome_type_",o)
    chrome_length[,paste("chrome_type_",o)] <- i
    o=o+1

  }
  print(chrome_length)
  x <- colnames(chrome_length)
  print(x[length(x)])
  test <- paste(x[3])
  print(x[3])
  test_1 <- paste(x[length(x)])
  print(test_1)
  sbicolor_length<-gather(chrome_length,chrome_type,chrome_name,paste(x[3]):paste(x[length(x)]),factor_key = TRUE)
  sbicolor_length$size <- as.integer(as.character(sbicolor_length$size))

  all_colnames <- colnames(vcfTablefile)
  always_there <- c("CHROM","POS","REF","ALT","QUAL")
  new_ctrl <- append(always_there,ctrl_names)
  mutant_names <- setdiff(all_colnames,new_ctrl)
  print(mutant_names)
  print(all_colnames)
  df <- data.frame()
  for (i in mutant_names){
    name_call <- paste(i,sep = "")
    print(name_call)
    #print(paste("free_bayes$", i,sep = ""))
    #print(gatk_testing[gatk_testing[,i]=="1/1",])
    df <-rbind(df,vcfTablefile[vcfTablefile[,i]=="1/1",])
  }
  df$POS <- as.integer(as.character(df$POS))
  final_data <- df[!duplicated(df$POS),]
  final_data <- final_data[order(final_data$POS),]
  final_data <- final_data[order(final_data$CHROM),]

  for (i in unique(sbicolor_length$chromosome))
  {
    x<-sbicolor_length[sbicolor_length$chromosome==i,]
    p_data<- as.data.frame(final_data[final_data$CHROM==i,])
    print(colnames(x))
    y <- colnames(p_data)
    p_data <- droplevels(p_data)
    p_data1<- gather(p_data,chrome_type,GT,paste(y[6]):paste(y[length(y)]),factor_key=TRUE)
    print(colnames(p_data1))
    p_data1$POS <- as.integer(as.character(p_data1$POS))
    p_data1<- p_data1[p_data1$GT=="0/0" | p_data1$GT=="1/1" | p_data1$GT=="0/1" | p_data1$GT==".",]
    print(x)
    p_data1$chrome_type <- as.character(p_data1$chrome_type)
    file_name <- paste("Chromosome_",i,".png")
    #jpeg(file_name)
    p1 <- ggplot(data = x,aes(x=chrome_name,y=size))+geom_bar(stat = "identity",width = 0.3)+coord_flip()+ geom_segment(data = p_data1,mapping = aes(x=as.integer(factor(chrome_type))+0.1,y=POS,xend=as.integer(factor(chrome_type))-0.1,yend=POS,color=factor(GT)))
    final_plot <- p1 +scale_color_manual(values = c("black","blue","green","yellow"))+ggtitle(file_name)+labs(x="Samples",y="Size",colour="GT")
    #dev.off()
    #ggsave(file_name)
    show(final_plot)

  }
}
