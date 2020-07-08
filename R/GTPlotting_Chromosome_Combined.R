#' Generate the genotype plots for all the chromosomes in single plot.
#'
#' @param vcfTablefile Dataframe which contains the important features extracted from a vcf file using VcfToTable function.
#' @param chrome_length Dataframe which contains the size of each chromosome which is the output of the VcfToTable function.
#'
#' @return genotype plot for all the chromosome
#' @export
#'
#'
#'
#'
GTPlotting_Chromosome_Combined<-function(vcfTablefile,chrome_length)
{
  length_of_samples <- length(colnames(vcfTablefile))
  colnames_x<- colnames(vcfTablefile)
  print(class(colnames_x))
  o=1
  for (i in colnames_x[6:length_of_samples]){
    #print(i)
    #tp=paste("chrome_type_",o)
    chrome_length[,paste("chrome_type_",o)] <- i
    o=o+1

  }
  #print(colnames(vcfTablefile))
  #print(vcfTablefile[1:3,])
  #print(chrome_length)
  x <- colnames(chrome_length)
  #print(x[length(x)])
  test <- paste(x[3])
  #print(x[3])
  test_1 <- paste(x[length(x)])
  #print(test_1)
  y <- colnames(vcfTablefile)
  sbicolor_length<-gather(chrome_length,chrome_type,chrome_name,paste(x[3]):paste(x[length(x)]),factor_key = TRUE)
  p_data1<- gather(vcfTablefile,chrome_type,GT,paste(y[6]):paste(y[length(y)]),factor_key=TRUE)
  #print(colnames(p_data1))
  p_data1$POS <- as.integer(as.character(p_data1$POS))
  p_data1<- p_data1[p_data1$GT=="0/0" | p_data1$GT=="1/1",]
  p_data1$chrome_unique <- paste(p_data1$CHROM,"_",p_data1$chrome_type)
  sbicolor_length$chrome_unique <- paste(sbicolor_length$chromosome,"_",sbicolor_length$chrome_name)
  sbicolor_length$size <- as.integer(as.character(sbicolor_length$size))
  #print(x)
  file_name <- paste("Chromosome_",i,".png")
  #jpeg(file_name)
  p1 <- ggplot(data = sbicolor_length,aes(x=chrome_unique,y=size))+geom_bar(stat = "identity",width = 0.5)+coord_flip()+ geom_segment(data = p_data1,mapping = aes(x=as.integer(factor(chrome_unique))-0.1,y=POS,xend=as.integer(factor(chrome_unique))+0.1,yend=POS,color=factor(GT)))
  final_plot <- p1 +scale_color_manual(values = c("blue","green","yellow"))+ggtitle(file_name)+labs(x="Samples",y="Size",colour="GT")
  #dev.off()
  #ggsave(file_name)
  show(final_plot)

}
