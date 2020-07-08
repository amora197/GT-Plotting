#' Converts the VCF file into a dataframe by extracting the required features for Genotype plotting.
#'
#' @param vcffile String specifying name of VCF file in .vcf or .vcf.gz format.
#'
#' @return Object which consists of two Data Frames the first one vcfdata contains the VCF DataFrame which consists of the features required
#'     for genotype plotting and the second dataframe consists of the length of the different Chromosomes in the VCF file.
#' @export
#'
#'
#'
VcfToTable<- function(vcffile){
  vcf<- read.vcfR(vcffile,verbose = FALSE, convertNA = FALSE)
  scanvcf<- scanVcf(vcffile)
  vcfr_fix<-data.frame(vcf@fix,stringsAsFactors=F)

  chrome_length<- as.data.frame(matrix(ncol = 2, nrow = length(scanvcf$`*:*-*`$rowRanges@seqnames@values)))
  colnames(chrome_length)<- c('chromosome','size')
  chrome_length$chromosome<- scanvcf$`*:*-*`$rowRanges@seqnames@values
  chrome_length$size<- scanvcf$`*:*-*`$rowRanges@seqnames@lengths

  vcf_dataframe <- as.data.frame(matrix(ncol=5+dim(scanvcf$`*:*-*`$GENO$GT)[2],nrow = dim(scanvcf$`*:*-*`$GENO$GT)[1]))
  always_there <- c("CHROM","POS","REF","ALT","QUAL")
  sample_names<- colnames(scanvcf$`*:*-*`$GENO$GT)
  colnames(vcf_dataframe)<- append(always_there,sample_names)
  vcf_dataframe$CHROM <- vcfr_fix$CHROM
  vcf_dataframe$POS<- vcfr_fix$POS
  vcf_dataframe$REF<- vcfr_fix$REF
  vcf_dataframe$ALT<- vcfr_fix$ALT
  vcf_dataframe$QUAL<- vcfr_fix$QUAL

  for (i in colnames(scanvcf$`*:*-*`$GENO$GT)){
    vcf_dataframe[,i]<- scanvcf$`*:*-*`$GENO$GT[,i]
  }
  #vcf_dataframe$GK715_M4<- scanvcf$`*:*-*`$GENO$GT[,sample_names[1]]
  #vcf_dataframe$Grinkan_CTRL<- scanvcf$`*:*-*`$GENO$GT[,sample_names[2]]
  return(list(vcfdata=vcf_dataframe,chromelen=chrome_length))
}
