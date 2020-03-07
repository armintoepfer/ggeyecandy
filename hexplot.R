library(data.table)
library(dplyr)
library(ggplot2)
library(hexbin)
library(Hmisc)

readdata = function(input) {
  x = fread(input, header=F)
  names(x) = c("length", "rq_float")
  x %>% mutate(rq_phred=ifelse(rq_float==1,ceiling(-10*log10(1/length)),round(-10*log10(1-rq_float))))
}

customhex = function(xvar, yvar, ybins, xlim = c(-1,51000), ylim = c(20,61)) {
  hex = hexbin(xvar, yvar, ybins)
  gghex = data.frame(hcell2xy(hex), count = hex@count,
                      xo = hex@xcm, yo = hex@ycm,
                      c = cut(hex@count, c(0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9), labels=0:8))

  ggplot(gghex) +
    geom_hex(aes(x = x, y = y, fill = c),
             stat = "identity")+
    scale_fill_manual(values=c("#FDE72544","#FDE725AA", "#8FD744FF", "#35B779FF", "#21908CFF", "#31688EFF", "#443A83FF", "#440154FF", "#25012e"),
                      labels = c("[0, 10)", "[10, 100)", "[100, 1K)", "[1K, 10K)", "[10K, 100K)", "[100K, 1M)", "[1M, 10M)", "[10M, 100M)", "[100M, 1B)"),
                      name = "Bin counts")+
    xlim(xlim)+
    ylim(ylim)+
    xlab("Insert size")+
    ylab("Predicted Accuracy")+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size=14), legend.position="right",
          axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"),
          legend.text = element_text(size=14), legend.title = element_text(size=14,face="bold"))
}
x = readdata("data/redwood.rqs")
customhex(x$length, x$rq_phred, 35, c(-1,65000))

filtered = x %>% filter(length <= 50000)
customhex(filtered$length, filtered$rq_phred, 35, c(-1,65000))
