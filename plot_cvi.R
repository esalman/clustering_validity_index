library(clusterCrit)
library(miceadds)
library(ggplot2)
library(reshape2)

num_clusters = 2:14
swc_window_lens = seq(20, 100, 20)
cvi_aswc = FALSE
criteria = getCriteriaNames(isInternal = TRUE)
criteria = criteria[-c(3, 8:24, 36, 38)]

if( cvi_aswc ) {
  outpath = '../results/ASWC/'
  results = load.Rdata("../results/cvi_internal_ASWC50.Rdata", 'results')
  gg_title = 'ASWC'
} else {
  outpath = '../results/SWC/'
  results = load.Rdata("../results/cvi_internal_SWC.Rdata", 'results')
  gg_title = 'SWC'
}
dir.create(outpath, showWarnings = FALSE)

for( i in 1:length(criteria) ) {
  df = data.frame(results[[i]], row.names = swc_window_lens)
  colnames(df) = num_clusters
  df1 = melt(df, value.name = 'cvi', variable.name = 'K')
  df1$w_len = swc_window_lens
  p = ggplot(data = df1, aes(x=K, y=cvi, group = factor(w_len))) +
    geom_line( aes(color = factor(w_len)) ) +
    geom_point( aes(color = factor(w_len)) ) +
    ggtitle( paste(criteria[i], 'index for', gg_title) )
  ggsave(paste(outpath, criteria[i], '.png', sep = ''))
  # print(p)
  # break
}

