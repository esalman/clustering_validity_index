tic = Sys.time()
library(R.matlab)
library(clusterCrit)
library(pbmcapply)

input_path = '/export/mialab/users/fespinoza/Clustering/codes/simulated_data/ASWC/'
outpath = '../results/'
criteria = getCriteriaNames(isInternal = TRUE)
# remove C_index, Gamma, G_plus, GDI**, S_Dbw and Tau. takes 10 minutes to compute and GDI**
# migth be redundant
criteria = criteria[-c(3, 8:24, 36, 38)]
# criteria = criteria[c(1:2,4)]
n_centroids = 2:14
swc_idx = 1
mc_cores = 70
swc_window_lens = seq(20, 100, 20)
cvi_aswc = FALSE # if false cluster SWC

# load previous window result
if ( cvi_aswc ) {
  output_file = paste(outpath, 'cvi_internal_ASWC50.Rdata', sep = '')
} else {
  output_file = paste(outpath, 'cvi_internal_SWC.Rdata', sep = '')
}
if( file.exists( output_file ) ) {
  load( output_file )
} else {
  results = list()
  for( j in criteria ) {
    results[[j]] = matrix(nrow = 5, ncol = 13)
  }
}

for( swc_idx in 1:length(swc_window_lens) ) {
  swc_window_len = swc_window_lens[swc_idx]
  
  # load mat file
  dat = readMat(paste(input_path, 'Subjects_SWC', swc_window_len ,'_ASWC50.mat', sep = ''))

  if ( cvi_aswc ) {
    ASWC = data.frame()
    for ( i in 201:400 ) {
      ASWC = rbind(ASWC, dat$SASWC[[i]][[1]])
    }
    X = data.matrix(ASWC)
    c_labels = readMat(paste(input_path, 'SWC', swc_window_len ,'_ASWC50_Kmeans_NumClu1_14_CorrDist.mat', sep = ''))
    print( paste('ASWC CVI for window length', swc_window_len) )
  } else {
    SWC = data.frame()
    for ( i in 1:200 ) {
      SWC = rbind(SWC, dat$SASWC[[i]][[1]])
    }
    X = data.matrix(SWC)
    c_labels = readMat(paste(input_path, 'SWC', swc_window_len ,'_Kmeans_NumClu1_14_CorrDist.mat', sep = ''))
    print( paste('SWC CVI for window length', swc_window_len) )
  }
  grid_ = expand.grid(n_centroids, criteria)
  idx_vals = pbmcmapply(function(x, y){
    idx_val = intCriteria(X, as.integer(c_labels[[1]][[x]][[1]]), y)
    return(idx_val)
  }, grid_$Var1, grid_$Var2, mc.cores=mc_cores)

  for( i in 1:nrow(grid_) ) {
    results[[ grid_[i,2] ]][swc_idx, grid_[i,1]-1] = as.numeric(idx_vals[i])
  }
}

save(results, file = output_file)

toc = Sys.time()
toc-tic
