universal_distance = function(x, dist_fun, ndim, ...){
  if (dist_fun %in% philentropy::getDistMethods()){
    di = philentropy::distance(x, method = dist_fun, test.na = FALSE,
                                     as.dist.obj = TRUE, mute.message = TRUE, ...)
  } else if (requireNamespace("proxy", quietly = TRUE) && dist_fun %in% names(summary(proxy::pr_DB)$names)) {
    di = as.vector(proxy::dist(x, method = dist_fun, auto_convert_data_frames = FALSE, ...))
  } else if (requireNamespace("dtwclust", quietly = TRUE) && dist_fun == "dtw"){
    di = dtw_dist(x, ndim = ndim, ...)
  }
  return(di)
}
dtw_dist = function(x, ndim, ...){
  x_list = split.data.frame(x, seq_len(nrow(x)))
  x_list = lapply(x_list, function(x, ndim) matrix(as.vector(x), ncol = ndim), ndim = ndim)
  proxy::dist(x_list, method = "dtw_basic", ...)
}

universal_dist_many_many = function(x, y, dist_fun, ndim, ...){
  if (dist_fun %in% philentropy::getDistMethods()){
    di = philentropy::dist_many_many(x, y,
                                     method = dist_fun,
                                     testNA = FALSE, ...)
  } else if (requireNamespace("proxy", quietly = TRUE) && dist_fun %in% names(summary(proxy::pr_DB)$names)) {
    di = proxy_dist_many_many(x, y, method = dist_fun, ...)
  } else if (requireNamespace("dtwclust", quietly = TRUE) && dist_fun == "dtw"){
    di = dtw_dist_many_many(x, y, ndim = ndim, ...)
  }
  return(di)
}

proxy_dist_many_many = function(x, y, method, ...){
  nrows_x = nrow(x)
  nrows_y = nrow(y)
  dist_mat = matrix(nrow = nrows_x, ncol = nrows_y)
  for (i in 1:nrows_x){
    for (j in 1:nrows_y){
      mat = rbind(x[i, ], y[j, ])
      dist_mat[i, j] =  as.vector(proxy::dist(mat, method = method,
                                          auto_convert_data_frames = FALSE, ...))
    }
  }
  return(dist_mat)
}
dtw_dist_many_many = function(x, y, ndim, ...){
  nrows_x = nrow(x)
  nrows_y = nrow(y)
  dist_mat = matrix(nrow = nrows_x, ncol = nrows_y)
  for (i in 1:nrows_x){
    for (j in 1:nrows_y){
      dist_mat[i, j] = dtw_multidim(x[i, ], y[j, ], ndim, ...)
    }
  }
  return(dist_mat)
}

# norm = "L2", step.pattern = dtw::symmetric2
dtw_multidim = function(x, y, ndim, ...){
  mat1 = matrix(unlist(x), ncol = ndim)
  mat2 = matrix(unlist(y), ncol = ndim)
  dtwclust::dtw_basic(mat1, mat2, error.check = FALSE, ...)
}
