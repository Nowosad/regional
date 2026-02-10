# Compute pairwise distances within a single matrix; returns a distance vector.
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
# Compute DTW distances for a matrix; returns a distance vector.
dtw_dist = function(x, ndim, ...){
  x_list = split.data.frame(x, seq_len(nrow(x)))
  x_list = lapply(x_list, function(x, ndim) matrix(as.vector(x), ncol = ndim), ndim = ndim)
  proxy::dist(x_list, method = "dtw_basic", ...)
}

# Compute distances between rows of two matrices; returns a distance matrix.
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

# Compute proxy distances between rows of two matrices; returns a distance matrix.
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
# Compute DTW distances between rows of two matrices; returns a distance matrix.
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

# Compute DTW distance between two multi-dimensional vectors; returns a scalar.
dtw_multidim = function(x, y, ndim, ...){
  mat1 = matrix(unlist(x), ncol = ndim)
  mat2 = matrix(unlist(y), ncol = ndim)
  dtwclust::dtw_basic(mat1, mat2, error.check = FALSE, ...)
}

# Build a region-value accessor using cached or on-demand extraction; returns a function(i) -> matrix.
get_region_values = function(v, raster, optimize_for) {
  if (optimize_for == "speed") {
    n_regions = length(v)
    vals_list = vector(mode = "list", length = n_regions)
    for (i in seq_len(n_regions)) {
      vals_list[[i]] = terra::extract(raster, v[i], ID = FALSE, raw = TRUE)
    }
    function(i) vals_list[[i]]
  } else {
    function(i) terra::extract(raster, v[i], ID = FALSE, raw = TRUE)
  }
}

# Sample rows from a region's value matrix; returns a (possibly sampled) matrix.
sample_region_values = function(vals, sample_size, min_size = 0, replace = FALSE) {
  if (sample_size < 1) {
    size = max(sample_size * nrow(vals), min_size)
    vals = vals[sample(nrow(vals), size = size, replace = replace), , drop = FALSE]
  } else if (sample_size > 1) {
    vals = vals[sample(nrow(vals), size = min(c(nrow(vals), sample_size))), , drop = FALSE]
  }
  vals
}
