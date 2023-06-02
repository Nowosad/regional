sample_vals = function(raster, poly, sample_size){
  vals = terra::extract(raster, poly, ID = FALSE, raw = TRUE)
  if (sample_size < 1){
    vals = vals[sample(nrow(vals), size = max(sample_size * nrow(vals), 3), replace = TRUE), , drop = FALSE]
  } else if (sample_size > 1) {
    vals = vals[sample(nrow(vals), size = min(c(nrow(vals), sample_size))), , drop = FALSE]
  }
  return(vals)
}


sample_cells = function(raster, cell_id, sample_size){
  if (sample_size < 1){
    cell_id = cell_id[sample(length(cell_id), size = max(c(sample_size * length(cell_id), 3)), replace = FALSE)] #TRUE OR FALSE??
  } else if (sample_size > 1) {
    cell_id = cell_id[sample(length(cell_id), size = min(c(length(cell_id), sample_size)))]
  }
  vals = terra::extract(raster, cell_id)
  return(vals)
}


