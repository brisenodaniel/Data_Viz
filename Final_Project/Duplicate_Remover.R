



rmDuplicates <- function(data, nms) {
  dupes <- duplicated(data)
  dupes_idx <- which(dupes)
  data_nd <- data[-dupes_idx]
  nms_nd <- nms[-dupes_idx]
  return( list(data_nd, nms_nd) )
}


dupes <- duplicated(train)
dupes_idx <- which(dupes)
train[-dupes_idx,]


unique_m_nd <- 
