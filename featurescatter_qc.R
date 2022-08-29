FeatureScatter_qc <- function(sob){
  p1 <- FeatureScatter(sob, feature1 = 'nCount_RNA', feature2 = 'nFeature_RNA')
  p2 <- FeatureScatter(sob, feature1 = 'nCount_RNA', feature2 = 'percent.mt')
  p3 <- FeatureScatter(sob, feature1 = 'nFeature_RNA', feature2 = 'percent.mt')
  p1 + p2 + p3
}