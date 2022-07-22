get_genelist <- function(genes, org = "org.Hs.eg.db", fc_df, ora = TRUE){
  require(clusterProfiler)
  ifelse(org == "org.Hs.eg.db", require(org.Hs.eg.db), require(org.Mm.eg.db))
  require(dplyr)
  fc <- ifelse('avg_log2FC' %in% names(fc_df), 'avg_log2FC', 'log2FoldChange')
  
  df <- genes %>% 
    bitr('SYMBOL', 'ENTREZID', org) %>% 
    merge(fc_df, by.x = 'SYMBOL', by.y = 'gene') %>% 
    arrange(., desc(.[fc]))
  
  if (ora) {df[['ENTREZID']]} else
    dplyr::select(df, 'ENTREZID', fc) %>% 
    tibble::deframe()
}
# parameters
# genes: character, the genes for enrichment analysis.
# org: character, the database to convert genes, should be "org.Hs.eg.db" or other characters(will set to org.Mm.eg.db).
# fc_df: data.frame, differential expression results, log2foldchange should named as "avg_log2FC", genes should named as "gene".
# ora: if for over representation analysis, will return a entrezid sequence, else for gene set enrichment analysis, will return a avg_log2FC named entrezid sequence.
