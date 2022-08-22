dotplot_enrichresult <- function(object, pval = NULL, desc_item = NULL, head_num = NULL, go_ontology = 'ALL'){
  require(ggplot2)
  er <- object
  pval <- pval %||% 0.05
  desc_item <- desc_item %||% 'Count'
  head_num <- head_num %||% 5
  
  if (er@ontology == 'KEGG') {
    plot_df <- as.data.frame(er@result) %>% 
      filter(pvalue < pval) %>% 
      arrange(desc(.[desc_item])) %>% 
      head(n = head_num)
  } else {
    if (go_ontology == 'ALL') {
      plot_df <- as.data.frame(er@result) %>% 
        filter(pvalue < pval) %>% 
        arrange(desc(.[desc_item])) %>% 
        head(n = head_num)
    } else {
      plot_df <- as.data.frame(er@result) %>% 
        filter(ONTOLOGY == go_ontology) %>% 
        filter(pvalue < pval) %>% 
        arrange(desc(.[desc_item])) %>% 
        head(n = head_num)
    }
  }
  
  plot_df$Description <- factor(plot_df$Description, levels = rev(plot_df$Description))
  
  ggplot(plot_df) +
    geom_point(aes_string(desc_item, 'Description', size = desc_item, color = 'pvalue')) +
    scale_color_continuous(low="red", high="blue", name = 'pvalue',
                           guide=guide_colorbar(reverse=TRUE)) +
    theme_bw(base_size = 20, base_family = 'arial') + 
    theme(axis.text = element_text(family = 'arial', face = 'bold', colour = 'black'))
}
