#' This function creates a custom legend that is organized by same group classifications
#'
#' @param mdf data.frame melted dataframe
#' @param cdf data.frame color dataframe
#' @param group_level string of larger taxonomic group
#' @param subgroup_level string of smaller taxonomic group
#' @param legend_key_size numeric determines overall size of the legend keys
#' @param legend_text_size integer determines size of legend text
#'
#' @import dplyr
#' @import ggplot2
#' @import cowplot
#'
#' @return complete_legend ggplot
#' @export
#'
#' @examples
#' legend_new <- custom_legend(mdf_GP, cdf_GP)
#'
#'
custom_legend <- function (mdf, cdf, group_level = "Phylum", subgroup_level = "Genus", legend_key_size = 0.4, legend_text_size = 10)
{
  col_name_group <- paste0("Top_", group_level)

  group_level_names <- unique(cdf[[col_name_group]])

  for (i in 1:length(group_level_names))
  {
    if( i == 1)
    {
      complete_legend <-individual_legend (mdf, cdf, group_level_names[i], group_level, subgroup_level, legend_key_size, legend_text_size)
    }
    else
    {
      new_legend <-individual_legend (mdf, cdf, group_level_names[i], group_level, subgroup_level, legend_key_size, legend_text_size)

      complete_height <- i -1
      new_height <- 1

      complete_legend <-plot_grid(complete_legend, new_legend, ncol = 1, rel_heights = c(complete_height,new_height))
    }
  }
  complete_legend
}


#' This function creates a custom legend that is organized by same group classifications
#'
#' @param mdf data.frame melted dataframe
#' @param cdf data.frame color dataframe
#' @param group_name name of the larger taxonomic group to extract small legend from
#' @param group_level string of larger taxonomic group
#' @param subgroup_level string of smaller taxonomic group
#' @param legend_key_size numeric determines overall size of the legend keys
#' @param legend_text_size integer determines size of legend text
#'
#' @import dplyr
#' @import ggplot2
#' @import cowplot
#'
#' @return legend ggplot
#' @export
#'
#'
#'
individual_legend <- function (mdf, cdf, group_name, group_level = "Phylum", subgroup_level = "Genus", legend_key_size = 0.4, legend_text_size = 10)
{
  col_name_group <- paste0("Top_", group_level)
  col_name_subgroup <- paste0("Top_", subgroup_level)

  select_mdf <- mdf %>% filter(!!sym(col_name_group) == group_name)
  select_cdf <- cdf %>% filter(!!sym(col_name_group) == group_name)

  select_plot <- ggplot(select_mdf, aes(x = Sample, y = Abundance)) +
    aes_string(fill = col_name_subgroup, text = col_name_subgroup) +
    geom_col( position="fill") +
    scale_fill_manual(name = group_name,
                      values = select_cdf$hex,
                      breaks = select_cdf[[col_name_subgroup]]) +
    theme(legend.justification = "left") +
    theme(legend.title = element_text(face = "bold")) +
    theme(legend.key.size = unit(legend_key_size, "lines"), text=element_text(size=legend_text_size))

  legend <- get_legend(select_plot)
}
