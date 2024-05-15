#' Plot the mdf created with the microshades package
#'
#' Creates a stacked abundance ggplot with color abundance organization
#'
#' @param mdf_group data.frame, melted data frame processed by microshades functions
#' @param cdf data.frame containing the color key
#' @param group_label string of smaller taxonomic group
#' @param x string x-axis element
#' @param y string y-axis element
#'
#' @import dplyr
#' @import ggplot2
#'
#' @return A \code{\link{ggplot}}2 plot.
#'
#' @export
#'
#' @examples
#' library(phyloseq)
#' data(GlobalPatterns)
#'
#' mdf <- prep_mdf(GlobalPatterns)
#'
#' color_obj <- create_color_dfs(mdf)
#'
#' mdf_group <- color_obj$mdf
#' cdf <- color_obj$cdf
#'
#' plot <- plot_microshades(mdf_group, cdf)

plot_microshades <- function (mdf_group,
                                cdf,
                                group_label = "Phylum Genus",
                                x = "Sample",
                                y = "Abundance")
{
  if (class(mdf_group) != "data.frame")
  {
    stop("mdf_group argument must be a data frame")
  }

  if( (sum(is.na(cdf$hex)) > 0) || (sum(is.na(cdf$group)) > 0) )
  {
    stop("cdf does not contain complete color information - missing hex or group info")
  }


  plot <- mdf_group %>%
    ggplot(aes(x = .data[[x]], y = .data[[y]]), fill = group_label) +
    scale_fill_manual(name = group_label,
                      values = cdf$hex,
                      breaks = cdf$group) +
    scale_colour_manual(name = group_label,
                        values = cdf$hex,
                        breaks = cdf$group) +
    geom_bar(aes(color=group, fill=group), stat="identity", position="stack") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  plot
}

#' Creates median and mean abundance barplots, and boxplots for microshades taxa contribution plots keeping
#' colors consistent in the microshades stacked barplots
#'
#' @param mdf melted data frame with microshades group processing
#' @param cdf color data frame
#' @param col_name name of column to subset from
#' @param data_match specific data to subset match
#' @param short_name alternate group name in both the mdf and cdf, note the group column still must exist
#'
#' @import dplyr
#' @import ggplot2
#'
#' @return list
#'   \itemize{
#'     \item{"box"}{ boxplot}
#'     \item{"mean"}{ barplot of mean abundance}
#'     \item{"median"}{barplot of median abundance}
#'   }
#'
#' @export
#'
#' @examples
#'
#' plots <- plot_contributions(mdf, cdf,  "dmm", 1, short_name = "group_short")
#'
#' plots$box
#' plots$mean
#' plots$median
#'
#' plots <- plot_contributions(mdf, cdf,  "sex", "female")
#'

plot_contributions <- function(mdf, cdf,  col_name, data_match, short_name = NULL) {

  cluster_subset <- mdf %>% filter(!!sym(col_name) == data_match)

  n_samples <- length(unique(cluster_subset$Sample))

  if(is.null(short_name)){

    # Aggregate the data, keep minimum info needed
    agg_subset <- cluster_subset  %>%
      ungroup() %>%
      select( Sample, Abundance,  group) %>%
      group_by(Sample,  group) %>%
      summarize(Abundance = sum(Abundance), .groups = "keep")

    cluster_subset_barplot <- agg_subset  %>%
      ungroup() %>%
      select( Abundance, group) %>%
      group_by(group) %>%
      summarize(mean_abundance = round(mean(Abundance),2), median_abundance = round(median(Abundance),2),  sd = sd(Abundance), .groups = "keep")

    # assign the short name to be group
    short_name <- "group"

  } else {
    # Aggregate the data, keep minimum info needed
    agg_subset <- cluster_subset  %>%
      ungroup() %>%
      select( Sample, Abundance,  group, !!sym(short_name)) %>%
      group_by(Sample,  group,  !!sym(short_name)) %>%
      summarize(Abundance = sum(Abundance), .groups = "keep")

    cluster_subset_barplot <- agg_subset  %>%
      ungroup() %>%
      select( Abundance, group, !!sym(short_name)) %>%
      group_by(group, !!sym(short_name)) %>%
      summarize(mean_abundance = mean(Abundance), median_abundance = median(Abundance), sd = sd(Abundance), .groups = "keep")

    group_levels <- cluster_subset_barplot %>% ungroup() %>% select(group, !!sym(short_name)) %>% unique() %>% arrange(group)

    cluster_subset_barplot[[short_name]] <- factor( cluster_subset_barplot[[short_name]], levels = group_levels[[short_name]])
  }

  # Boxplot
   top_drivers_boxplot <- ggplot(cluster_subset, aes(x = .data[[short_name]], y = .data[["Abundance"]], fill = .data[["group"]])) +
    geom_boxplot() +
    coord_flip() +
    ggtitle(paste(col_name, data_match, "(n =",n_samples,")")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = cdf$hex, breaks = cdf$group) +
    theme(legend.position = "none")

  # Mean Barplot
  top_drivers_mean_barplot <- ggplot(cluster_subset_barplot, aes(x = .data[[short_name]], y = .data[["mean_abundance"]], fill = .data[["group"]])) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ggtitle(paste(col_name, data_match, "(n =",n_samples,")")) +
    theme(plot.title = element_text(hjust = 0.5))  +
    scale_fill_manual(values = cdf$hex, breaks = cdf$group) +
    theme(legend.position = "none")

  # Median Barplot
  top_drivers_median_barplot <- ggplot(cluster_subset_barplot, aes(x = .data[[short_name]], y = .data[["mean_abundance"]], fill = .data[["group"]])) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ggtitle(paste(col_name, data_match, "(n =",n_samples,")")) +
    theme(plot.title = element_text(hjust = 0.5))  +
    scale_fill_manual(values = cdf$hex, breaks = cdf$group) +
    theme(legend.position = "none")

  list(
    box = top_drivers_boxplot,
    mean = top_drivers_mean_barplot,
    median = top_drivers_median_barplot
  )

}




