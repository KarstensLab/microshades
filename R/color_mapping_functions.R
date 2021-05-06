#' Prepare melted phyloseq object to map colors to
#'
#' This function prepares a melted phyloseq object to map colors to top phyla
#' and genera.
#'
#' Notes:
#'
#' - This normalizes the phyloseq object to relative abundance
#' - This agglomerates to the smaller taxonomic group `subgroup_level`
#'
#' @param ps phyloseq-class object
#' @param subgroup_level string of smaller taxonomic group
#'
#' @import phyloseq
#' @import dplyr
#' @importFrom speedyseq tax_glom psmelt
#'
#' @return data.frame, a melted phyloseq object from `psmelt()`
#' @export
#'
#' @examples
#' phyloseq::data(GlobalPatterns)
#'
#' # Use defaults
#' mdf <- prep_mdf(GlobalPatterns)
#'
#' # Subgroup as "Family"
#' mdf_fam <- prep_mdf(GlobalPatterns, subgroup_level = "Family")
prep_mdf <- function(ps,
                     subgroup_level = "Genus")
    {
    # Agglomerate, normalizes, and melts phyloseq object -----

    if (!(subgroup_level %in% colnames(ps@tax_table))) {
      stop("'subgroup_level' does not exist")
    }

    mdf <- ps %>%
        tax_glom(subgroup_level) %>%
        transform_sample_counts(function(x) { x/sum(x) }) %>%
        psmelt()

    # Removes 0 abundance
    mdf_prep <- mdf[mdf$Abundance > 0, ]


    # Return melted and prepped data frame -----
    return(mdf_prep)
}

#' Return default hex colors
#'
#' Returns a built-in data frame with hex code.
#'
#' @param n_groups integer of number of selected groups
#' @param cvd logical Color Vision Deficent Friendly palette useage
#'
#' @import dplyr
#' @return data.frame
#' @keywords internal
#'
#' @examples
#' # Get hex codes for 5 groups
#' hex_df <- default_hex()
#'
#' # Get hex codes for 3 groups with CVD palette
#' hex_df <- default_hex(3, TRUE)
default_hex <- function(n_groups = 5, cvd = FALSE) {
    # Hex is ordered light to dark
    # Numbers in name refer to the bottom up order in color stack

    if (cvd)
      {
        hex_df <- data.frame(sapply(microshades_cvd_palettes, c))
      } else {
        hex_df <- data.frame(sapply(microshades_palettes, c))
    }

    # Data frame of colors for the taxa, these are concatenated by column.
    # Higher row number = less abundant = lighter color

    hex_df <- hex_df %>% arrange(desc(row_number()))

    # Remove columns that aren't necessary if asked by counting number of
    # columns to remove then setting those columns to NULL, effectively removing
    # the column.
    if(!(ncol(hex_df) == n_groups ))
    {
      num_rm_cols <- ncol(hex_df) - n_groups + 1
      hex_df[, 2:num_rm_cols] <- NULL
    }
    # Return hex codes data frame
    hex_df
}


#' Generates abundance sorted data frame and a color palette data frame
#'
#' Use `create_color_dfs()` to create a specialized data frames containing color
#' and abundance organized microbiome data.
#'
#'
#' The top group categories are user specified through the `selected_groups` parameter,
#' and top subgroup categories are dynamically generated based on abundance.
#' For the top group, the categories not in `selected_groups` will be changed to
#' "Other". The `top_n_subgroups` will be determined for each selected group.
#'
#'  Notes:
#'
#' - In SILVA 138, some phylum names are different and you should consider
#'   passing in the vector
#'   `c("Proteobacteria", "Actinobacteriota", "Bacteroidota", "Firmicutes")`
#'
#' @param mdf data.frame, melted phyloseq data frame containing microbiome data
#' @param selected_groups list of groups in group_level taxomomy to specify and color in plot.
#'   The vector order is the stacking order. "Other" is always on the top of the stack,
#'   but then the rest will follow. The default is "Proteobacteria", "Actinobacteria",
#'   "Bacteroidetes", "Firmicutes". "Firmicutes" is on the bottom of the stack.
#' @param top_n_subgroups integer number of top subgroups to show for each selected group
#'   the max is 4 top subgroups
#' @param group_level string of larger taxonomic group
#' @param subgroup_level string of smaller taxonomic group
#' @param cvd logical, determines which palette to use,
#' color vision deficent (microshades_cvd_palettes) or microshades_palettes
#' @param top_orientation logical most abundant shades oriented at the top of the stack
#'   otherwise, most abundant shades are bottom oriented
#'
#' @import phyloseq
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import forcats
#' @import tidyselect
#'
#' @return list
#'   \itemize{
#'     \item{"mdf"}{ melted data frame to pass into ggplot2}
#'     \item{"cdf"}{ mapping to be used in manual color filling}
#'   }
#'
#' @export
#'
#' @examples
#' colorframe <- create_color_dfs(mdf)
#' colorframe <- create_color_dfs(mdf, selected_groups = c("Proteobacteria", "Actinobacteriota", "Bacteroidota", "Firmicutes"))
#' colorframe <- create_color_dfs(mdf, cvd = TRUE)
create_color_dfs <- function(mdf,
                             selected_groups = c("Proteobacteria",
                                                 "Actinobacteria",
                                                 "Bacteroidetes",
                                                 "Firmicutes"),
                             top_n_subgroups = 4,
                             group_level = "Phylum",
                             subgroup_level = "Genus",
                             cvd = FALSE,
                             top_orientation = FALSE)
    {
    # Throws error if too many subgroups
    if (top_n_subgroups > 4) {
        stop("'top_n_subgroups' exceeds MAX value 4")
    }

   if (class(mdf) == "phyloseq")
   {
       stop("mdf argument must be a data frame. Melt the 'phyloseq' object.")
   }
    if (!is.null(mdf$group)) {
        stop("'group' column name already exists; consider renaming or removing")
    }

    if (is.null(mdf[[group_level]])) {
       stop("'group_level' does not exist")
    }

    if (is.null(mdf[[subgroup_level]])) {
       stop("'subgroup_level' does not exist")
    }


    # Create new column for group level -----
    # Add "Other" category immediately
    col_name_group <- paste0("Top_", group_level)
    mdf[[col_name_group]] <- "Other"

    # Index and find rows containing the selected groups
    rows_to_change <- mdf[[group_level]] %in% selected_groups
    taxa_names_mdf <- row.names(mdf[rows_to_change, ])
    mdf[taxa_names_mdf, col_name_group] <-
        as.character(mdf[taxa_names_mdf, group_level])

    # Create factor for the group level column
    mdf[[col_name_group]] <- factor(mdf[[col_name_group]],
                                    levels = c("Other", selected_groups))

    # Check to make sure the selected_groups specified all exist in the dataset
    if(sum (selected_groups %in% as.character(unique(mdf[[col_name_group]]))) != length(selected_groups))
    {
      stop("some 'selected_groups' do not exist in the dataset. Consider SILVA 138 c('Proteobacteria', 'Actinobacteriota', 'Bacteroidota', 'Firmicutes')")
    }

    # Rename missing genera
    mdf_unknown_subgroup <- mdf %>%
        mutate(!!sym (subgroup_level) := fct_explicit_na(!!sym(subgroup_level), "Unknown"))

    # Rank group-subgroup categories by ranked abundance and add order
    # Ranked abundance aggregated using sum() function
    col_name_subgroup <- paste0("Top_", subgroup_level)
    subgroup_ranks <- mdf_unknown_subgroup %>%
        group_by_at(all_of(vars(subgroup_level, col_name_group))) %>%
        summarise(rank_abundance = sum(Abundance)) %>%
        arrange(desc(rank_abundance)) %>%
        group_by_at(vars(col_name_group)) %>%
        mutate(order = row_number()) %>%
        ungroup()

    # Correctly keep "Other" for lower abundant genera
    # Pseudocode:
    # - set all (top) subgroups to "Other"
    # - change subgroups back to actual subgroups (e.g., Genus) if it is in the
    #   top N number of subgroups passed into `top_n_subgroups` (e.g., 4)
    subgroup_ranks[[col_name_subgroup]] <- "Other"
    rows_to_change <- subgroup_ranks$order <= top_n_subgroups
    subgroup_ranks[rows_to_change, col_name_subgroup] <-
        as.vector(subgroup_ranks[rows_to_change, subgroup_level])

    # Generate group-subgroup categories -----
    # There are `top_n_subgroups` additional groups because each group level has
    # an additional subgroup of "Other"
    # E.g., 4 selected_groups + 1 Other, 4 top_n_groups + 1 Other => 25 groups
    group_info <- subgroup_ranks %>%
        mutate(group = paste(!!sym(col_name_group),
                             !!sym(col_name_subgroup),
                             sep = "-"))

    # Ensure that the "Other" subgroup is always the lightest shade
    group_info$order[group_info[[col_name_subgroup]] == "Other"] <- top_n_subgroups +1

    # Merge group info back to melted phyloseq -----
    # Get relevant columns from data frame with group info
    group_info_to_merge <-
        group_info[, c(col_name_group, subgroup_level,
                       col_name_subgroup, "group")]
    mdf_group <- mdf_unknown_subgroup %>%
        left_join(group_info_to_merge, by = c(col_name_group, subgroup_level))

    # Get beginning of color data frame with top groups/subgroups
    # E.g., 4 selected_groups + 1 Other, 4 top_n_groups + 1 Other => 25 groups
    prep_cdf <- group_info %>%
        select(c("group", "order", col_name_group, col_name_subgroup)) %>%
        filter(order <= top_n_subgroups + 1) %>%  # "+ 1" for other subgroup
        arrange(!!sym(col_name_group), order)

    # Prepare hex colors -----

    # Generates default 5 row x 6 cols of 5 colors for 6 phylum categories
    # Parameter for number of selected phylum
    # "+ 1" is for "Other" group
    num_group_colors <- length(selected_groups) + 1

    hex_df <- default_hex(num_group_colors, cvd)

    # Add hex codes in ranked way
    # creates nested data frame
    # https://tidyr.tidyverse.org/articles/nest.html
    # https://tidyr.tidyverse.org/reference/nest.html
    cdf <- prep_cdf %>%
        group_by_at(all_of(vars(col_name_group))) %>%
        tidyr::nest() %>%
        arrange(!!sym(col_name_group))

    # Loop through top group and add colors by nested data frame
    # Higher row number = less abundant = lighter color

    if ("Other" %in% mdf[[col_name_group]])
    {
      start <- 1
    } else
    {
      start <- 2
      num_group_colors <- num_group_colors -1
    }

    for (i in 1:num_group_colors) {
      cdf$data[[i]]$hex <- hex_df[1:length(cdf$data[[i]]$group),start]
      start = start + 1
    }

    # Unnest colors and groups and polish for output
    cdf <- cdf %>%
        ungroup() %>%
        arrange(desc(row_number())) %>%
        tidyr::unnest(data) %>%
        select(!!sym(col_name_group),
               !!sym(col_name_subgroup),
               group, hex) %>%
        mutate_all(as.character)  # Remove factor from hex codes

    cdf <- cdf %>% filter( !is.na(hex))

    if (top_orientation)
    {
      level_assign = unique(cdf$group)
    }
    else
    {
      level_assign = unique(rev(cdf$group))
    }

    mdf_group$group <- factor(mdf_group$group, levels = level_assign)

    # Return final objects -----
    list(
        mdf = mdf_group,
        cdf = cdf
    )
}


#' Apply the color factoring from one cdf to a different melted phyloseq data frame
#'
#' Now both melted dataframes will contain the same color mapping information.
#' This can be useful if you want to make sure that different graphs have consistent legends
#'
#' @param mdf data.frame, melted data frame to apply legend to
#' @param mdf_group data.frame, melted data frame to use legend from
#' @param group_level string of larger taxonomic group
#' @param subgroup_level string of smaller taxonomic group
#'
#' @import phyloseq
#' @import dplyr
#' @import tidyr
#' @import forcats
#' @import tidyselect
#'
#' @return data.frame, melted df with ordered factor that are consistent with the legend
#'
#' @export
#'
#' @examples
#' mdf_to_plot <- match_cdf(mdf, mdf_group)
#'

match_cdf <- function(mdf,
                            mdf_group,
                            group_level = "Phylum",
                            subgroup_level = "Genus"
                            )

    {

      if (class(mdf) == "phyloseq")
      {
        stop("mdf argument must be a data frame. Melt the 'phyloseq' object.")
      }
      if (!is.null(mdf$group)) {
        stop("mdf 'group' column name already exists; consider renaming or removing")
      }

      if (is.null(mdf[[group_level]])) {
        stop("mdf 'group_level' does not exist")
      }

      if (is.null(mdf[[subgroup_level]])) {
        stop("mdf 'subgroup_level' does not exist")
      }

      if (is.null(mdf_group$group)) {
        stop("mdf_group 'group' column is missing")
      }

      # Create new column for group level -----
      # Add "Other" category immediately
      col_name_group <- paste0("Top_", group_level)
      mdf[[col_name_group]] <- "Other"

      selected_groups <- levels(mdf_group[[col_name_group]])
      selected_groups <- selected_groups[selected_groups != "Other"]

      # Index and find rows to change
      rows_to_change <- mdf[[group_level]] %in% selected_groups
      taxa_names_mdf <- row.names(mdf[rows_to_change, ])
      mdf[taxa_names_mdf, col_name_group] <-
        as.character(mdf[taxa_names_mdf, group_level])

      mdf[[col_name_group]] <- factor(mdf[[col_name_group]],
                                      levels = c("Other", selected_groups))

      # Add Top subgroup_level column
      col_name_subgroup <- paste0("Top_", subgroup_level)
      mdf[[col_name_subgroup]] <- "Other"

      selected_subgroups <- unique(mdf_group[[col_name_subgroup]])
      selected_subgroups <- selected_subgroups[selected_subgroups != "Other"]

      # Index and find rows to change
      rows_to_change <- mdf[[subgroup_level]] %in% selected_subgroups
      taxa_names_mdf <- row.names(mdf[rows_to_change, ])
      mdf[taxa_names_mdf, col_name_subgroup] <-
       as.character(mdf[taxa_names_mdf, subgroup_level])

      mdf <- mdf %>% mutate(group = paste(!!sym(col_name_group),
                                         !!sym(col_name_subgroup),
                                         sep = "-"))

      new_levels <- levels(mdf_group$group)

      mdf$group <- factor(mdf$group, levels = new_levels)

      mdf
  }

#' Reorder the samples or stacked group levels by abundance
#'
#' This function will reorder the user selected subgroup taxa based on abundance, and can also
#' reorder the stacked groups levels based on abundance, using `sink_abundant_groups`
#'
#' @param mdf_group data.frame, melted phyloseq data frame
#' @param cdf data.frame containing the color key
#' @param order string of subgroup to reorder by
#' @param group_level string of larger taxonomic group
#' @param subgroup_level string of smaller taxonomic group
#' @param sink_abundant_groups logical reorder the phylum groups so the most abundant is the bottom group
#'
#' @import dplyr
#' @import forcats
#' @import tidyselect
#'
#' @return list
#'   \itemize{
#'     \item{"mdf"}{ reordered melted data frame, ready for plotting}
#'     \item{"cdf"}{ reordered manual color filling according to new order}
#'   }
#'
#' @export
#'
#' @examples
#' mdf_to_plot <- reorder_samples_by(mdf_group)
reorder_samples_by <- function (mdf_group,
                                cdf,
                                order = "NA",
                                group_level = "Phylum",
                                subgroup_level = "Genus",
                                sink_abundant_groups = TRUE)
{
  if (class(mdf_group) == "phyloseq")
  {
    stop("mdf_group argument must be a data frame. Melt the 'phyloseq' object.")
  }

  if (is.null(mdf_group$Sample)) {
    stop("mdf_group 'Sample' column does not exist in the data")
  }

  if (is.null(mdf_group[[subgroup_level]])) {
    stop("mdf_group 'subgroup_level' does not exist")
  }

  if (is.null(mdf_group[[group_level]])) {
    stop("mdf_group 'group_level' does not exist")
  }

  col_name_group <- paste0("Top_", group_level)
  col_name_subgroup <- paste0("Top_", subgroup_level)

  if (order %in% as.character(unique(mdf_group[[col_name_subgroup]])))
  {
    col_name_order <- col_name_subgroup
  }
  else if (order %in% as.character(unique(mdf_group[[col_name_group]])))
  {
    col_name_order <- col_name_group
  }
  else if (order == "NA")
  {
    col_name_order <- NULL
  }
  else
  {
    stop("variable 'order' does not exist in the dataset")
  }

  if( sink_abundant_groups)
  {
    # reorder Top group
    reorder_groups <- mdf_group %>% group_by(!!sym(col_name_group))  %>%
      filter(!!sym(col_name_group) != "Other") %>%
      dplyr::summarise(rank_abundance = sum(Abundance))

    top_group_order <- reorder_groups[order(reorder_groups$rank_abundance), col_name_group]

    final_group_order <- c("Other", as.character(top_group_order[[col_name_group]]))

    mdf_group[[col_name_group]] <- factor(mdf_group[[col_name_group]], final_group_order)

    # column group
    mdf_select <- mdf_group %>%
      distinct(!!sym(col_name_group), group) %>%
      arrange(group) %>%
      arrange(!!sym(col_name_group))

    group_order <- as.character(mdf_select$group)
    mdf_group$group <- factor(mdf_group$group, group_order)

    # cdf
    reverse_group_order <-rev(group_order)
    cdf <- cdf %>% filter( !is.na(hex))
    cdf$group <- factor(cdf$group, reverse_group_order)

    cdf <- cdf %>%
      arrange(group)
    cdf$group <- as.character(cdf$group)
  }

  if (order != "NA")
  {
    # Reorder samples
    reorder_samples <- mdf_group %>%
      group_by(Sample) %>%
      filter(!!sym(col_name_order) == order) %>%
      dplyr::summarise(rank_abundance = sum(Abundance))


    new_order <- reorder_samples[order(-reorder_samples$rank_abundance),"Sample"]

    all_samples <- unique(mdf_group$Sample)
    samples_no_subgroup <- setdiff(all_samples, reorder_samples$Sample)

    sample_order <- c(new_order$Sample, samples_no_subgroup)

    mdf_group$Sample <- factor(mdf_group$Sample, sample_order)
  }


  list(
    mdf = mdf_group,
    cdf = cdf
  )
}


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
#' plot <- plot_microshades(mdf_group, cdf)

plot_microshades <- function (mdf_group,
                                cdf,
                                group_label = "Phylum Genus",
                                x = "Sample",
                                y = "Abundance")
{
  if (class(mdf_group) == "phyloseq")
  {
    stop("mdf_group argument must be a data frame. Melt the 'phyloseq' object.")
  }

  if(is.na(cdf$hex) || is.na(cdf$group))
  {
    stop("cdf does not contain complete color information - missing hex or group info")
  }


  plot <- mdf_group %>%
    ggplot(aes_string(x = x, y = y), fill = group_label) +
    geom_col(position = "fill") +
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

#' Reassign the microshades colors to different groups
#'
#' To customize the color order in the plot, use this function to directly assign
#' groups a specific color
#'
#' @param cdf data.frame containing the color key
#' @param group_assignment string vector of taxonomic groups
#' @param color_assignment sting vector of corresponding color assignment
#' @param group_level string indiating the taxonomic level of group_assignment
#'
#' @import dplyr
#'
#' @return cdf data.frame
#'
#' @export
#'
#' @examples
#' new_cdf <- color_reassign(cdf,
#'                           group_assignment = c("Firmicutes", "Actinobacteria"),
#'                           color_assignment = c("micro_cvd_blue", "micro_cvd_purple"))
#'
color_reassign <- function (cdf, group_assignment, color_assignment, group_level = "Phylum")
{
  col_name_group <- paste0("Top_", group_level)

  if (is.null(cdf[[col_name_group]])) {
    stop(paste0("cdf Top_", group_level, " does not exist"))
  }

  if (is.null(cdf$hex)) {
    stop("cdf 'hex' column does not exist in the data")
  }

  for ( i in 1:length(group_assignment))
  {
    temp <- cdf %>% filter(!!sym(col_name_group) == group_assignment[i])
    temp$hex <- rev(microshades_palette(color_assignment[i], nrow(temp)))

    # replace the old hex with new hex
    cdf$hex[cdf[[col_name_group]] == group_assignment[i]] <- temp$hex
  }

  if (length(unique(cdf$hex)) != nrow(cdf))
  {
    warning("Duplicate Hexcodes")
  }

  cdf
}
