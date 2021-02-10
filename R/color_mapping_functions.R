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
#' @importFrom speedyseq tax_glom transform_sample_counts psmelt
#'
#' @return data.frame, a melted phyloseq object from `psmelt()`
#' @export
#'
#' @examples
#' data(GlobalPatterns)
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
#' @param cvdf logical Color Vision Deficent Friendly palette useage
#'
#' @import dplyr
#' @return data.frame
#' @export
#'
#' @examples
#' # Get hex codes for 5 groups
#' hex_df <- default_hex()
#'
#' # Get hex codes for 3 groups
#' hex_df <- default_hex(3)
default_hex <- function(n_groups = 5, cvdf = FALSE) {
    # Hex is ordered light to dark
    # Numbers in name refer to the bottom up order in color stack

    if (cvdf)
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


#' Generate melted data frame and color palette data frame
#'
#' The "Top" group and subgroup categories are dynamically generated. For the
#' top group, the categories not in `selected_groups` will be changed to
#' "Other". The `top_n_subgroups` will be determined for each selected group.
#'
#' #' Notes:
#'
#' - In SILVA 138, some phylum names are different and you should consider
#'   passing in the vector
#'   `c("Proteobacteria", "Actinobacteriota", "Bacteroidota", "Firmicutes")`
#'
#' @param mdf data.frame, melted data frame
#' @param selected_groups list of phyla to keep, the vector order is the
#'   stacking order. "Other" is always on the top of the stack,
#'   but then the rest will follow. The default is "Proteobacteria", "Actinobacteria",
#'   "Bacteroidetes", "Firmicutes".
#' @param top_n_subgroups integer number of top subgroups to show for each selected group
#'   the max is 4 top subgrorups
#' @param group_level string of larger taxonomic group
#' @param subgroup_level string of smaller taxonomic group
#' @param cvdf logical stands for Color Vision Deficent Friendly palette
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
#'     \item{"color_df"}{ mapping to be used in manual color filling}
#'   }
#'
#' @export
#'
#' @examples
#' colorframe <- create_color_dfs(mdf)
#' colorframe <- create_color_dfs(mdf, selected_groups = c("Proteobacteria", "Actinobacteriota", "Bacteroidota", "Firmicutes"))
#' colorframe <- create_color_dfs(mdf, cvdf = TRUE)
create_color_dfs <- function(mdf,
                             selected_groups = c("Proteobacteria",
                                                 "Actinobacteria",
                                                 "Bacteroidetes",
                                                 "Firmicutes"),
                             top_n_subgroups = 4,
                             group_level = "Phylum",
                             subgroup_level = "Genus",
                             cvdf = FALSE)
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

    # Index and find rows to change
    rows_to_change <- mdf[[group_level]] %in% selected_groups
    taxa_names_mdf <- row.names(mdf[rows_to_change, ])
    mdf[taxa_names_mdf, col_name_group] <-
        as.character(mdf[taxa_names_mdf, group_level])


    # Induce phylum ordering -----
    col_name_group <- paste0("Top_", group_level)
    mdf[[col_name_group]] <- factor(mdf[[col_name_group]],
                                    levels = c("Other", selected_groups))

    # Check to make sure the selected_groups specified all exist in the dataset
    if(sum (selected_groups %in% as.character(unique(mdf[[col_name_group]]))) != length(selected_groups))
    {
      stop("some 'selected_groups' do not exist in the dataset. Consider SILVA 138 c('Proteobacteria', 'Actinobacteriota', 'Bacteroidota', 'Firmicutes')")
    }

    # Assigning "Other" for subgroup -----
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
    group_info$order[group_info$Top_Genus == "Other"] <- top_n_subgroups +1

    # Merge group info back to melted phyloseq -----
    # Get relevant columns from data frame with group info
    group_info_to_merge <-
        group_info[, c(col_name_group, subgroup_level,
                       col_name_subgroup, "group")]
    mdf_group <- mdf_unknown_subgroup %>%
        left_join(group_info_to_merge, by = c(col_name_group, subgroup_level))


    # Rank bacteria by group-subgroup abundances -----
    # Get beginning of color data frame with top groups/subgroups
    # E.g., 4 selected_groups + 1 Other, 4 top_n_groups + 1 Other => 25 groups
    prep_color_df <- group_info %>%
        select(c("group", "order", col_name_group, col_name_subgroup)) %>%
        filter(order <= top_n_subgroups + 1) %>%  # "+ 1" for other subgroup
        arrange(!!sym(col_name_group), order)
        # mutate(group = fct_inorder(group))


    # Prepare hex colors -----

    # Generates default 5 row x 6 cols of 5 colors for 6 phylum categories
    # Parameter for number of selected phylum
    # "+ 1" is for "Other" group
    hex_df <- default_hex(length(selected_groups) + 1, cvdf)

    # Add hex codes in ranked way
    # creates nested data frame
    # https://tidyr.tidyverse.org/articles/nest.html
    # https://tidyr.tidyverse.org/reference/nest.html
    color_df <- prep_color_df %>%
        group_by_at(all_of(vars(col_name_group))) %>%
        tidyr::nest() %>%
        arrange(!!sym(col_name_group))

    # Loop through top group and add colors by nested data frame
    # Higher row number = less abundant = lighter color
    # "+ 1" is "Other"
    for (i in 1:(length(selected_groups) + 1)) {
      color_df$data[[i]]$hex <- hex_df[1:length(color_df$data[[i]]$group),i]
    }

    # Unnest colors and groups and polish for output
    color_df <- color_df %>%
        ungroup() %>%
        arrange(desc(row_number())) %>%
        tidyr::unnest(data) %>%
        select(!!sym(col_name_group),
               !!sym(col_name_subgroup),
               group, hex) %>%
        mutate_all(as.character)  # Remove factor from hex codes

    level_assign = unique(rev(color_df$group))

    mdf_group$group <- factor(mdf_group$group, levels = level_assign)

    # Return final objects -----
    list(
        mdf = mdf_group,
        color_df = color_df
    )
}



#' Apply the color factoring from one color df to a different melted phyloseq object.
#'
#' Now both melted dataframes will contain the same color mapping information
#'
#' @param mdf data.frame, melted data frame to apply legend to
#' @param mdf_group data.frame, melted data frame to use legend from
#' @param selected_groups list. "Other" is always on the top of the stack,
#'   but then the rest will follow, with Firmicutes as the base of the stack
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
#' mdf_to_plot <- match_color_df(mdf, mdf_group)
#'

match_color_df <- function(mdf,
                            mdf_group,
                            selected_groups = c("Proteobacteria",
                                                "Actinobacteria",
                                                "Bacteroidetes",
                                                "Firmicutes"),
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

#' Reorder the samples by abundance of user selected subgroup taxa
#'
#'
#'
#' @param mdf_group data.frame, melted data frame
#' @param order string of subgroup to reorder by
#' @param subgroup_level string of smaller taxonomic group
#'
#' @import dplyr
#' @import forcats
#' @import tidyselect
#'
#' @return data.frame, mdf dataframe ready to plot
#'
#' @export
#'
#' @examples
#' mdf_to_plot <- reorder_samples_by(mdf_group)
reorder_samples_by <- function (mdf_group,
                             order = "Lactobacillus",
                             subgroup_level = "Genus")
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

  col_name_subgroup <- paste0("Top_", subgroup_level)

  if(!(order %in% as.character(unique(mdf_group[[col_name_subgroup]]))))
  {
    stop("variable 'order' does not exist in the dataset")
  }



  # Reorder samples
  reorder_samples <- mdf_group %>%
    group_by(Sample) %>%
    filter(!!sym(col_name_subgroup) == order) %>%
    dplyr::summarise(rank_abundance = sum(Abundance))


  new_order <- reorder_samples[order(-reorder_samples$rank_abundance),"Sample"]

  all_samples <- unique(mdf_group$Sample)
  samples_no_subgroup <- setdiff(all_samples, reorder_samples$Sample)

  sample_order <- c(new_order$Sample, samples_no_subgroup)

  mdf_group$Sample <- factor(mdf_group$Sample, sample_order)

  mdf_group
}


#' Plot the mdf created with the microshades package
#'
#'
#'
#' @param mdf_group data.frame, melted data frame processed by microshades functions
#' @param color_df data.frame containing the color key
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
#' plot <- plot_microshades(mdf_group, color_df)

plot_microshades <- function (mdf_group,
                                color_df,
                                group_label = "Phylum Genus",
                                x = "Sample",
                                y = "Abundance")
{
  if (class(mdf_group) == "phyloseq")
  {
    stop("mdf_group argument must be a data frame. Melt the 'phyloseq' object.")
  }


  plot <- mdf_group %>%
    ggplot(aes_string(x = x, y = y), fill = group_label) +
    geom_col(position = "fill") +
    scale_fill_manual(name = group_label,
                      values = color_df$hex,
                      breaks = color_df$group) +
    scale_colour_manual(name = group_label,
                        values = color_df$hex,
                        breaks = color_df$group) +
    geom_bar(aes(color=group, fill=group), stat="identity", position="stack") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  plot
}
