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
#' @import dplyr
#'
#'
#' @return data.frame, a melted phyloseq object from `psmelt()`
#' @export
#'
#' @examples
#' library(phyloseq)
#' library(speedyseq)
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

      if (!requireNamespace("phyloseq", quietly = TRUE)) {
        stop("Package \"phyloseq\" needed for this function to work. Please install it.",
             call. = FALSE)
      }

      if (!requireNamespace("speedyseq", quietly = TRUE)) {
        stop("Package \"speedyseq\" needed for this function to work. Please install it.",
             call. = FALSE)
      }


    # Agglomerate, normalizes, and melts phyloseq object -----

    if (!(subgroup_level %in% colnames(phyloseq::tax_table(ps)))) {
      stop("'subgroup_level' does not exist")
    }

    mdf <- ps %>%
        speedyseq::tax_glom(subgroup_level) %>%
        phyloseq::transform_sample_counts(function(x) { x/sum(x) }) %>%
        speedyseq::psmelt()

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
#' @export
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
#' @param mdf data.frame, melted data frame containing microbiome data
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
#' library(phyloseq)
#' data(GlobalPatterns)
#'
#' mdf <- prep_mdf(GlobalPatterns)
#'
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

   if (class(mdf) != "data.frame")
   {
       stop("mdf argument must be a data frame")
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

    # Merge group info back to df -----
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


#' Apply the color factoring from one cdf to a different melted data frame
#'
#' Now both melted dataframes will contain the same color mapping information.
#' This can be useful if you want to make sure that different graphs have consistent legends
#'
#' @param mdf data.frame, melted data frame to apply legend to
#' @param df_match data.frame, data frame to use legend from
#' @param df_is_mdf logical, true if df_match is a mdf, false if df_match is a cdf
#' @param group_level string of larger taxonomic group
#' @param subgroup_level string of smaller taxonomic group
#'
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
#' mdf_to_plot <- match_cdf(mdf, df_match)
#'

match_cdf <- function(mdf,
                            df_match,
                            df_is_mdf = TRUE,
                            group_level = "Phylum",
                            subgroup_level = "Genus"
                            )

    {

      if (class(mdf) != "data.frame")
      {
        stop("mdf argument must be a data frame")
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

      if (is.null(df_match$group)) {
        stop("df_match 'group' column is missing")
      }

      # Create new column for group level -----
      # Add "Other" category immediately
      col_name_group <- paste0("Top_", group_level)
      mdf[[col_name_group]] <- "Other"

      if(df_is_mdf)
      {
        selected_groups <- levels(df_match[[col_name_group]])
      }
      else
      {
        selected_groups <-rev(unique(df_match[[col_name_group]]))
      }

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

      selected_subgroups <- unique(df_match[[col_name_subgroup]])
      selected_subgroups <- selected_subgroups[selected_subgroups != "Other"]

      # Index and find rows to change
      rows_to_change <- mdf[[subgroup_level]] %in% selected_subgroups
      taxa_names_mdf <- row.names(mdf[rows_to_change, ])
      mdf[taxa_names_mdf, col_name_subgroup] <-
       as.character(mdf[taxa_names_mdf, subgroup_level])

      mdf <- mdf %>% mutate(group = paste(!!sym(col_name_group),
                                         !!sym(col_name_subgroup),
                                         sep = "-"))

      if(df_is_mdf)
      {
        new_levels <- levels(df_match$group)
      }
      else
      {
        new_levels <- rev(unique(df_match$group))
      }

      mdf$group <- factor(mdf$group, levels = new_levels)

      mdf
  }

#' Reorder the samples or stacked group levels by abundance
#'
#' This function will reorder the user selected subgroup taxa based on abundance, and can also
#' reorder the stacked groups levels based on abundance, using `sink_abundant_groups`
#'
#' @param mdf_group data.frame, data frame containf microbiome data
#' @param cdf data.frame containing the color key
#' @param order string of subgroup to reorder by
#' @param group_level string of larger taxonomic group
#' @param subgroup_level string of smaller taxonomic group
#' @param sample_variable sample variable to reorder (x- axis component for plot)
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
#' mdf_new <- reorder_samples_by(mdf_group, cdf)
#' mdf_new <- reorder_samples_by(mdf_group, cdf, order = "Bacteroides")
reorder_samples_by <- function (mdf_group,
                                cdf,
                                order_tax = "NA",
                                group_level = "Phylum",
                                subgroup_level = "Genus",
                                sample_variable = "Sample",
                                sink_abundant_groups = TRUE)
{
  if (class(mdf_group) != "data.frame")
  {
    stop("mdf_group argument must be a data frame")
  }

  if (is.null(mdf_group[[sample_variable]])) {
    stop("mdf_group 'sample_variable' column does not exist in the data")
  }

  if (is.null(mdf_group[[subgroup_level]])) {
    stop("mdf_group 'subgroup_level' does not exist")
  }

  if (is.null(mdf_group[[group_level]])) {
    stop("mdf_group 'group_level' does not exist")
  }

  col_name_group <- paste0("Top_", group_level)
  col_name_subgroup <- paste0("Top_", subgroup_level)

  if (order_tax %in% as.character(unique(mdf_group[[col_name_subgroup]])))
  {
    col_name_order <- col_name_subgroup
  }
  else if (order_tax %in% as.character(unique(mdf_group[[col_name_group]])))
  {
    col_name_order <- col_name_group
  }
  else if (order_tax == "NA")
  {
    col_name_order <- NULL
  }
  else
  {
    stop("variable 'order_tax' does not exist in the dataset")
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

  if (order_tax != "NA")
  {
    # Reorder samples
    reorder_samples <- mdf_group %>%
      group_by(!!sym(sample_variable)) %>%
      filter(!!sym(col_name_order) == order_tax) %>%
      dplyr::summarise(rank_abundance = sum(Abundance))


    new_order <- reorder_samples[order(-reorder_samples$rank_abundance),sample_variable]

    all_samples <- unique(mdf_group[[sample_variable]])
    samples_no_subgroup <- setdiff(all_samples, reorder_samples[[sample_variable]])

    sample_order <- c(new_order[[sample_variable]], samples_no_subgroup)

    mdf_group[[sample_variable]] <- factor(mdf_group[[sample_variable]], sample_order)
  }


  list(
    mdf = mdf_group,
    cdf = cdf
  )
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
    temp$hex <- rev(microshades_palette(color_assignment[i], nrow(temp), lightest = FALSE))

    # replace the old hex with new hex
    cdf$hex[cdf[[col_name_group]] == group_assignment[i]] <- temp$hex
  }

  if (length(unique(cdf$hex)) != nrow(cdf))
  {
    warning("Duplicate Hexcodes")
  }

  cdf
}


#' Extends the number of subgroups shown for one particular group by adding additional colors.
#'
#' @param mdf melted data frame with microshades group processing
#' @param cdf color data frame
#' @param group_level string of larger taxonomic group
#' @param subgroup_level string of smaller taxonomic group
#' @param group_name name of the group to extend the palette for
#' @param existing_palette name of current palette of group to extend
#' @param new_palette name of new palette to add for extending the particular group's colors
#' @param n_add number of colors to add
#' @param light orientation of colors added; will make a difference if n_add is less than
#' the number of colors in the new palette
#'
#' @import dplyr
#'
#' @return list
#'   \itemize{
#'     \item{"mdf"}{new mdf with reclassified groups that include the group exention}
#'     \item{"cdf"}{new cdf with reclassified groups that include the group exention}
#'   }
#'
#' @export
#'
#' @examples
#'
#' updated_objs <- extend_group(mdf, cdf, "Phylum", "Genus", "Firmicutes", "micro_purple", "micro_cvd_purple")
#'
#' updated_objs$mdf
#' updated_objs$cdf
#'
extend_group <- function(mdf, cdf, group_level, subgroup_level, group_name, existing_palette, new_palette, n_add = 5, light = TRUE)
{
  # Subset to group to be expanded
  group_subset <- mdf %>% filter(group == paste(group_name,"Other", sep= "-"))

  # Rank Group subgroup categories ranked by abundance and order
  subgroup_ranks <- group_subset %>%
    group_by(!!sym(subgroup_level)) %>%
    summarise(rank_abundance = sum(Abundance)) %>%
    arrange(desc(rank_abundance)) %>%
    mutate(order = row_number()) %>%
    ungroup()

  # Set column default to Other
  col_name_group <- paste0("Top_", group_level)
  col_name_subgroup <- paste0("Top_", subgroup_level)
  subgroup_ranks[[col_name_subgroup]] <- "Other"
  subgroup_ranks[[col_name_group]] <- group_name

  # select rows that are less than or equal to n_add
  rows_to_change <- subgroup_ranks$order <= n_add
  subgroup_ranks[rows_to_change, col_name_subgroup] <-
    as.vector(subgroup_ranks[rows_to_change, subgroup_level])

  # create new group names
  group_info <- subgroup_ranks %>%
    mutate(group = paste(group_name,
                         !!sym(col_name_subgroup),
                         sep = "-"))

  # select the cols
  group_info <- group_info %>% select(!!sym(col_name_group),!!sym(col_name_subgroup), group)

  new_tax <-distinct(group_info)

  new_tax$hex <- c(microshades_palette(existing_palette,n = 1, lightest = light ), rev(microshades_palette(new_palette, n= n_add, lightest = light)))

  row_num_extend <- which(cdf$group == paste(group_name,"Other", sep= "-"))
  total_rows <- nrow(cdf)

  # cdf_full contains all the correct new hexcodes and information
  cdf_full <- full_join(cdf[1:row_num_extend-1,], new_tax)
  cdf_full <- full_join(cdf_full, cdf[row_num_extend+1:total_rows,])


  # Now add new groups to mdf
  mdf$group<-NULL
  mdf_new <- match_cdf(mdf, cdf_full, df_is_mdf = FALSE, group_level, subgroup_level)

  list(
    mdf = mdf_new,
    cdf = cdf_full
  )
}
