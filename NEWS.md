# microshades 1.10

* Added a `NEWS.md` file to track changes to the package.

# microshades 1.11

* Major change: default behavior updated to NOT remove NAs during agglomeration step, see [Remove NA vignette](docs/articles/remove_na_option.html). To remove NAs / replicate figures generated with microshades 1.10 are earlier, use `remove_na = TRUE` in the `prep_mdf` function. 
* Feature Added: Custom legends can now be displayed below the plot instead of along the right hand side. For details, see [issue #17](https://github.com/KarstensLab/microshades/issues/17)
* Fixed minor issues detailed in [pull request 23](https://github.com/KarstensLab/microshades/pull/23) -removing deprecated functions, updating dependencies, etc.


# microshades 1.12

* Minor change: Updated vignettes for clarity, added custom legend vignette
* Resolved warnings from dplyr and cowplot
* Fixed median barplot in plot_contributions
* Added dependencies = TRUE in install instructions  
