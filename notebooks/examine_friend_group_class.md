Explore Correlations with Friend Group Class
================
Aman Choudhri
2024-09-09

``` r
CLEANED_DATA_FILENAME <- 'dat/processed.rds'

df <- readRDS(CLEANED_DATA_FILENAME)
```

Save some instance variables for use in the notebook based on the
specified plot variable, `friend_group_class`.

``` r
outcome_var <- params$categorical_var
outcome_var_name <- params$cat_var_display_name
```

Also, since we’ll be using these parameters over and over again, define
a helper function so we don’t have to repeat code. For more information
on the `make_plot_base` function, see the appendix at the bottom.

``` r
make_plot <- function (group_var1, group_var2=NULL) {
  return(make_plot_base(df, outcome_var, outcome_var_name, group_var1, group_var2))
}
```

## By Personal Party ID

Start by plotting `friend_group_class` by a person’s individual party
ID.

``` r
make_plot('collapsed_pid')
```

![](img/examine_friend_group_class/unnamed-chunk-3-1.png)<!-- -->

## By Urbanicity

We’ll repeat the same plot, now by urbanicity.

``` r
make_plot('urbancity')
```

![](img/examine_friend_group_class/unnamed-chunk-4-1.png)<!-- -->

## Personal Party ID and Urbanicity

Now let’s interact personal PID and urbanicity.

``` r
make_plot('collapsed_pid', 'urbancity')
```

![](img/examine_friend_group_class/unnamed-chunk-5-1.png)<!-- -->

## By State Partisan Leaning

Let’s create a rough analysis by state. Splitting states into D, R, and
swing, we’ll again display the proportions of friend group party ID.
We’ll use data from the Cook Political Report. To simplify the analysis,
we won’t consider the congressional districts in Maine and Nebraska
separately. We mark Maine as D, and Nebraska as R.

``` r
cook_ratings <- read.csv('dat/cook_political_report_ratings.csv')

# remove maine and nebraska's congressional districts
cook_ratings <- cook_ratings[cook_ratings$state %in% state.name,]

# collapse "leans" and "likelies" down to hard D/R/Tossup
cook_ratings <- cook_ratings %>%
  mutate(
    category = case_when(
      str_detect(category, "D$") ~ "Democratic",
      str_detect(category, "R$") ~ "Republican",
      TRUE ~ "Tossup"
    ) %>%
      factor(levels = c("Democratic", "Tossup", "Republican"))
  )

# add into the df
df <- merge(df, cook_ratings, by.x='inputstate', by.y='state')
# rename column
df$state_leaning <- df$category
```

First, just plot friend group PID by state leaning.

``` r
make_plot('state_leaning')
```

![](img/examine_friend_group_class/unnamed-chunk-7-1.png)<!-- -->

Now repeat, facetting once more by individual PID as well.

``` r
make_plot('collapsed_pid', 'state_leaning')
```

![](img/examine_friend_group_class/unnamed-chunk-8-1.png)<!-- -->

## Appendix

``` r
library(dplyr)
library(ggplot2)
library(stringr)
```

``` r
make_plot_base <- function(
    data,
    categorical_plot_var,
    categorical_plot_var_name,
    group_var1,
    group_var2 = NULL
    ) {
  # Ensure categorical_plot_var is a string
  categorical_plot_var <- as.character(categorical_plot_var)
  
  # Drop rows where categorical_plot_var is null
  data <- data[!is.na(df[categorical_plot_var]),]
  
  five_level_partisan_colors <- c("blue", "lightblue", "purple", "pink", "red", "grey")
  five_level_nonpartisan_colors <- c('#ffffcc','#a1dab4','#41b6c4','#2c7fb8','#253494', "grey")
  
  color_palettes <- list(
    friend_group_pid3 = c("blue", "purple", "red", "grey"),
    friend_group_pid5 = five_level_partisan_colors,
    friend_group_copartisanship = five_level_nonpartisan_colors,
    # colors created using ColorBrewer 2.0
    # https://colorbrewer2.org/?type=sequential&scheme=YlGnBu&n=5
    friend_group_class = five_level_nonpartisan_colors
  )
  
  # Select the appropriate color palette
  if (!categorical_plot_var %in% names(color_palettes)) {
    stop(paste("No color palette defined for", categorical_plot_var))
  }
  colors <- color_palettes[[categorical_plot_var]]
  
  # Create the grouping expression
  if (is.null(group_var2)) {
    group_vars <- c(as.character(group_var1))
    facet_formula <- as.formula(paste("~", quo_name(group_var1)))
  } else {
    group_vars <- c(as.character(group_var1), as.character(group_var2))
    facet_formula <- as.formula(paste(quo_name(group_var2), "~", quo_name(group_var1)))
  }
  
  # Prepare the data
  plot_data <- data %>%
    group_by(across(all_of(c(group_vars, categorical_plot_var)))) %>%
    summarise(count = n(), .groups = "drop_last") %>%
    mutate(proportion = count / sum(count))
  
  # Create the plot
  p <- ggplot(plot_data, aes(y = !!sym(categorical_plot_var), x = proportion)) +
    geom_bar(stat = "identity", aes(fill = !!sym(categorical_plot_var))) +
    facet_grid(facet_formula) +
    labs(
        title = paste(
          categorical_plot_var_name,
          "by",
          group_var1,
          if(!is.null(group_var2)) paste("and", group_var2)
          ),
         y = categorical_plot_var_name,
         x = "Proportion") +
         # fill = categorical_plot_var_name) +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_discrete(limits = rev(levels(plot_data[[categorical_plot_var]]))) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Add color scale if colors are provided
  if (!missing(colors)) {
    p <- p + scale_fill_manual(values = colors, guide="none")
  }
  
  return(p)
}
```
