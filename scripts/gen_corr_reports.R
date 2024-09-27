library(rmarkdown)

VARS <- c(
  "friend_group_pid3" = "Friend Group Party ID",
  "friend_group_pid5" = "Friend Group Party ID",
  "friend_group_class" = "Friend Group Class",
  "friend_group_copartisanship" = "Friend Group Copartisanship"
  )

# pdf_format <- rmarkdown::pdf_document()
# md_format <- rmarkdown::md_document(variant="markdown_github")

# FORMATS <- c("pdf_document", "github_document")

for (var_id in names(VARS)) {
  fname <- paste0('examine_', var_id)
  parameters <- list(
    categorical_var = var_id,
    cat_var_display_name = VARS[var_id],
    img_dir = paste0('img/', fname, "/")
  )
  rmarkdown::render(
    'explore_correlations.Rmd',
    output_format = "all",
    output_file = fname,
    output_dir = "notebooks",
    params=parameters,
    envir = new.env()
  )
}
