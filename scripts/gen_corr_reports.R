library(rmarkdown)
library(ezknitr)

setwd("C:/Users/Jared/Desktop/Columbia 2024 (3rd)/Gelman Lab/social-ties")

VARS <- c(
  "friend_group_pid3" = "Friend Group Party ID",
  "friend_group_pid5" = "Friend Group Party ID",
  "friend_group_class" = "Friend Group Class",
  "friend_group_copartisanship" = "Friend Group Copartisanship",
  "taxwealth" = "Tax Support"
  )

# pdf_format <- rmarkdown::pdf_document()
# md_format <- rmarkdown::md_document(variant="markdown_github")

# FORMATS <- c("pdf_document", "github_document")

# NOTEBOOKS_DIR <- '/Users/amanchoudhri/aman/penumbra/notebooks'
# setwd(NOTEBOOKS_DIR)

for (var_id in names(VARS)) {
  fname <- paste0('examine_', var_id)
  parameters <- function (output_mode) {
    p <- list(
      categorical_var = var_id,
      cat_var_display_name = VARS[var_id],
      img_dir = paste0('figs/', fname, "/"),
      base_dir = NOTEBOOKS_DIR,
      output_mode = output_mode
    )
    return(p)
  }
  rmarkdown::render(
    'explore_correlations.Rmd',
    output_format = "pdf_document",
    output_file = fname,
    output_dir='notebooks',
    params=parameters('pdf'),
    envir = new.env()
  )
  rmarkdown::render(
    'explore_correlations.Rmd',
    output_format = "github_document",
    output_file = fname,
    output_dir='notebooks',
    params=parameters('github'),
    envir = new.env()
  )
}
