if (!require("pacman")) install.packages("pacman")
p_load(here,
       stringr)

senaste_rmd_filen <- list.files(here(), pattern = "\\.Rmd$") %>%
  .[which.max(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==file.info(.)$mtime)]

renderad_fil <- senaste_rmd_filen %>%
  str_replace(".Rmd", ".html")

rmarkdown::render(
  input = senaste_rmd_filen,
  output_file = renderad_fil,
  envir = parent.frame()
)
