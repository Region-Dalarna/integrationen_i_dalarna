if (!require("pacman")) install.packages("pacman")
p_load(usethis)

# skapa git först
use_git(message = "Skapa repository")

# därefter github

use_github(
  organisation = Region-Dalarna,
  private = FALSE,
  visibility = "public"
)

use_github_pages(branch = git_default_branch(), path = "/docs", cname = NA)
