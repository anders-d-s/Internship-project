library(usethis)
create_github_token()
library(gitcreds)
gitcreds_set()

#Git Bash commands to set user
git config --global user.email "andersdrejergaard@gmail.com"
git config --global user.name "anders-d-s"