# Dp6 Project management

# Install package
install.packages("devtools")
devtools::install_github("EcoDynIZW/d6")

# load package
library(d6)


# Start a new project
# d6::new_project("Chapter_1")# you can set a path for the new project, otherwise the project will automatically be created in the root directory
d6::new_project(name="mugerwa_2019_01_chapter1",
                github = TRUE,
                private_repo = TRUE,
                geo = TRUE,
                path = "D:/Dropbox (ScreenForBio)/Mwezi_B_Mugerwa/IMac/IZW/my_PhD/")

getwd()
### 3. Configure git with Rstudio ############################################
## set your user name and email:
usethis::use_git_config(user.name = "mweziMugerwa", user.email = "bmugerwa@gmail.com")

## create a personal access token for authentication:
usethis::browse_github_token() ## or: usethis::create_github_token()

## set personal access token:
credentials::set_github_pat("789ad80fb71ca10f13d50044c443952d31790fc9")

## or store it manually in '.Renviron':
usethis::edit_r_environ()`
## store your personal access token with: GITHUB_PAT=xxxyyyzzz
## and make sure '.Renviron' ends with a newline



