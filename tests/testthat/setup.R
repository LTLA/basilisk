# Setting up common variables.

test.pandas <- "pandas==2.2.3"
test.pandas.deps <- c("python-dateutil==2.8.2", "pytz==2024.1")

old.pandas <- "pandas==2.2.2"
old.pandas.deps <- c("python-dateutil==2.8.2", "pytz==2023.4")

client.dir <- "install-test-client"
unlink(client.dir, recursive=TRUE)
dir.create(client.dir)

# See reticulate:::check_forbidden_install().
Sys.setenv(`_RETICULATE_I_KNOW_WHAT_IM_DOING_`="true")
