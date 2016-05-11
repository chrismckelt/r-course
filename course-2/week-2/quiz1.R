library(httr)
require(httpuv)
require(jsonlite)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
#oauth_endpoints("github")

# 2. Register an application at https://github.com/settings/applications
#    Insert your values below - if secret is omitted, it will look it up in
#    the GITHUB_CONSUMER_SECRET1 environmental variable.

#myapp <- oauth_app("RCourse2Week2",
#                  key = "97bd1d45351d525c7fab",
#                  secret = "6d7f92539603b934f8a3c1204dfe6daf37584f12")

# 3. Get OAuth credentials
#github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)


# 4. Use API
req <- GET("https://api.github.com/users/jtleek/repos", config(token = github_token))
stop_for_status(req)
output <- content(req)
l <- Filter(function(x) {x$name=="datasharing"}, output)
list(l[[1]]$name, l[[1]]$created_at)



