#' @import httr
#' @import dplyr

NULL

#' Retrieve authentication token based on login
#'
#' @param clientid App client ID
#' @param clientsecret App client secret
#' @param username Salesforce username
#' @param password Salesforce password
#' @param security.token Added security token, concatenated to the password
#' @param api.version Version of API to use
#' @param ... Further arguments are passed to httr::POST
#'
#' @return Authentication token used in subsequent API requests
#'
#' @export
password.login <- function(clientid, clientsecret,
                           username, password, security.token="",
                           api.version = '35.0', ..., raw=T) {
  resp <- POST("https://login.salesforce.com/services/oauth2/token",
               accept_json(), ...,
               encode="form",
               body=list(
                 grant_type="password",
                 client_id=clientid,
                 client_secret=clientsecret,
                 username=username,
                 password=paste0(password, security.token)))
  stop_for_status(resp)
  resp %>%
    content %>%
    c(api.version=api.version)
}
