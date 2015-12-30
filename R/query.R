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
         password=paste0(password, security.token))) %T>%
    stop_for_status %>%
    content %>%
    c(api.version=api.version)
}

api.request <- function(token, method = c('GET', 'POST', 'PUT', 'DELETE'),
                        path, ...,
                        path.prefix = sprintf('/services/data/v%s/', token$api.version), raw=F) {
  resp <- VERB(match.arg(method), token$instance_url,
               path = paste0(path.prefix, path),
               add_headers(Authorization = paste(token$token_type, token$access_token)), ...)
  if(raw) return(resp)
  if(status_code(resp) >= 400) {
    err <- content(resp)[[1]]
    stop(paste0(err$errorCode, ": ", err$message))
  } else {
    content(resp)
  }
}

frame.records <- function(res) {
  recfr <- bind_rows(Map(as.data.frame, res$records))
  attr(recfr, "nextRecordsUrl") <- res$nextRecordsUrl
  select(recfr, -starts_with("attributes."))
}

#' Execute a SOQL query
#'
#' @param token Token returned by login
#' @param soql SOQL query to be executed
#' @param ... Further arguments passed to api.request
#' @param raw Flag whether or not to post-process results into a data.frame, otherwise return raw response.
#'
#' @export
query <- function(token, soql, ..., raw=F) {
  res <- api.request(token, 'GET', 'query', query=list(q=soql), ..., raw=raw)
  if(raw) res else frame.records(res)
}

#' Retrieve further results based on URL paging of SOQL query
#'
#' @param token Token returned by login
#' @param url URL return by SOQL query call (stored in nextRecordsUrl attr)
#' @param ... Further arguments passed to api.request
#' @param raw Flag whether or not to post-process results into a data.frame, otherwise return raw response
#'
#' @export
queryMore <- function(token, url, ..., raw=F) {
  res <- api.request(token, 'GET', url, path.prefix='', ..., raw=raw)
  if(raw) res else frame.records(res)
}

#' High level method automatically page through SOQL query responses, combining query and queryMore
#'
#' @param token Token returned by login
#' @param soql SOQL query to be executed
#' @param limit Threshold of record results to stop calling queryMore
#' @param ... Further arguments (generally simply passed through to api.request)
#'
#' @export
queryAll <- function(token, soql, limit=10000, ...) {
  results <- query(token, soql, ...)
  next.url <- attr(results, 'nextRecordsUrl')
  while(!is.null(next.url) && nrow(results) < limit) {
    results <- rbind(results, queryMore(token, next.url, ...))
  }
  results
}
