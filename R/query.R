#' @import httr
#' @import dplyr

NULL
#' Private method for building Salesforce requests from tokens
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

#' Utility to transform the results of query and queryMore to data.frames
frame.records <- function(res) {
  parse.records <- function(records) {
    bind_rows(lapply(records, function(rec) {
      rec[names(rec) != 'attributes'] %>%
        lapply(function(e) {
          if(is.null(e)) NA
          else if(is.list(e)) parse.records(e$records)
          else e
        }) %>%
        data.frame(stringsAsFactors=F)
    }))
  }
  recfr <- parse.records(res$records)
  attr(recfr, "nextRecordsUrl") <- res$nextRecordsUrl
  blanks <- unlist(lapply(recfr, function(col) any(!is.na(col))))
  recfr[blanks]
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

#' Describe schema of SObject
#'
#' @param token Token returned by login
#' @param sobject.name Sobject class name (ends in __c for custom objects)
#' @param describe.full Flag whether to fetch extended detail (including recently updated items)
#'
#' @export
describeSObject <- function(token, sobject.name, describe.full=F) {
  describe.path <- if(describe.full) {
    paste('sobjects', sobject.name, sep='/')
  } else {
    paste('sobjects', sobject.name, 'describe', sep='/')
  }
  api.request(token, 'GET', describe.path)
}

#' Retrieve SObject by ID
#'
#' @param token Token returned by login
#' @param sobject.name Sobject class name (ends in __c for custom objects)
#' @param id ID of sobject to be queried
#' @param external.field if not NULL, name of column for lookup (which must be an External ID)
#'
#' @export
lookupSObjectID <- function(token, sobject.name, id, external.field=NULL) {
  api.request(token, 'GET', do.call(paste, as.list(c('sobjects',
                                                     sobject.name,
                                                     external.field,
                                                     id, sep='/'))))
}
