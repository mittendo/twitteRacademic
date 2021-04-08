# get data from resource

#' Get Dataframe from resource
#' @importFrom rlang .data
#' @importFrom dplyr mutate
#' @importFrom lubridate as_date
#' @importFrom lubridate as_datetime
#' @importFrom magrittr %>%
#' @param .data Resource, retrieved by \code{twitter_fas()}.
#' @param withUsers (Logical) if true, add information about author
#' and the authors of referred tweets.
#' @param withRetweeted (Logical) if true, add information about the retweeted
#' or cited tweet.
#' @param withMedia (Logical) if true, add information about media included
#' within the tweet.
#' @param withPlaces (Logical) if true, add geo-information  included
#' within the tweet.
#' @param tz Format dates with time zone
#' @param ... Any other option passed to helper functions like \code{withUsers}.
#' @return A Data Frame with Tweet data from Resource
#'
#' @export
#' @examples
#' data(cducsuTweets)
#' tweets <- getTweets(cducsuTweets)
#' head(tweets)
#'
#'
getTweets <- function(.data,
                      withUsers=TRUE,
                      withRetweeted=TRUE,
                      withMedia=TRUE,
                      withPlaces=TRUE,
                      tz="UTC", ...){
  created_at <- NULL
  tweets <- .data$tweets$tweets
  tweets <- tweets %>% mutate(created_at = as_datetime(created_at, tz=tz))
  if(withUsers == TRUE) tweets <- withUsers(.data, tweets)

  if(withRetweeted == TRUE) tweets <- withRetweeted(.data, tweets)

  if(withMedia == TRUE) tweets <- withMedia(.data, tweets)

  if(withPlaces == TRUE) tweets <- withMedia(.data, tweets)

  return(tweets)
}


#' Add User Information to Tweet-Dataframe
#' @importFrom dplyr left_join
#' @importFrom lubridate as_datetime
#' @importFrom rlang .data
#' @importFrom dplyr mutate
#' @importFrom dplyr rename_with
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom magrittr %>%
#' @param res A resource, retrieved by \code{twitter_fas()}.
#' @param tweets The tweets-dataframe, as returned by \code{getTweets()}
#' @param tz The time zone, with which the \code{created at} columns should be formatted.
#' @param allcols (logical) if TRUE, all columns will be added, resulting in a larger
#'        number of rows that get returned. If FALSE, only id, username, created_at and
#'        verfication status is returnded. Defaults to FALSE.
#' @return A data frame, actually with the length of the tweets-data, with user-information added.
#' @export
#' @examples
#' tweets <- withUsers(cducsuTweets, getTweets(cducsuTweets))
#' head(tweets)

withUsers <- function(res, tweets, tz="UTC", allcols = FALSE){
  user_created_at <- username <- verified <- id <- NULL
  n <- dim(tweets)[1]
  users <- res$includes_users$users
  users <- mutate(users,
                  user_created_at =
                    as_datetime(created_at, tz=tz)) %>%
    distinct()
  if(allcols==FALSE) users <- users %>%
    select(id, username,  verified, user_created_at) %>%
    distinct()
  tweets <- left_join(tweets, users, by=c("author_id"="id"))
  ## Following lines return duplicated rows, because tweets can refer to
  ## more than one users. It's commented out but may be, it will be used as
  ## as an option.
  # if("in_reply_to_user_id" %in% colnames(tweets)) {
  #   reply_to <- users %>% rename_with(~paste0("reply_to_", .x))
  #   tweets <- left_join(tweets, reply_to, by=c("in_reply_to_user_id"="reply_to_id"))
  # }
  if(dim(tweets)[1] != n) warning(
    paste0("Number of observations has changed from n=",
           n, "to n=",
           dim(tweets)[1], "!"))
  return(tweets)
}


#' Add Information on retweeted tweets to Tweet-Dataframe
#' @importFrom dplyr left_join
#' @importFrom lubridate as_datetime
#' @importFrom rlang .data
#' @importFrom dplyr mutate
#' @importFrom dplyr rename_with
#' @importFrom dplyr rename
#' @param res A resource, retrieved by \code{twitter_fas()}.
#' @param tweets The tweets-dataframe, as returned by \code{getTweets()}
#' @return A data frame, actually with the length of the tweets-data, with information on retweeted data added.
#' @export
#' @examples
#' tweets <- withRetweeted(cducsuTweets, getTweets(cducsuTweets))
#' head(tweets)
#'
#'

withRetweeted <- function(res, tweets){
  id <- type <- NULL
  referenced_tweets <- res$tweets$referenced_tweets
  if("referenced_id" %in% colnames(referenced_tweets)){
  n <- dim(tweets)[1]
  retweeted <- res$includes_retweeted$tweets
  retweeted <- withUsers(res, retweeted)
  retweeted <- rename_with(retweeted, ~paste0("referenced_", .x))
  res$tweets$referenced_tweets <- rename(res$tweets$referenced_tweets, referenced_id = id)
  res$tweets$referenced_tweets <- rename(res$tweets$referenced_tweets, referenced_type = type)
  tweets <- left_join(tweets, res$tweets$referenced_tweets, by=c("id"="parent_id" ))
  tweets <- left_join(tweets, retweeted, by=c("referenced_id"))
  if(dim(tweets)[1] != n) warning(
    paste0("Number of observations has changed from!",
           n, "to ",
           dim(tweets)[1], "!"))
}
  return(tweets)
}
#' Add enclosed Media-Information to Tweet-Dataframe
#' @importFrom dplyr left_join
#' @param res A resource, retrieved by \code{twitter_fas()}.
#' @param tweets The tweets-dataframe, as returned by \code{getTweets()}
#' @return A data frame, actually with the length of the tweets-data, with media-information added.
#' @export
#' @examples
#' tweets <- withMedia(cducsuTweets, getTweets(cducsuTweets))
#' head(tweets)
withMedia  <- function(res, tweets) {
  n <- dim(tweets)[1]
  if("media_keys" %in% colnames(res$includes_media)){
  tweets <- left_join(tweets, res$includes_media, by=c("attacments_media_keys"="media_keys"))
  }
  if(dim(tweets)[1] != n) warning(
    paste0("Number of observations has changed from!",
           n, "to ",
           dim(tweets)[1], "!"))
  return(tweets)

}



#' Add enclosed Geo-Information to Tweet-Dataframe
#' @importFrom dplyr left_join
#' @param res A resource, retrieved by \code{twitter_fas()}.
#' @param tweets The tweets-dataframe, as returned by \code{getTweets()}
#' @return A data frame, actually with the length of the tweets-data, with media-information added.
#' @export
#' @examples
#' tweets <- withPlaces(cducsuTweets, getTweets(cducsuTweets))
#' head(tweets)
withPlaces  <- function(res, tweets) {
  n <- dim(tweets)[1]
  if("id" %in% colnames(res$includes_places)){
    tweets <- left_join(tweets, res$includes_places, by=c("geo_places_id"="id"))
  }
  if(dim(tweets)[1] != n) warning(
    paste0("Number of observations has changed from!",
           n, "to ",
           dim(tweets)[1], "!"))
  return(tweets)

}
