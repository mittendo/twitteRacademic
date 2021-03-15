


#' twitter_fas - Full Academic Search in Twitter Academic Research Track (API 2.0)
#'
#' @param query String to search for. Check https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all for further details.
#' @param start Starting time for full archive research. Formattad as string of form: "2021-01-01T00:00:01Z"
#' @param end  End time for full archive research. Formattad as string of form: "2021-03-01T00:00:01Z"
#' @param bearer_token Developer Token for authorization to Academic Project. An application at developer.twitter.com is required.
#' @param max_results The Twitter api retrieves a maximum of 500 tweets per request.
#'                    This function  defaults to the maximum of 500, but can be lowered by setting this argument.
#' @param pause Pause requests, because twitter allows only 300 requests per 15 minutes.
#'              Standard is 250 requests per 15 minutes.
#' @return A ressource. Actually an array of data frames (tibbles),
#' consisting of different entities (tweets and linked entities like
#' users, media etc.). Each entity consists of different sets of additional
#' information like annotations, hashtags etc.:
#' \describe{
#' \item{tweets:}{Tweets retrieved by your query as well as additional infomation
#' found within these tweets, e.g.: hashtags, cashtags,
#' mentions, annotations, urls and referenced tweets (cf. includes_retweets).}
#' \item{includes_retweets:}{Referenced tweets with additional information,
#' similar to those information found in tweets.}
#' \item{includes_media:}{Media referenced in the retrieved set of tweets.}
#' \item{includes_places:}{Places mentioned in the retrieved set of tweets.}
#' \item{includes_users:}{Information about authors and people mentioned in
#' tweets.}
#' \item{includes_polls:}{Polls, which are sent with the tweets together with
#' the different options, which were asked in these polls.}
#' \item{errors:}{A tibble with informations on entities and keys, on which no
#' information could be retrieved (withheld tweets, suspended users, non-public
#' tweets etc.)}
#' }
#' The different entitie´s each have unique ids or keys by which they
#' are referenced. Additional tables have an parent_id key, by which these
#' entries can be linked to their respective entity.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # You need to provide your bearer token.
#' # Application at developer.twitter.com required.
#' # You can store your bearer token in your .Rprofile in your home directory,
#' # creating an entry like:
#' # twitterBearerToken = "Your Token"
#'
#' query <- "#OccupyWallStreet"
#' startdate <- "2012-07-12T00:00:01Z"
#' enddate <- "2012-07-15T00:00:01Z"
#' bearer_token <- readline("Please enter your bearer_token")
#' occupy_search <- twitter_fas(query, startdate, enddate, bearer_token)
#' occupy_tweets <- occupy_search$tweets$tweets
#' hist(occupy_tweets$public_metrics_reply_count)
#' }

twitter_fas <- function(query,
                        start,
                        end,
                        bearer_token,
                        max_results = 500,
                        pause = 250
                      #  tweet_fields_attachments = TRUE,
                       # tweet_fields_author = TRUE,
                        #tweet_fields_context = TRUE
                        ) {
  # define needed objects

  df <- tibble::tibble()
  df_tweets <- tibble::tibble()
  df_media <- tibble::tibble()
  df_places <- tibble::tibble()
  df_users <- tibble::tibble()
  df_polls <- tibble::tibble()
  df_errors <- tibble::tibble()

  next_token <- ''



  params = list(
    `max_results` = max_results,
    `tweet.fields` = 'attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source,text,withheld',
    `user.fields` = 'created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,username,verified,withheld',
    `media.fields` = 'url,preview_image_url,public_metrics,height,width,duration_ms,non_public_metrics',
    `poll.fields` = 'duration_minutes,end_datetime,id,options,voting_status',
    `place.fields` = 'contained_within,country,country_code,name,place_type',
    `start_time` = start,
    `end_time`  = end,
    `expansions`= 'attachments.poll_ids,attachments.media_keys,author_id,entities.mentions.username,geo.place_id,in_reply_to_user_id,referenced_tweets.id,referenced_tweets.id.author_id',
    `query` = query

  )

  # Define headers for httr-request
  headers = c(
    `Authorization` = sprintf('Bearer %s', bearer_token)
  )

  # define loop parameters
  count <- 1

  while (!is.na(next_token)) {
    print(paste0("Pagination (", params[['max_results']], " tweets per page), loading page: ", count))

  if( stringr::str_length(next_token)>0)   {
    params[['next_token']] <- next_token
    }

    response <- try(
    httr::GET(url = 'https://api.twitter.com/2/tweets/search/all',
              httr::add_headers(.headers=headers),
              query = params)
    )

  fas_body <-
    try(
      httr::content(
        response,
        as = 'parsed',
        type = 'application/json',
        simplifyDataFrame = TRUE
      )
    )
  if(exists("fas_body$title")) stop( fas_body$title)

  df <- if(exists("fas_body")) dplyr::bind_rows(df, fas_body$data)
  df_tweets <- if(exists("fas_body")) dplyr::bind_rows(df_tweets, fas_body$includes$tweets)
  df_media <- if(exists("fas_body")) dplyr::bind_rows(df_media, fas_body$includes$media)
  df_places <- if(exists("fas_body")) dplyr::bind_rows(df_places, fas_body$includes$places)
  df_users <- if(exists("fas_body")) dplyr::bind_rows(df_users, fas_body$includes$users)
  df_polls <- if(exists("fas_body")) dplyr::bind_rows(df_polls, fas_body$includes$polls)
  df_errors <- if(exists("fas_body")) dplyr::bind_rows(df_errors, fas_body$errors)


  if(count<pause)
    {
    count <- count+1
  }   else
  {
    Sys.sleep(900)
    print(paste0(Sys.time(),
                 ": Maximum Rate of requests (nearly) reached (300 requests).
                 Waiting 15 minutes to go on with 250 new requests."))
    count <- 1
  }

  if(length(fas_body$meta$next_token)>0)
          {
            next_token = try(fas_body$meta$next_token)
          } else {
         next_token <- NA


  }
}

  # Postprocessing of data
  df_polls <- make_tidyrespoll(df_polls)
  df_users <- make_tidyresusers(df_users)
  df <- make_tidyrestweets(df)
  df_retweeted <- make_tidyrestweets(df_tweets)
  # Return resource

    return( list(tweets = df,
               includes_retweeted = df_retweeted,
               includes_media = df_media,
               includes_places = df_places,
               includes_users = df_users,
               includes_polls = df_polls,
               errors = df_errors


              ))
}
