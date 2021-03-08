


#' twitter_fas - Full Academic Search in Twitter Academic Research Track (API 2.0)
#'
#' @param query String to search for. Check https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all for further details.
#' @param start Starting time for full archive research. Formattad as string of form: "2021-01-01T00:00:01Z"
#' @param end  End time for full archive research. Formattad as string of form: "2021-03-01T00:00:01Z"
#' @param bearer_token Developer Token for authorization to Academic Project. An application at developer.twitter.com is required.
#' @param max_results The Twitter api retrieves a maximum of 500 tweets per request.
#'                    This function  defaults to the maximum of 500, but can be lowered by setting this argument.
#' @return List of Data Frames
#' @export
#'
#' @examples
#' source("~/.Rprofile")
#' bearer_token <- twitterBearerToken
#' # Your bearer token. Application at developer.twitter.com required. You can store
#' # your bearer token in your .Rprofile in your home directory, creating following entry:
#' # twitterBearerToken <- "replace with your token"
#' query <- "#OccupyWallStreet"
#' startdate <- "2012-07-12T00:00:01Z"
#' enddate <- "2012-07-15T00:00:01Z"
#' occupy_search <- twitter_fas(query, startdate, enddate, bearer_token)
#' occupy_tweets <- occupy_search$data

twitter_fas <- function(query,
                        start,
                        end,
                        bearer_token,
                        max_results = 500
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


  if(count<250)
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
  return( list(data = df,
               includes_tweets = df_tweets,
               includes_media = df_media,
               includes_places = df_places,
               includes_users = df_users,
               includes_polls = df_polls,
               errors = df_errors


              ))
}
