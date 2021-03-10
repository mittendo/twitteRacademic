# helpers.r

tweetbindcol <- function(.data, subdf) {
  nametest <- deparse(substitute(subdf))
  test <- .data[[substitute(subdf)]]  %>% rename_with(~ paste0(nametest , "_", .x))

  # rename cols with df-name
  # test <- test %>% rename_with(~ paste0(. , "_", .x))
   .data <- .data %>% select(- {{subdf}})
  .data <- .data %>%
    bind_cols(test)
  return(.data)
}

unnestcol <- function(.data, item, ...){
  newidname <- paste0(substitute(.data), "_id")
   idrename <- colnames(.data) == "id"
  .data <- dplyr::select(.data, c( id, ({{item}})))

  colnames(.data)[idrename] <- newidname
  .data <- unnest(.data, {{item}})

  return(.data)
}

## Helpers for Postprocessing of resource data
check_col <- function(.data, col) {

  test <- sum(names(.data)==col) > 0

  return(test)
}

make_tidyresusers <- function(.data) {

  if(check_col(.data, "public_metrics")) .data <- .data %>%
      tweetbindcol(public_metrics) %>% rename_with(~ paste0("user_", .x), starts_with("public"))
  .data <- .data %>% rename(user_created_at = created_at)

  if(check_col(.data, "entities")) .data <- .data %>%
      tweetbindcol(entities)
  if(check_col(.data, "entities_description")) {
    .data <- .data %>%
      tweetbindcol(entities_description)

  }

  if(check_col(.data, "entities_description_hashtags")) {
    description_hashtags <- .data %>% unnestcol(entities_description_hashtags)
  } else description_hashtags <- tibble()

  if(check_col(.data, "entities_description_cashtags"))    {
    description_cashtags <- .data %>% unnestcol(entities_description_cashtags)

    } else description_cashtags <- tibble()
  if(check_col(.data, "entities_description_mentions"))    {
    description_mentions <- .data %>% unnestcol(entities_description_mentions)
    } else description_mentions <- tibble()
  if(check_col(.data, "entities_description_urls"))    {
    description_urls <- .data %>% unnestcol(entities_description_urls) %>%
      mutate(source = "description")
    } else description_urls <- tibble()

  if(check_col(.data, "entities_url")) {
    .data <- .data %>%    tweetbindcol(entities_url)
    urls <- .data %>% unnestcol(entities_url_urls) %>% mutate(source = "url")
  } else {
    entities_url <- tibble()
  }

  urls <- bind_rows(urls, description_urls)

  users <- .data %>% select(- starts_with("entities"))

  return(list(users = users,
              urls = urls,
              description_hashtags = description_hashtags,
              description_mentions = description_mentions,
              description_cashtags = description_cashtags
              ))


}


make_tidyrespoll <- function(.data) {
  if(dim(.data)[1]>0) .data <- .data %>% unnestcol(options)
return(.data)
}


make_tidyrestweets <- function(.data){
  # postprocess tweet-data
  # bind attached data frames
  if(check_col(.data, "public_metrics")) .data <- .data %>%
      tweetbindcol(public_metrics)
  if(check_col(.data, "entities")) .data <- .data %>%
      tweetbindcol(entities)
  if(check_col(.data, "attachments")) .data <- .data %>%
    tweetbindcol(attachments)

  if(sum(names(.data)=="geo")>0) {
    .data <- .data %>%   tweetbindcol(geo)
  }

  if(check_col(.data, "geo_coordinates")) {
    geo_coordinates  <- .data %>% unnestcol(geo_coordinates)
  } else {
    geo_coordinates <- tibble()
  }

  # unnest subdf's, referenced_tweets,
  #                 entities_hashtags, entities_cashtags, entities_mentions,
  #                 entities_annotations, entities_urls,
  #                 geo_coordinates
  if(check_col(.data, "entities_hashtags"))   {
    hashtags <- .data %>% unnestcol(entities_hashtags)
  } else {
    hashtags <- tibble()
  }

  if(check_col(.data, "entities_cashtags"))   {
    cashtags <- .data %>% unnestcol(entities_cashtags)
  } else {
    cashtags <- tibble()
  }

  if(check_col(.data, "entities_mentions"))   {
    mentions <- .data %>% unnestcol(entities_mentions)
  } else {
    mentions <- tibble()
  }

  if(check_col(.data, "entities_annotations"))   {
    annotations <- .data %>% unnestcol(entities_annotations)
  } else {
    annotations <- tibble()
  }

  if(check_col(.data, "entities_urls"))   {
  urls <- .data %>% unnestcol(entities_urls)
  } else {
    urls <- tibble()
  }

  tweets <- .data %>% select(- starts_with("entities"))


     df <- list(tweets = tweets,
                hashtags = hashtags,
                mentions = mentions,
                annotations = annotations,
                urls = urls,
                geo_coordinates = geo_coordinates)
     return(df)


}


# data -> bind : c(public_metrics, entities, geo, attachments, geo_coordinates)
# data -> unnest : c(referenced_tweets, hashtags, cashtags, mentions, annotations,urls,media_keys, coordinates)
# includes_tweets -> bind : c(public_metrics, entities, attachments)
# includes_tweets -> unnest : c(referenced_tweets, hashtags, cashtags, mentions, annotations,urls,media_keys)
# includes_media -> flat
# includes_places -> flat
# includes_polls -> unnest : c(options)
