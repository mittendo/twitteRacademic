# helpers.r
#' @importFrom rlang .data
#' @importFrom magrittr %>%

tweetbindcol <- function(.data, subdf) {
  # remark to developer: dplyr-programming: one need to @importFrom :=
  # to make this code more tidy
  nametest <- deparse(substitute(subdf))
  test <- .data[[substitute(subdf)]]  %>% dplyr::rename_with(~ paste0(nametest , "_", .x))

  # rename cols with df-name
   .data <- .data %>% dplyr::select(- {{subdf}})
  .data <- .data %>%
    dplyr::bind_cols(test)
  return(.data)
}

#' @importFrom rlang .data
unnestcol <- function(.data, item, ...){
  id <- NULL
  # remark to developer: dplyr-programming: one need to @importFrom :=
  # to make this code more tidy and readable
  nametest <- deparse(substitute(.data))

  newidname <- paste0("parent", "_id") #' muss noch angepasst werden und durch den Namen der übergeordneten Tabelle ersetzt werden

   .data <- dplyr::select(.data, c( id, {{item}}))
   idrename <- colnames(.data) == "id"

  colnames(.data)[idrename] <- newidname
  .data <- tidyr::unnest(.data, {{item}})

  return(.data)
}

## Helpers for Postprocessing of resource data
#' @importFrom rlang .data
check_col <- function(.data, col) {
  # probably it is better to use "col %in% colnames(.data)"
  # or assertthat::has_name(.data, col) for the tidy way
  test <- sum(names(.data)==col) > 0

  return(test)
}

#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom tidyselect all_of
#' @importFrom tibble tibble
make_tidyresusers <- function(.data) {
  # preventing R CMD check from complaining on global variables

  created_at <- entities <- entities_description <- NULL
  entities_description_hashtags <- entities_description_cashtags <- NULL
  entities_description_mentions <- entities_description_urls <- NULL
  entities_url_urls <- public_metrics <- NULL
  urls <- tibble()
  # remark to developer: it may possible to put the complicated if(check)
  # structure into check_col function...
  if(check_col(.data, "public_metrics")) .data <- .data %>%
      tweetbindcol(public_metrics) %>% dplyr::rename_with(~ paste0("user_", .x), dplyr::starts_with("public"))
  .data <- .data %>% dplyr::rename(user_created_at = all_of(created_at))

  if(check_col(.data, "entities")) .data <- .data %>%
      tweetbindcol(entities)
  if(check_col(.data, "entities_description")) {
    .data <- .data %>%
      tweetbindcol(entities_description)

  }

  if(check_col(.data, "entities_description_hashtags")) {
    description_hashtags <- unnestcol(.data, entities_description_hashtags)
  } else description_hashtags <- tibble::tibble()

  if(check_col(.data, "entities_description_cashtags"))    {
    description_cashtags <- unnestcol(.data, entities_description_cashtags)

    } else description_cashtags <- tibble::tibble()
  if(check_col(.data, "entities_description_mentions"))    {
    description_mentions <- unnestcol(.data, entities_description_mentions)
    } else description_mentions <- tibble::tibble()
  if(check_col(.data, "entities_description_urls"))    {
    description_urls <- unnestcol(.data, entities_description_urls) %>%
      dplyr::mutate(source = "description")
    } else description_urls <- tibble::tibble()

  if(check_col(.data, "entities_url")) {
    .data <- .data %>%    tweetbindcol(entities_url)
    urls <- unnestcol(.data, entities_url_urls) %>% dplyr::mutate(source = "url")
  } else {
    entities_url <- tibble::tibble()
  }

  urls <- dplyr::bind_rows(urls, description_urls)

  users <- .data %>% dplyr::select(- dplyr::starts_with("entities"))

  return(list(users = users,
              urls = urls,
              description_hashtags = description_hashtags,
              description_mentions = description_mentions,
              description_cashtags = description_cashtags
              ))


}


#' @importFrom rlang .data
#' @importFrom magrittr %>%
make_tidyrespoll <- function(.data) {
  if(dim(.data)[1]>0) .data <- unnestcol(.data, options)
return(.data)
}


#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
make_tidyrestweets <- function(.data){
  public_metrics <- entities <- attachments <-
    geo <- geo_coordinates <-
    geo_coordinates_coordinates <- entities_hashtags <-
    entities_cashtags <- entities_mentions <-
    entities_annotations <- entities_urls <-
    domain <- entity <-
    attachments_media_keys <- attachments_poll_ids <- NULL
   referenced_tweets <- tibble()
  # postprocess tweet-data
  # bind attached data frames
  if(check_col(.data, "public_metrics")) .data <- .data %>%
      tweetbindcol(public_metrics)
  if(check_col(.data, "entities")) .data <- .data %>%
      tweetbindcol(entities)
  if(check_col(.data, "attachments")) .data <- .data %>%
    tweetbindcol(attachments)

  if(sum(names(.data)=="geo")>0) {
     .data <- .data %>%   tweetbindcol(geo) # %>%
    #   tweetbindcol(geo_coordinates) %>% tweetbindcol(coordinates)
    if(check_col(.data, "geo_coordinates")) {
     .data <- .data %>% tweetbindcol(geo_coordinates)
     .data <- .data %>%
       dplyr::mutate(lon=purrr::map_dbl(geo_coordinates_coordinates,1, .default=NA),
              lat=purrr::map_dbl(geo_coordinates_coordinates,2, .default=NA)) %>%
       dplyr::select(-geo_coordinates_coordinates)
    }
  }


  # unnest subdf's, referenced_tweets,
  #                 entities_hashtags, entities_cashtags, entities_mentions,
  #                 entities_annotations, entities_urls,
  #                 geo_coordinates
  if(check_col(.data, "entities_hashtags"))   {
    #print(.data$entities_hashtags)
    hashtags <- unnestcol(.data, entities_hashtags)
  } else {
    hashtags <- tibble::tibble()
  }

  if(check_col(.data, "entities_cashtags"))   {
    cashtags <- unnestcol(.data, entities_cashtags)
  } else {
    cashtags <- tibble::tibble()
  }


 if(check_col(.data, "referenced_tweets")) {
   referenced_tweets <- unnestcol(.data, referenced_tweets)

 }

  if(check_col(.data, "entities_mentions"))   {
    mentions <- unnestcol(.data, entities_mentions)
  } else {
    mentions <- tibble::tibble()
  }

  if(check_col(.data, "entities_annotations"))   {
    annotations <- unnestcol(.data, entities_annotations)
  } else {
    annotations <- tibble::tibble()
  }
  if(check_col(.data, "context_annotations"))   {
    context_annotations <- unnestcol(.data, context_annotations) %>%
      tweetbindcol(domain) %>%
      tweetbindcol(entity)


  } else {
    context_annotations <- tibble::tibble()
  }


  if(check_col(.data, "entities_urls"))   {
  urls <- unnestcol(.data, entities_urls)
  } else {
    urls <- tibble::tibble()
  }



  if(check_col(.data, "attachments_media_keys")) {
    #attachments_media_keys <- unnestcol(attachments_media_keys)
   .data <-  .data %>% dplyr::mutate(attachments_media_keys =
                       purrr::map_chr(attachments_media_keys, 1, .default=NA))
  }


  if(check_col(.data, "attachments_poll_ids")){
    .data <-  .data %>% dplyr::mutate(attachments_poll_ids =
                                        purrr::map_chr(attachments_poll_ids, 1, .default=NA))
  }


  tweets <- .data %>% dplyr::select(- dplyr::starts_with("entities"),
                                    - dplyr::starts_with("referenced_tweets"),
                                    - dplyr::starts_with("context"))


     df <- list(tweets = tweets,
                hashtags = hashtags,
                mentions = mentions,
                annotations = annotations,
                referenced_tweets = referenced_tweets,
                context_annotations = context_annotations,
                urls = urls)
     return(df)


}

