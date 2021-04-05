# Title: Functions for Creating Tidy Data from Wikipedia API
# Authors: Taylor Arnold; Jiayi Du; Sabrina Duarte; Scarlett Sun
# Date: 04 April 2021

library(dplyr)
library(readr)
library(stringi)
library(xml2)

wiki_get_page <- function(page_name, lang = "en")
{
  if (!dir.exists(fp <- file.path(".cache", lang))) { dir.create(fp) }
  fname <- file.path(".cache", lang, paste0(page_name, ".json"))

  if (!file.exists(fname))
  {
    base_url <- sprintf("https://%s.wikipedia.org/w/api.php", lang)
    options <- "?action=parse&format=json&redirects&page="
    src_url <- paste0(base_url, options, URLencode(page_name))
    download.file(src_url, fname)

    message(sprintf("Downloading %s", page_name))
  }

  obj_json <- jsonlite::read_json(fname)
  return(obj_json)
}

wiki_add_data <- function(page_name, lang, data = list())
{
  obj <- wiki_get_page(page_name, lang)
  para <- .wiki_get_paragraph_xml(obj)

  data$text <- append(data$text, list(.wiki_tbl_text(obj, para)))
  data$link <- append(data$link, list(.wiki_tbl_link(obj, para)))

  data$meta <- append(data$meta, list(.wiki_tbl_meta(obj)))
  data$langlink <- append(data$langlink, list(.wiki_tbl_langlink(obj)))
  data$category <- append(data$category, list(.wiki_tbl_category(obj)))

  # data$template <- append(data$template, list(.wiki_tbl_template(obj)))
  # data$iwlink <- append(data$iwlink, list(.wiki_tbl_iwlink(obj)))
  # data$property <- append(data$property, list(.wiki_tbl_property(obj)))
  # data$image <- append(data$image, list(.wiki_tbl_image(obj)))

  return(data)
}

wiki_collapse_save <- function(data, prefix)
{
  for (nom in names(data))
  {
    df <- bind_rows(data[[nom]])
    write_csv(df, paste0(prefix, "_", nom, ".csv.gz"))
  }
}

.wiki_tbl_meta <- function(obj)
{
  df <- tibble(
    title = obj$parse$title,
    pageid = obj$parse$pageid,
    revid = obj$parse$revid,
    displaytitle = obj$parse$displaytitle
  )

  return(df)
}

.wiki_tbl_langlink <- function(obj)
{
  links_item <- vector("list", length(obj$parse$langlinks))

  for (i in seq_along(links_item)) {
    links_item[[i]] <- tibble(
      pageid = obj$parse$pageid,
      link_order = i,
      lang = obj$parse$langlinks[[i]]$lang,
      url = (obj$parse$langlinks[[i]]$url),
      langname = obj$parse$langlinks[[i]]$langname,
      autonym = obj$parse$langlinks[[i]]$autonym,
      star = obj$parse$langlinks[[i]]$`*`
    )
  }

  return(bind_rows(links_item))
}

.wiki_tbl_category <- function(obj)
{
  category_item <- vector("list", length = length(obj$parse$categories))

  for (i in seq_along(category_item))
  {
    category_item[[i]] <- tibble(
      pageid = obj$parse$pageid,
      record_num = i,
      sort_key = as.character(obj$parse$categories[[i]]$sortkey),
      hidden = !is.null(obj$parse$categories[[i]]$hidden),
      star = obj$parse$categories[[i]]$`*`
    )
  }

  return(bind_rows(category_item))
}

.wiki_tbl_template <- function(obj)
{

}

.wiki_tbl_iwlink <- function(obj)
{

}

.wiki_tbl_property <- function(obj)
{

}

.wiki_tbl_text <- function(obj, para)
{
  dt <- select(
    para$dt, pageid, pnum, section = h2, subsection = h3, text = text
  )

  nom <- unique(select(dt, section, subsection))
  nom <- group_by(nom, section) %>%
    mutate(snum = seq_along(subsection)) %>%
    mutate(snum = if_else(
      rep(any(subsection == "none"), length(subsection)), snum - 1L, snum)
    ) %>%
    ungroup()
  nom$num <- cumsum(!duplicated(nom$section))
  if (any(dt$section == "none")) { nom$num <- nom$num - 1L }

  nom$sec_num <- sprintf("%02d-%02d", nom$num, nom$snum)
  dt <- left_join(dt, nom, by = c("section", "subsection"))

  return(select(dt, pageid, sec_num, pnum, section, subsection, text))
}

.wiki_tbl_link <- function(obj, para)
{
  links <- lapply(para$inst, xml_find_all, ".//a")
  dt <- tibble(
    pageid = obj$parse$pageid,
    pnum = rep(seq_len(nrow(para$dt)), as.numeric(sapply(links, length))),
    link_text = unlist(lapply(links, xml_text)),
    link_href = unlist(lapply(links, xml_attr, "href"))
  )

  dt$type <- ""
  dt$type[stri_sub(dt$link_href, 1L, 6L) == "/wiki/"] <- "internal"
  dt$type[stri_sub(dt$link_href, 1L, 10L) == "#cite_note"] <- "cite"

  dt$internal <- stri_sub(dt$link_href, 7L, -1)
  dt$cite <- sapply(
    stri_split(dt$link_href, fixed = "-"), function(v) v[length(v)]
  )
  dt$internal[dt$type != "internal"] <- ""
  dt$cite[dt$type != "cite"] <- ""
  dt$cite <- as.integer(dt$cite)

  dt <- select(dt, pageid, pnum, link_text, type, internal, cite)
  return(dt)
}

.wiki_tbl_image <- function(obj)
{

}

.wiki_get_paragraph_xml <- function(obj)
{
  obj_html <- xml2::read_html(obj$parse$text[[1]])

  refs <- xml2::xml_find_all(obj_html, ".//sup/a")
  xml2::xml_text(refs) <- ""
  probs <- xml2::xml_find_all(obj_html, ".//span[not(@class)]")
  xml2::xml_text(probs) <- ""

  inst <- xml2::xml_find_all(
    obj_html, ".//*[self::p or self::h2[not(@id)] or self::h3[not(@id)]]"
  )
  tags <- xml2::xml_name(inst)
  text <- stringi::stri_replace_all(
    xml2::xml_text(inst), "", regex = "\\[[\\W\\w]+\\]"
  )

  p <- rep(NA_character_, length(tags))
  h2 <- rep(NA_character_, length(tags))
  h3 <- rep(NA_character_, length(tags))

  p[tags == "p"] <- text[tags == "p"]
  h2[tags == "h2"] <- text[tags == "h2"]
  h3[tags == "h3"] <- text[tags == "h3"]

  dt <- tibble::tibble(pageid =obj$parse$pageid, h2 = h2, h3 = h3, text = p)
  dt <- ungroup(tidyr::fill(group_by(dt, h2), h3))
  dt <- tidyr::fill(dt, h2)
  dt$h2[is.na(dt$h2)] <- "none"
  dt$h3[is.na(dt$h3)] <- "none"

  these <- which( (!is.na(dt$text)) & (stringi::stri_length(dt$text) > 25) )
  dt <- dt[these,]
  inst <- inst[these]
  dt$pnum <- seq_along(dt$pageid)

  return(list(dt = dt, inst = inst))
}
