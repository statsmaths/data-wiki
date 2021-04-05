source("funs.R")

page_list <- readr::read_csv(file.path("data", "page_list.csv"))

for (this_group in unique(page_list$group))
{
  page_group <- page_list[page_list$group == this_group]

  data <- NULL
  for (i in seq_along(page_group$name))
  {
    data <- wiki_add_data(page_group$name[i], page_group$lang[i], data)
  }

  wiki_collapse_save(data, file.path("data", this_group, this_group))
}
