library(inventory)

prj_path <- function (...) {
  file.path(rprojroot::find_root("DESCRIPTION"), ...)
}

import_annual_data_ <- function (file) {
  prj_path("data-raw", file) %>%
    read_csv(col_types = "icicdddddddddd") %>%
    ensure(all_true(.$season == "Annual")) %>%
    select(-season, -cat_type) %>%
    rename(SO2 = SOx) %>%
    ensure_distinct(year, cat_id) %>%
    gather(pol_abbr, ems_qty, PM, TOG, NOx, SO2, CO) %>%
    mutate(ems_qty = parse_double(ems_qty)) %>%
    filter(ems_qty > 0) %>%
    mutate(pol_abbr = as.character(pol_abbr), ems_unit = "tons/day") %>%
    select(year, cat_id, pol_abbr, ems_qty, ems_unit) %>%
    ensure_distinct(year, cat_id, pol_abbr) %>%
    ensure(is.integer(.$cat_id))
}

BY2002_P_data <-
  import_annual_data_("BY2002_Pointtpd.csv") %>%
  mutate(cat_id = str_c("P", cat_id))

BY2002_A_data <-
  import_annual_data_("BY2002_Areatpd.csv") %>%
  mutate(cat_id = str_c("A", cat_id))

BY2002_annual <-
  bind_rows(BY2002_P_data, BY2002_A_data) %>%
  convert_units(from = "tons/day", to = "tons/yr") %>%
  select(year, cat_id, pol_abbr, ems_qty, ems_unit) %>%
  with_comment("BY2002 emissions, by category.")

# Save the datasets to the same .Rda file
devtools::use_data(
  BY2002_annual,
  overwrite = TRUE)
