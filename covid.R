
# https://github.com/Test01DezWebSite/covid/blob/master/covid.Rmd

## Preparation


install.packages("readr")
library(readr)

install.packages("tidyr")
library(tidyr)

install.packages("lubridate")
library(lubridate)

install.packages("here")
library(here)

install.packages("janitor")
library(janitor)

install.packages("socviz")
library(socviz)

install.packages("ggrepel")
library(ggrepel)

install.packages("paletteer")
library(paletteer)

#install.packages(c('devtools','curl'))
#library(devtools)

install.packages("devtools")
devtools::install_github("hadley/tidyverse")

install.packages("dplyr")
library(dplyr)

## --------------------------------------------------------------------
## Custom font and theme, omit if you don't have the myriad library
## (https://github.com/kjhealy/myriad) and associated Adobe fonts.
## --------------------------------------------------------------------

install.packages("showtext")
library(showtext)
showtext_auto()

#install.packages("myriad")
#library(myriad)
#import_myriad_semi()

install.packages("cowplot")
library(cowplot)

theme_covid <- function() {
    #theme_myriad_semi() +
    theme(plot.title = element_text(size = rel(2), face = "bold"),
          plot.subtitle = element_text(size = rel(1.5)),
          plot.caption = element_text(size = rel(0.9)),
          axis.text.y = element_text(size = rel(2)),
          axis.title.x = element_text(size = rel(1.5)),
          axis.title.y = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(2)),
          legend.text = element_text(size = rel(2))
          )
}


#theme_set(theme_covid())
theme_set(theme_cowplot())

### --------------------------------------------------------------------


## cb-friendly palette
col_pal <- c("#E69F00", "#0072B2", "#000000", "#56B4E9",
             "#009E73", "#F0E442", "#D55E00", "#CC79A7")



## Either Download today's CSV file, saving it to data/ and also read it in
get_ecdc_csv <- function(url = "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                          date = lubridate::today(),
                          writedate = lubridate::today(),
                          fname = "ecdc-cumulative-",
                          ext = "csv",
                          dest = "data") {

  target <- url
  message("target: ", target)

  destination <- fs::path(here::here("data"), paste0(fname, writedate), ext = ext)
  message("saving to: ", destination)

  tf <- tempfile(fileext = ext)
  curl::curl_download(target, tf)
  fs::file_copy(tf, destination)

  janitor::clean_names(readr::read_csv(tf))
}


## OR Download today's excel file, saving it to data/ and reading it in
get_ecdc_data <- function(url = "https://www.ecdc.europa.eu/sites/default/files/documents/",
                          fname = "COVID-19-geographic-distribution-worldwide-",
                          date = lubridate::today(),
                          ext = "xlsx",
                          dest = "data") {

  target <-  paste0(url, fname, date, ".", ext)
  message("target: ", target)

  destination <- fs::path(here::here("data"), paste0(fname, date), ext = ext)
  message("saving to: ", destination)

  tf <- tempfile(fileext = ext)
  curl::curl_download(target, tf)
  fs::file_copy(tf, destination)

  switch(ext,
         xls = janitor::clean_names(readxl::read_xls(tf)),
         xlsx = janitor::clean_names(readxl::read_xlsx(tf))
  )
}

## A useful function from Edward Visel, which does a thing
## with tibbles that in the past I've done variable-by-variable
## using match(), like an animal. The hardest part was
## figuring out that this operation is called a coalescing join
## https://alistaire.rbind.io/blog/coalescing-joins/
coalesce_join <- function(x, y,
                          by = NULL, suffix = c(".x", ".y"),
                          join = dplyr::full_join, ...) {
    joined <- join(x, y, by = by, suffix = suffix, ...)
    # names of desired output
    cols <- dplyr::union(names(x), names(y))

    to_coalesce <- names(joined)[!names(joined) %in% cols]
    suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
    # remove suffixes and deduplicate
    to_coalesce <- unique(substr(
        to_coalesce,
        1,
        nchar(to_coalesce) - nchar(suffix_used)
    ))

    coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
        joined[[paste0(.x, suffix[1])]],
        joined[[paste0(.x, suffix[2])]]
    ))
    names(coalesced) <- to_coalesce

    dplyr::bind_cols(joined, coalesced)[cols]
}



## Country codes. The ECDC does not quite use standard codes for countries
## These are the iso2 and iso3 codes, plus some convenient groupings for
## possible use later
## Thus we set up some country codes using ISO2 and ISO3 abbreviations.
iso3_cnames <- read_csv("data/countries_iso3.csv")
iso2_to_iso3 <- read_csv("data/iso2_to_iso3.csv")

cname_table <- left_join(iso3_cnames, iso2_to_iso3)

cname_table

eu <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
        "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
        "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")

europe <- c("ALB", "AND", "AUT", "BLR", "BEL", "BIH", "BGR", "HRV", "CYP", "CZE",
        "DNK", "EST", "FRO", "FIN", "FRA", "DEU", "GIB", "GRC", "HUN", "ISL",
        "IRL", "ITA", "LVA", "LIE", "LTU", "LUX", "MKD", "MLT", "MDA", "MCO",
        "NLD", "NOR", "POL", "PRT", "ROU", "RUS", "SMR", "SRB", "SVK", "SVN",
        "ESP", "SWE", "CHE", "UKR", "GBR", "VAT", "RSB", "IMN", "MNE")

north_america <- c("AIA", "ATG", "ABW", "BHS", "BRB", "BLZ", "BMU", "VGB", "CAN", "CYM",
        "CRI", "CUB", "CUW", "DMA", "DOM", "SLV", "GRL", "GRD", "GLP", "GTM",
        "HTI", "HND", "JAM", "MTQ", "MEX", "SPM", "MSR", "ANT", "KNA", "NIC",
        "PAN", "PRI", "KNA", "LCA", "SPM", "VCT", "TTO", "TCA", "VIR", "USA",
        "SXM")

south_america <- c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "FLK", "GUF", "GUY", "PRY",
                   "PER", "SUR", "URY", "VEN")


africa <- c("DZA", "AGO", "SHN", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF",
        "TCD", "COM", "COG", "DJI", "EGY", "GNQ", "ERI", "ETH", "GAB", "GMB",
        "GHA", "GNB", "GIN", "CIV", "KEN", "LSO", "LBR", "LBY", "MDG", "MWI",
        "MLI", "MRT", "MUS", "MYT", "MAR", "MOZ", "NAM", "NER", "NGA", "STP",
        "REU", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SHN", "SDN",
        "SWZ", "TZA", "TGO", "TUN", "UGA", "COD", "ZMB", "TZA", "ZWE", "SSD",
        "COD")

asia <- c("AFG", "ARM", "AZE", "BHR", "BGD", "BTN", "BRN", "KHM", "CHN", "CXR",
        "CCK", "IOT", "GEO", "HKG", "IND", "IDN", "IRN", "IRQ", "ISR", "JPN",
        "JOR", "KAZ", "PRK", "KOR", "KWT", "KGZ", "LAO", "LBN", "MAC", "MYS",
        "MDV", "MNG", "MMR", "NPL", "OMN", "PAK", "PHL", "QAT", "SAU", "SGP",
        "LKA", "SYR", "TWN", "TJK", "THA", "TUR", "TKM", "ARE", "UZB", "VNM",
        "YEM", "PSE")

oceania <- c("ASM", "AUS", "NZL", "COK", "FJI", "PYF", "GUM", "KIR", "MNP", "MHL",
        "FSM", "UMI", "NRU", "NCL", "NZL", "NIU", "NFK", "PLW", "PNG", "MNP",
        "SLB", "TKL", "TON", "TUV", "VUT", "UMI", "WLF", "WSM", "TLS")

#sink("cname_table_12042020.txt");cname_table;sink()
#write.csv(cname_table, "cname_table_12042020.csv")

## European Centers for Disease Control Data
## There's a typo in the file name. Note 'disbtribution' rather than 'distribution'
## e.g. COVID-19-geographic-disbtribution-worldwide-2020-03-18.xls
## I assume this'll get fixed or changed at some point.
## The good thing is that these data files are cumulative


## Now Actually Get the Data

## on 3/25/20 they changed the file name to omit the date
#covid_raw0 <- get_ecdc_data(url = "https://www.ecdc.europa.eu/sites/default/files/documents/",
#                            fname = "COVID-19-geographic-disbtribution-worldwide-",
#                            ext = "xlsx")

## on 3/27/2020 they broadened the data formats and changed the URLS

#For some reason, cannot install curl package, thus downloading file manually
# source https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx
#covid_raw <- get_ecdc_csv()

# source: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
# OR https://opendata.ecdc.europa.eu/covid19/casedistribution/json/
# for today’s data on the geographic distribution of COVID-19 cases worldwide
covid_raw <- read.csv("data/COVID-19-geographic-disbtribution-worldwide_17052020.csv", sep=',')
head(covid_raw, 2)

#write_csv(covid_raw, file.path(dir, "covid_raw.csv"))

## on 3/27/20 they changed the contents of the file too, including the date format
covid <- covid_raw %>%
  mutate(date = lubridate::dmy(dateRep),
         iso2 = geoId)

tail(covid, 3)
#sink('covid_13022020.txt'); covid; sink()
#write_csv(covid, "covid_13022020.csv")

## merge in the iso country names
covid <- left_join(covid, cname_table)

covid

#sink('covid1_13022020.txt'); covid; sink()
#write_csv(covid, "covid1_21122020.csv")

## Looks like a missing data code
## Also note that not everything in this dataset is a country
covid %>%
  filter(cases == -9)


## A few ECDC country codes are non-iso, notably the UK
anti_join(covid, cname_table) %>%
  select(geoId, countriesAndTerritories, iso2, iso3, cname) %>%
  distinct()


## A small crosswalk file that we'll coalesce into the missing values
## We need to specify the na explicity because the xwalk file has Namibia
## as a country -- i.e. country code = string literal "NA"
cname_xwalk <- read_csv("data/ecdc_to_iso2_xwalk.csv",
                        na = "")

colnames(cname_xwalk)[which(names(cname_xwalk) == "geo_id")] <- "geoId"

cname_xwalk

## How to do this manually
covid <- covid %>%
   left_join(cname_xwalk, by = "geoId") %>%
   mutate(iso3 = coalesce(iso3.x, iso3.y),
          cname = coalesce(cname.x, cname.y)) %>%
   select(-iso3.x, -iso3.y, cname.x, cname.y)

## But nicer to use the function from Edward Visel
#covid <- coalesce_join(covid, cname_xwalk,
#                       by = "records.geoId", join = dplyr::left_join)


## Take a look again
anti_join(covid, cname_table) %>%
  select(geoId, countriesAndTerritories, iso2, iso3, cname) %>%
  distinct()


## cumulative cases for selected countries
cu_out <- covid %>%
  filter(iso3 %in% c("DNK", "NOR", "SWE", "FIN",
                     "ISL")) %>%
  group_by(cname) %>%
  arrange(date) %>%
  mutate(cases_na = na_if(cases, 0),
         deaths_na = na_if(deaths,0),
         cu_cases = cumsum(cases),
         cu_deaths = cumsum(deaths)) %>%
  select(date, cname, cu_cases, cu_deaths) %>%
  pivot_longer(cu_cases:cu_deaths,
               names_to = "measure", values_to = "count") %>%
  mutate(measure = recode(measure, `cu_cases` = "Cases",
                         `cu_deaths` = "Deaths")) %>%
  mutate(count = na_if(count, 0))

cu_out %>%
  arrange(desc(date))

#sink('covid2_17052020.txt'); cu_out; sink()
#write_csv(cu_out, "covid2_21122020.csv")


cu_out %>%
  ggplot(mapping = aes(x = date, y = count,
                       color = measure)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = col_pal) +
#  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  scale_y_continuous(trans = "log2",
                     labels = scales::comma_format(accuracy = 1),
                     breaks = 2^c(seq(1, 17, 2))) +
  # scale_y_continuous(trans = "log10",
  #                    labels = scales::comma_format(accuracy = 1)) +
  labs(title = paste0("COVID-19 Cumulative Recorded Cases and Deaths to ",
                      max(cu_out$date)),
       x = "Date", y = "Cumulative Reported Events (log 2 scale)", color = "Measure") +
  facet_wrap(~ reorder(cname, -count, na.rm = TRUE)) +
  theme(legend.position = "top")

## Just deaths
cu_out %>%
  filter(measure == "Deaths") %>%
  ggplot(mapping = aes(x = date, y = count)) +
  geom_line(size = 1.5) +
#  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  scale_y_continuous(trans = "log2",
                     labels = scales::comma_format(accuracy = 1),
                     breaks = 2^c(seq(1, 17, 2))) +
  # scale_y_continuous(trans = "log10",
  #                    labels = scales::comma_format(accuracy = 1)) +
  labs(title = paste0("COVID-19 Cumulative Recorded Deaths to ",
                      max(cu_out$date)),
       x = "Date", y = "Cumulative Reported Events (log 2 scale)") +
  facet_wrap(~ reorder(cname, -count, na.rm = TRUE)) +
  theme(legend.position = "top")


## The graph everyone draws ;)
cov_curve <- covid %>%
  select(date, cname, iso3, cases, deaths) %>%
  drop_na(iso3) %>%
  group_by(iso3) %>%
  arrange(date) %>%
  mutate(cu_cases = cumsum(cases),
         cu_deaths = cumsum(deaths)) %>%
  filter(cu_deaths > 9) %>%
  mutate(days_elapsed = date - min(date),
          end_label = ifelse(date == max(date), cname, NA),
          end_label = recode(end_label, `Belgium` = "Belgium",
                        `Rwanda` = "Rwanda",
                        `Sweden` = "Sweden",
                        `France` = "France"),
         cname = recode(cname, `Belgium` = "Belgium",
                        `Rwanda` = "Rwanda",
                        `Sweden` = "Sweden",
                        `France` = "France"))

cov_curve

#sink('cov_curve_17052020.txt'); cov_curve; sink()
#write.csv(cov_curve,"cov_curve_21122020.csv")


focus_cn <- c("DNK", "NOR", "SWE", "FIN", "ISL")

## Colors
cgroup_cols <- c(prismatic::clr_darken(paletteer_d("ggsci::category20_d3"), 0.2)[1:length(focus_cn)], "gray70")


(p_death_curve <- cov_curve %>%
  mutate(end_label = case_when(iso3 %in% focus_cn ~ end_label,
                               TRUE ~ NA_character_),
         cgroup = case_when(iso3 %in% focus_cn ~ iso3,
                            TRUE ~ "ZZOTHER")) %>%
  ggplot(mapping = aes(x = days_elapsed, y = cu_deaths,
         color = cgroup, label = end_label,
         group = cname)) +
  geom_line(size = 0.5) +
  geom_text_repel(nudge_x = 1.1,
                  nudge_y = 0.1,
                  segment.color = NA) +
  guides(color = FALSE) +
  scale_color_manual(values = cgroup_cols) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                     breaks = 2^seq(4, 14),
                     trans = "log2") +
  labs(x = "Days Since 10th Confirmed Death",
       y = "Cumulative Number of Deaths (log2 scale)",
       title = "Cumulative Number of Reported Deaths from COVID-19, Selected Countries",
       subtitle = paste("Data as of", format(max(cov_curve$date), "%A, %B %e, %Y")),
        caption = "@DesireYavro / Data: https://www.ecdc.europa.eu/")
  #caption = "@DesireYavro inspired by @kjhealy / Data: https://www.ecdc.europa.eu/")
)

cov_curve %>%
  group_by(iso3) %>%
  filter(cu_deaths == max(cu_deaths)) %>%
  arrange(desc(cu_deaths)) %>%
  select(cname, cu_cases, cu_deaths, days_elapsed)

#sink('cov_curve1_17052020.txt'); cov_curve; sink()
#write.csv(cov_curve,"cov_curve1_21122020.csv")

cov_case_curve <- covid %>%
  select(date, cname, iso3, cases, deaths) %>%
  drop_na(iso3) %>%
  group_by(iso3) %>%
  arrange(date) %>%
  mutate(cu_cases = cumsum(cases),
         cu_deaths = cumsum(deaths)) %>%
  filter(cu_cases > 99) %>%
  mutate(days_elapsed = date - min(date),
          end_label = ifelse(date == max(date), cname, NA),
          end_label = recode(end_label, `Denmark` = "Denmark",
                        `Norway` = "Norway",
                        `Sweden` = "Sweden",
                        `Finland` = "Finland",
                        `Iceland` = "Iceland"),
         cname = recode(cname, `Denmark` = "Denmark",
                        `Norway` = "Norway",
                        `Sweden` = "Sweden",
                        `Finland` = "Finland",
                        `Iceland` = "Iceland"))

#write.csv(cov_case_curve,"cov_case_curve_21122020.csv")

(p_case_curve <- cov_case_curve %>%
  mutate(end_label = case_when(iso3 %in% focus_cn ~ end_label,
                               TRUE ~ NA_character_),
         cgroup = case_when(iso3 %in% focus_cn ~ iso3,
                            TRUE ~ "ZZOTHER")) %>%
  ggplot(mapping = aes(x = days_elapsed, y = cu_cases,
         color = cgroup, label = end_label,
         group = cname)) +
  geom_line(size = 0.5) +
  geom_text_repel(nudge_x = 0.75,
                  segment.color = NA) +
  guides(color = FALSE) +
  scale_color_manual(values = cgroup_cols) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                     breaks = 2^seq(4, 18, 1),
                     trans = "log2") +
  labs(x = "Days Since 100th Confirmed Case",
       y = "Cumulative Number of Reported Cases (log2 scale)",
       title = "Cumulative Reported Cases of COVID-19, Selected Countries",
       subtitle = paste("ECDC data as of", format(max(cov_curve$date), "%A, %B %e, %Y")),
       caption = "@DesireYavro / Data: https://www.ecdc.europa.eu/")
  # caption = "@DesireYavro inspired by Kieran Healy @kjhealy / Data: https://www.ecdc.europa.eu/")
)


#ggsave("figures/cov_case_grouped_21122020.png", p_case_curve,
#       width = 10, height = 8, dpi = 300)


## Replicate JB Murdoch's small multiple, only don't alphabetize countries.
## https://www.ft.com/coronavirus-latest

## Top N countries by >> 100 cases, let's say.
## OR better, the last N=50 countries by number >> cases
n_countries0 <- -50
#n_countries0 <- 30

country_panels <- cov_case_curve %>%
  group_by(cname) %>%
  filter(cu_cases == max(cu_cases)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  top_n(n_countries0, cu_cases) %>%
  select(iso3, cname, cu_cases) %>%
  mutate(days_elapsed = 1,
             cu_cases = max(cov_case_curve$cu_cases) - 9e4)

country_panels
#write.csv(country_panels,"country_panels_21122020.csv")

cov_case_curve_bg <- cov_case_curve %>%
  select(-cname) %>%
  filter(iso3 %in% country_panels$iso3)

cov_case_curve_endpoints <- cov_case_curve %>%
  filter(iso3 %in% country_panels$iso3) %>%
  group_by(iso3) %>%
  arrange(desc(date)) %>%
  filter(cu_cases == max(cu_cases)) %>%
  filter(row_number() == 1) %>%
  select(cname, iso3, days_elapsed, cu_cases) %>%
  ungroup()

## color blue instead of firebrick & fill firebrick2
(p_cov_case_sm <- cov_case_curve  %>%
  filter(iso3 %in% country_panels$iso3) %>%
  ggplot(mapping = aes(x = days_elapsed, y = cu_cases)) +
  geom_line(data = cov_case_curve_bg,
            aes(group = iso3),
            size = rel(0.2), color = "gray80") +
  geom_line(color = "blue",
            lineend = "round") +
  geom_point(data = cov_case_curve_endpoints,
             size = rel(1),
             shape = 21,
             color = "blue",
             fill = "blue"
             ) +
  geom_text(data = country_panels,
             mapping = aes(label = cname),
             vjust = "inward",
             hjust = "inward",
             fontface = "bold",
             color = "blue",
             size = rel(1.85)) +
  scale_y_log10(labels = scales::label_number_si()) +
  facet_wrap(~ reorder(cname, -cu_cases), ncol = 5) +
  labs(x = "Days Since 100th Confirmed Case",
       y = "Cumulative Number of Cases (log10 scale)",
       title = "Cumulative Number of COVID-19 Cases for bottom 50 countries by >> 100 cases",
       subtitle = paste("Data as of", format(max(cov_curve$date), "%A, %B %e, %Y")),
        caption = "@DesireYavro / Data: https://www.ecdc.europa.eu/") +
  theme(plot.title = element_text(size = rel(1), face = "bold"),
          plot.subtitle = element_text(size = rel(0.7)),
          plot.caption = element_text(size = rel(1)),
          strip.text = element_blank(),
          panel.spacing.x = unit(-0.05, "lines"),
          panel.spacing.y = unit(0.3, "lines"),
          axis.text.y = element_text(size = rel(0.5)),
          axis.title.x = element_text(size = rel(1)),
          axis.title.y = element_text(size = rel(1)),
          axis.text.x = element_text(size = rel(0.5)),
          legend.text = element_text(size = rel(1)))
)


ggsave("figures/cov_case_sm_21122020.pdf",
       p_cov_case_sm, width = 8, height = 12)

ggsave("figures/cov_case_sm_21122020.png",
       p_cov_case_sm, width = 8, height = 12, dpi = 300)



## Plotly Example
install.packages("plotly")
library(plotly)

country_plotly <- cov_case_curve %>%
  ungroup() %>%
  select(date, cname, cu_cases, cu_deaths, days_elapsed) %>%
  rename(Country = cname, Date = date, Cases = cu_cases, Deaths = cu_deaths, Days = days_elapsed)

country_plotly <- highlight_key(country_plotly, ~ Country)

# theme_minimal replaced by cowplot
(p_cov_case_sm <-
  ggplot(data = country_plotly,
         mapping = aes(x = Days, y = Cases, group = Country)) +
  geom_line(lineend = "round", size = 0.15) +
  scale_y_log10(labels = scales::label_number_si()) +
  labs(x = "Days Since 100th Confirmed Case",
       y = "Cumulative Number of Cases (log10 scale)") +
    theme_cowplot()
)

gg <- ggplotly(p_cov_case_sm, tooltip = c("Country", "Days", "Cases"))

plotly_cases <- layout(highlight(gg),
                        font = "inherit")

# theme_minimal replaced by cowplot
(p_cov_deaths_sm <-
  ggplot(data = country_plotly,
         mapping = aes(x = Days, y = Deaths, group = Country)) +
  geom_line(lineend = "round", size = 0.15) +
  scale_y_log10(labels = scales::label_number_si()) +
  labs(x = "Days Since 10th Confirmed Death",
       y = "Cumulative Number of Deaths (log10 scale)") +
    theme_cowplot()
)

gg <- ggplotly(p_cov_deaths_sm, tooltip = c("Country", "Days", "Deaths"))

plotly_deaths <- layout(highlight(gg), font = "inherit")
