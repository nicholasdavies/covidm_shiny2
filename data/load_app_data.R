
# load in the population data
cm_force_rebuild = F;
cm_build_verbose = F;
cm_path <- "./"
source(paste0(cm_path, "/R/covidm.R"))

if (!file.exists(paste0(cm_path,"data/population_info.rds"))){
    populations <- data.table(readRDS(paste0(cm_path,"data/wpp2019_pop2020.rds")))
    populations_ <- unique(populations[,"name"], by = c("name"))
    populations_[,iso3n := sp_name_to_iso3(name)
                 ][,iso3c := countrycode::countrycode(sourcevar = iso3n,
                                                      origin = "iso3n",
                                                      destination = "iso3c")]
    populations <- merge(populations, na.omit(populations_))
    saveRDS(populations, file = paste0(cm_path,"data/population_info.rds"))
} else {
    populations <- readRDS(file = paste0(cm_path,"data/population_info.rds"))
}
populations_names_parent <- unique(
    sub(x = grep(pattern = "\\|",
                 x = populations$name,
                 value = T),
        pattern = "\\s\\|.*",
        replacement = ""))
populations_names <- unique(
    populations[
        #grepl("\\|", name)
        ][, c("name", "country_code", "iso3n", "iso3c")])

if (!file.exists(paste0(cm_path,"data/wb_region.rds"))){
    require(wbstats)
    wbcountries <- wbstats::wbcountries()
    wb_region   <- wbcountries[,c("iso3c", "region")]
    wb_region <- dplyr::filter(wb_region, region != "Aggregates")
    wb_region <- dplyr::mutate(wb_region, 
                               region = ifelse(region == "North America", 
                                               "Northern America", region))
    saveRDS(wb_region, file = paste0(cm_path,"data/wb_region.rds"))
} else {
    wb_region <- readRDS(file = paste0(cm_path,"data/wb_region.rds"))
}
regions <- c("All", sort(unique(wb_region$region)))

populations <- merge(
    populations, 
    wb_region,
    by = "iso3c")[,
                  region := forcats::fct_explicit_na(f = factor(region, 
                                                                levels = c(regions, "Unknown")),
                                                     na_level = "Unknown")]
#matrices    <- readRDS("../data/all_matrices.rds")
# now we need a table to go from the `cm_matrices` name to an iso3c code so we can do matching
cm_matrix_names <- data.table(cm_matrices_name = names(cm_matrices))
cm_matrix_names[ , base_name := gsub(pattern = "\\s\\|\\s.*", replacement = "\\1", x = cm_matrices_name)]
cm_matrix_names[ , base_country.name.en := unlist(lapply(base_name, sp_name_to_country_name_en))]
cm_matrix_names[ , iso3c := countrycode::countrycode(base_country.name.en,
                                                    "country.name.en",
                                                    "iso3c")
                 ][,
                   base_un.name.en := countrycode::countrycode(iso3c,
                                                             "iso3c",
                                                             "un.name.en")]

cm_matrix_names[ , cm_matrices_input_name := ifelse(grepl(pattern = "(\\||Mossong|Melegaro|Leung)",
                                                          x = cm_matrices_name),
                                                    cm_matrices_name,
                                                    base_un.name.en) ]
cm_matrix_names <- cm_matrix_names %>% dplyr::filter(!grepl("\\| | \\(", cm_matrices_input_name))



## matrix lookup table
matrix_lookup = readRDS(paste0(cm_path,"data/lookup_table_contactmatrix.rds"))
eligible_iso3 = matrix_lookup[!is.na(population_name) & !is.na(matrix_name), country_code_iso_alpha3]

populations <- populations[ iso3c %in% eligible_iso3, ]

populations_names <- populations_names[ iso3c %in% eligible_iso3, ]


## load interventions
intervention_options <- list(
    `None`              = function(s) list(),
    `School Closures`   = function(s) list(contact = c(1.0, 1.0, 1-s, 1.0,  1.0, 1.0, 1-s, 1.0)),
    `Social Distancing` = function(s) list(contact = c(1.0, 1-s, 1.0, 1-s,  1.0, 1-s, 1.0, 1-s)),
    `Elderly Shielding` = function(s) list(contact = c(1.0, 1.0, 1.0, 1.0,  1.0, 1-s, 1.0, 1-s)),
    `Self-Isolation`    = function(s) list(fIs = rep(1-s, 16)),
    `Lockdown`          = function(s) list(contact = c(1.0, 1-s, 1-s, 1-s,  1.0, 1-s, 1-s, 1-s))
)

# raw disease progression rates
health_burden_probs <- fread(
            "AgeUpper, Prop_fatal,Prop_symp_hospitalised,Prop_hospitalised_critical
                   10,          0,                     0,                       0.3
                   20,   9.47E-04,           0.007615301,                       0.3
                   30,0.001005803,           0.008086654,                       0.3
                   40,0.001231579,           0.009901895,                       0.3
                   50,0.002305449,           0.018535807,                       0.3
                   60,0.006754596,           0.054306954,                       0.3
                   70,0.018720727,           0.150514645,                       0.3
                   80,0.041408882,           0.332927412,                       0.3
                  100,0.076818182,           0.617618182,                       0.3")

# user's country code
user_countrycode = "GBR";#countrycode(geoloc::wtfismyip()$Your_CountryCode, origin = "iso2c", dest = "iso3c")

# Load susceptibility and clinical fraction
covid_scenario = qread("data/covuy.qs")
covu = unname(rep(colMeans(covid_scenario[,  5:12]), each = 2));
covy = unname(rep(colMeans(covid_scenario[, 13:20]), each = 2));
