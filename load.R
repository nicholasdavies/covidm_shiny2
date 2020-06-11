library(data.table)
library(ggplot2)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyjqui)
library(shinythemes)
library(shinyWidgets)
library(rhandsontable)
library(stringr)
library(leaflet)
library(NCoVUtils)
library(yaml)
library(qs)


# COVIDM
cm_path = "./covidm/";
cm_force_rebuild = F;
cm_build_verbose = T;
cm_version = 2;
source(paste0(cm_path, "/R/covidm.R"))
source("./vvplot.R")

shiny_observer = c(
    "dyn.Obs(t, 0, 0, 0) = estimate_R0(P, t, 0, 50);",
    "dyn.Obs(t, 0, 0, 1) = estimate_Rt(P, dyn, t, 0, 50);"
);

# TODO if t > time_min && dyn(cases) < 1e-3

cm_source_backend(
    user_defined = list(
        model_v2 = list(
            cpp_observer = shiny_observer
        )
    )
);


# Health burden processes
probs = fread(
"Age,Prop_symptomatic,IFR,Prop_inf_hosp,Prop_inf_critical,Prop_critical_fatal,Prop_noncritical_fatal,Prop_symp_hospitalised,Prop_hospitalised_critical
10,0.66,8.59E-05,0.002361009,6.44E-05,0.5,0,0,0.15
20,0.66,0.000122561,0.003370421,9.19E-05,0.5,9.47E-04,0.007615301,0.15
30,0.66,0.000382331,0.010514103,0.000286748,0.5,0.001005803,0.008086654,0.15
40,0.66,0.000851765,0.023423527,0.000638823,0.5,0.001231579,0.009901895,0.15
50,0.66,0.001489873,0.0394717,0.001117404,0.5,0.002305449,0.018535807,0.15
60,0.66,0.006933589,0.098113786,0.005200192,0.5,0.006754596,0.054306954,0.15
70,0.66,0.022120421,0.224965092,0.016590316,0.5,0.018720727,0.150514645,0.15
80,0.66,0.059223786,0.362002579,0.04441784,0.5,0.041408882,0.332927412,0.15
100,0.66,0.087585558,0.437927788,0.065689168,0.5,0.076818182,0.617618182,0.15")

reformat = function(P)
{
    # 70-74,3388.488  75-79,2442.147  80-84,1736.567  85-89,1077.555  90-94,490.577  95-99,130.083  100+,15.834
    x = c(P[1:7], weighted.mean(c(P[8], P[9]), c(3388.488 + 2442.147, 1736.567 + 1077.555 + 490.577 + 130.083 + 15.834)));
    return (rep(x, each = 2))
}

P.icu_symp    = reformat(probs[, Prop_symp_hospitalised * Prop_hospitalised_critical]);
P.nonicu_symp = reformat(probs[, Prop_symp_hospitalised * (1 - Prop_hospitalised_critical)]);
P.death       = reformat(probs[, Prop_noncritical_fatal]);

burden_processes = list(
    list(source = "cases_reported", type = "multinomial", names = c("to_icu", "to_nonicu", "null"), report = c("", "", ""),
        prob = matrix(c(P.icu_symp, P.nonicu_symp, 1 - P.icu_symp - P.nonicu_symp), nrow = 3, ncol = 16, byrow = T),
        delays = matrix(c(cm_delay_gamma(9, 3, 60, 0.25)$p, cm_delay_gamma(9, 3, 60, 0.25)$p, cm_delay_skip(60, 0.25)$p), nrow = 3, byrow = T)),
    
    list(source = "to_icu", type = "multinomial", names = "icu", report = "pi",
        prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
        delays = matrix(cm_delay_gamma(10, 3, 60, 0.25)$p, nrow = 1, byrow = T)),
    
    list(source = "to_nonicu", type = "multinomial", names = "nonicu", report = "pi",
        prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
        delays = matrix(cm_delay_gamma(8, 3, 60, 0.25)$p, nrow = 1, byrow = T)),

    list(source = "Ip", type = "multinomial", names = c("death", "null"), report = c("o", ""),
        prob = matrix(c(P.death, 1 - P.death), nrow = 2, ncol = 16, byrow = T),
        delays = matrix(c(cm_delay_gamma(18.8, 5, 60, 0.25)$p, cm_delay_skip(60, 0.25)$p), nrow = 2, byrow = T)) # 18.8, 5 from Verity et al
);

# DATA

# ECDC case data
ecdc_cases = as.data.table(get_ecdc_cases());
ecdc_cases = ecdc_cases[order(country, date)]

# Population distributions
wp_reg = qread("./data/worldpop5yr.qs");
wp_reg[, tot_pop := rowSums(.SD), .SDcols = f_0:m_80] # Calculate total population
wp_reg = wp_reg[!is.na(lon) & !is.na(lat)]; # Restrict to regions with a lon and lat; this removes places with 0 population as well.
countries = wp_reg[parent == "", key];
names(countries) = wp_reg[parent == "", name];

# Matrix lookup
matrices = names(cm_matrices)
kvz_matrix_lookup = readRDS("./data/lookup_table_contactmatrix.rds");
matrix_lookup = data.table(country = unique(wp_reg$country));
matrix_lookup = merge(matrix_lookup, kvz_matrix_lookup[, .(country = country_code_iso_alpha3, matrix = matrix_name)], all.x = T)
matrix_lookup[is.na(matrix), matrix := "Finland"]

# Susceptibility and clinical fraction
covuy = qread("./data/covuy.qs")
covu = rep(unname(unlist(covuy[, colMeans(.SD), .SDcols = u_00:u_70])), each = 2)
covy = rep(unname(unlist(covuy[, colMeans(.SD), .SDcols = y_00:y_70])), each = 2)

iv_def = list(
    list(name = "School closures",
        strength_default = 100,
        strength_name = "Schools closed (%)",
        contact = function(x) c(1, 1, 1-x, 1,  1, 1, 1-x, 1),
        fIs = function(x) rep(1, 16)
    ),
    list(name = "Social distancing",
        strength_default = 50,
        strength_name = "Intensity of social distancing (%)",
        contact = function(x) c(1, 1-x, 1, 1-x,  1, 1-x, 1, 1-x),
        fIs = function(x) rep(1, 16)
    ),
    list(name = "Elderly shielding (stay-at-home)",
        strength_default = 75,
        strength_name = "Proportion of elderly individuals shielded, non-home contacts (%)",
        contact = function(x) c(1, 1, 1, 1,  1, 1-x, 1-x, 1-x),
        fIs = function(x) rep(1, 16)
    ),
    list(name = "Elderly shielding (full shielding)",
        strength_default = 75,
        strength_name = "Proportion of elderly individuals shielded, all contacts (%)",
        contact = function(x) c(1, 1, 1, 1,  1-x, 1-x, 1-x, 1-x),
        fIs = function(x) rep(1, 16)
    ),
    list(name = "Self-isolation",
        strength_default = 50,
        strength_name = "Adherence to self-isolation among symptomatics (%)",
        contact = function(x) c(1, 1, 1, 1,  1, 1, 1, 1),
        fIs = function(x) rep(1 - 0.7*x, 16)
    ),
    list(name = "Lockdown",
        strength_default = 90,
        strength_name = "Intensity of lockdown (%)",
        contact = function(x) c(1, 1-x, 1-x, 1-x,  1, 1-x, 1-x, 1-x),
        fIs = function(x) rep(1, 16)
    )#,
    #list(name = "Custom",
    #    strength_default = 50,
    #    strength_name = "To do",
    #    contact = function(x) c(1, 1, 1, 1,  1, 1, 1, 1),
    #    fIs = function(x) 1
    #)
)

intervention_types = 1:length(iv_def);
names(intervention_types) = sapply(iv_def, function(x) x$name);

intervention_adders = c("add_school_closure", "add_social_distancing", "add_elderly_shielding_part", "add_elderly_shielding_full", "add_self_isolation", "add_lockdown", "add_custom")
intervention_strength_names = c("Schools closed (%)", "Intensity of social distancing (%)", "Effectiveness of elderly shielding (%)", 
    "Adherence to self-isolation among symptomatics (%)", "Intensity of lockdown (%)", "NULL");
intervention_strength_defaults = c(100, 50, 75, 50, 90, 0);

# Helper to change CSS
setcss = function(id, ...)
{
    arguments = list(...);
    n = names(arguments);
    v = sapply(arguments, function(x) { if (is.numeric(x)) paste0(x, "px") else x });
    nv = paste0('"', n, '" : "', v, '"', collapse = ", ");
    command = paste0('$("#', id, '").css({ ', nv, ' });');
    runjs(command);
}


# UI COMPONENTS

nbsp = function(text)
{
    str_replace_all(text, " ", "&nbsp;")
}

customSlider = function(inputId, label, min, max, value, step = 1, width = NULL)
{
    HTML(paste0(
        "<div class=\"form-group shiny-input-container\"", if (is.null(width)) NULL else paste0(" style=\"", width, "\""), ">",
        "<label class=\"control-label\" for=\"", inputId, "\">", label, "</label>",
        "<input class=\"js-range-slider\" id=\"", inputId, "\" data-min=\"", min, "\" data-max=\"", max, "\" data-from=\"", value, "\" data-step=\"", step, "\"",
        " data-grid=\"false\" data-prettify-separator=\",\" data-prettify-enabled=\"true\" data-keyboard=\"true\" data-data-type=\"number\"/></div>"))
}

customAgeSlider = function(inputId, label, value1, value2, step = 1)
{
    HTML(paste0(
        "<div class=\"form-group shiny-input-container\">",
        "<label class=\"control-label\" for=\"", inputId, "\">", label, "</label>",
        "<input class=\"js-range-slider\" id=\"", inputId, "\" data-type=\"double\" data-min=\"0\" data-max=\"80\" data-from=\"", value1, "\" data-to=\"", value2, "\" data-step=\"5\"",
        " data-grid=\"false\" data-prettify-separator=\",\" data-prettify-enabled=\"true\" data-keyboard=\"true\" data-data-type=\"number\" data-max-postfix=\"+\" data-min-interval=\"5\"/></div>"))
}

iconTab = function(id, title, icon)
{
    HTML(paste0(
        "<span id=\"", id, "\"><i class=\"fa fa-", icon, " fa-3x fa-fw\"></i>",
        "<br />", title, "</span>"))
}

interventionRect = function(n, display = "block")
{
    colours   = c("#162d6040", "#3771c840", "#37c8ab40", "#ffd42a40", "#ff7f2a40", "#ff006640", "#bc5fd340", "#9d93ac40");
    tags$div(
        dropdownButton(inputId = paste0("int_menu_", n),
            tags$label(id = paste0("int_timespan_", n), "Date range"),
            selectInput(inputId = paste0("int_type_", n), label = NULL, choices = intervention_types, selected = 2),
            customSlider(inputId = paste0("int_strength_", n), label = 'Strength', value = 50, min = 1, max = 100),
            bsButton(inputId = paste0("int_remove_", n), label = "Remove", icon = icon("trash"), style = "warning", size = "small"),
            circle = TRUE, icon = icon("gear"), width = "200px", size = "xs", status = "int"
        ),
        div(id = paste0("iv_title_", n), style = "position: absolute; left: 22px; top: 4px", class = "noselect"),
        id = paste0("iv_", n), 
        style = paste0("font-size: 8pt; background-color: ", colours[n], "; height: ", 430 - n * 20, "px; width: 100px; position: absolute; left: 0px; bottom: 0px; margin: 0px; display: ", display),
        class = "noselect interv_rect"
    )
}

vaccineRect = function(n, display = "block")
{
    colours   = c("#dddddd40", "#dddddd40", "#dddddd40");
    outlines  = c("#44aa7780", "#4477aa80", "#4444aa80");
    tags$div(
        dropdownButton(inputId = paste0("vax_menu_", n),
            tags$label(id = paste0("vax_timespan_", n), "Date range"),
            customAgeSlider(paste0("vax_ages_", n), "Ages to vaccinate", value1 = 0, value2 = 80, step = 5),
            numericInput(paste0("vax_n_", n), "Vaccines administered", value = 1000000, min = 0, step = 5000),
            radioButtons(paste0("vax_rate_", n), NULL, c("total", "per day"), selected = "total", inline = TRUE),
            bsButton(inputId = paste0("vax_remove_", n), label = "Remove", icon = icon("trash"), style = "warning", size = "small"),
            circle = TRUE, icon = icon("gear"), width = "200px", size = "xs", status = "int"
        ),
        div(id = paste0("vv_title_", n), style = "position: absolute; left: 22px; top: 4px", class = "noselect"),
        id = paste0("vv_", n), 
        style = paste0("font-size: 8pt; background-color: ", colours[n], "; border-color: ", outlines[n], "; border-style: dashed; border-width: 1px 1px 0px 1px; height: ", 430, "px; width: 100px; position: absolute; left: 0px; bottom: 0px; margin: 0px; display: ", display),
        class = "noselect interv_rect"
    )
}

# for communicating position of intervention rectangles to server code
shsize = list(
    mv = list(
        `dragstop` = JS('function(event, ui) { return ui.position.left; }')
    ),
    sz = list(
        `resizestop` = JS('function(event, ui) { return { x : ui.position.left, w : ui.size.width }; }')
    )
)

shdrop = list(
    notify = list(
        `drop` = JS("function(event, ui) { return { x : ui.position.left, y : ui.position.top, id : $(ui.draggable).attr('id') }; }")
    )
)


# # EXAMPLE IP ADDRESS
# library(shiny)
# 
# ui <- function(req) {
#   fluidPage(
#     div(style = "display: none;",
#       textInput("remote_addr", "remote_addr",
#         paste0("(", req[["HTTP_X_FORWARDED_FOR"]], "|", req[["REMOTE_ADDR"]], ")")
#       )
#     ),
#     textOutput("ip")
#   )
# }
# 
# server <- function(input, output, session) {
#   output$ip = renderText(paste0("The remote IP is ", isolate(input$remote_addr)));
# }
# 
# shinyApp(ui, server)


