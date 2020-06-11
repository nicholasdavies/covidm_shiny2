# covidm shiny app: interface

source("./load.R")

fluidPage(theme = shinytheme("sandstone"),
    useShinyjs(),
    
    tags$head(
        # Newer version of fontawesome
        tags$style("@import url(https://use.fontawesome.com/releases/v5.13.0/css/all.css);"),
        
        # Rounded and centered tabs
        tags$style(HTML(".nav-tabs > li { text-align: center; margin-right: 0 }")),
        tags$style(HTML(".nav-tabs > li > a { border-radius: 10px 10px 0 0 }")),
        
        # Custom slider
        tags$style(".irs-bar, .irs-bar-edge { background-color: #98978b }"),
        tags$style(".irs-slider { border: 3px solid #98978b; border-radius: 10px }"),
        tags$style(".irs-single { background-color: #f0f0f0; color: #000000 }"),
        tags$style(".irs-min, .irs-max { background-color: #f8f5f0; color: #98978b }"),
        
        # For help button
        tags$style(".btn-info.active { background-color: #e48721 }"),
        tags$style(".btn-info.active:hover { background-color: #e48721 }"),
        tags$style(".btn-info.active:focus { background-color: #e48721 }"),
        tags$style(".btn-info.active:active { background-color: #e48721 }"),
        tags$style(".btn-info { background-color: #98978b }"),
        tags$style(".btn-info:hover { background-color: #98978b }"),
        tags$style(".btn-info:focus { background-color: #98978b }"),
        tags$style(".btn-info:active { background-color: #98978b }"),
        
        # For new interventions
        tags$style(".new_intervention { background-color: #f8f5f0; padding: 4px; cursor: copy }"),
        tags$style(".new_intervention:hover { background-color: #e8e5f0 }"),

        # For intervention rectangle
        tags$style(".interv_rect:hover { cursor: grab }"),
        tags$style(".interv_rect:active { cursor: grabbing }"),

        # For intervention dropdown
        tags$style(".btn-int { background: none; margin: 2px }"),
        tags$style(".noselect { -webkit-touch-callout: none; -webkit-user-select: none; -khtml-user-select: none; -moz-user-select: none; -ms-user-select: none; user-select: none }"),
        tags$style(".dropdown-menu { opacity: 0.75 }")
    ),
    chooseSliderSkin("Square"),
    
    h3(id = "title", style = "text-align: center", "CMMID COVID-19 transmission app"),
    h4(style = "text-align: center", "Beta testing version: This app is still under development"),
    bsPopover("help_tooltips", "Show help", "Enables in-app help, which appears in bubbles like this one.", placement = "bottom", trigger = "hover", options = list(container = "body")),
    
    # ---------- DISPLAY PANEL ----------
    
    div(id = "display", style = "max-width: 1000px; min-width: 1000px; margin: 0 auto !important; float: none !important; padding: 10px; border-radius: 20px 10px 10px 10px; border: 1px solid #dddddd; position:relative", 
        # Help button
        div(id = "help_loc", style = "position: absolute; right: 10px; top: 10px",
            bsButton("help_tooltips", label = HTML("Show help"), icon = icon("question-circle", "fa"), style = "info", size = "small", type = "toggle", value = FALSE)
        ),

        # Compare button
        div(id = "compare_loc", style = "position: absolute; right: 112px; top: 10px",
            bsButton("compare", label = HTML("Compare"), icon = icon("exchange-alt", "fa"), style = "info", size = "small", type = "toggle", value = TRUE)
        ),
        
        # Display tabs
        tabsetPanel(id = "display_tabs",
            tabPanel(value = "cases", title = iconTab("tab_cases", "Cases", "head-side-cough"),
                imageOutput("cases_plot", width = 960, height = 480), style = "padding:10px"),
            tabPanel(value = "hospital", title = iconTab("tab_hospital", "Hospital", "ambulance"), 
                imageOutput("hospital_plot", width = 960, height = 480), style = "padding:10px"),
            tabPanel(value = "deaths", title = iconTab("tab_deaths", "Deaths", "times"), 
                imageOutput("deaths_plot", width = 960, height = 480), style = "padding:10px"),
            tabPanel(value = "trasmission", title = iconTab("tab_transmission", "R Number", "wave-square"),
                imageOutput("transmission_plot", width = 960, height = 480), style = "padding:10px"),
            tabPanel(value = "dynamics", title = iconTab("tab_dynamics", "Dynamics", "project-diagram"),
                imageOutput("dynamics_plot", width = 960, height = 480), style = "padding:10px")
        ),
        
        # Intervention rectangles
        tags$div(id = "iv_bounds", style = "width: 810px; height: 430px; position: absolute; left: 60px; bottom: 60px; margin: 0px; padding: 0px",
            vaccineRect(1, "none"),
            vaccineRect(2, "none"),
            vaccineRect(3, "none"),
            interventionRect(1, "none"),
            interventionRect(2, "none"),
            interventionRect(3, "none"),
            interventionRect(4, "none"),
            interventionRect(5, "none"),
            interventionRect(6, "none"),
            interventionRect(7, "none"),
            interventionRect(8, "none")
        )
    ),
    
    # ---------- CONTROL PANEL ----------
    
    div(id = "control", style = "max-width: 980px; min-width: 980px; margin: 0 auto !important; float: none !important; padding: 10px; border-radius: 0px 0px 10px 10px; border-width: 0px 1px 1px 1px; border-color: #dddddd; border-style: solid",
        tabsetPanel(id = "control_tabs",
            # Location controls
            tabPanel(value = "location", title = iconTab("tab_location", "Location", "globe-africa"),
                tags$br(),
                fluidRow(
                    column(4, 
                        fluidRow(column(3, h6("Country", style="padding-top: 2px; text-align: right")), column(9, selectInput("loc_reg0", label = NULL, choices = countries))),
                        fluidRow(column(3, h6("Admin 1", style="padding-top: 2px; text-align: right")), column(9, selectInput("loc_reg1", label = NULL, choices = ""))),
                        fluidRow(column(3, h6("Admin 2", style="padding-top: 2px; text-align: right")), column(9, selectInput("loc_reg2", label = NULL, choices = ""))),
                        fluidRow(column(3, h6("Admin 3", style="padding-top: 2px; text-align: right")), column(9, selectInput("loc_reg3", label = NULL, choices = "")))
                    ),
                    column(4,
                        leafletOutput("loc_map", width = "100%", height = "300px")
                    ),
                    column(4,
                        imageOutput("loc_pyramid", width = 300, height = 300)
                    )
                )
            ),
            
            # Contact controls
            tabPanel(value = "contact", title = iconTab("tab_contact", "Contact", "people-arrows"),
                tags$br(),
                fluidRow(
                    column(12,
                        radioButtons("con_matrix", label = "Contact matrix", width = "100%", choiceValues = list("default", "custom"),
                            choiceNames = list(
                                span(id = "con_default_label", "Use default matrix"),
                                "Use custom matrix:"
                            )
                        ),
                        disabled(selectInput("con_custom", label = NULL, choices = matrices))
                    )
                ),
                fluidRow(
                    column(3, imageOutput("con_home", width = 230, height = 230)),
                    column(3, imageOutput("con_work", width = 230, height = 230)),
                    column(3, imageOutput("con_school", width = 230, height = 230)),
                    column(3, imageOutput("con_other", width = 230, height = 230))
                )
            ),
            
            # Epidemic controls
            tabPanel(value = "epidemic", title = iconTab("tab_epidemic", "Epidemic", "viruses"),
                tags$br(),
                fluidRow(
                    column(4, 
                        dateInput("epi_seed_date", label = "Epidemic start date", value = "2020-01-01", format = "d M yyyy", min = "2019-11-01", max = "2021-12-31"),
                        selectInput("epi_sim_time", label = "Simulate for", choices = c("1 year" = 365, "2 years" = 730, "3 years" = 1095, "4 years" = 1460, "5 years" = 1825)),
                        customSlider("epi_seed_size", label = "Starting number of infections", min = 1, max = 100, value = 10),
                    ),
                    column(4,
                        customSlider("epi_R0", "Basic reproduction number, R\u2080", min = 1.1, max = 4.5, value = 2.4, step = 0.1),
                        customSlider("epi_immune", "Proportion immune at start", min = 0, max = 1, value = 0, step = 0.01),
                        customSlider("epi_rho", "Case ascertainment rate", min = 0.01, max = 1, value = 1, step = 0.01)
                    ),
                    column(4,
                        customSlider("epi_season_phase", "Seasonality: peak week", min = 1, max = 52, value = 1, step = 1),
                        customSlider("epi_season_amp", "Seasonality: amplitude", min = 0.0, max = 1.0, value = 0, step = 0.01),
                        imageOutput("epi_season_plot", width = 300, height = 75)
                    )
                )
            ),
          
            # Virus controls
            tabPanel(value = "virus", title = iconTab("tab_virus", "Infection", "lungs-virus"),
                tags$br(),
                fluidRow(
                    column(4,
                        selectInput("vir_uy", label = "Susceptibility and clinical fraction", choices = "Age-varying"),
                        imageOutput("vir_uy_plot", width = 300, height = 300)
                    ),
                    column(4,
                        customSlider("vir_dE", "Latent period (days)", min = 0.1, max = 10, value = 3.0, step = 0.1),
                        customSlider("vir_dP", "Preclinical period (days)", min = 0.1, max = 10, value = 2.1, step = 0.1),
                        customSlider("vir_dC", "Clinical period (days)", min = 0.1, max = 10, value = 2.9, step = 0.1),
                        customSlider("vir_dS", "Subclinical period (days)", min = 0.1, max = 10, value = 5.0, step = 0.1)
                    ),
                    column(4, 
                        customSlider("imm_wn", "Natural immunity waning rate (per year)", min = 0, max = 2, value = 0, step = 0.02),
                        imageOutput("model_diagram", width = 300, height = 300)
                    )
                )
            ),

            
            # Intervention controls
            tabPanel(value = "interventions", title = iconTab("tab_interventions", "Interventions", "star-of-life"),
                tags$br(),
                fluidRow(
                    column(8,
                        tags$h5(HTML("Drag the labels below onto the plot above to add new interventions.<br/>They can be moved and resized directly on the plot.")),
                        span(HTML("School&nbsp;closure"),                               id = "add_school_closure",         class = "new_intervention", style = "position: relative"),
                        span(HTML("Social&nbsp;distancing"),                            id = "add_social_distancing",      class = "new_intervention", style = "position: relative"),
                        tags$br(), tags$br(),
                        span(HTML("Elderly&nbsp;shielding&nbsp;(stay-at-home)"),        id = "add_elderly_shielding_part", class = "new_intervention", style = "position: relative"),
                        span(HTML("Elderly&nbsp;shielding&nbsp;(full&nbsp;shielding)"), id = "add_elderly_shielding_full", class = "new_intervention", style = "position: relative"),
                        tags$br(), tags$br(),
                        span(HTML("Self-isolation"),                                    id = "add_self_isolation",         class = "new_intervention", style = "position: relative"),
                        span(HTML("Lockdown"),                                          id = "add_lockdown",               class = "new_intervention", style = "position: relative"),
                        tags$br(), tags$br(),
                        span(HTML("Vaccine"),                                           id = "add_vaccine",                class = "new_intervention", style = "position: relative")
                    ),
                    column(4,
                        customSlider("int_elderly", "Age threshold for elderly shielding", min = 50, max = 75, value = 70, step = 1, width = "450px"),
                        customSlider("imm_ev", "Vaccine effectiveness", min = 0, max = 1, value = 0.8, step = 0.01),
                        customSlider("imm_wv", "Vaccine protection waning rate (per year)", min = 0, max = 2, value = 0, step = 0.02)
                    )
                )
            ),
            
            # Health controls
            tabPanel(value = "health", title = iconTab("tab_health", "Health", "procedures"),
                tags$br(),
                fluidRow(
                    column(4,
                        tags$label("Edit hospitalisation probability, ICU probability and CFR below."),
                        rHandsontableOutput("hea_burdens")
                    ),
                    column(4,
                        imageOutput("hea_plot", width = 300, height = 420)
                    ),
                    column(4,
                        customSlider("hea_hosp_delay",  "Symptom onset to hospitalisation, days", min = 0.1, max = 30, value = 9., step = 0.1),
                        customSlider("hea_icu_los",     "Length of stay in ICU, days",            min = 0.1, max = 30, value = 10, step = 0.1),
                        customSlider("hea_nonicu_los",  "Length of stay in general ward, days",   min = 0.1, max = 30, value = 8., step = 0.1),
                        customSlider("hea_death_delay", "Symptom onset to death, days",           min = 0.1, max = 30, value = 18, step = 0.1),
                        numericInput("hea_icu", "ICU bed capacity", value = 0, min = 0, step = 100),
                        numericInput("hea_nonicu", "Non-ICU bed capacity", value = 0, min = 0, step = 100)
                    )
                )
            ),

            # Data controls
            tabPanel(value = "data", title = iconTab("tab_data", "Data", "list-ol"),
                tags$br(),
                fluidRow(
                    column(4,
                        tags$label("Right-click on the table to add/remove rows."),
                        rHandsontableOutput("dat_points")
                    ),
                    column(4,
                        imageOutput("dat_plot", width = 300, height = 300)
                    ),
                    column(4,
                        checkboxInput("dat_show", label = "Show data on main display", value = F),
                        selectInput("dat_import_from", label = "Import ECDC case data from", choices = ecdc_cases[, unique(country)]),
                        actionButton("dat_import", label = "Import", icon = icon("arrow-left"))
                    )
                )
            ),

            # Save / load controls
            tabPanel(value = "saveload", title = iconTab("tab_saveload", "Save / Load", "save"),
                tags$br(),
                downloadButton("sav_download", label = "Download epidemic data")
            ),
            
            # About controls
            tabPanel(value = "about", title = iconTab("tab_about", "About", "ellipsis-h"),
                h2("To do"))
        )
    ),
    
    # Notices
    div(id = "notices", style = "max-width: 980px; min-width: 980px; margin: 0 auto !important; float: none !important; padding: 10px",
        tags$br(),
        h6(HTML("Made by the Centre for Mathematical Modelling of Infectious Diseases &middot; London School of Hygiene and Tropical Medicine")),
        tags$br()
    )
)