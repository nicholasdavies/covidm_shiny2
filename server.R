# covidm shiny app: server

function(input, output, session)
{
    # ---------- SET UP ----------

    # Health burdens and data points data.tables    
    burdens_dt = reactiveVal();
    burdens_dt(data.table(Hosp = reformat(probs[, Prop_symp_hospitalised]), ICU = reformat(probs[, Prop_hospitalised_critical]), CFR = reformat(probs[, Prop_noncritical_fatal])));
    points_dt = reactiveVal();
    points_dt(data.table(date = c(ymd("2020-05-01"), ymd("2020-05-02")), variable = c("cases", "deaths"), value = 1));

    # Set location
    user_ccode = ip2location(isolate(input$remote_addr), ip_file, c("country_code"))$country_code;
    user_country = country_codes[which(user_ccode == names(country_codes))];
    if (length(user_country) != 1) {
        user_country = country_codes[which("GB" == names(country_codes))];
    }
    updateSelectInput(session, "loc_reg0", selected = user_country);
    
    jqui_resizable(jqui_draggable(ui = "#vv_1", options = list(axis = "x", containment = "#iv_bounds")), 
        options = list(maxHeight = 430, minHeight = 430, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#vv_2", options = list(axis = "x", containment = "#iv_bounds")), 
        options = list(maxHeight = 430, minHeight = 430, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#vv_3", options = list(axis = "x", containment = "#iv_bounds")), 
        options = list(maxHeight = 430, minHeight = 430, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    
    # Set intervention rectangles as horizontally draggable and resizable
    jqui_resizable(jqui_draggable(ui = "#iv_1", options = list(axis = "x", containment = "#iv_bounds")), 
        options = list(maxHeight = 410, minHeight = 410, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_2", options = list(axis = "x", containment = "#iv_bounds")), 
        options = list(maxHeight = 390, minHeight = 390, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_3", options = list(axis = "x", containment = "#iv_bounds")), 
        options = list(maxHeight = 370, minHeight = 370, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_4", options = list(axis = "x", containment = "#iv_bounds")), 
        options = list(maxHeight = 350, minHeight = 350, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_5", options = list(axis = "x", containment = "#iv_bounds")), 
        options = list(maxHeight = 330, minHeight = 330, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_6", options = list(axis = "x", containment = "#iv_bounds")), 
        options = list(maxHeight = 310, minHeight = 310, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_7", options = list(axis = "x", containment = "#iv_bounds")), 
        options = list(maxHeight = 290, minHeight = 290, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_8", options = list(axis = "x", containment = "#iv_bounds")), 
        options = list(maxHeight = 270, minHeight = 270, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))

    # New interventions
    jqui_draggable(ui = "#add_school_closure",         options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#add_social_distancing",      options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#add_elderly_shielding_part", options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#add_elderly_shielding_full", options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#add_self_isolation",         options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#add_lockdown",               options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#add_custom",                 options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#add_vaccine",                options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_droppable(ui = "#display", options = list(accept = "[id^=add]", shiny = shdrop));

    # ---------- INTERVENTIONS ----------
    
    # Set up interventions
    iv = reactiveValues(
        active   = rep(F, 8),
        x        = rep(0, 8), # left pixel position of intervention rectangle
        w        = rep(0, 8), # pixel width of intervention rectangle
        t0       = rep(0, 8), # absolute start time of intervention
        t1       = rep(1, 8)  # absolute end time of intervention
    );
    vv = reactiveValues(
        active   = rep(F, 3),
        x        = rep(0, 3), # left pixel position of intervention rectangle
        w        = rep(0, 3), # pixel width of intervention rectangle
        t0       = rep(0, 3), # absolute start time of intervention
        t1       = rep(1, 3)  # absolute end time of intervention
    );

    # Set t0 and t1 of interventions from x and w, updating label for intervention i
    update_iv_t = function(i) { 
        # Calculate range of times shown on X axis
        params = parameters();
        xaxis_breaks = breaks_pretty(5)(c(ymd(params$date0), ymd(params$date0) + params$time1));
        ax_t0 = as.numeric(xaxis_breaks[1]);
        ax_t1 = as.numeric(tail(xaxis_breaks, 1));
        
        # Set t0 and t1
        iv$t0 = (iv$x / 810) * (ax_t1 - ax_t0) + ax_t0;
        iv$t1 = ((iv$x + iv$w) / 810) * (ax_t1 - ax_t0) + ax_t0;
        vv$t0 = (vv$x / 810) * (ax_t1 - ax_t0) + ax_t0;
        vv$t1 = ((vv$x + vv$w) / 810) * (ax_t1 - ax_t0) + ax_t0;
        
        # Set timespan text
        d = function(t) format(as.Date(round(t), origin = "1970-01-01"), "%b %d");
        if (i > 0) {
            html(paste0("int_timespan_", i), paste0(d(iv$t0[i]), " &ndash; ", d(iv$t1[i])));
        } else {
            html(paste0("vax_timespan_", -i), paste0(d(vv$t0[-i]), " &ndash; ", d(vv$t1[-i])));
        }
    }
    
    # Set x and w of interventions from t0 and t1
    update_iv_x = function() {
        # Calculate range of times shown on X axis
        params = parameters();
        xaxis_breaks = breaks_pretty(5)(c(ymd(params$date0), ymd(params$date0) + params$time1));
        ax_t0 = as.numeric(xaxis_breaks[1]);
        ax_t1 = as.numeric(tail(xaxis_breaks, 1));
        
        # Ensure times are in proper range
        iv$t0 = pmax(iv$t0, min(as.numeric(ymd(params$date0)), ax_t0));
        iv$t1 = pmin(iv$t1, max(as.numeric(ymd(params$date0) + params$time1), ax_t1));
        iv$t1 = pmax(iv$t1, iv$t0 + 7);
        
        vv$t0 = pmax(vv$t0, min(as.numeric(ymd(params$date0)), ax_t0));
        vv$t1 = pmin(vv$t1, max(as.numeric(ymd(params$date0) + params$time1), ax_t1));
        vv$t1 = pmax(vv$t1, vv$t0 + 7);
        
        # Set x and w, as well as updating actual positions
        iv$x = (iv$t0 - ax_t0) * 810 / (ax_t1 - ax_t0);
        iv$w = (iv$t1 - iv$t0) * 810 / (ax_t1 - ax_t0);
        for (i in 1:8) {
            setcss(paste0("iv_", i), left = iv$x[i], width = iv$w[i]);
        }

        vv$x = (vv$t0 - ax_t0) * 810 / (ax_t1 - ax_t0);
        vv$w = (vv$t1 - vv$t0) * 810 / (ax_t1 - ax_t0);
        for (i in 1:3) {
            setcss(paste0("vv_", i), left = vv$x[i], width = vv$w[i]);
        }
    }

    # Observe changing type of interventions
    observeEvent(input$int_type_1, { i = as.numeric(input$int_type_1); updateSliderInput(session, inputId = "int_strength_1", label = iv_def[[i]]$strength_name); html("iv_title_1", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_2, { i = as.numeric(input$int_type_2); updateSliderInput(session, inputId = "int_strength_2", label = iv_def[[i]]$strength_name); html("iv_title_2", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_3, { i = as.numeric(input$int_type_3); updateSliderInput(session, inputId = "int_strength_3", label = iv_def[[i]]$strength_name); html("iv_title_3", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_4, { i = as.numeric(input$int_type_4); updateSliderInput(session, inputId = "int_strength_4", label = iv_def[[i]]$strength_name); html("iv_title_4", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_5, { i = as.numeric(input$int_type_5); updateSliderInput(session, inputId = "int_strength_5", label = iv_def[[i]]$strength_name); html("iv_title_5", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_6, { i = as.numeric(input$int_type_6); updateSliderInput(session, inputId = "int_strength_6", label = iv_def[[i]]$strength_name); html("iv_title_6", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_7, { i = as.numeric(input$int_type_7); updateSliderInput(session, inputId = "int_strength_7", label = iv_def[[i]]$strength_name); html("iv_title_7", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_8, { i = as.numeric(input$int_type_8); updateSliderInput(session, inputId = "int_strength_8", label = iv_def[[i]]$strength_name); html("iv_title_8", nbsp(iv_def[[i]]$name)); });
    
    observeEvent(input$vax_rate_1, { n = if (input$vax_rate_1 == "total") input$vax_n_1 * (vv$t1[1] - vv$t0[1]) else input$vax_n_1 / (vv$t1[1] - vv$t0[1]); updateNumericInput(session, inputId = "vax_n_1", value = n); });
    observeEvent(input$vax_rate_2, { n = if (input$vax_rate_2 == "total") input$vax_n_2 * (vv$t1[2] - vv$t0[2]) else input$vax_n_2 / (vv$t1[2] - vv$t0[2]); updateNumericInput(session, inputId = "vax_n_2", value = n); });
    observeEvent(input$vax_rate_3, { n = if (input$vax_rate_3 == "total") input$vax_n_3 * (vv$t1[3] - vv$t0[3]) else input$vax_n_3 / (vv$t1[3] - vv$t0[3]); updateNumericInput(session, inputId = "vax_n_3", value = n); });
    
    # Observe dragging and resizing of interventions
    observeEvent(input$iv_1_mv, { iv$x[1] = input$iv_1_mv; update_iv_t(1); });
    observeEvent(input$iv_2_mv, { iv$x[2] = input$iv_2_mv; update_iv_t(2); });
    observeEvent(input$iv_3_mv, { iv$x[3] = input$iv_3_mv; update_iv_t(3); });
    observeEvent(input$iv_4_mv, { iv$x[4] = input$iv_4_mv; update_iv_t(4); });
    observeEvent(input$iv_5_mv, { iv$x[5] = input$iv_5_mv; update_iv_t(5); });
    observeEvent(input$iv_6_mv, { iv$x[6] = input$iv_6_mv; update_iv_t(6); });
    observeEvent(input$iv_7_mv, { iv$x[7] = input$iv_7_mv; update_iv_t(7); });
    observeEvent(input$iv_8_mv, { iv$x[8] = input$iv_8_mv; update_iv_t(8); });
    
    observeEvent(input$iv_1_sz, { iv$x[1] = input$iv_1_sz$x; iv$w[1] = input$iv_1_sz$w; update_iv_t(1); });
    observeEvent(input$iv_2_sz, { iv$x[2] = input$iv_2_sz$x; iv$w[2] = input$iv_2_sz$w; update_iv_t(2); });
    observeEvent(input$iv_3_sz, { iv$x[3] = input$iv_3_sz$x; iv$w[3] = input$iv_3_sz$w; update_iv_t(3); });
    observeEvent(input$iv_4_sz, { iv$x[4] = input$iv_4_sz$x; iv$w[4] = input$iv_4_sz$w; update_iv_t(4); });
    observeEvent(input$iv_5_sz, { iv$x[5] = input$iv_5_sz$x; iv$w[5] = input$iv_5_sz$w; update_iv_t(5); });
    observeEvent(input$iv_6_sz, { iv$x[6] = input$iv_6_sz$x; iv$w[6] = input$iv_6_sz$w; update_iv_t(6); });
    observeEvent(input$iv_7_sz, { iv$x[7] = input$iv_7_sz$x; iv$w[7] = input$iv_7_sz$w; update_iv_t(7); });
    observeEvent(input$iv_8_sz, { iv$x[8] = input$iv_8_sz$x; iv$w[8] = input$iv_8_sz$w; update_iv_t(8); });
    
    observeEvent(input$vv_1_mv, { vv$x[1] = input$vv_1_mv; update_iv_t(-1); });
    observeEvent(input$vv_2_mv, { vv$x[2] = input$vv_2_mv; update_iv_t(-2); });
    observeEvent(input$vv_3_mv, { vv$x[3] = input$vv_3_mv; update_iv_t(-3); });

    observeEvent(input$vv_1_sz, { vv$x[1] = input$vv_1_sz$x; vv$w[1] = input$vv_1_sz$w; update_iv_t(-1); });
    observeEvent(input$vv_2_sz, { vv$x[2] = input$vv_2_sz$x; vv$w[2] = input$vv_2_sz$w; update_iv_t(-2); });
    observeEvent(input$vv_3_sz, { vv$x[3] = input$vv_3_sz$x; vv$w[3] = input$vv_3_sz$w; update_iv_t(-3); });

    # ---------- SIMULATION ----------

    # TODO tidy this up...
    parameters = reactive({
        # # return default parameters if no input
        # if (is.null(input$epi_seed_size)) {
        #     return (cm_translate_parameters(cm_parameters_SEI3R("UK | UNITED KINGDOM")))
        # }
        
        params = cm_parameters_SEI3R("UK | UNITED KINGDOM", current_matrix(), deterministic = T, 
            date_start = input$epi_seed_date, date_end = input$epi_seed_date + as.numeric(input$epi_sim_time),
            dE  = cm_delay_gamma(input$vir_dE, 4.0, t_max = 15, t_step = 0.25)$p,
            dIp = cm_delay_gamma(input$vir_dP, 4.0, t_max = 15, t_step = 0.25)$p,
            dIs = cm_delay_gamma(input$vir_dC, 4.0, t_max = 15, t_step = 0.25)$p,
            dIa = cm_delay_gamma(input$vir_dS, 4.0, t_max = 15, t_step = 0.25)$p,
            u = 0.1 * covu, y = covy);
        
        locations = c(input$loc_reg3, input$loc_reg2, input$loc_reg1, input$loc_reg0)
        location = locations[locations != "0" & locations != ""][1];
        size = unlist(wp_reg[key == location, f_0:f_80] + wp_reg[key == location, m_0:m_80]);
        size = c(size[1:15], sum(size[16:length(size)]));
        params$pop[[1]]$size = round(size);
        
        params$pop[[1]]$seed_times = rep(0, as.numeric(input$epi_seed_size));
        params = cm_split_matrices_ex_in(params, 1 + as.numeric(input$int_elderly) / 5);
        
        burd = burdens_dt();
        params$processes = burden_processes;
        params$processes[[1]]$prob = matrix(c(burd$Hosp * burd$ICU, burd$Hosp * (1 - burd$ICU), 1 - burd$Hosp), nrow = 3, ncol = 16, byrow = T);
        params$processes[[4]]$prob = matrix(c(burd$CFR, 1 - burd$CFR), nrow = 2, ncol = 16, byrow = T);
        params$processes[[1]]$delays = matrix(c(
            cm_delay_gamma(input$hea_hosp_delay, 3, 60, 0.25)$p, 
            cm_delay_gamma(input$hea_hosp_delay, 3, 60, 0.25)$p, 
            cm_delay_skip(60, 0.25)$p), nrow = 3, byrow = T);
        params$processes[[2]]$delays = matrix(cm_delay_gamma(input$hea_icu_los, 3, 60, 0.25)$p, nrow = 1, byrow = T);
        params$processes[[3]]$delays = matrix(cm_delay_gamma(input$hea_nonicu_los, 3, 60, 0.25)$p, nrow = 1, byrow = T);
        params$processes[[4]]$delays = matrix(c(
            cm_delay_gamma(input$hea_death_delay, 5, 60, 0.25)$p, 
            cm_delay_skip(60, 0.25)$p), nrow = 2, byrow = T);

        params$pop[[1]]$imm0 = rep(input$epi_immune, 16);
        params$pop[[1]]$wn = rep(as.numeric(input$imm_wn) / 365.25, 16);
        
        params$pop[[1]]$season_A = input$epi_season_amp;
        params$pop[[1]]$season_T = 365.25;
        params$pop[[1]]$season_phi = as.numeric(ymd(input$epi_seed_date) - ymd("2020-01-01")) + input$epi_season_phase * 7 - 7;
        
        R0_unadjusted = cm_calc_R0(params, 1);
        params$pop[[1]]$u = params$pop[[1]]$u * input$epi_R0 / R0_unadjusted;

        return (cm_translate_parameters(params))
    });
    
    # Update intervention rectangles when parameters change
    observeEvent(parameters(), { update_iv_x() });
    
    # Simulation dynamics for unmitigated epidemic
    unmitigated = reactive({
        params = parameters();
        params$schedule = list();
        dynamics = cm_backend_simulate_v2(params, 1, 0)$dynamics[[1]];
        setDT(dynamics);
        dynamics[, date := ymd(params$date0) + t];
        return (dynamics)
    });

    # Simulation dynamics for mitigated epidemic
    mitigated = reactive({
        params = parameters();
        
        # Calculate range of times shown on X axis
        xaxis_breaks = breaks_pretty(5)(c(ymd(params$date0), ymd(params$date0) + params$time1));
        ax_t0 = as.numeric(xaxis_breaks[1]);
        ax_t1 = as.numeric(tail(xaxis_breaks, 1));
        tref = as.numeric(params$date0);

        # Set intervention schedule
        params$schedule = list();
        for (i in 1:length(iv$active)) {
            if (iv$active[i]) {
                params$schedule[[length(params$schedule) + 1]] = list(
                    parameter = "contact",
                    pops = 0,
                    mode = "lowerto",
                    values = list(iv_def[[as.numeric(input[[paste0("int_type_", i)]])]]$contact(input[[paste0("int_strength_", i)]] / 100), numeric(0)),
                    times = c(iv$t0[i] - tref, iv$t1[i] - tref)
                );
                params$schedule[[length(params$schedule) + 1]] = list(
                    parameter = "fIs",
                    pops = 0,
                    mode = "lowerto",
                    values = list(iv_def[[as.numeric(input[[paste0("int_type_", i)]])]]$fIs(input[[paste0("int_strength_", i)]] / 100), numeric(0)),
                    times = c(iv$t0[i] - tref, iv$t1[i] - tref)
                );
            }
        }
        
        # Set vaccination
        params$pop[[1]]$wv = rep(as.numeric(input$imm_wv) / 365.25, 16);
        params$pop[[1]]$ev = rep(as.numeric(input$imm_ev), 16);
        for (i in 1:length(vv$active)) {
            if (vv$active[i]) {
                imm_mask = rep(0, 16);
                imm_mask[(input[[paste0("vax_ages_", i)]][1]/5):(input[[paste0("vax_ages_", i)]][2]/5)] = 1;
                imm_n = as.numeric(input[[paste0("vax_n_", i)]]);
                if (input[[paste0("vax_rate_", i)]] == "total") {
                    imm_n = imm_n / (vv$t1[i] - vv$t0[i]);
                }
                params$schedule[[length(params$schedule) + 1]] = list(
                    parameter = "v",
                    pops = 0,
                    mode = "add",
                    values = list(params$pop[[1]]$size * imm_mask * imm_n / sum(params$pop[[1]]$size * imm_mask), numeric(0)),
                    times = c(vv$t0[i] - tref, vv$t1[i] - tref)
                );
            }
        }

        dynamics = cm_backend_simulate_v2(params, 1, 0)$dynamics[[1]];
        setDT(dynamics);
        dynamics[, date := ymd(params$date0) + t];
        return (dynamics)
    });
    
    # ---------- DISPLAY PANEL ----------
    
    # Cases view
    output$cases_plot = renderImage({
        dynU = unmitigated();
        dynM = mitigated();
        
        # TODO epi rho -- do this in code
        casesUt = dynU[, .(cases = sum(cases) * input$epi_rho), by = date];
        casesMt = dynM[, .(cases = sum(cases) * input$epi_rho), by = date];
        
        # TODO refactor as below
        casesU = dynU[, sum(cases)];
        casesM = dynM[, sum(cases)];
        hospsU = dynU[, sum(icu_i + nonicu_i)];
        hospsM = dynM[, sum(icu_i + nonicu_i)];
        deathsU = dynU[, sum(death_o)];
        deathsM = dynM[, sum(death_o)];
        
        case_points = points_dt()[variable == "cases"];
        show_points = F;
        if (input$dat_show && nrow(case_points) > 0) {
            show_points = T;
        }

        embed_svg(vvplot(960, 480, 
            vvpanel(
                if (input$compare) vvline(casesUt$date, casesUt$cases, style = "stroke:#afc6e9") else NULL, 
                #-#vvribbon(casesMt$date, casesMt$cases * 0.8, casesMt$cases * 1.2, style = "fill:#0044aa40"),
                vvline(casesMt$date, casesMt$cases, style = "stroke:#0044aa; stroke-width: 2px"),
                if (show_points) vvpoint(case_points$date, case_points$value, rescale = F) else NULL,
                vvlegend("line", 0.8, 0.95, "#0044aa", "Cases"),
                ylab = "Cases"
            ),

            vvpanel(vvbar(c("Unmit.", "Mit."), c(casesU, casesM), c("fill:#afc6e9", "fill:#0044aa"), show = TRUE), ylab = "Cases"),
            vvpanel(vvbar(c("Unmit.", "Mit."), c(hospsU, hospsM), c("fill:#c6afe9", "fill:#9955ff"), show = TRUE), ylab = "Hospitalisations"),
            vvpanel(vvbar(c("Unmit.", "Mit."), c(deathsU, deathsM), c("fill:#e9afaf", "fill:#c83737"), show = TRUE), ylab = "Deaths"),
            layout = list(
                c(0, 0, 860, 480),
                c(860, 0, 100, 160),
                c(860, 160, 100, 160),
                c(860, 320, 100, 160)
            )
        ));
    }, deleteFile = TRUE);

    # Hospital view
    output$hospital_plot = renderImage({
        dynU = unmitigated();
        dynM = mitigated();
        
        # TODO refactor as below
        icuUt = dynU[, .(icu = sum(icu_p)), by = date];
        icuMt = dynM[, .(icu = sum(icu_p)), by = date];
        nonicuUt = dynU[, .(nonicu = sum(nonicu_p)), by = date];
        nonicuMt = dynM[, .(nonicu = sum(nonicu_p)), by = date];

        casesU = dynU[, sum(cases)];
        casesM = dynM[, sum(cases)];
        hospsU = dynU[, sum(icu_i + nonicu_i)];
        hospsM = dynM[, sum(icu_i + nonicu_i)];
        deathsU = dynU[, sum(death_o)];
        deathsM = dynM[, sum(death_o)];
        
        embed_svg(vvplot(960, 480, 
            vvpanel(
                if (is.numeric(input$hea_icu) && input$hea_icu != 0) vvhline(as.numeric(input$hea_icu), style = "stroke:#0066ff; stroke-width: 1px; stroke-dasharray: 5"),
                if (is.numeric(input$hea_nonicu) && input$hea_nonicu != 0) vvhline(as.numeric(input$hea_nonicu), style = "stroke:#9955ff; stroke-width: 1px; stroke-dasharray: 5"),
                if (input$compare) vvline(icuUt$date, icuUt$icu, style = "stroke:#afc6e9") else NULL,
                vvline(icuMt$date, icuMt$icu, style = "stroke:#0066ff; stroke-width: 2px"),
                if (input$compare) vvline(nonicuUt$date, nonicuUt$nonicu, style = "stroke:#c6afe9") else NULL, 
                vvline(nonicuMt$date, nonicuMt$nonicu, style = "stroke:#9955ff; stroke-width: 2px"),
                vvlegend("line", 0.8, 0.95, c("#0066ff", "#9955ff"), c("ICU beds required", "Non-ICU beds required")),
                ylab = "Beds occupied"
            ),
            vvpanel(vvbar(c("Unmit.", "Mit."), c(casesU, casesM), c("fill:#afc6e9", "fill:#0044aa"), show = TRUE), ylab = "Cases"),
            vvpanel(vvbar(c("Unmit.", "Mit."), c(hospsU, hospsM), c("fill:#c6afe9", "fill:#9955ff"), show = TRUE), ylab = "Hospitalisations"),
            vvpanel(vvbar(c("Unmit.", "Mit."), c(deathsU, deathsM), c("fill:#e9afaf", "fill:#c83737"), show = TRUE), ylab = "Deaths"),
            layout = list(
                c(0, 0, 860, 480),
                c(860, 0, 100, 160),
                c(860, 160, 100, 160),
                c(860, 320, 100, 160)
            )
        ));
    }, deleteFile = TRUE);

    # Deaths view
    output$deaths_plot = renderImage({
        dynU = unmitigated();
        dynM = mitigated();
        
        # TODO refactor as below
        deathsUt = dynU[, .(deaths = sum(death_o)), keyby = date];
        deathsMt = dynM[, .(deaths = sum(death_o)), keyby = date];

        casesU = dynU[, sum(cases)];
        casesM = dynM[, sum(cases)];
        hospsU = dynU[, sum(icu_i + nonicu_i)];
        hospsM = dynM[, sum(icu_i + nonicu_i)];
        deathsU = dynU[, sum(death_o)];
        deathsM = dynM[, sum(death_o)];
        
        death_points = points_dt()[variable == "deaths"];
        show_points = F;
        if (input$dat_show && nrow(death_points) > 0) {
            show_points = T;
        }
        
        embed_svg(vvplot(960, 480, 
            vvpanel(
                if (input$compare) vvline(deathsUt$date, deathsUt$deaths, style = "stroke:#e9afaf") else NULL, 
                vvline(deathsMt$date, deathsMt$deaths, style = "stroke:#c83737; stroke-width: 2px"),
                if (show_points) vvpoint(death_points$date, death_points$value, rescale = F, style="fill:#bb0000") else NULL,
                vvlegend("line", 0.8, 0.95, "#c83737", "Deaths"),
                ylab = "Deaths"
            ),
            vvpanel(vvbar(c("Unmit.", "Mit."), c(casesU, casesM), c("fill:#afc6e9", "fill:#0044aa"), show = TRUE), ylab = "Cases"),
            vvpanel(vvbar(c("Unmit.", "Mit."), c(hospsU, hospsM), c("fill:#c6afe9", "fill:#9955ff"), show = TRUE), ylab = "Hospitalisations"),
            vvpanel(vvbar(c("Unmit.", "Mit."), c(deathsU, deathsM), c("fill:#e9afaf", "fill:#c83737"), show = TRUE), ylab = "Deaths"),
            layout = list(
                c(0, 0, 860, 480),
                c(860, 0, 100, 160),
                c(860, 160, 100, 160),
                c(860, 320, 100, 160)
            )
        ));
    }, deleteFile = TRUE);

    # Transmission view
    output$transmission_plot = renderImage({
        dynU = unmitigated();
        dynM = mitigated();
        
        dynUt = dynU[, .(R0 = sum(obs0), Re = sum(obs1)), by = date];
        dynMt = dynM[, .(R0 = sum(obs0), Re = sum(obs1)), by = date];

        casesU = dynU[, sum(cases)];
        casesM = dynM[, sum(cases)];
        hospsU = dynU[, sum(icu_i + nonicu_i)];
        hospsM = dynM[, sum(icu_i + nonicu_i)];
        deathsU = dynU[, sum(death_o)];
        deathsM = dynM[, sum(death_o)];
        
        embed_svg(vvplot(960, 480, 
            vvpanel(
                vvhline(1),
                if (input$compare) vvline(dynUt$date, dynUt$R0, style = "stroke:#afdde9") else NULL, 
                vvline(dynMt$date, dynMt$R0, style = "stroke:#00ceff; stroke-width: 2px"),
                if (input$compare) vvline(dynUt$date, dynUt$Re, style = "stroke:#afc6e9") else NULL, 
                vvline(dynMt$date, dynMt$Re, style = "stroke:#0044aa; stroke-width: 2px"),
                vvlegend("line", 0.8, 0.95, c("#00ceff", "#0044aa"), c("R<tspan baseline-shift=\"sub\">0</tspan>", "R<tspan baseline-shift=\"sub\">e</tspan>")),
                ylab = "Reproduction number"
            ),
            vvpanel(vvbar(c("Unmit.", "Mit."), c(casesU, casesM), c("fill:#afc6e9", "fill:#0044aa"), show = TRUE), ylab = "Cases"),
            vvpanel(vvbar(c("Unmit.", "Mit."), c(hospsU, hospsM), c("fill:#c6afe9", "fill:#9955ff"), show = TRUE), ylab = "Hospitalisations"),
            vvpanel(vvbar(c("Unmit.", "Mit."), c(deathsU, deathsM), c("fill:#e9afaf", "fill:#c83737"), show = TRUE), ylab = "Deaths"),
            layout = list(
                c(0, 0, 860, 480),
                c(860, 0, 100, 160),
                c(860, 160, 100, 160),
                c(860, 320, 100, 160)
            )
        ));
    }, deleteFile = TRUE);

    # Dynamics view
    output$dynamics_plot = renderImage({
        dynU = unmitigated();
        dynM = mitigated();
        
        dynUt = dynU[, .(R = sum(R), E = sum(E), Ip = sum(Ip), Is = sum(Is), Ia = sum(Ia)), by = date]
        dynMt = dynM[, .(R = sum(R), E = sum(E), Ip = sum(Ip), Is = sum(Is), Ia = sum(Ia)), by = date]

        casesU = dynU[, sum(cases)];
        casesM = dynM[, sum(cases)];
        hospsU = dynU[, sum(icu_i + nonicu_i)];
        hospsM = dynM[, sum(icu_i + nonicu_i)];
        deathsU = dynU[, sum(death_o)];
        deathsM = dynM[, sum(death_o)];
        
        embed_svg(vvplot(960, 480, 
            vvpanel(
                vvline(dynUt$date, dynUt$R, style = "stroke:#afdde9"), 
                vvline(dynMt$date, dynMt$R, style = "stroke:#00ceff; stroke-width: 2px"),
                vvline(dynUt$date, dynUt$E, style = "stroke:#afc6e9"), 
                vvline(dynMt$date, dynMt$E, style = "stroke:#0044aa; stroke-width: 2px"),
                vvline(dynUt$date, dynUt$Ip, style = "stroke:#ffeeaa"), 
                vvline(dynMt$date, dynMt$Ip, style = "stroke:#ffcc00; stroke-width: 2px"),
                vvline(dynUt$date, dynUt$Is, style = "stroke:#ffccaa"), 
                vvline(dynMt$date, dynMt$Is, style = "stroke:#ff6600; stroke-width: 2px"),
                vvline(dynUt$date, dynUt$Ia, style = "stroke:#afe9af"), 
                vvline(dynMt$date, dynMt$Ia, style = "stroke:#71c837; stroke-width: 2px"),
                vvlegend("line", 0.8, 0.95, c("#0044aa", "#ffcc00", "#ff6600", "#71c837", "#00ceff"), c("E (Exposed)", "Ip (Preclinical)", "Ic (Clinical)", "Is (Subclinical)", "R (Recovered)")),
                ylab = "Prevalence"
            ),
            vvpanel(vvbar(c("Unmit.", "Mit."), c(casesU, casesM), c("fill:#afc6e9", "fill:#0044aa"), show = TRUE), ylab = "Cases"),
            vvpanel(vvbar(c("Unmit.", "Mit."), c(hospsU, hospsM), c("fill:#c6afe9", "fill:#9955ff"), show = TRUE), ylab = "Hospitalisations"),
            vvpanel(vvbar(c("Unmit.", "Mit."), c(deathsU, deathsM), c("fill:#e9afaf", "fill:#c83737"), show = TRUE), ylab = "Deaths"),
            layout = list(
                c(0, 0, 860, 480),
                c(860, 0, 100, 160),
                c(860, 160, 100, 160),
                c(860, 320, 100, 160)
            )
        ));
    }, deleteFile = TRUE);

    # ---------- CONTROL PANEL ----------
    
    # LOCATION CONTROL PANEL
    # Create map
    output$loc_map = renderLeaflet({
        leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
            addProviderTiles(providers$Stamen.TonerLite)
    })

    # Update dropdowns when a new parent location is chosen
    observe({
        pid = wp_reg[key == input$loc_reg0, id];
        keys = c(0, wp_reg[parent == pid, key]);
        names(keys) = c("All", wp_reg[parent == pid, name]);
        updateSelectInput(session, "loc_reg1", choices = keys);
        updateSelectInput(session, "loc_reg2", choices = c("All" = 0));
        updateSelectInput(session, "loc_reg3", choices = c("All" = 0));
    });
    
    observe({
        pid = wp_reg[key == input$loc_reg1, id];
        if (length(pid) > 0) {
            keys = c(0, wp_reg[parent == pid, key]);
            names(keys) = c("All", wp_reg[parent == pid, name]);
            updateSelectInput(session, "loc_reg2", choices = keys);
            updateSelectInput(session, "loc_reg3", choices = c("All" = 0));
        }
    });
    
    observe({
        pid = wp_reg[key == input$loc_reg2, id];
        if (length(pid) > 0) {
            keys = c(0, wp_reg[parent == pid, key]);
            names(keys) = c("All", wp_reg[parent == pid, name]);
            updateSelectInput(session, "loc_reg3", choices = keys);
        }
    });

    # Update map when a new location is chosen
    observe({
        # Choose location
        locations = c(input$loc_reg3, input$loc_reg2, input$loc_reg1, input$loc_reg0)
        location = locations[locations != "0"][1];
        lat = wp_reg[key == location, lat];
        lon = wp_reg[key == location, lon];
        size = wp_reg[key == location, sqrt(area)];
        
        # Update map
        leafletProxy("loc_map") %>%
            clearMarkers() %>%
            addCircleMarkers(lat = lat, lng = lon, color = "#aa0000", stroke = F, radius = 6, fillOpacity = 0.8) %>%
            flyTo(lat = lat, lng = lon, zoom = (8000 - size) / 1500 + ((sum(locations != "0") - 1) * 1.5))
    })
    
    # Population pyramid
    output$loc_pyramid = renderImage({
        locations = c(input$loc_reg3, input$loc_reg2, input$loc_reg1, input$loc_reg0)
        location = locations[locations != "0" & locations != ""][1];
        pyramid = data.table(age = rep(seq(0, 80, by = 5), 2), sex = rep(c("F", "M"), each = 17), 
            n = c(-unlist(wp_reg[key == location][, f_0:f_80]), unlist(wp_reg[key == location][, m_0:m_80])))

        embed_svg(vvplot(300, 300, vvpanel(
                vvbarh(pyramid[sex == "F", n], pyramid[sex == "F", age], style = "fill:#ffffff; stroke:black"),
                vvbarh(pyramid[sex == "M", n], pyramid[sex == "M", age], style = "fill:#a0a0a0; stroke:black"),
                vvtextann(c(0.15, 0.85), c(0.91, 0.91), c("Females", "Males")),
            xlab = "Population", xpositive = T)
        ))
    }, deleteFile = TRUE)
    
    # CONTACT CONTROL PANEL
    observeEvent(input$con_matrix, {
        if (input$con_matrix == "default") {
            disable("con_custom");
        } else {
            enable("con_custom");
        }
    });

    current_matrix = reactive({
        pid = wp_reg[key == input$loc_reg0, id];
        default_mat = matrix_lookup[country == pid, matrix][1];
        html("con_default_label", paste0(default_mat, " (default matrix for ", wp_reg[key == input$loc_reg0, name][1], ")"));

        if (input$con_matrix == "default") {
            if (!is.null(default_mat) && !is.na(default_mat) && default_mat %in% names(cm_matrices)) {
                return (default_mat);
            } else {
                return ("United Kingdom (Mossong)");
            }
        } else {
            return (input$con_custom);
        }
    });
    
    output$con_home   = renderImage({ embed_svg(vvplot(230, 230, vvpanel(vvmatrix(cm_matrices[[current_matrix()]]$home), xtick = 90, title = "Contacts at home", xlab = "Age of individual", ylab = "Age of contacts", margin = c(15, 15, 45, 45)))) }, deleteFile = TRUE);
    output$con_work   = renderImage({ embed_svg(vvplot(230, 230, vvpanel(vvmatrix(cm_matrices[[current_matrix()]]$work), xtick = 90, title = "Contacts at work", xlab = "Age of individual", ylab = "Age of contacts", margin = c(15, 15, 45, 45)))) }, deleteFile = TRUE);
    output$con_school = renderImage({ embed_svg(vvplot(230, 230, vvpanel(vvmatrix(cm_matrices[[current_matrix()]]$school), xtick = 90, title = "Contacts at school", xlab = "Age of individual", ylab = "Age of contacts", margin = c(15, 15, 45, 45)))) }, deleteFile = TRUE);
    output$con_other  = renderImage({ embed_svg(vvplot(230, 230, vvpanel(vvmatrix(cm_matrices[[current_matrix()]]$other), xtick = 90, title = "Other contacts", xlab = "Age of individual", ylab = "Age of contacts", margin = c(15, 15, 45, 45)))) }, deleteFile = TRUE);
    
    # EPIDEMIC CONTROL PANEL
    output$epi_season_plot = renderImage({
        wave = data.table(t = 1:365);
        wave[, amp := cos((t - input$epi_season_phase * 7 + 7) * 2 * pi / 365) * input$epi_season_amp + 1];
        wave[, date := ymd("2000-01-01") + t];
        embed_svg(vvplot(300, 75,
            vvpanel(
                vvline(wave$date, wave$amp),
                vvlimits(NULL, c(0, 2)),
                margin = c(10, 10, 20, 30),
                xdateformat = "%b",
                ylab = "Seasonality"
            )
        ))
    }, deleteFile = TRUE);                
    
    # INFECTION CONTROL PANEL
    output$vir_uy_plot = renderImage({
        embed_svg(vvplot(300, 270, 
            vvpanel(
                vvline((1:8) * 10 - 5, covu[(1:8)*2], style = "stroke:#44d0d0; stroke-width: 2px"),
                vvline((1:8) * 10 - 5, covy[(1:8)*2], style = "stroke:#d04444; stroke-width: 2px"),
                vvlegend("line", 0.1, 0.95, c("#44d0d0", "#d04444"), c("Susceptibility", "Clinical fraction")),
                xlab = "Age"
            )
        ))
    }, deleteFile = TRUE);
    
    output$model_diagram = renderImage({
        list(src = normalizePath(file.path("./data", "fig-model.png")),
            width = "300px", height = "135px", alt = "Model diagram")
    }, deleteFile = FALSE)
    
    # INTERVENTIONS CONTROL PANEL
    # Add new intervention
    observeEvent(input$display_notify, {
        if (!is.null(input$display_notify)) {
            if (input$display_notify$id == "add_vaccine") {
                available = which(vv$active == F)[1];
                if (!is.na(available)) {
                    # Calculate range of times shown on X axis
                    params = parameters();
                    xaxis_breaks = breaks_pretty(5)(c(ymd(params$date0), ymd(params$date0) + params$time1));
                    ax_t0 = as.numeric(xaxis_breaks[1] - params$date0);
                    ax_t1 = as.numeric(tail(xaxis_breaks, 1) - params$date0);
    
                    vv$active[available]   = T;
                    vv$w[available]        = 56 * 810 / (ax_t1 - ax_t0);
                    vv$x[available]        = max(0, min(810 - vv$w[available], input$display_notify$x - 55));
                    update_iv_t(available);
    
                    html(paste0("vv_title_", available), nbsp(paste("Vaccination", available)));
                    setcss(paste0("vv_", available), display = "block", left = vv$x[available], width = vv$w[available]);
                }
            } else if (input$display_notify$id %like% "^add") {
                available = which(iv$active == F)[1];
                if (!is.na(available)) {
                    # Calculate range of times shown on X axis
                    params = parameters();
                    xaxis_breaks = breaks_pretty(5)(c(ymd(params$date0), ymd(params$date0) + params$time1));
                    ax_t0 = as.numeric(xaxis_breaks[1] - params$date0);
                    ax_t1 = as.numeric(tail(xaxis_breaks, 1) - params$date0);
    
                    iv_type_i = match(input$display_notify$id, intervention_adders, nomatch = 1);
                    iv$active[available]   = T;
                    iv$w[available]        = 56 * 810 / (ax_t1 - ax_t0);
                    iv$x[available]        = max(0, min(810 - iv$w[available], input$display_notify$x - 55));
                    update_iv_t(available);

                    updateSelectInput(session, inputId = paste0("int_type_", available), label = NULL, choices = intervention_types, selected = iv_type_i);
                    updateSliderInput(session, inputId = paste0("int_strength_", available), label = iv_def[[iv_type_i]]$strength_name, value = iv_def[[iv_type_i]]$strength_default);
                    html(paste0("iv_title_", available), nbsp(iv_def[[iv_type_i]]$name));
                    setcss(paste0("iv_", available), display = "block", left = iv$x[available], width = iv$w[available]);
                }
            }
        }
    });
    
    # Remove interventions
    observeEvent(input$int_remove_1, { iv$active[1] = F; setcss("iv_1", display = "none"); toggleDropdownButton("int_menu_1"); });
    observeEvent(input$int_remove_2, { iv$active[2] = F; setcss("iv_2", display = "none"); toggleDropdownButton("int_menu_2"); });
    observeEvent(input$int_remove_3, { iv$active[3] = F; setcss("iv_3", display = "none"); toggleDropdownButton("int_menu_3"); });
    observeEvent(input$int_remove_4, { iv$active[4] = F; setcss("iv_4", display = "none"); toggleDropdownButton("int_menu_4"); });
    observeEvent(input$int_remove_5, { iv$active[5] = F; setcss("iv_5", display = "none"); toggleDropdownButton("int_menu_5"); });
    observeEvent(input$int_remove_6, { iv$active[6] = F; setcss("iv_6", display = "none"); toggleDropdownButton("int_menu_6"); });
    observeEvent(input$int_remove_7, { iv$active[7] = F; setcss("iv_7", display = "none"); toggleDropdownButton("int_menu_7"); });
    observeEvent(input$int_remove_8, { iv$active[8] = F; setcss("iv_8", display = "none"); toggleDropdownButton("int_menu_8"); });

    observeEvent(input$vax_remove_1, { vv$active[1] = F; setcss("vv_1", display = "none"); toggleDropdownButton("vax_menu_1"); });
    observeEvent(input$vax_remove_2, { vv$active[2] = F; setcss("vv_2", display = "none"); toggleDropdownButton("vax_menu_2"); });
    observeEvent(input$vax_remove_3, { vv$active[3] = F; setcss("vv_3", display = "none"); toggleDropdownButton("vax_menu_3"); });

    # HEALTH CONTROL PANEL
    observeEvent(input$hea_burdens, {
        burdens_dt(hot_to_r(input$hea_burdens));
    });

    output$hea_burdens = renderRHandsontable({
        dt = burdens_dt();
        if (!is.null(dt)) {
            rhandsontable(dt, rowHeaders = c(paste0(seq(0, 70, by = 5), "-", seq(4, 74, by = 5)), "75+"), useTypes = TRUE, stretchH = "all") %>%
                hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
                hot_validate_numeric(1:3, min = 0, max = 1, allowInvalid = FALSE)
        }
    })
    
    output$hea_plot = renderImage({ 
        dt = burdens_dt();
        embed_svg(vvplot(300, 420, ncol = 1, nrow = 2, 
            vvpanel(
                vvbar(c(paste0(seq(0, 70, by = 5), "-", seq(4, 74, by = 5)), "75+"), dt[, Hosp], style = "fill: #0066ff"), 
                vvbar(c(paste0(seq(0, 70, by = 5), "-", seq(4, 74, by = 5)), "75+"), dt[, Hosp * ICU], style = "fill: #9955ff"),
                vvlegend("rect", 0.1, 0.95, c("#0066ff", "#9955ff"), c("Severe", "Critical")),
                xtick = 45, ylab = "Hospitalisation"),
            vvpanel(vvbar(c(paste0(seq(0, 70, by = 5), "-", seq(4, 74, by = 5)), "75+"), dt[, CFR], style = "fill: c83737"), 
                xtick = 45, ylab = "Case fatality ratio")
        ))
    }, deleteFile = TRUE);

    # icu = reactive(input$hea_icu);
    # observe({ print(debounce(icu, 1000)()); });
    
    # DATA CONTROL PANEL
    observeEvent(input$dat_points, {
        points_dt(hot_to_r(input$dat_points));
    });

    output$dat_points = renderRHandsontable({
        dt = points_dt();
        if (!is.null(dt)) {
            rhandsontable(dt, useTypes = TRUE, stretchH = "all", height = 300) %>%
                hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
                hot_col(col = "variable", type = "dropdown", source = c("cases", "deaths"), strict = TRUE)
        }
    });

    output$dat_plot = renderImage({
        cases = points_dt()[variable == "cases"];
        deaths = points_dt()[variable == "deaths"];
        
        embed_svg(vvplot(300, 300, vvpanel(
            if (nrow(cases) > 0) vvpoint(cases[, date], cases[, value], style = "fill:#000000") else NULL,
            if (nrow(deaths) > 0) vvpoint(deaths[, date], deaths[, value], style = "fill:#bb0000") else NULL,
            vvlegend("point", 0.1, 0.9, c("#000000", "#bb0000"), c("Cases", "Deaths")),
            xtick = 45, margin = c(10, 10, 50, 40), ylab = "Data"))) 
    }, deleteFile = TRUE);
    
    observeEvent(input$dat_import, {
        cases = ecdc_cases[country == input$dat_import_from];
        points_dt(rbind(
            cases[, .(date, variable = "cases", value = cases_new)],
            cases[, .(date, variable = "deaths", value = deaths_new)]));
    });

    # SAVE / LOAD CONTROL PANEL
    output$sav_download = downloadHandler(
        filename = function() { 
            paste0("covidm-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".csv")
        },
        content = function(con) {
            dynU = unmitigated();
            dynM = mitigated();
            data = rbind(
                cbind(dynU, scenario = "Unmitigated"),
                cbind(dynM, scenario = "Mitigated")
            );
            fwrite(data, con)
        }
    );
    
    # RESET TAB
    observeEvent(input$reset, {
        if (input$control_tabs == "location") {
            updateSelectInput(session, "loc_reg0", selected = user_country);
        } else if (input$control_tabs == "contact") {
            updateRadioButtons(session, "con_matrix", selected = "default");
            disable("con_custom");
        } else if (input$control_tabs == "epidemic") {
            updateDateInput(session, "epi_seed_date", value = "2020-01-01");
            updateSelectInput(session, "epi_sim_time", selected = 365);
            updateSliderInput(session, "epi_seed_size", value = 10);
            updateSliderInput(session, "epi_R0", value = 2.4);
            updateSliderInput(session, "epi_immune", value = 0);
            updateSliderInput(session, "epi_rho", value = 1);
            updateSliderInput(session, "epi_season_phase", value = 1);
            updateSliderInput(session, "epi_season_amp", value = 0);
        } else if (input$control_tabs == "virus") {
            updateSelectInput(session, "vir_uy", selected = 1);
            updateSliderInput(session, "vir_dE", value = 3.0);
            updateSliderInput(session, "vir_dP", value = 2.1);
            updateSliderInput(session, "vir_dC", value = 2.9);
            updateSliderInput(session, "vir_dS", value = 5.0);
            updateSliderInput(session, "imm_wn", value = 0);
        } else if (input$control_tabs == "interventions") {
            iv$active[1] = F; setcss("iv_1", display = "none");
            iv$active[2] = F; setcss("iv_2", display = "none");
            iv$active[3] = F; setcss("iv_3", display = "none");
            iv$active[4] = F; setcss("iv_4", display = "none");
            iv$active[5] = F; setcss("iv_5", display = "none");
            iv$active[6] = F; setcss("iv_6", display = "none");
            iv$active[7] = F; setcss("iv_7", display = "none");
            iv$active[8] = F; setcss("iv_8", display = "none");
            vv$active[1] = F; setcss("vv_1", display = "none");
            vv$active[2] = F; setcss("vv_2", display = "none");
            vv$active[3] = F; setcss("vv_3", display = "none");
            updateSliderInput(session, "int_elderly", value = 70);
            updateSliderInput(session, "imm_ev", value = 0.8);
            updateSliderInput(session, "imm_wv", value = 0);
        } else if (input$control_tabs == "health") {
            burdens_dt(data.table(Hosp = reformat(probs[, Prop_symp_hospitalised]), ICU = reformat(probs[, Prop_hospitalised_critical]), CFR = reformat(probs[, Prop_noncritical_fatal])));
            updateSliderInput(session, "hea_hosp_delay",  value = 9.);
            updateSliderInput(session, "hea_icu_los",     value = 10);
            updateSliderInput(session, "hea_nonicu_los",  value = 8.);
            updateSliderInput(session, "hea_death_delay", value = 18);
            updateNumericInput(session, "hea_icu", value = 0);
            updateNumericInput(session, "hea_nonicu", value = 0);
        } else if (input$control_tabs == "data") {
            points_dt(data.table(date = c(ymd("2020-05-01"), ymd("2020-05-02")), variable = c("cases", "deaths"), value = 1));
            updateCheckboxInput(session, "dat_show", value = F);
            updateSelectInput(session, "dat_import_from", selected = "Afghanistan");
        }
    });

    # HELP TOOLTIPS
    observe({
        if (input$help_tooltips) {
            for (h in seq_along(help_bubbles)) {
                addPopover(session, names(help_bubbles)[h], help_bubbles[[h]][1], help_bubbles[[h]][3], 
                    placement = help_bubbles[[h]][2], trigger = "hover", options = list(container = "body"));
            }
        } else {
            for (h in seq_along(help_bubbles)) {
                removePopover(session, names(help_bubbles)[h]);
            }
        }
    });
}
