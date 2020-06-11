# makecontact_shiny2.R
library(readxl)
library(data.table)
library(socialmixr)
library(wpp2019)
library(countrycode)
library(ggplot2)

# CONTACT MATRICES
survey = get_survey("https://doi.org/10.5281/zenodo.1043437")

matrices = list()

s_countries = as.character(survey$participants[, unique(country)]);

for (co in s_countries) {
    home = contact_matrix(survey, countries = co, age.limits = seq(0, 75, by = 5), filter = list(cnt_home = 1), symmetric = T)$matrix
    work = contact_matrix(survey, countries = co, age.limits = seq(0, 75, by = 5), filter = list(cnt_work = 1), symmetric = T)$matrix
    scho = contact_matrix(survey, countries = co, age.limits = seq(0, 75, by = 5), filter = list(cnt_school = 1), symmetric = T)$matrix
    othe = contact_matrix(survey, countries = co, age.limits = seq(0, 75, by = 5), filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0), symmetric = T)$matrix
    
    rownames(home) = colnames(home) = limits_to_agegroups(seq(0, 75, by = 5))
    rownames(work) = colnames(work) = limits_to_agegroups(seq(0, 75, by = 5))
    rownames(scho) = colnames(scho) = limits_to_agegroups(seq(0, 75, by = 5))
    rownames(othe) = colnames(othe) = limits_to_agegroups(seq(0, 75, by = 5))
    
    nm = paste0(co, " (Mossong et al.)");
    matrices[[nm]]$home = home;
    matrices[[nm]]$work = work;
    matrices[[nm]]$school = scho;
    matrices[[nm]]$other = othe;
}


# Béraud et al (France)
f_survey = get_survey("https://doi.org/10.5281/zenodo.1157918")
f_survey$contacts[, cnt_age_exact := as.character(cnt_age_exact)]
f_survey$contacts[cnt_age_exact %like% "[a-z]", cnt_age_exact := 0.5] # convert ages given in days, weeks, months to 0.5
f_survey$contacts[, cnt_age_exact := as.numeric(cnt_age_exact)]

f_pop = cm_populations[name %like% "France", f + m, by = age]$V1 * 1000
f_survey_pop = data.frame(lower.age.limit = seq(0, 75, by = 5),
    population = c(f_pop[1:15], sum(f_pop[16:21])))

f_home = contact_matrix(f_survey, age.limits = seq(0, 75, by = 5), survey.pop = f_survey_pop, filter = list(cnt_home = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
f_work = contact_matrix(f_survey, age.limits = seq(0, 75, by = 5), survey.pop = f_survey_pop, filter = list(cnt_work = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
f_scho = contact_matrix(f_survey, age.limits = seq(0, 75, by = 5), survey.pop = f_survey_pop, filter = list(cnt_school = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
f_othe = contact_matrix(f_survey, age.limits = seq(0, 75, by = 5), survey.pop = f_survey_pop, filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix

rownames(f_home) = colnames(f_home) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(f_work) = colnames(f_work) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(f_scho) = colnames(f_scho) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(f_othe) = colnames(f_othe) = limits_to_agegroups(seq(0, 75, by = 5))

matrices[["France (Béraud et al.)"]] = list();
matrices[["France (Béraud et al.)"]]$home   = f_home;
matrices[["France (Béraud et al.)"]]$work   = f_work;
matrices[["France (Béraud et al.)"]]$school = f_scho;
matrices[["France (Béraud et al.)"]]$other  = f_othe;

# Leung et al (HK)
hk_survey = get_survey("https://doi.org/10.5281/zenodo.1165561")

hk_pop = cm_populations[name %like% "Hong Kong", f + m, by = age]$V1 * 1000
hk_survey_pop = data.frame(lower.age.limit = seq(0, 75, by = 5),
    population = c(hk_pop[1:15], sum(hk_pop[16:21])))

hk_home = contact_matrix(hk_survey, age.limits = seq(0, 75, by = 5), survey.pop = hk_survey_pop, filter = list(cnt_home = 1), symmetric = T)$matrix
hk_work = contact_matrix(hk_survey, age.limits = seq(0, 75, by = 5), survey.pop = hk_survey_pop, filter = list(cnt_work = 1), symmetric = T)$matrix
hk_scho = contact_matrix(hk_survey, age.limits = seq(0, 75, by = 5), survey.pop = hk_survey_pop, filter = list(cnt_school = 1), symmetric = T)$matrix
hk_othe = contact_matrix(hk_survey, age.limits = seq(0, 75, by = 5), survey.pop = hk_survey_pop, filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0), symmetric = T)$matrix

rownames(hk_home) = colnames(hk_home) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(hk_work) = colnames(hk_work) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(hk_scho) = colnames(hk_scho) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(hk_othe) = colnames(hk_othe) = limits_to_agegroups(seq(0, 75, by = 5))

matrices[["Hong Kong (Leung et al.)"]] = list();
matrices[["Hong Kong (Leung et al.)"]]$home   = hk_home;
matrices[["Hong Kong (Leung et al.)"]]$work   = hk_work;
matrices[["Hong Kong (Leung et al.)"]]$school = hk_scho;
matrices[["Hong Kong (Leung et al.)"]]$other  = hk_othe;


# Grijalva et al (Peru)
p_survey = get_survey("https://doi.org/10.5281/zenodo.1095664")
p_survey$contacts[, cnt_age_exact := as.character(cnt_age_exact)]
p_survey$contacts[cnt_age_exact %like% "[a-z]", cnt_age_exact := 0.5] # convert ages given in days, weeks, months to 0.5
p_survey$contacts[, cnt_age_exact := as.numeric(cnt_age_exact)]

p_pop = cm_populations[name %like% "Peru", f + m, by = age]$V1 * 1000
p_survey_pop = data.frame(lower.age.limit = seq(0, 75, by = 5),
    population = c(p_pop[1:15], sum(p_pop[16:21])))

p_home = contact_matrix(p_survey, age.limits = seq(0, 75, by = 5), survey.pop = p_survey_pop, filter = list(cnt_home = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
p_work = contact_matrix(p_survey, age.limits = seq(0, 75, by = 5), survey.pop = p_survey_pop, filter = list(cnt_work = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
p_scho = contact_matrix(p_survey, age.limits = seq(0, 75, by = 5), survey.pop = p_survey_pop, filter = list(cnt_school = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
p_othe = contact_matrix(p_survey, age.limits = seq(0, 75, by = 5), survey.pop = p_survey_pop, filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix

rownames(p_home) = colnames(p_home) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(p_work) = colnames(p_work) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(p_scho) = colnames(p_scho) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(p_othe) = colnames(p_othe) = limits_to_agegroups(seq(0, 75, by = 5))

matrices[["Peru (Grijalva et al.)"]] = list();
matrices[["Peru (Grijalva et al.)"]]$home   = p_home;
matrices[["Peru (Grijalva et al.)"]]$work   = p_work;
matrices[["Peru (Grijalva et al.)"]]$school = p_scho;
matrices[["Peru (Grijalva et al.)"]]$other  = p_othe;

# Horby et al (Vietnam)
v_survey = get_survey("https://doi.org/10.5281/zenodo.1289473")
v_survey$contacts[, cnt_age_exact := as.character(cnt_age_exact)]
v_survey$contacts[cnt_age_exact %like% "[a-z]", cnt_age_exact := 0.5] # convert ages given in days, weeks, months to 0.5
v_survey$contacts[, cnt_age_exact := as.numeric(cnt_age_exact)]

v_pop = cm_populations[name %like% "Viet Nam", f + m, by = age]$V1 * 1000
v_survey_pop = data.frame(lower.age.limit = seq(0, 75, by = 5),
    population = c(v_pop[1:15], sum(v_pop[16:21])))

v_home = contact_matrix(v_survey, age.limits = seq(0, 75, by = 5), survey.pop = v_survey_pop, filter = list(cnt_home = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
v_work = contact_matrix(v_survey, age.limits = seq(0, 75, by = 5), survey.pop = v_survey_pop, filter = list(cnt_work = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
v_scho = contact_matrix(v_survey, age.limits = seq(0, 75, by = 5), survey.pop = v_survey_pop, filter = list(cnt_school = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
v_othe = contact_matrix(v_survey, age.limits = seq(0, 75, by = 5), survey.pop = v_survey_pop, filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix

rownames(v_home) = colnames(v_home) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(v_work) = colnames(v_work) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(v_scho) = colnames(v_scho) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(v_othe) = colnames(v_othe) = limits_to_agegroups(seq(0, 75, by = 5))

matrices[["Vietnam (Horby et al.)"]] = list();
matrices[["Vietnam (Horby et al.)"]]$home   = v_home;
matrices[["Vietnam (Horby et al.)"]]$work   = v_work;
matrices[["Vietnam (Horby et al.)"]]$school = v_scho;
matrices[["Vietnam (Horby et al.)"]]$other  = v_othe;


# Melegaro et al (Zimbabwe)
z_survey = get_survey("https://doi.org/10.5281/zenodo.1127693")
z_survey$contacts[, cnt_age_exact := as.character(cnt_age_exact)]
z_survey$contacts[cnt_age_exact %like% "[a-z]", cnt_age_exact := 0.5] # convert ages given in days, weeks, months to 0.5
z_survey$contacts[, cnt_age_exact := as.numeric(cnt_age_exact)]

z_pop = cm_populations[name %like% "Zimbabwe", f + m, by = age]$V1 * 1000
z_survey_pop = data.frame(lower.age.limit = seq(0, 75, by = 5),
    population = c(z_pop[1:15], sum(z_pop[16:21])))

z_home = contact_matrix(z_survey, age.limits = seq(0, 75, by = 5), survey.pop = z_survey_pop, filter = list(cnt_home = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
z_work = contact_matrix(z_survey, age.limits = seq(0, 75, by = 5), survey.pop = z_survey_pop, filter = list(cnt_work = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
z_scho = contact_matrix(z_survey, age.limits = seq(0, 75, by = 5), survey.pop = z_survey_pop, filter = list(cnt_school = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
z_othe = contact_matrix(z_survey, age.limits = seq(0, 75, by = 5), survey.pop = z_survey_pop, filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix

rownames(z_home) = colnames(z_home) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(z_work) = colnames(z_work) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(z_scho) = colnames(z_scho) = limits_to_agegroups(seq(0, 75, by = 5))
rownames(z_othe) = colnames(z_othe) = limits_to_agegroups(seq(0, 75, by = 5))

matrices[["Zimbabwe (Melegaro et al.)"]] = list();
matrices[["Zimbabwe (Melegaro et al.)"]]$home   = z_home;
matrices[["Zimbabwe (Melegaro et al.)"]]$work   = z_work;
matrices[["Zimbabwe (Melegaro et al.)"]]$school = z_scho;
matrices[["Zimbabwe (Melegaro et al.)"]]$other  = z_othe;


# # Zhang et al. (China)
# c_survey = get_survey("https://doi.org/10.5281/zenodo.3366396")
# c_survey$contacts[, cnt_age_exact := as.character(cnt_age_exact)]
# c_survey$contacts[cnt_age_exact %like% "[a-z]", cnt_age_exact := 0.5] # convert ages given in days, weeks, months to 0.5
# c_survey$contacts[, cnt_age_exact := as.numeric(cnt_age_exact)]
# 
# c_pop = cm_populations[name %like% "China", f + m, by = age]$V1 * 1000
# c_survey_pop = data.frame(lower.age.limit = seq(0, 75, by = 5),
#     population = c(c_pop[1:15], sum(c_pop[16:21])))
# 
# c_home = contact_matrix(c_survey, age.limits = seq(0, 75, by = 5), survey.pop = c_survey_pop, filter = list(cnt_home = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
# c_work = contact_matrix(c_survey, age.limits = seq(0, 75, by = 5), survey.pop = c_survey_pop, filter = list(cnt_work = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
# c_scho = contact_matrix(c_survey, age.limits = seq(0, 75, by = 5), survey.pop = c_survey_pop, filter = list(cnt_school = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
# c_othe = contact_matrix(c_survey, age.limits = seq(0, 75, by = 5), survey.pop = c_survey_pop, filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
# 
# rownames(c_home) = colnames(c_home) = limits_to_agegroups(seq(0, 75, by = 5))
# rownames(c_work) = colnames(c_work) = limits_to_agegroups(seq(0, 75, by = 5))
# rownames(c_scho) = colnames(c_scho) = limits_to_agegroups(seq(0, 75, by = 5))
# rownames(c_othe) = colnames(c_othe) = limits_to_agegroups(seq(0, 75, by = 5))

# # van Hoek et al. (United Kingdom)
# u_survey = get_survey("https://doi.org/10.5281/zenodo.1409506")
# u_survey$contacts[, cnt_age_exact := as.character(cnt_age_exact)]
# u_survey$contacts[cnt_age_exact %like% "[a-z]", cnt_age_exact := 0.5] # convert ages given in days, weeks, months to 0.5
# u_survey$contacts[, cnt_age_exact := as.numeric(cnt_age_exact)]
# 
# u_pop = cm_populations[name %like% "United Kingdom", f + m, by = age]$V1 * 1000
# u_survey_pop = data.frame(lower.age.limit = seq(0, 75, by = 5),
#     population = c(u_pop[1:15], sum(u_pop[16:21])))
# 
# u_home = contact_matrix(u_survey, age.limits = seq(0, 75, by = 5), survey.pop = u_survey_pop, filter = list(cnt_home = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
# u_work = contact_matrix(u_survey, age.limits = seq(0, 75, by = 5), survey.pop = u_survey_pop, filter = list(cnt_work = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
# u_scho = contact_matrix(u_survey, age.limits = seq(0, 75, by = 5), survey.pop = u_survey_pop, filter = list(cnt_school = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
# u_othe = contact_matrix(u_survey, age.limits = seq(0, 75, by = 5), survey.pop = u_survey_pop, filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
# 
# rownames(u_home) = colnames(u_home) = limits_to_agegroups(seq(0, 75, by = 5))
# rownames(u_work) = colnames(u_work) = limits_to_agegroups(seq(0, 75, by = 5))
# rownames(u_scho) = colnames(u_scho) = limits_to_agegroups(seq(0, 75, by = 5))
# rownames(u_othe) = colnames(u_othe) = limits_to_agegroups(seq(0, 75, by = 5))

# # Litvitnova et al. (Russia)
# r_survey = get_survey("https://doi.org/10.5281/zenodo.3415222")
# r_survey$contacts[, cnt_age_exact := as.character(cnt_age_exact)]
# r_survey$contacts[cnt_age_exact %like% "[a-z]", cnt_age_exact := 0.5] # convert ages given in days, weeks, months to 0.5
# r_survey$contacts[, cnt_age_exact := as.numeric(cnt_age_exact)]
# 
# r_pop = cm_populations[name %like% "Russia", f + m, by = age]$V1 * 1000
# r_survey_pop = data.frame(lower.age.limit = seq(0, 75, by = 5),
#     population = c(r_pop[1:15], sum(r_pop[16:21])))
# 
# r_home = contact_matrix(r_survey, age.limits = seq(0, 75, by = 5), survey.pop = r_survey_pop, filter = list(cnt_home = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
# r_work = contact_matrix(r_survey, age.limits = seq(0, 75, by = 5), survey.pop = r_survey_pop, filter = list(cnt_work = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
# r_scho = contact_matrix(r_survey, age.limits = seq(0, 75, by = 5), survey.pop = r_survey_pop, filter = list(cnt_school = 1), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
# r_othe = contact_matrix(r_survey, age.limits = seq(0, 75, by = 5), survey.pop = r_survey_pop, filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0), symmetric = T, estimated.contact.age = "sample", missing.contact.age = "sample")$matrix
# 
# rownames(r_home) = colnames(r_home) = limits_to_agegroups(seq(0, 75, by = 5))
# rownames(r_work) = colnames(r_work) = limits_to_agegroups(seq(0, 75, by = 5))
# rownames(r_scho) = colnames(r_scho) = limits_to_agegroups(seq(0, 75, by = 5))
# rownames(r_othe) = colnames(r_othe) = limits_to_agegroups(seq(0, 75, by = 5))



# Prem et al
read_prem_cm = function(n, country, loc = "all_locations")
{
    filename = paste0("~/Dropbox/nCoV/Readings/contact_matrices_152_countries/MUestimates_", loc, "_", n, ".xlsx");
    cm = as.matrix(read_excel(filename, country, col_names = ifelse(n == 1, T, F)));
    rownames(cm) = colnames(cm) = limits_to_agegroups(seq(0, 75, by = 5));
    return (cm)
}

# Get contact matrices
sheets1 = excel_sheets("~/Dropbox/nCoV/Readings/contact_matrices_152_countries/MUestimates_all_locations_1.xlsx");
sheets2 = excel_sheets("~/Dropbox/nCoV/Readings/contact_matrices_152_countries/MUestimates_all_locations_2.xlsx");


for (s in sheets1) {
    mh = read_prem_cm(1, s, "home");
    mw = read_prem_cm(1, s, "work");
    ms = read_prem_cm(1, s, "school");
    mo = read_prem_cm(1, s, "other_locations");
    nm = paste0(s, " (Prem et al.)");
    
    matrices[[nm]] = list(home = mh, work = mw, school = ms, other = mo);
}

for (s in sheets2) {
    mh = read_prem_cm(2, s, "home");
    mw = read_prem_cm(2, s, "work");
    ms = read_prem_cm(2, s, "school");
    mo = read_prem_cm(2, s, "other_locations");
    nm = paste0(s, " (Prem et al.)");
    
    matrices[[nm]] = list(home = mh, work = mw, school = ms, other = mo);
}




# getting group contacts from Zhang
part = merge(fread("~/Dropbox/nCoV/Contact Data/2019_Zhang_China_participant_common.csv"), fread("~/Dropbox/nCoV/Contact Data/2019_Zhang_China_participant_extra.csv"), by = "part_id");
cont = merge(fread("~/Dropbox/nCoV/Contact Data/2019_Zhang_China_contact_common.csv"), part[, .(part_id, part_age)], by = "part_id")
NROW = 18;

# group = function(age)
# {
#     pmin(age %/% 5 + 1, NROW)
# }
# 
# get.individual.contacts = function(part, cont, which)
# {
#     matk = matrix(0, nrow = NROW, ncol = NROW);
#     matu = matrix(0, nrow = NROW, ncol = NROW);
#     c_known = cont[!is.na(cnt_age_exact)];
#     
#     if (which == "h") {
#         c_known = c_known[cnt_home == 1];
#     } else if (which == "w") {
#         c_known = c_known[cnt_work == 1];
#     } else if (which == "s") {
#         c_known = c_known[cnt_school == 1];
#     } else {
#         c_known = c_known[cnt_home == 0 & cnt_work == 0 & cnt_school == 0];
#     }
#     
#     c_known[is.na(cnt_age_est_min), cnt_age_est_min := cnt_age_exact];
#     c_known[is.na(cnt_age_est_max), cnt_age_est_max := cnt_age_exact];
#     c_known[is.na(cnt_age_est_min), cnt_age_est_min := 0];
#     c_known[is.na(cnt_age_est_max), cnt_age_est_max := NROW*5];
#     c_unknown = cont[is.na(cnt_age_exact)];
#     
#     part_age_counts = part[, .N, keyby = group(part_age)]$N
#     
#     # do known contacts
#     for (r in 1:nrow(c_known))
#     {
#         pg = group(c_known[r, part_age]);
#         cg = group(c_known[r, runif(1, min(cnt_age_est_min, cnt_age_est_max), max(cnt_age_est_min, cnt_age_est_max))]);
#         matk[pg, cg] = matk[pg, cg] + 1 / part_age_counts[pg];
#     }
#     
#     # do unknown contacts
#     for (r in 1:nrow(c_unknown))
#     {
#         pg = group(c_unknown[r, part_age]);
#         if (sum(matk[pg,]) > 0) {
#             matu[pg,] = matu[pg,] + rmultinom(1, 1, matk[pg,]) / part_age_counts[pg];
#         } else {
#             lower = max(0, pg-1);
#             upper = min(NROW, pg+1);
#             matu[pg,] = matu[pg,] + rmultinom(1, 1, colMeans(matk[lower:upper,])) / part_age_counts[pg];
#         }
#     }
#     
#     matu + matk
# }
# 
# get.group.contacts = function(part, mat)
# {
#     grc = part[group_yn == 1]
#     mat = matrix(0, nrow = NROW, ncol = NROW)
#     
#     part_age_counts = part[, .N, keyby = group(part_age)]$N
#     
#     for (r in 1:nrow(grc))
#     {
#         pg = group(grc[r, part_age]);
#         n_cont_groups = grc[r, group_age0 + group_age6 + group_age12 + group_age19 + group_age60]
#         if (n_cont_groups == 0) {
#             possible_ages = 0:(NROW*5-1);
#         } else {
#             possible_ages = grc[r, c(rep(group_age0, 6), rep(group_age6, 6), rep(group_age12, 7), rep(group_age19, 41), rep(group_age60, (NROW*5)-60))]
#         }
# 
#         n_each = c(rmultinom(1, grc[r, group_n], possible_ages))
#         cgroups = group(rep(0:(NROW*5-1), n_each))
#         for (cg in cgroups) {
#             mat[pg, cg] = mat[pg, cg] + 1 / part_age_counts[pg];
#         }
#     }
#     mat
# }
# 
# nsamp = 50;
# 
# mat_h = matrix(0, nrow = NROW, ncol = NROW)
# mat_w = matrix(0, nrow = NROW, ncol = NROW)
# mat_s = matrix(0, nrow = NROW, ncol = NROW)
# mat_o = matrix(0, nrow = NROW, ncol = NROW)
# mat_g = matrix(0, nrow = NROW, ncol = NROW)
# 
# for (i in 1:nsamp) {
#     print(i)
#     mat_h = mat_h + get.individual.contacts(part, cont, "h") / nsamp
#     mat_w = mat_w + get.individual.contacts(part, cont, "w") / nsamp
#     mat_s = mat_s + get.individual.contacts(part, cont, "s") / nsamp
#     mat_o = mat_o + get.individual.contacts(part, cont, "o") / nsamp
#     mat_g = mat_g + get.group.contacts(part, mat) / nsamp
# }
# 
# image(mat_h+mat_w+mat_o+0.5*mat_g)
# 
# saveRDS(mat_h, "~/Dropbox/nCoV/shanghai_raw_h.rds")
# saveRDS(mat_w, "~/Dropbox/nCoV/shanghai_raw_w.rds")
# saveRDS(mat_s, "~/Dropbox/nCoV/shanghai_raw_s.rds")
# saveRDS(mat_o, "~/Dropbox/nCoV/shanghai_raw_o.rds")
# saveRDS(mat_g, "~/Dropbox/nCoV/shanghai_raw_g.rds")

mat_h = readRDS("~/Dropbox/nCoV/shanghai_raw_h.rds")
mat_w = readRDS("~/Dropbox/nCoV/shanghai_raw_w.rds")
mat_s = readRDS("~/Dropbox/nCoV/shanghai_raw_s.rds")
mat_o = readRDS("~/Dropbox/nCoV/shanghai_raw_o.rds")
mat_g = readRDS("~/Dropbox/nCoV/shanghai_raw_g.rds")

symmetrize = function(mat, pop)
{
    norm.mat = diag(pop) %*% mat;
    return (0.5 * diag(1/pop) %*% (norm.mat + t(norm.mat)));
}

add_names = function(mat)
{
    dimnames(mat) = list(
        c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"),
        c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
    )
    # dimnames(mat) = list(
    #     c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+"),
    #     c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+")
    # )
    mat
}

# plUK = sp_contact_matrices("United Kingdom (Mossong)", matrices[["United Kingdom (Mossong)"]], legpos = "right")

# # ASSUMPTION 1 - all group contacts involving 0-19 year olds are school
# mat_g_school = mat_g
# mat_g_nonschool = mat_g
# mat_g_school[5:18, 5:18] = 0
# mat_g_nonschool[, 1:4] = 0
# mat_g_nonschool[1:4, ] = 0
# 
# wuhan_pop = populations[name == "Wuhan", f + m]
# 
# wuhan_new_matrices = list(
#     home   = add_names(symmetrize(mat_h, wuhan_pop)),
#     work   = add_names(symmetrize(mat_w, wuhan_pop)),
#     school = add_names(symmetrize(mat_s + mat_g_school, wuhan_pop)),
#     other  = add_names(symmetrize(mat_o + mat_g_nonschool, wuhan_pop))
# )
# 
# # check
# with(matrices[["United Kingdom (Mossong)"]], rowSums(school)/rowSums(work + home + other))
# with(wuhan_new_matrices, rowSums(school)/rowSums(work + home + other))
# 
# plWu1 = sp_contact_matrices("Wuhan (group = full strength; school is all with 0-19)", wuhan_new_matrices, legpos = "right")

# ASSUMPTION 2 - only contacts between 0-19 year olds are school, and group contacts are worth 0.5 as much as individual contacts
mat_g_school = mat_g
mat_g_nonschool = mat_g
mat_g_school[1:4, 5:NROW] = 0
mat_g_school[5:NROW, 1:NROW] = 0
mat_g_nonschool[1:4, 1:4] = 0

china_pops = populations[name %like% "^China \\| ", unique(name)]
for (reg_name in china_pops) {
    reg_pop = populations[name == reg_name, f + m];
    reg_pop = c(reg_pop[1:NROW-1], sum(reg_pop[NROW:length(reg_pop)]));

    reg_matrices = list(
        home   = add_names(symmetrize(mat_h, reg_pop)),
        work   = add_names(symmetrize(mat_w, reg_pop)),
        school = add_names(symmetrize(mat_s + 0.5*mat_g_school, reg_pop)),
        other  = add_names(symmetrize(mat_o + 0.5*mat_g_nonschool, reg_pop))
    )

    matrices[[reg_name]] = reg_matrices
}

# All China
C_pop = populations[name == "China", f + m]
C_pop = c(C_pop[1:(NROW-1)], sum(C_pop[NROW:length(C_pop)]))

C_new_matrices = list(
    home   = add_names(symmetrize(mat_h, C_pop)),
    work   = add_names(symmetrize(mat_w, C_pop)),
    school = add_names(symmetrize(mat_s + 0.5*mat_g_school, C_pop)),
    other  = add_names(symmetrize(mat_o + 0.5*mat_g_nonschool, C_pop))
)

matrices[["China | China"]] = C_new_matrices

# UK regional contact matrices
UKregions = populations[name %like% "^UK \\| ", unique(name)]

for (reg in UKregions) {
    print(reg)
    
    ruk_pop = populations[name == reg, f + m, by = age]$V1 * 1000
    ruk_survey_pop = data.frame(lower.age.limit = seq(0, 75, by = 5),
        population = c(ruk_pop[1:15], sum(ruk_pop[16:19])))

    ruk_home = contact_matrix(uk_survey, countries = "United Kingdom", age.limits = seq(0, 75, by = 5), 
        survey.pop = ruk_survey_pop, filter = list(cnt_home = 1), symmetric = T)$matrix
    ruk_work = contact_matrix(uk_survey, countries = "United Kingdom", age.limits = seq(0, 75, by = 5), 
        survey.pop = ruk_survey_pop, filter = list(cnt_work = 1), symmetric = T)$matrix
    ruk_scho = contact_matrix(uk_survey, countries = "United Kingdom", age.limits = seq(0, 75, by = 5), 
        survey.pop = ruk_survey_pop, filter = list(cnt_school = 1), symmetric = T)$matrix
    ruk_othe = contact_matrix(uk_survey, countries = "United Kingdom", age.limits = seq(0, 75, by = 5), 
        survey.pop = ruk_survey_pop, filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0), symmetric = T)$matrix

    rownames(ruk_home) = colnames(ruk_home) = limits_to_agegroups(seq(0, 75, by = 5))
    rownames(ruk_work) = colnames(ruk_work) = limits_to_agegroups(seq(0, 75, by = 5))
    rownames(ruk_scho) = colnames(ruk_scho) = limits_to_agegroups(seq(0, 75, by = 5))
    rownames(ruk_othe) = colnames(ruk_othe) = limits_to_agegroups(seq(0, 75, by = 5))
    
    matrices[[reg]] = list(
        home = ruk_home,
        work = ruk_work,
        school = ruk_scho,
        other = ruk_othe
    );
}

# Italy regional contact matrices
regioni = populations[name %like% "^Italy \\| ", unique(name)]

for (reg in regioni) {
    print(reg)
    
    rit_pop = populations[name == reg, f + m, by = age]$V1 * 1000
    rit_survey_pop = data.frame(lower.age.limit = seq(0, 75, by = 5),
        population = c(rit_pop[1:15], sum(rit_pop[16:19])))

    rit_home = contact_matrix(uk_survey, countries = "Italy", age.limits = seq(0, 75, by = 5), 
        survey.pop = rit_survey_pop, filter = list(cnt_home = 1), symmetric = T)$matrix
    rit_work = contact_matrix(uk_survey, countries = "Italy", age.limits = seq(0, 75, by = 5), 
        survey.pop = rit_survey_pop, filter = list(cnt_work = 1), symmetric = T)$matrix
    rit_scho = contact_matrix(uk_survey, countries = "Italy", age.limits = seq(0, 75, by = 5), 
        survey.pop = rit_survey_pop, filter = list(cnt_school = 1), symmetric = T)$matrix
    rit_othe = contact_matrix(uk_survey, countries = "Italy", age.limits = seq(0, 75, by = 5), 
        survey.pop = rit_survey_pop, filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0), symmetric = T)$matrix

    rownames(rit_home) = colnames(rit_home) = limits_to_agegroups(seq(0, 75, by = 5))
    rownames(rit_work) = colnames(rit_work) = limits_to_agegroups(seq(0, 75, by = 5))
    rownames(rit_scho) = colnames(rit_scho) = limits_to_agegroups(seq(0, 75, by = 5))
    rownames(rit_othe) = colnames(rit_othe) = limits_to_agegroups(seq(0, 75, by = 5))
    
    matrices[[reg]] = list(
        home = rit_home,
        work = rit_work,
        school = rit_scho,
        other = rit_othe
    );
}

# Zimbabwe regional contact matrices
zimreg = populations[name %like% "^Zimbabwe \\| ", unique(name)]

msum = function(mat)
{
    m = mat$matrices[[1]]$matrix * 1/length(mat$matrices)
    for (i in 2:length(mat$matrices)) {
        m = m + mat$matrices[[i]]$matrix * 1/length(mat$matrices)
    }
    m
}

for (reg in zimreg) {
    print(reg)
    
    rzm_pop = populations[name == reg, f + m, by = age]$V1 * 1000
    rzm_survey_pop = data.frame(lower.age.limit = seq(0, 75, by = 5),
        population = c(rzm_pop[1:15], sum(rzm_pop[16:18])))

    rzm_home = msum(contact_matrix(z_survey, age.limits = seq(0, 75, by = 5), 
        survey.pop = rzm_survey_pop, filter = list(cnt_home = 1), symmetric = T, estimated.contact.age = "sample", n = 10))
    rzm_work = msum(contact_matrix(z_survey, age.limits = seq(0, 75, by = 5), 
        survey.pop = rzm_survey_pop, filter = list(cnt_work = 1), symmetric = T, estimated.contact.age = "sample", n = 10))
    rzm_scho = msum(contact_matrix(z_survey, age.limits = seq(0, 75, by = 5), 
        survey.pop = rzm_survey_pop, filter = list(cnt_school = 1), symmetric = T, estimated.contact.age = "sample", n = 10))
    rzm_othe = msum(contact_matrix(z_survey, age.limits = seq(0, 75, by = 5), 
        survey.pop = rzm_survey_pop, filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0), symmetric = T, estimated.contact.age = "sample", n = 10))

    rownames(rzm_home) = colnames(rzm_home) = limits_to_agegroups(seq(0, 75, by = 5))
    rownames(rzm_work) = colnames(rzm_work) = limits_to_agegroups(seq(0, 75, by = 5))
    rownames(rzm_scho) = colnames(rzm_scho) = limits_to_agegroups(seq(0, 75, by = 5))
    rownames(rzm_othe) = colnames(rzm_othe) = limits_to_agegroups(seq(0, 75, by = 5))
    
    matrices[[reg]] = list(
        home = rzm_home,
        work = rzm_work,
        school = rzm_scho,
        other = rzm_othe
    );
}

saveRDS(matrices, "~/Dropbox/nCoV/0all_matrices.rds");
