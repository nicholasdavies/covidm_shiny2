library(geosphere)

wp_reg = qread("./data/worldpop5yr.qs");

# Create table with country properties and matrix if available
countries = wp_reg[level == 0];
avail = data.table(matrix = names(cm_matrices)[7:158]);
avail[, iso3c := countrycode(matrix, origin = "country.name", destination = "iso3c")];
countries = merge(countries, avail, by.x = "country", by.y = "iso3c", all.x = T);
countries[, mean_age := weighted.mean(0:16, unlist(.SD)), by = country, .SDcols = f_0:f_80]
countries = countries[!is.na(lon)]


# Get closest match for each
candidates = countries[!is.na(matrix)];
for (i in 1:nrow(countries))
{
    if (is.na(countries[i, matrix])) {
        candidates[, dist := distHaversine(c(countries[i, lon], countries[i, lat]), c(lon, lat)), by = country];
        candidates[, agediff := abs(mean_age - countries[i, mean_age]), by = country];
        candidates[, dist_rank := dist / mean(dist)];
        candidates[, age_rank := agediff / mean(agediff)];
        candidates[, overall_rank := rank(2 * dist_rank + age_rank)];
        candidates = candidates[order(overall_rank)];
        countries[i, matrix2 := candidates[1, matrix]];
    } else {
        countries[i, matrix2 := matrix];
    }
}

fwrite(countries[, .(iso3c = country, name = name, matrix = matrix2)], "./data/matrix_lookup.csv");

