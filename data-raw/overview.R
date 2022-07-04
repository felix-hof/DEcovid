overview <- tribble(
  ~dataset,                     ~time_sensitive, ~time_resolution,     ~space_sensitive, ~space_resolution, ~age_sensitive, ~age_resolution, ~needs_fetching, ~is_intercept, ~include,
  "cases",                      TRUE,            "daily",              TRUE,             3L,                TRUE,           "age",           TRUE,            FALSE,         function(time_res, spat_res, age_res) TRUE, 
  "population",                 FALSE,           NA_character_,        TRUE,             3L,                TRUE,           "age",           TRUE,            FALSE,         function(time_res, spat_res, age_res) TRUE, 
  "density",                    FALSE,           NA_character_,        TRUE,             3L,                FALSE,          "age",           TRUE,            FALSE,         function(time_res, spat_res, age_res) if(spat_res == 0L) FALSE else TRUE, 
  "gravity",                    FALSE,           NA_character_,        TRUE,             3L,                TRUE,           "age",           TRUE,            FALSE,         function(time_res, spat_res, age_res) if(spat_res == 0L) FALSE else TRUE, 
  "urbanicity",                 FALSE,           NA_character_,        TRUE,             3L,                FALSE,          "no_age",        TRUE,            FALSE,         function(time_res, spat_res, age_res) if(spat_res == 0L) FALSE else TRUE, 
  "testing",                    TRUE,            "weekly",             FALSE,            0L,                FALSE,          "no_age",        TRUE,            FALSE,         function(time_res, spat_res, age_res) TRUE, 
  "temperature",                TRUE,            "daily",              TRUE,             3L,                FALSE,          "no_age",        TRUE,            FALSE,         function(time_res, spat_res, age_res) TRUE, 
  "vaccination",                TRUE,            "weekly",             TRUE,             1L,                TRUE,           "age",           TRUE,            FALSE,         function(time_res, spat_res, age_res) TRUE, 
  "stringency",                 TRUE,            "daily",              FALSE,            0L,                FALSE,          "no_age",        TRUE,            FALSE,         function(time_res, spat_res, age_res) TRUE, 
  "holidays",                   TRUE,            "daily",              TRUE,             1L,                FALSE,          "no_age",        TRUE,            FALSE,         function(time_res, spat_res, age_res) TRUE, 
  "variants",                   TRUE,            "weekly",             FALSE,            0L,                FALSE,          "no_age",        TRUE,            FALSE,         function(time_res, spat_res, age_res) TRUE, 
  "boundary_inds",              FALSE,           NA_character_,        TRUE,             3L,                FALSE,          "no_age",        TRUE,            FALSE,         function(time_res, spat_res, age_res) if(spat_res == 0L) FALSE else TRUE, 
  "weekday",                    TRUE,            "daily",              FALSE,            3L,                FALSE,          "no_age",        TRUE,            FALSE,         function(time_res, spat_res, age_res) if(time_res == "daily") TRUE else FALSE, 
  "weekend",                    TRUE,            "daily",              FALSE,            3L,                FALSE,          "no_age",        TRUE,            FALSE,         function(time_res, spat_res, age_res) if(time_res == "daily") TRUE else FALSE, 
  "seasonality",                TRUE,            "both",               FALSE,            3L,                FALSE,          "no_age",        FALSE,           FALSE,         function(time_res, spat_res, age_res) TRUE, 
  "fe(1, unitSpecific = TRUE)", FALSE,           NA_character_,        TRUE,             3L,                TRUE,           "no_age",        FALSE,           TRUE,          function(time_res, spat_res, age_res) if(spat_res == 0L) FALSE else TRUE 
)

usethis::use_data(overview, overwrite = TRUE)
