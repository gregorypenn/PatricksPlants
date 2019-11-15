library(sp)
library(rgdal)
library(rgeos)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(e1071)
library(tibble)

associates <- function(input,taxon,elev_buffer=0,horiz_buffer="0",startdate="0",enddate){

df <- surveys
alpha <- 0.05

# example values for arguments...

# input <- surveys
# taxon <- "BOWA"
# elev_buffer <- 500 #in meters
# horiz_buffer <- 75 #in kilometers
# startdate <- "01-01"
# enddate <- "12-31"

df <- df %>%
  filter(exclude_from_analyses == FALSE) %>%
  mutate(present = 1,
         elevation_m = elevation*0.3048) %>%
  select(date,
         survey_id,
         site,
         longitude,
         latitude,
         elevation_m,
         plant_id,
         present)

# Filter for focal taxon and select relevant variables
taxon.df <- df %>% filter(plant_id == taxon) %>%
  select(latitude, longitude, elevation_m, survey_id, plant_id)

# Identify surveys with focal taxon present
surveys_with_taxon <- taxon.df$survey_id %>% unique()
n_surveys_with_taxon <- length(surveys_with_taxon)

# date filter; will not cross years (e.g., 11-01 to 2-01); startdate and enddate of form mm-dd, 
# must be enclosed in quotation marks in the argument.

if (!(startdate=="0")) {
  df <- filter(df,
               month(date) >= as.numeric(substr(startdate,0,2)),
               day(date) >= as.numeric(substr(startdate,4,5)),
               month(date) <= as.numeric(substr(enddate,0,2)),
               day(date) <= as.numeric(substr(enddate,4,5)))
}

# spatial filter, based on the horizontal buffer in km

if (!(horiz_buffer=="0")) {
  # Convert to spatial data
  taxon.sp <- taxon.df
  coordinates(taxon.sp) <- c("longitude", "latitude")
  proj4string(taxon.sp) <- CRS("+proj=longlat +datum=NAD83")
  
  # Transform to metric coord system, calculate hull, buffer hull
  taxon.sp <- spTransform(taxon.sp, CRS=CRS("+proj=utm +zone=12 +units=m"))
  taxon_hull <- gConvexHull(taxon.sp)
  study_area.polygon <- gBuffer(taxon_hull, width = horiz_buffer*10^3)
  
  # Make spatial tibble
  df.sp <- df
  coordinates(df.sp) <- c("longitude", "latitude")
  proj4string(df.sp) <- CRS("+proj=longlat +datum=NAD83")
  
  # Transform the polygon's coord system to be consistent with the dataset
  study_area.polygon <- spTransform(study_area.polygon, CRS=proj4string(df.sp))
  # is_in_polygon <- !is.na(over(df.sp, study_area.polygon))
  df <- df.sp[study_area.polygon,] %>% as_tibble()
}

# elevation filter, based on elevation buffer in meters

if (!(elev_buffer==0)) {
  elevation.min.taxon <- taxon.df$elevation_m %>% min()
  elevation.max.taxon <- taxon.df$elevation_m %>% max()
  
  df <- df %>%
    filter(elevation_m >= elevation.min.taxon - elev_buffer,
           elevation_m <= elevation.max.taxon + elev_buffer) %>%
    mutate(elevation_m = round(elevation_m))
}



surveys.df <- df %>%
  select(date,
         survey_id, 
         site, 
         longitude, 
         latitude, 
         elevation_m) %>%
  distinct()

# The below has something to do with display stuff for knitr?

# write.csv(surveys.df, file = "surveys.csv", row.names = FALSE)
# surveys.kable <- kable(surveys.df)


# Select relevant fields
presence.df <- df %>%
  select(plant_id
         , survey_id
         , present
  )

presence_at_taxon_sites.df <- presence.df %>%
  filter(survey_id %in% surveys_with_taxon)
species_with_taxon <- presence_at_taxon_sites.df$plant_id %>% unique()

n_species_with_taxon <- length(species_with_taxon)
n_species_total <- presence.df$plant_id %>% unique() %>% length()

# Filter out species that never occur with taxon.
# Negative associations have been previously ruled out.
presence.df <- filter(presence.df, plant_id %in% species_with_taxon)

n_surveys <- presence.df$survey_id %>% unique() %>% length()

cooccurrence_full.df <- group_by(presence_at_taxon_sites.df, plant_id) %>% 
  summarize(count = sum(present)) %>%
  mutate(rate = count/n_surveys_with_taxon) %>%
  arrange(desc(rate))

presence_absence_surveys.df <- presence.df %>%
  complete(plant_id, survey_id, fill=list(present=0)) %>%
  mutate(present = as.factor(present))

presence_absence_surveys_w.df <- presence_absence_surveys.df %>%
  filter(plant_id %in% species_with_taxon) %>%
  spread(key = plant_id, value = present)

presence_absence_w.df <- select(presence_absence_surveys_w.df, -survey_id)



#### Find significant associations ####

colnames(presence_absence_w.df)[colnames(presence_absence_w.df)==taxon] <- "taxon"

taxonbin.df <- as.numeric(as.character(presence_absence_w.df$taxon))
candidates.df <- select(presence_absence_w.df, -taxon)
model_association <- function (x) { glm(taxonbin.df ~ x, family = binomial(link = "logit")) }
extract_p <- function (x) { coef(summary(x))[2,4] }
p_values <- apply(candidates.df, 2, function (x) extract_p(model_association(x))) %>%
  p.adjust(method = "bonferroni")
p_values.df <- data_frame(associate = names(p_values), p = p_values) %>% arrange(p)
sig_p_values.df <- filter(p_values.df, p < alpha)
associates.df <- select(candidates.df, one_of(sig_p_values.df$associate))
sig_presence_absence_w.df <- select(presence_absence_w.df, taxon, 
                                    one_of(sig_p_values.df$associate))
associates <- names(associates.df)
n_associates <- length(associates)

#### Naive Bayes Classifier ####
library(e1071)
sig_presence_absence_w.df$taxon <- as.factor(sig_presence_absence_w.df$taxon)
model_naiveBayes <- naiveBayes(taxon ~ ., data = sig_presence_absence_w.df)
nB_pred <- predict(model_naiveBayes, sig_presence_absence_w.df)
table(nB_pred, sig_presence_absence_w.df$taxon)

# taxon_and_top4.df <- select(sig_presence_absence_w.df, one_of(c("taxon", top4predictors)))
# top4.model <- naiveBayes(taxon ~ ., data = taxon_and_top4.df)
# top4.predicted <- predict(top4.model, newdata = taxon_and_top4.df)
# table(top4.predicted, sig_presence_absence_w.df$taxon)

# Posterior distributions of P(taxon | associate)
# Beta distributions
require(tibble)
posteriors.matrix <- associates.df %>%
  apply(2, function (x) table(x, taxonbin.df)[c(4,3)]) %>%
  apply(2, function (x) x + 0.5) %>%
  t()
colnames(posteriors.matrix) <- c("shape1", "shape2")

# posteriors.df <- as.data.frame(p_taxon_posterior_params.matrix) %>%
#   rownames_to_column(var = "associate") %>%
#   mutate(expected = qbeta(0.5, shape1, shape2))

beta_plot <- function (a, b, interval = 0.95) {
  tail.size <- (1 - interval) / 2
  lower <- qbeta(tail.size, a, b, lower.tail = TRUE)
  upper <- qbeta(tail.size, a, b, lower.tail = FALSE)
  ggplot(data.frame(x = c(0, 1)), aes(x)) +
    stat_function(fun = dbeta, args = list(shape1 = a, shape2 = b)) +
    geom_vline(xintercept = lower, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = upper, linetype = "dashed", color = "grey") +
    ggtitle("Posterior distribution of P(taxon | associate)",
            subtitle = str_c("Credible interval ", 100*interval, "%", sep = "")) +
    labs(x = "probability", y = "density")
}

beta_expected_value <- function (a1, a2) { return(a1/(a1 + a2)) }
beta_variance <- function (a1, a2) {
  v <- (a1*a2) /
    ((a1 + a2)^2 * (a1 + a2 + 1))
  return(v)
}

hyper_prior <- 0.5
precision <-   function (tb, report.var = FALSE) {
  a1 <- hyper_prior + tb[2,2]
  a2 <- hyper_prior + tb[2,1]
  estimate <- beta_expected_value(a1, a2)
  
  if (report.var) {
    v <- beta_variance(a1, a2)
    result <- c(estimate, v)
    names(result) <- c("precision.est", "precision.var")
    return(result)
  }
  else { return(estimate) }
}

sensitivity <- function (tb, report.var = FALSE) {
  a1 <- hyper_prior + tb[2,2]
  a2 <- hyper_prior + tb[1,2]
  estimate <- beta_expected_value(a1, a2)
  
  if (report.var) {
    v <- beta_variance(a1, a2)
    result <- c(estimate, v)
    names(result) <- c("sensitivity.est", "sensitivity.var")
    return(result)
  }
  else { return(estimate) }
}

accuracy <- function (tb, report.var = FALSE) {
  a1 <- tb[1,1] + tb[2,2]
  a2 <- tb[1,2] + tb[2,1]
  estimate <- beta_expected_value(a1, a2)
  if (report.var) {
    v <- beta_variance(a1, a2)
    result <- c(estimate, v)
    names(result) <- c("sensitivity.est", "sensitivity.var")
    return(result)
  }
  else { return(estimate) }
}

accuracy_plot <- function (tb, interval = 0.95) {
  associate <- names(dimnames(tb))[1]
  title <- str_c("Accuracy Posterior distribution of P(", associate, ", taxon) + P(!", associate, ", !taxon)", sep = " ")
  interval_label <- str_c("Credible interval ", 100*interval, "%", sep="")
  a1 <- 0.5 + tb[1,1] + tb[2,2]
  a2 <- 0.5 + tb[1,2] + tb[2,1]
  tail <- (1 - interval)/2
  lower <- qbeta(tail, a1, a2)
  upper <- qbeta(1 - tail, a1, a2)
  mu <- beta_expected_value(a1, a2)
  dmax <- dbeta(mu, a1, a2)
  p <- beta_plot(a1, a2) +
    geom_vline(xintercept = lower, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = upper, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = mu, linetype = "dotted", color = "grey") +
    annotate("text", x = mu, y = dmax/2,
             label = paste("mu=", round(mu, 2), sep="")) +
    labs(x = "probability", y = "density") +
    ggtitle(title, subtitle = interval_label)
  
  return(p)
}

sensitivity_plot <- function (tb, interval = 0.95) {
  associate <- names(dimnames(tb))[1]
  title <- str_c("Sensitivity: Posterior distribution of P(", associate, "| taxon)", sep = " ")
  interval_label <- str_c("Credible interval ", 100*interval, "%", sep="")
  a1 <- 0.5 + tb[2,2]
  a2 <- 0.5 + tb[1,2]
  tail <- (1 - interval)/2
  lower <- qbeta(tail, a1, a2)
  upper <- qbeta(1 - tail, a1, a2)
  mu <- beta_expected_value(a1, a2)
  dmax <- dbeta(mu, a1, a2)
  p <- beta_plot(a1, a2) +
    geom_vline(xintercept = lower, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = upper, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = mu, linetype = "dotted", color = "grey") +
    annotate("text", x = mu, y = dmax/2,
             label = paste("mu=", round(mu, 2), sep="")) +
    labs(x = "probability", y = "density") +
    ggtitle(title, subtitle = interval_label)
  
  return(p)
}

precision_plot <- function (tb, interval = 0.95) {
  associate <- names(dimnames(tb))[1]
  title <- str_c("Precision: Posterior distribution of P( taxon |", associate,  ")", sep = " ")
  interval_label <- str_c("Credible interval ", 100*interval, "%", sep="")
  a1 <- 0.5 + tb[2,2]
  a2 <- 0.5 + tb[2,1]
  tail <- (1 - interval)/2
  lower <- qbeta(tail, a1, a2)
  upper <- qbeta(1 - tail, a1, a2)
  mu <- beta_expected_value(a1, a2)
  dmax <- dbeta(mu, a1, a2)
  p <- beta_plot(a1, a2) +
    geom_vline(xintercept = lower, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = upper, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = mu, linetype = "dotted", color = "grey") +
    annotate("text", x = mu, y = dmax/2,
             label = paste("mu=", round(mu, 2), sep="")) +
    labs(x = "probability", y = "density") +
    ggtitle(title, subtitle = interval_label)
  
  return(p)
}

ctbl <- function (species) {
  i <- which(names(associates.df) == species)
  tb <- table(associates.df[[i]], taxonbin.df, dnn = c(associates[i], taxon)) %>% addmargins()
  
  return(tb)
}

predictors.df <- data_frame()
for (i in 1:length(names(associates.df))) {
  associate <- names(associates.df)[i]
  tb <- table(associates.df[[i]], taxonbin.df, dnn = c(associate, taxon)) %>% addmargins()
  d <- data_frame(associate = names(associates.df)[i],
                  accuracy.est = accuracy(tb),
                  accuracy.var = accuracy(tb, report.var = TRUE)[2],
                  sensitivity.est = sensitivity(tb),
                  sensitivity.var = sensitivity(tb, report.var = TRUE)[2],
                  precision.est = precision(tb),
                  precision.var = precision(tb, report.var = TRUE)[2]
  )
  predictors.df <- bind_rows(predictors.df, d)
}

predictors.df <- predictors.df %>%
  inner_join(select(sig_p_values.df, associate, p)) %>%
  arrange(desc(accuracy.est))

# The below has something to do with display stuff for knitr?

# predictors.kable <- predictors.df %>%
#  select(Associate = associate,
#         Accuracy = accuracy.est,
#         Sensitivity = sensitivity.est,
#         Precision = precision.est,
#         "p-value" = p) %>%
#  kable(digits = 3, row.names = FALSE, caption = "Associated taxa")

predictors.df
csvname <- paste(taxon,"_associates.csv",sep="")
write.csv(predictors.df, csvname, row.names = FALSE)

library(ggmap)
bounding_box <- make_bbox(lon = longitude, lat = latitude,
                          data = surveys.df)

nm_stamen_toner <- get_map(bounding_box, 
                           source = "stamen",
                           maptype = "toner", 
                           crop = FALSE)

taxon_presence_absence.df <- presence_absence_surveys.df %>%
  filter(plant_id == taxon) %>%
  left_join(surveys.df)

levels(taxon_presence_absence.df$present) <- c("absent", "present")
presence_absence.map <- ggmap(nm_stamen_toner) +
  geom_point(aes(x = longitude, y = latitude, color = present),
             alpha = 0.5, data = taxon_presence_absence.df) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  guides(color = guide_legend(title = NULL)) +
  ggtitle("Survey sites")

mapname <- paste(taxon,"_map.pdf", sep="")
ggsave(filename = mapname, plot = presence_absence.map)

rate.cutoff <- 0.23
cooccurrence.df <- filter(cooccurrence_full.df, plant_id != taxon, rate >= rate.cutoff) %>%
  left_join(predictors.df, by = c("plant_id" = "associate")) %>%
  select(plant_id, count, rate, accuracy = accuracy.est) %>%
  mutate(associate = !is.na(accuracy))

cooccurrence.plot <- ggplot(filter(cooccurrence.df),
                            aes(x = reorder(plant_id, -rate), y = rate, color = associate)) +
  geom_point() +
  scale_color_manual(values = c("black", "dark orange")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "species", y = "cooccurrence rate", title = "Species cooccurrence")

plotname <- paste(taxon,"_cooccurrence.pdf", sep="")
ggsave(filename = plotname, plot = cooccurrence.plot)

return(predictors.df)
}
