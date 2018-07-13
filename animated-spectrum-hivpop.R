library(data.table)
library(ggplot2)
library(magrittr)

## install.packages("animation")
## install.packages("~/Downloads/gganimate-0.1.1.tar.gz")
library(gganimate)

devtools::load_all("~/Documents/Code/R/eppasminputs")

#' ## WPP 2017

wpp17m <- as.data.frame(readxl::read_excel("~/Documents/Data/WPP/WPP2017/WPP2017_POP_F15_2_ANNUAL_POPULATION_BY_AGE_MALE.xlsx", "ESTIMATES", skip=16))
wpp17f <- as.data.frame(readxl::read_excel("~/Documents/Data/WPP/WPP2017/WPP2017_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE.xlsx", "ESTIMATES", skip=16))

wpp17Mm <- as.data.frame(readxl::read_excel("~/Documents/Data/WPP/WPP2017/WPP2017_POP_F15_2_ANNUAL_POPULATION_BY_AGE_MALE.xlsx", "MEDIUM VARIANT", skip=16))
wpp17Mf <- as.data.frame(readxl::read_excel("~/Documents/Data/WPP/WPP2017/WPP2017_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE.xlsx", "MEDIUM VARIANT", skip=16))

names(wpp17m)[1:6] <- names(wpp17f)[1:6] <- 
  names(wpp17Mm)[1:6] <- names(wpp17Mf)[1:6] <- c("index", "variant", "country", "notes", "countrycode", "year")

wpp17m$notes <- wpp17f$notes <-
  wpp17Mm$notes <- wpp17Mf$notes <- NULL

wpp17m$sex <- wpp17Mm$sex <- "male"
wpp17f$sex <- wpp17Mf$sex <- "female"


idvars <- c("index", "variant", "country", "countrycode", "year", "sex")
wpp17m <- melt(wpp17m, id.vars = idvars, variable.name = "agegr", value.name = "pop")
wpp17f <- melt(wpp17f, id.vars = idvars, variable.name = "agegr", value.name = "pop")
wpp17Mm <- melt(wpp17Mm, id.vars = idvars, variable.name = "agegr", value.name = "pop")
wpp17Mf <- melt(wpp17Mf, id.vars = idvars, variable.name = "agegr", value.name = "pop")

wpp17 <- rbind(wpp17m, wpp17f, subset(wpp17Mm, year != 2015), subset(wpp17Mf, year != 2015)) %>% data.table
wpp17$source <- "WPP 2017"
wpp17$pop <- as.numeric(wpp17$pop)

wpp17$agegr[wpp17$agegr %in% c("80-84", "85-89", "90-94", "95-99", "100+")] <- "80+"
wpp17 <- wpp17[!is.na(pop) , .(pop = sum(pop)), .(country, source, year, sex, agegr)]
wpp17$pop <- 1e3*wpp17$pop


#' ## Spectrum 2017 estimates

data(specres_nat17)

zw <- specres_nat17[["Zimbabwe"]]

names(dimnames(zw$hivpop)) <- c("age", "sex", "year")


df <- zw[c("totpop.m", "totpop.f",
           "hivnum.m", "hivnum.f",
            "artnum.m", "artnum.f",
            "aidsdeaths.m", "aidsdeaths.f",
            "newinf.m", "newinf.f")] %>%
  lapply(function(x) create_beers(17) %*% x) %>%
  lapply("dimnames<-", list(age = 0:80, year = 1970:2021)) %>%
  lapply(melt) %>%
  Map(f=data.table, stringsAsFactors = FALSE,
      indicator = rep(c("totpop", "hivnum", "artnum", "aidsdeaths", "newinf"), each = 2),
      sex = rep(c("male", "female"), 5), .) %>%
  do.call(rbind, .) %>%
  "$<-"("age", as.integer(.$age)) %>%
  "$<-"("year", as.integer(.$year))

df$cohort <- df$year - df$age

  
df <- rbind(df,
            df[indicator == "aidsdeaths" & age >= 15,
                   .(indicator = "cumhivdeaths", year,
                     age, value = cumsum(value)),
                   .(sex, cohort)])

df <- rbind(df,
            dcast(df[indicator %in% c("hivnum", "newinf")], ... ~ indicator)[
            , .(sex, age, year, cohort, indicator = "previnf", value = hivnum - newinf)])

df <- rbind(df, 
            df[indicator %in% c("cumhivdeaths", "hivnum"),
               .(indicator = "cumhivpop", value = sum(value)),
               .(sex, age, year, cohort)])

df <- df[year %in% 1988:2017 & age >= 15 & age < 50]

df <- merge(df, df[indicator == "totpop" & age == 15, .(sex, cohort, cohortsize = value)])
df[ , prop := value / cohortsize]

dfp <- df

dfp$indicator <- factor(dfp$indicator, c("cumhivpop", "hivnum", "previnf", "artnum"))
dfp$group <- interaction(dfp$indicator, dfp$sex)
dfp[ , plotval := value * c(male = -1, female = 1)[sex]]
dfp[ , plotprop := prop * c(male = -1, female = 1)[sex]]
dfp$coloridx <- sub("cumhivpop.*", "cumhivpop", dfp$group) %>%
  factor(c("hivnum.male", "previnf.male",  "artnum.male",
           "hivnum.female", "previnf.female",  "artnum.female",
           "cumhivpop"))

fill_pal <- setNames(c(brewer.pal(6, "RdBu"), "grey90"),
                     c("hivnum.female", "previnf.female", "artnum.female",
                       "artnum.male", "previnf.male", "hivnum.male", "cumhivpop"))

fill_lab <- c(hivnum.female = "New infections, Women", hivnum.male = "New infections, Men",
              previnf.female = "Untreated HIV, Women", previnf.male = "Untreated HIV, Men",
              artnum.female = "Receiving ART, Women", artnum.male = "Receiving ART, Men",
              cumhivpop = "AIDS deaths")


## dfp <- dfp[year %in% c(1988, 1998, 2008, 2017)]
dfp <- dfp[year %in% 1988:2017] # c(1988, 1998, 2008, 2017)]

plot_coh <- c(1965, 1975, 1985, 1995)

dflag <- merge.default(df[indicator == "cumhivpop" & cohort %in% plot_coh],
                       data.frame(lag = 0:15), by = NULL) %>%
  data.table

dflag$yearlag <- dflag$year
dflag$year <- dflag$year + dflag$lag
dflag[ , plotval := value * c(male = -1, female = 1)[sex]]
dflag[ , plotprop := prop * c(male = -1, female = 1)[sex]]

dflag <- dflag[year %in% 1989:2017 & !(lag == 0 & age == 15),
               c(setNames(approx(lag, plotval, seq(4/3, max(lag), 1/3)), c("lag", "plotval")),
                 .(plotprop = approx(lag, plotprop, seq(4/3, max(lag), 1/3))$y)),
               .(sex, year, cohort)]
dflag[ , age := year - cohort - lag]

p <- ggplot(dfp[indicator %in%  c("cumhivpop", "hivnum", "previnf", "artnum")],
            aes(age, ymin = 0,
                ymax = plotval,
                group = group,
                fill = coloridx, frame = year)) +
  geom_ribbon(alpha=0.75) +
  geom_point(data = dflag,
             aes(y = plotval,
                 group = paste0(sex, cohort),
                 fill = NULL,
                 size = lag),
             show.legend = FALSE) +
  geom_text(data = dfp[indicator == "cumhivpop" & cohort %in% plot_coh],
            aes(age, plotval, label = paste0("Born\n", cohort)),
            size = rel(4.5)) + 
  scale_y_continuous(labels=abs, limits=max(dfp[indicator == "cumhivpop"]$value) * 1.1 * c(-1, 1)) +
  scale_size_continuous(range = c(1, 0.25)) +
  scale_fill_manual(element_blank(),
                    values = fill_pal,
                    labels = fill_lab) +
  coord_flip() +
  labs(title = "Zimbabwe, HIV population:",
       x = "Age",
       y = "Number of adults",
       caption = "Source: UNAIDS 2017 estimates (aidsinfo.unaids.org)") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold"))


p_prev <- ggplot(dfp[indicator %in%  c("cumhivpop", "hivnum", "previnf", "artnum")],
                 aes(age, ymin = 0,
                     ymax = plotprop,
                     group = group,
                     fill = coloridx, frame = year)) +
  geom_ribbon(alpha=0.75) +
  geom_point(data = dflag,
             aes(y = plotprop,
                 group = paste0(sex, cohort),
                 fill = NULL,
                 size = lag),
             show.legend = FALSE) +
  geom_text(data = dfp[indicator == "cumhivpop" & cohort %in% plot_coh],
            aes(age, plotprop, label = paste0("Born\n", cohort)),
            size = rel(4.5)) + 
  scale_y_continuous(labels=abs, limits=max(dfp[indicator == "cumhivpop"]$prop) * 1.1 * c(-1, 1)) +
  scale_size_continuous(range = c(1, 0.25)) +
  scale_fill_manual(element_blank(),
                    values = fill_pal,
                    labels = fill_lab) +
  coord_flip() +
  labs(title = "Zimbabwe, cumulative HIV prevalence:",
       x = "Age",
       y = "Proportion of cohort",
       caption = "Source: UNAIDS 2017 estimates (aidsinfo.unaids.org)") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold"))


animation::ani.options(loop = 3)
gganimate(p, filename = "~/Downloads/zw-hivpop.gif",
          loop = 3, ani.width = 750, ani.height = 550, ani.res = 150, interval = 0.5)

gganimate(p_prev, filename = "~/Downloads/zw-hivprev.gif",
          loop = 3, ani.width = 750, ani.height = 550, ani.res = 150, interval = 0.5)
