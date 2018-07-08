Potential outcomes causal model
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Yule (1899)

``` r
# get the data
dat <- yule

# reshape yule to long
pat <- '(^[A-z]+[65]*)(\\d{2,4})(_\\d|O|I)*'
dat %<>% gather(variable, value, -1:-7)
dat %<>% mutate(
  series = str_replace(dat$variable, pat, '\\1\\3'),
  year = str_replace(dat$variable, pat, '\\2')
  ) %>%
  filter(!year %in% c('7181', '8191')) %>%
  select(-variable)

# fix two digit years
dat %<>% within({
  year[year %in% str_c(7:9, 1)] <- str_c(18, year[year %in% str_c(7:9, 1)])
  })

# reshape yule to wide (panel data)
dat %<>% spread(series, value, c(-1:-7, -10), fill = NA)

# prefer Popn_2 for population changes
dat %<>% mutate(
  Popn_2 = ifelse(is.na(Popn_2), Popn, Popn_2)
  )

# calculate out/old ratios
dat %<>% mutate(
  outratio = (ChABO + ChNotABO + FABO + FNotABO + MABO + MNotABO) / 
    (ChABI + ChNotABI + FABI + FNotABI + MABI + MNotABI),
  oldratio = (F65 + M65) / Popn_2,
  outratio = ifelse(outratio == Inf, NA, outratio),
  oldratio = ifelse(oldratio == Inf, NA, oldratio)
  )

# calculate decadal changes
dat %<>%
  group_by_at(1:7) %>%
  mutate(
    dpop  = Popn_2 / lag(Popn_2, order_by = year) * 100,
    dold  = oldratio / lag(oldratio, order_by = year) * 100,
    dout  = outratio / lag(outratio, order_by = year) * 100,
    dpaup = pauper / lag(pauper, order_by = year) * 100
    )

# impute dpop (= 0) if only missing 1891
IDs <- dat %>%
  group_by(ID) %>%
  summarise(na = sum(is.na(Popn_2))) %>%
  filter(na == 2) %>%
  use_series('ID')
dat %<>% mutate(
  dpop  = ifelse(is.na(dpop) & year == 1891 & !ID %in% IDs, 0, dpop)
  )

# adjust if dpaup = Inf
dat %<>% mutate(
  dpaup = ifelse(dpaup == Inf, NA, dpaup)
  )

# model of poverty and public assistance
reg <- lm(dpaup ~ dout + dold + dpop, data = filter(dat, Type > 1))

# table 10
vars <- c('Outrelief', 'Old', 'Pop')
stargazer(
  reg,
  #column.labels = labs,
  covariate.labels = vars,
  dep.var.labels = 'Pauperism growth',
  dep.var.labels.include = T,
  notes = '@ p < 0.10, @@ p < 0.05, @@@ p < 0.01',
  notes.append = F,
  star.char = c("@", "@@", "@@@"),
  type = 'html'
  )
```

<table style="text-align:center">

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="1" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Pauperism growth

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Outrelief

</td>

<td>

0.312<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.019)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Old

</td>

<td>

\-0.031<sup>@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.017)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Pop

</td>

<td>

\-0.224<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.036)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

81.392<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(5.084)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

703

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.302

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.299

</td>

</tr>

<tr>

<td style="text-align:left">

Residual Std. Error

</td>

<td>

21.519 (df = 699)

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

100.872<sup>@@@</sup> (df = 3; 699)

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td style="text-align:right">

@ p \< 0.10, @@ p \< 0.05, @@@ p \< 0.01

</td>

</tr>

</table>

## Simple difference in means decomposition

``` r
# construct the data
dat <- tribble(
  ~patient, ~y1, ~y0,
  1, 7, 1,
  2, 5, 6,
  3, 5, 1,
  4, 7, 8,
  5, 4, 2,
  6, 10, 1,
  7, 1, 10,
  8, 5, 6,
  9, 3, 7,
  10, 9, 8
  )
dat %<>% mutate(delta = y1 - y0)

# gap function
gap <- function(data) {
  tmp <- sample(1:10, 5, replace = F)
  mean(data$y1[1:10 %in% tmp]) - mean(data$y0[!1:10 %in% tmp])
}

# simulate
sdo <- replicate(1E4, gap(dat)) ; skim(sdo)
```

    ## 
    ## Skim summary statistics
    ## 
    ## ── Variable type:numeric ────────────────────────────────────────────────────────────
    ##  variable missing complete     n mean  sd   p0  p25 p50 p75 p100     hist
    ##       sdo       0    10000 10000 0.61 1.1 -1.8 -0.2 0.6 1.4    3 ▂▅▅▇▆▅▃▂

## STAR Experiment

``` r
library(lfe)

# get the data
dat <- star_sw

# models
mod <- 'tscorek ~ sck + rak' %>%
  list(
    str_c(., ' | schidkn'),
    str_c(., ' + white + boy + freelunk | schidkn'),
    str_c(., ' + white + boy + freelunk + totexpk | schidkn')
    )

# regressions
reg <- map(mod, ~felm(as.formula(.), data = dat))

# figure 8
vars <- c(
  'Small class',
  'Regular/aide class',
  'White',
  'Boy',
  'Free lunch',
  'Teacher experience'
  )
stargazer(
  reg,
  #column.labels = labs,
  covariate.labels = vars,
  dep.var.labels = 'Kindergarten test score',
  dep.var.labels.include = T,
  notes = '@ p < 0.10, @@ p < 0.05, @@@ p < 0.01',
  notes.append = F,
  star.char = c("@", "@@", "@@@"),
  type = 'html'
  )
```

<table style="text-align:center">

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="4">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="4">

Kindergarten test score

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

<td>

(4)

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Small class

</td>

<td>

13.899<sup>@@@</sup>

</td>

<td>

16.022<sup>@@@</sup>

</td>

<td>

15.977<sup>@@@</sup>

</td>

<td>

15.901<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.409)

</td>

<td>

(2.169)

</td>

<td>

(2.091)

</td>

<td>

(2.090)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Regular/aide class

</td>

<td>

0.314

</td>

<td>

1.699

</td>

<td>

2.146

</td>

<td>

1.765

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.310)

</td>

<td>

(2.085)

</td>

<td>

(2.011)

</td>

<td>

(2.019)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

White

</td>

<td>

</td>

<td>

</td>

<td>

23.986<sup>@@@</sup>

</td>

<td>

24.224<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

(3.470)

</td>

<td>

(3.468)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Boy

</td>

<td>

</td>

<td>

</td>

<td>

\-12.161<sup>@@@</sup>

</td>

<td>

\-12.088<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

(1.664)

</td>

<td>

(1.666)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Free lunch

</td>

<td>

</td>

<td>

</td>

<td>

\-35.022<sup>@@@</sup>

</td>

<td>

\-34.882<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

(2.010)

</td>

<td>

(2.014)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Teacher experience

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

0.661<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

(0.162)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

918.043<sup>@@@</sup>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.641)

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

5,786

</td>

<td>

5,786

</td>

<td>

5,768

</td>

<td>

5,748

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.007

</td>

<td>

0.231

</td>

<td>

0.288

</td>

<td>

0.291

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.007

</td>

<td>

0.220

</td>

<td>

0.278

</td>

<td>

0.280

</td>

</tr>

<tr>

<td style="text-align:left">

Residual Std. Error

</td>

<td>

73.490 (df = 5783)

</td>

<td>

65.136 (df = 5705)

</td>

<td>

62.716 (df = 5684)

</td>

<td>

62.667 (df = 5663)

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="4" style="text-align:right">

@ p \< 0.10, @@ p \< 0.05, @@@ p \< 0.01

</td>

</tr>

</table>

``` r
library(lfe)

# get the data
dat <- star_sw

# models
mod <- 'tscore1 ~ sc1 + ra1' %>%
  list(
    str_c(., ' | schid1n'),
    str_c(., ' + white + boy + freelun1 | schid1n'),
    str_c(., ' + white + boy + freelun1 + totexp1 | schid1n')
    )

# regressions
reg <- map(mod, ~felm(as.formula(.), data = dat))

# figure 8
vars <- c(
  'Small class',
  'Regular/aide class',
  'White',
  'Boy',
  'Free lunch',
  'Teacher experience'
  )
stargazer(
  reg,
  #column.labels = labs,
  covariate.labels = vars,
  dep.var.labels = 'First grade test score',
  dep.var.labels.include = T,
  notes = '@ p < 0.10, @@ p < 0.05, @@@ p < 0.01',
  notes.append = F,
  star.char = c("@", "@@", "@@@"),
  type = 'html'
  )
```

<table style="text-align:center">

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="4">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="4">

First grade test score

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

<td>

(4)

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Small class

</td>

<td>

29.781<sup>@@@</sup>

</td>

<td>

29.009<sup>@@@</sup>

</td>

<td>

27.194<sup>@@@</sup>

</td>

<td>

26.716<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.807)

</td>

<td>

(2.491)

</td>

<td>

(2.412)

</td>

<td>

(2.427)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Regular/aide class

</td>

<td>

11.959<sup>@@@</sup>

</td>

<td>

7.240<sup>@@@</sup>

</td>

<td>

6.817<sup>@@@</sup>

</td>

<td>

6.292<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.686)

</td>

<td>

(2.417)

</td>

<td>

(2.356)

</td>

<td>

(2.374)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

White

</td>

<td>

</td>

<td>

</td>

<td>

24.476<sup>@@@</sup>

</td>

<td>

24.426<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

(3.701)

</td>

<td>

(3.700)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Boy

</td>

<td>

</td>

<td>

</td>

<td>

\-11.016<sup>@@@</sup>

</td>

<td>

\-11.079<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

(1.933)

</td>

<td>

(1.933)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Free lunch

</td>

<td>

</td>

<td>

</td>

<td>

\-45.841<sup>@@@</sup>

</td>

<td>

\-45.834<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

(2.320)

</td>

<td>

(2.320)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Teacher experience

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

0.223<sup>@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

(0.126)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

1,039.393<sup>@@@</sup>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.836)

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

6,379

</td>

<td>

6,379

</td>

<td>

6,227

</td>

<td>

6,227

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.017

</td>

<td>

0.257

</td>

<td>

0.319

</td>

<td>

0.319

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.017

</td>

<td>

0.248

</td>

<td>

0.310

</td>

<td>

0.310

</td>

</tr>

<tr>

<td style="text-align:left">

Residual Std. Error

</td>

<td>

90.501 (df = 6376)

</td>

<td>

79.161 (df = 6302)

</td>

<td>

75.830 (df = 6147)

</td>

<td>

75.817 (df = 6146)

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="4" style="text-align:right">

@ p \< 0.10, @@ p \< 0.05, @@@ p \< 0.01

</td>

</tr>

</table>

``` r
# first graders by class
fir <- rep('Regular', nrow(dat))
fir[is.na(dat$sc1) & is.na(dat$ra1)] <- NA
fir[dat$sc1 == 1] <- 'Small'
fir[dat$ra1 == 1] <- 'Aide'

# second graders by class
sec <- rep('Regular', nrow(dat))
sec[is.na(dat$sc2) & is.na(dat$ra2)] <- NA
sec[dat$sc2 == 1] <- 'Small'
sec[dat$ra2 == 1] <- 'Aide'

# figure 11
table(fir, sec)
```

    ##          sec
    ## fir       Aide Regular Small
    ##   Aide    1560     115    40
    ##   Regular  202    1498   152
    ##   Small     24      23  1435

``` r
library(lfe)

# get the data
dat <- star_sw

# models
mod <- 'tscore1 ~ sc + ra' %>%
  list(
    str_c(., ' | schid1n'),
    str_c(., ' + white + boy + freelun1 | schid1n'),
    str_c(., ' + white + boy + freelun1 + totexp1 | schid1n')
    )

# regressions
reg1 <- map(mod, ~felm(as.formula(.), data = mutate(dat, sc = sc1, ra = ra1)))
regk <- map(mod, ~felm(as.formula(.), data = mutate(dat, sc = sck, ra = rak)))

# figure 12
labs <- c('OLS: actual class size', 'Reduced form: initial class size')
vars <- c(
  'Small class',
  'Regular/aide class',
  'White',
  'Boy',
  'Free lunch',
  'Teacher experience'
  )
stargazer(
  reg1, regk,
  column.labels = labs,
  column.separate = c(4, 4),
  covariate.labels = vars,
  dep.var.labels = 'First grade test score',
  dep.var.labels.include = T,
  notes = '@ p < 0.10, @@ p < 0.05, @@@ p < 0.01',
  notes.append = F,
  star.char = c("@", "@@", "@@@"),
  type = 'html'
  )
```

<table style="text-align:center">

<tr>

<td colspan="9" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="8">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="8" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="8">

First grade test score

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="4">

OLS: actual class size

</td>

<td colspan="4">

Reduced form: initial class size

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

<td>

(4)

</td>

<td>

(5)

</td>

<td>

(6)

</td>

<td>

(7)

</td>

<td>

(8)

</td>

</tr>

<tr>

<td colspan="9" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Small class

</td>

<td>

29.781<sup>@@@</sup>

</td>

<td>

29.009<sup>@@@</sup>

</td>

<td>

27.194<sup>@@@</sup>

</td>

<td>

26.716<sup>@@@</sup>

</td>

<td>

19.782<sup>@@@</sup>

</td>

<td>

20.350<sup>@@@</sup>

</td>

<td>

21.045<sup>@@@</sup>

</td>

<td>

20.900<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.807)

</td>

<td>

(2.491)

</td>

<td>

(2.412)

</td>

<td>

(2.427)

</td>

<td>

(3.505)

</td>

<td>

(3.091)

</td>

<td>

(3.000)

</td>

<td>

(3.003)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Regular/aide class

</td>

<td>

11.959<sup>@@@</sup>

</td>

<td>

7.240<sup>@@@</sup>

</td>

<td>

6.817<sup>@@@</sup>

</td>

<td>

6.292<sup>@@@</sup>

</td>

<td>

\-2.082

</td>

<td>

\-0.660

</td>

<td>

0.642

</td>

<td>

0.657

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.686)

</td>

<td>

(2.417)

</td>

<td>

(2.356)

</td>

<td>

(2.374)

</td>

<td>

(3.404)

</td>

<td>

(3.010)

</td>

<td>

(2.919)

</td>

<td>

(2.919)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

White

</td>

<td>

</td>

<td>

</td>

<td>

24.476<sup>@@@</sup>

</td>

<td>

24.426<sup>@@@</sup>

</td>

<td>

</td>

<td>

</td>

<td>

21.592<sup>@@@</sup>

</td>

<td>

21.455<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

(3.701)

</td>

<td>

(3.700)

</td>

<td>

</td>

<td>

</td>

<td>

(5.190)

</td>

<td>

(5.191)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Boy

</td>

<td>

</td>

<td>

</td>

<td>

\-11.016<sup>@@@</sup>

</td>

<td>

\-11.079<sup>@@@</sup>

</td>

<td>

</td>

<td>

</td>

<td>

\-12.599<sup>@@@</sup>

</td>

<td>

\-12.656<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

(1.933)

</td>

<td>

(1.933)

</td>

<td>

</td>

<td>

</td>

<td>

(2.396)

</td>

<td>

(2.396)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Free lunch

</td>

<td>

</td>

<td>

</td>

<td>

\-45.841<sup>@@@</sup>

</td>

<td>

\-45.834<sup>@@@</sup>

</td>

<td>

</td>

<td>

</td>

<td>

\-49.372<sup>@@@</sup>

</td>

<td>

\-49.357<sup>@@@</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

(2.320)

</td>

<td>

(2.320)

</td>

<td>

</td>

<td>

</td>

<td>

(2.916)

</td>

<td>

(2.916)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Teacher experience

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

0.223<sup>@</sup>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

0.188

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

(0.126)

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

(0.156)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

1,039.393<sup>@@@</sup>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

1,056.508<sup>@@@</sup>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.836)

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

(2.426)

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td colspan="9" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

6,379

</td>

<td>

6,379

</td>

<td>

6,227

</td>

<td>

6,227

</td>

<td>

4,298

</td>

<td>

4,298

</td>

<td>

4,213

</td>

<td>

4,213

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.017

</td>

<td>

0.257

</td>

<td>

0.319

</td>

<td>

0.319

</td>

<td>

0.011

</td>

<td>

0.268

</td>

<td>

0.327

</td>

<td>

0.327

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.017

</td>

<td>

0.248

</td>

<td>

0.310

</td>

<td>

0.310

</td>

<td>

0.010

</td>

<td>

0.255

</td>

<td>

0.314

</td>

<td>

0.314

</td>

</tr>

<tr>

<td style="text-align:left">

Residual Std. Error

</td>

<td>

90.501 (df = 6376)

</td>

<td>

79.161 (df = 6302)

</td>

<td>

75.830 (df = 6147)

</td>

<td>

75.817 (df = 6146)

</td>

<td>

92.579 (df = 4295)

</td>

<td>

80.328 (df = 4221)

</td>

<td>

77.031 (df = 4133)

</td>

<td>

77.027 (df = 4132)

</td>

</tr>

<tr>

<td colspan="9" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="8" style="text-align:right">

@ p \< 0.10, @@ p \< 0.05, @@@ p \< 0.01

</td>

</tr>

</table>
