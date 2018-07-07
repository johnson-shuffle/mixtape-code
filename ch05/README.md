-   [Yule (1899)](#yule-1899)
-   [Simple difference in means
    decomposition](#simple-difference-in-means-decomposition)
-   [STAR Experiment](#star-experiment)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Yule (1899)
-----------

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

    # adjust if change in paupers == Inf
    dat %<>% mutate(
      dpaup = ifelse(dpaup == Inf, NA, dpaup)
      )

    # model of poverty and public assistance
    reg <- lm(dpaup ~ dout + dold + dpop, data = filter(dat, Type > 1))

    # table 10
    stargazer(reg)

% Table created by stargazer v.5.2 by Marek Hlavac, Harvard University.
E-mail: hlavac at fas.harvard.edu % Date and time: Fri, Jul 06, 2018 -
23:48:02
<br>

Simple difference in means decomposition
----------------------------------------

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

    ## Skim summary statistics
    ## 
    ## Variable type: numeric 
    ##  variable missing complete     n mean   sd   p0  p25 p50 p75 p100     hist
    ##       sdo       0    10000 10000  0.6 1.11 -1.8 -0.2 0.6 1.4    3 ▂▅▅▇▆▅▃▂

STAR Experiment
---------------

    library(lfe)

    # get the data
    dat <- star_sw

    # models
    mod <- 'tscorek ~ sck + rak' %>%
      list(
        .,
        str_c(., ' | schidkn'),
        str_c(., ' + white + boy + freelunk | schidkn'),
        str_c(., ' + freelunk + totexpk | schidkn')
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
      dep.var.label = 'Test score',
      dep.var.label.include = T,
      notes = '@ p < 0.10, @@ p < 0.05, @@@ p < 0.01',
      notes.append = F,
      star.char = c("@", "@@", "@@@"),
      type = 'html'
      )

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
.
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
16.014<sup>@@@</sup>
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
(2.108)
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
1.738
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
(2.036)
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
-12.161<sup>@@@</sup>
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
(1.664)
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
<td style="text-align:left">
Free lunch
</td>
<td>
</td>
<td>
</td>
<td>
-35.022<sup>@@@</sup>
</td>
<td>
-37.279<sup>@@@</sup>
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
(2.000)
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
0.680<sup>@@@</sup>
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
(0.163)
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
5,749
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
0.278
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
0.268
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
63.206 (df = 5666)
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
@ p &lt; 0.10, @@ p &lt; 0.05, @@@ p &lt; 0.01
</td>
</tr>
</table>
<table style="text-align:center">
<tr>
<td colspan="1" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td>
Test score
</td>
</tr>
<tr>
<td colspan="1" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
@ p &lt; 0.10, @@ p &lt; 0.05, @@@ p &lt; 0.01
</td>
</tr>
</table>
<table style="text-align:center">
<tr>
<td colspan="1" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td>
TRUE
</td>
</tr>
<tr>
<td colspan="1" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
@ p &lt; 0.10, @@ p &lt; 0.05, @@@ p &lt; 0.01
</td>
</tr>
</table>
<br>

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

    ##          sec
    ## fir       Aide Regular Small
    ##   Aide    1560     115    40
    ##   Regular  202    1498   152
    ##   Small     24      23  1435
