-   [Yule (1899)](#yule-1899)
-   [Simple difference in means
    decomposition](#simple-difference-in-means-decomposition)

Yule (1899)
-----------

    # reshape yule to long
    pat <- '(^[A-z]+[65]*)(\\d{2,4})(_\\d|O|I)*'
    dat <- gather(yule, variable, value, -1:-7)
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
    reg

    ## 
    ## Call:
    ## lm(formula = dpaup ~ dout + dold + dpop, data = filter(dat, Type > 
    ##     1))
    ## 
    ## Coefficients:
    ## (Intercept)         dout         dold         dpop  
    ##    81.39195      0.31246     -0.03133     -0.22418

Simple difference in means decomposition
----------------------------------------
