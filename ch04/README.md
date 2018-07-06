-   [Gender disparities controlling for
    occupation](#gender-disparities-controlling-for-occupation)
-   [Qualitative change in sign](#qualitative-change-in-sign)
-   [Nonrandom sample selection](#nonrandom-sample-selection)

Gender disparities controlling for occupation
---------------------------------------------

    library(ggraph)
    library(tidygraph)

    # network
    net <- tribble(
      ~from, ~to,
      'A', 'o',
      'A', 'y', 
      'F', 'd',
      'F', 'o',
      'd', 'o',
      'o', 'y',
      )
    net %<>% as_tbl_graph(directed = T)
    net %<>% 
      activate(edges) %>%
      mutate(
        lty  = c('u', 'u', 'o', 'u', 'o', 'o')
        )

    # dag
    ggraph(net, layout = 'kk') +
      geom_edge_link(
        aes(linetype = lty),
        arrow = arrow(length = unit(2, "mm")),
        start_cap = circle(3, "mm"), end_cap = circle(3, "mm")
      ) + 
      geom_node_point() +
      geom_node_text(
        aes(label = name), hjust = -1, vjust = 2
      ) +
      theme_void() +
      theme(legend.position = 'none')

![](../fig/gender-1.png)

    # construct the data
    set.seed(8)
    dat <- tibble(
      female = as.numeric(runif(1E4, 0, 1) >= 0.5),
      ability = rnorm(1E4, 0, 1)
      )

    # outcomes
    #   discrim = all females experience discrimination
    #   occupat = f(ability, discrimination) (no discrim <=> M/F sort the same)
    #   wage = g(ability, discrimination, occupation)
    dat %<>% mutate(
      discrim = female,
      occupat = 1 + 2 * ability + 0 * female - 2 * discrim + rnorm(1E4, 0, 1),
      wage = 1 + 2 * ability - 1 * discrim + 1 * occupat + rnorm(1E4, 0, 1)
      )

    # regressions
    #   unconditional effect of discrimination: wage (direct) + occupat (indirect)
    #   adding occupation controls for collider!: female -> occupat <- ability
    #   correct specification
    reg <- list(
      lm(wage ~ discrim, data = dat),
      lm(wage ~ discrim + occupat, data = dat),
      lm(wage ~ discrim + occupat + ability, data = dat)
      )

    # tidy the data
    reg %<>% map_dfr(tidy)
    reg$model <- c(
      rep('Biased unconditional', 2),
      rep('Biased', 3),
      rep('Unbiased conditional', 4)
      )

    # table 8
    out <- gather(reg, statistic, value, -term, -model) %>%
      filter(statistic %in% c('estimate', 'std.error')) %>%
      mutate(
        value = format(value, digits = 2),
        value = str_trim(value),
        value = if_else(statistic == 'std.error', str_c('(', value, ')'), value)
      ) %>%
      spread(model, value, fill = '') %>%
      select(-statistic)

    out <- out[c(1:2, 5:6, 7:8, 3:4), c(1, 3, 2, 4)]
    out[, 1] <- c(
      '(Intercept)', '',
      'Female', '',
      'Occupation', '',
      'Ability', ''
    )
    names(out)[1] <- c('Covariates:')
    out %<>% add_row(
      `Covariates:` = 'N',
      `Biased unconditional` = '10,000',
      `Biased` = '10,000',
      `Unbiased conditional` = '10,000'
      )

    knitr::kable(
      out,
      align = 'c',
      row.names = F,
      format = 'html'
      ) %>%
      kableExtra::row_spec(9, bold = T) %>%
      kableExtra::kable_styling()

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:center;">
Covariates:
</th>
<th style="text-align:center;">
Biased unconditional
</th>
<th style="text-align:center;">
Biased
</th>
<th style="text-align:center;">
Unbiased conditional
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
(Intercept)
</td>
<td style="text-align:center;">
2.0366
</td>
<td style="text-align:center;">
0.2065
</td>
<td style="text-align:center;">
0.9880
</td>
</tr>
<tr>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
(0.0602)
</td>
<td style="text-align:center;">
(0.0199)
</td>
<td style="text-align:center;">
(0.0172)
</td>
</tr>
<tr>
<td style="text-align:center;">
Female
</td>
<td style="text-align:center;">
-2.9937
</td>
<td style="text-align:center;">
0.6035
</td>
<td style="text-align:center;">
-0.9756
</td>
</tr>
<tr>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
(0.0850)
</td>
<td style="text-align:center;">
(0.0292)
</td>
<td style="text-align:center;">
(0.0280)
</td>
</tr>
<tr>
<td style="text-align:center;">
Occupation
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
1.8018
</td>
<td style="text-align:center;">
1.0093
</td>
</tr>
<tr>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
(0.0060)
</td>
<td style="text-align:center;">
(0.0099)
</td>
</tr>
<tr>
<td style="text-align:center;">
Ability
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
1.9794
</td>
</tr>
<tr>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
(0.0221)
</td>
</tr>
<tr>
<td style="text-align:center;font-weight: bold;">
N
</td>
<td style="text-align:center;font-weight: bold;">
10,000
</td>
<td style="text-align:center;font-weight: bold;">
10,000
</td>
<td style="text-align:center;font-weight: bold;">
10,000
</td>
</tr>
</tbody>
</table>
Qualitative change in sign
--------------------------

    library(sandwich)
    library(lmtest)

    set.seed(541)

    # dag properties
    #   Z -> D -> Y
    #   D -> X <- Y

    # construct the data 
    dat <- tibble(
      z = rnorm(2500, 0, 1),
      k = rnorm(2500, 10, 4),
      d = 0
      )
    dat$d[dat$k >= 12] <- 1

    # add x and y
    dat %<>% mutate(
      y = 50 * d + 100 + rnorm(2500, 0, 1),
      x = 50 * d + y + rnorm(2500, 50, 1)
      )

    # regressions
    reg <- list(
      lm(y ~ d, data = dat),
      lm(y ~ x, data = dat),
      lm(y ~ d + x, data = dat)
      )

    # use stata style robust standard errors
    reg <- map(reg, ~coeftest(.x, vcov = vcovHC(.x, "HC1")))

    # tidy the data
    reg %<>% map_dfr(tidy)
    reg$model <- c(
      rep(1, 2),
      rep(2, 2),
      rep(3, 3)
      )

    # table 9
    out <- gather(reg, statistic, value, -term, -model) %>%
      filter(statistic %in% c('estimate', 'std.error')) %>%
      mutate(
        value = format(value, digits = 2),
        value = str_trim(value),
        value = if_else(statistic == 'std.error', str_c('(', value, ')'), value)
      ) %>%
      spread(model, value, fill = '') %>%
      select(-statistic)

    out$term <- c(
      '(Intercept)', '',
      'd', '',
      'x', ''
    )
    names(out)[1] <- c('Covariates:')
    out %<>% add_row(
      `Covariates:` = 'N',
      `1` = '2,500',
      `2` = '2,500',
      `3` = '2,500'
      )

    knitr::kable(
      out,
      align = 'c',
      row.names = F,
      format = 'html'
      ) %>%
      kableExtra::row_spec(7, bold = T) %>%
      kableExtra::kable_styling()

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:center;">
Covariates:
</th>
<th style="text-align:center;">
1
</th>
<th style="text-align:center;">
2
</th>
<th style="text-align:center;">
3
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
(Intercept)
</td>
<td style="text-align:center;">
99.9825
</td>
<td style="text-align:center;">
24.9846
</td>
<td style="text-align:center;">
26.1083
</td>
</tr>
<tr>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
(0.0241)
</td>
<td style="text-align:center;">
(0.0561)
</td>
<td style="text-align:center;">
(1.4680)
</td>
</tr>
<tr>
<td style="text-align:center;">
d
</td>
<td style="text-align:center;">
50.0134
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
0.7502
</td>
</tr>
<tr>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
(0.0423)
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
(0.9811)
</td>
</tr>
<tr>
<td style="text-align:center;">
x
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
0.5000
</td>
<td style="text-align:center;">
0.4925
</td>
</tr>
<tr>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
(0.0003)
</td>
<td style="text-align:center;">
(0.0098)
</td>
</tr>
<tr>
<td style="text-align:center;font-weight: bold;">
N
</td>
<td style="text-align:center;font-weight: bold;">
2,500
</td>
<td style="text-align:center;font-weight: bold;">
2,500
</td>
<td style="text-align:center;font-weight: bold;">
2,500
</td>
</tr>
</tbody>
</table>
<br>

Nonrandom sample selection
--------------------------

    set.seed(3444)

    # construct the data
    dat <- tibble(
      beauty = rnorm(2500, 0, 1),
      talent = rnorm(2500, 0, 1),
      score  = beauty + talent
      )

    # add collider variable: star
    c85 <- quantile(dat$score, probs = 0.85)
    dat %<>% mutate(
      star = score > c85
      )

    # figure 7
    ggplot(dat) +
      geom_point(aes(talent, beauty, shape = star)) +
      geom_abline(
        intercept = c85,
        slope = -1,
        col = 'red'
      ) +
      scale_shape_manual(name = 'Star?', values = c(1, 16)) +
      labs(x = 'Talent', y = 'Beauty', title = 'Aspiring actors and actresses') +
      ggthemes::theme_tufte()

![](../fig/nonrandom-1.png)
