#' EIA Short Term Energy Outlook
#' @description Extract data and either plots or renders dataframe.
#' @param key Your private EIA API token. `character`
#' @param from Date as character "2020-07-01". Default to all dates available. `character`
#' @param market "globalOil" only currently implemented. `character`
#' @param fig.title Defaults to "EIA STEO Global Liquids SD Balance". `character`
#' @param fig.units Defaults to "million barrels per day" `character`
#' @param legend.pos Defaults to list(x = 0.4, y = 0.53)  `list`
#' @param output "chart" for plotly object or "data" for dataframe.
#' @returns A plotly chart `htmlwidget` or a `tibble`.
#' @export chart_eia_steo
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' chart_eia_steo(key = EIAkey, market = "globalOil")
#' }
#'
chart_eia_steo <- function(market = "globalOil",
                           key = "your EIA.gov API key",
                           from = "2018-07-01",
                           fig.title = "EIA STEO Global Liquids SD Balance",
                           fig.units = "million barrels per day",
                           legend.pos = list(x = 0.4, y = 0.53),
                           output = "chart") {
  if (market == "globalOil") {
    eia_df <- tibble::tribble(
      ~ticker, ~name,
      "STEO.PAPR_NONOPEC.M", "SupplyNOPEC",
      "STEO.PAPR_OPEC.M", "SupplyOPEC",
      "STEO.PATC_WORLD.M", "Demand",
      "STEO.T3_STCHANGE_WORLD.M", "Inv_Change"
    )
    eia_df <- RTL::eia2tidy_all(tickers = eia_df, key = key, long = FALSE) %>%
      dplyr::transmute(date,
        Supply = SupplyNOPEC + SupplyOPEC, Demand,
        Inv_Change = Inv_Change * -1
      ) %>%
      stats::na.omit()

    if (!is.null(from)) {
      eia_df <- eia_df %>% dplyr::filter(date >= from)
    }

    if (output == "data") {
      return(eia_df)
    } else {
      out <- eia_df %>%
        tidyr::pivot_longer(-date, names_to = "series", values_to = "value") %>%
        dplyr::mutate(group = dplyr::case_when(series == "Inv_Change" ~ 2, TRUE ~ 1))
      # split(.$group) %>%
      # lapply(function(d) plotly::plot_ly(d, x = ~date, y = ~value,
      #                                    color = ~series, colors = c("red","black","blue"),
      #                                    type = c("scatter"), mode = "lines")) %>%
      # plotly::subplot(nrows = NROW(.), shareX = TRUE) %>%
      # plotly::layout(title = list(text = fig.title, x = 0),
      #                xaxis = list(title = " "),
      #                yaxis = list(title = fig.units ),
      #                legend = legend.pos)

      p1 <- eia_df %>%
        plotly::plot_ly(x = ~date, y = ~Supply, name = "Supply", type = c("scatter"), mode = "lines") %>%
        plotly::add_lines(x = ~date, y = ~Demand, name = "Demand") %>%
        plotly::layout(
          title = list(text = fig.title, x = 0),
          xaxis = list(title = " "),
          yaxis = list(title = fig.units),
          legend = legend.pos,
          shapes = list(
            list(
              type = "rect", fillcolor = "blue", line = list(color = "blue"), opacity = 0.15,
              x0 = lubridate::rollback(Sys.Date(), roll_to_first = TRUE), x1 = max(eia_df$date), xref = "x",
              y0 = floor(min(eia_df$Demand)), y1 = ceiling(max(eia_df$Supply)), yref = "y"
            )
          )
        )


      p2 <- eia_df %>%
        plotly::plot_ly(x = ~date, y = ~Inv_Change, name = "Inv_Change", type = c("scatter"), mode = "lines", fill = "tozeroy") %>%
        plotly::layout(
          title = list(text = fig.title, x = 0),
          xaxis = list(title = " "),
          yaxis = list(title = fig.units),
          legend = legend.pos,
          shapes = list(
            list(
              type = "rect", fillcolor = "blue", line = list(color = "blue"), opacity = 0.15,
              x0 = lubridate::rollback(Sys.Date(), roll_to_first = TRUE), x1 = max(eia_df$date), xref = "x",
              y0 = floor(min(eia_df$Inv_Change)), y1 = ceiling(max(eia_df$Inv_Change)), yref = "y"
            )
          )
        )
      out <- plotly::subplot(p1, p2, nrows = 2, shareX = TRUE)

      return(out)
    }
  }
}


# chart_eia_steo(market = "globalOil",
#                            key = EIAkey,
#                            from = "2000-07-01",
#                            fig.title = "EIA STEO Global Liquids SD Balance",
#                            fig.units = "million barrels per day",
#                            legend.pos = list(x = 0.4, y = 0.53),
#                            output = "chart")
