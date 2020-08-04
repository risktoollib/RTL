#' \code{chart_eia_steo}
#' @description Supply Demand Balance from EIA Short Term Energy Outlook.
#' @param key Your private EIA API token.
#' @param from Date as character "2020-07-01". Default to all dates available.
#' @param market "globalOil" only currently implemented.
#' @param fig.title Defaults to "EIA STEO Global Liquids SD Balance".
#' @param fig.units Defaults to "million barrels per day"
#' @param legend.pos Defaults to list(x = 0.4, y = 0.53)
#' @param output "chart" for plotly object or "data" for dataframe.
#' @return A plotly object or a dataframe
#' @export chart_eia_steo
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' chart_eia_steo(key = EIAkey, market = "globalOil")
#'  }

chart_eia_steo <- function(market = "globalOil",
                           key = "your EIA.gov API key",
                           from = "2018-07-01",
                           fig.title = "EIA STEO Global Liquids SD Balance",
                           fig.units = "million barrels per day",
                           legend.pos = list(x = 0.4, y = 0.53),
                           output = "chart") {

  if (market == "globalOil") {
    eia_df <- tibble::tribble(~ticker, ~name,
                              "STEO.PAPR_NONOPEC.M", "SupplyNOPEC",
                              "STEO.PAPR_OPEC.M", "SupplyOPEC",
                              "STEO.PATC_WORLD.M", "Demand",
                              "STEO.T3_STCHANGE_WORLD.M", "Inv_Change") %>%
      dplyr::mutate(key = key) %>%
      dplyr::mutate(df = purrr::pmap(list(ticker,key,name),.f=RTL::eia2tidy)) %>%
      dplyr::select(df) %>% tidyr::unnest(df) %>%
      tidyr::pivot_wider(id_cols = date, names_from = series, values_from = value) %>%
      dplyr::transmute(date, Supply = SupplyNOPEC + SupplyOPEC, Demand,
                       Inv_Change = Inv_Change * -1) %>%
      stats::na.omit()

    if (!is.null(from)) {eia_df <- eia_df %>% dplyr::filter(date >= from)}

    if (output == "data") {return(eia_df)} else {
      out <- eia_df %>%
        tidyr::pivot_longer(-date,names_to = "series",values_to = "value") %>%
        dplyr::mutate(group = dplyr::case_when(series == "Inv_Change" ~ 2,TRUE ~ 1)) %>%
        split(.$group) %>%
        lapply(function(d) plotly::plot_ly(d, x = ~date, y = ~value,
                                           color = ~series, colors = c("red","black","blue"),
                                           type = c("scatter"), mode = "lines")) %>%
        plotly::subplot(nrows = NROW(.), shareX = TRUE) %>%
        plotly::layout(title = list(text = fig.title, x = 0),
                       xaxis = list(title = " "),
                       yaxis = list(title = fig.units ),
                       legend = legend.pos)
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

