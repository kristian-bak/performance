library(magrittr)

data <- readxl::read_excel("D:/Aktier/Analyse/Perfomance/index.xlsx")

df_stock <- kb.yahoo::load_data(ticker = data$Ticker, from = "2023-01-01")

summarise_return <- function(data) {
  
  data %>% 
    dplyr::filter(!is.na(Adjusted)) %>% 
    dplyr::summarise(N = dplyr::n(),
                     Start = dplyr::first(Adjusted), 
                     End = dplyr::last(Adjusted), 
                     Return = 100 * (End - Start) / Start, 
                     SD = sd(ChangeSinceStart),
                     SharpRatio = Return / SD
                     ) %>% 
    dplyr::select(-Start, -End)
  
}

df_summary <- df_stock %>% 
  dplyr::group_by(Ticker) %>% 
  summarise_return() %>% 
  dplyr::left_join(data %>% dplyr::select(-Link, -Correlation), by = "Ticker") %>% 
  dplyr::arrange(dplyr::desc(Return))

summarise_return_by <- function(data, var) {
  
  data %>% 
    dplyr::filter(!is.na({{var}})) %>% 
    dplyr::group_by({{var}}) %>% 
    dplyr::summarise(N = mean(N),
                     Return = mean(Return), 
                     SD = mean(SD), 
                     SharpRatio = mean(SharpRatio)) %>% 
    dplyr::arrange(dplyr::desc(Return))
  
}

## Best region
df_region <- df_summary %>% 
  summarise_return_by(var = Region) %>% 
  dplyr::slice(1)

## Best sektor
df_sector <- df_summary %>% 
  summarise_return_by(var = Sektor) %>% 
  dplyr::slice(1)

## Growth har v?ret bedst
df_type <- df_summary %>% 
  summarise_return_by(var = GrowthVsValue) %>% 
  dplyr::slice(1)

## Small har v?ret bedst
df_size <- df_summary %>% 
  summarise_return_by(var = LargeVsSmall) %>% 
  dplyr::slice(1)

## Mangler flere aktiv klasser
df_class <- df_summary %>% 
  summarise_return_by(var = AktivKlasse) %>% 
  dplyr::slice(1)

## Bedste investering
df_best <- df_summary %>% 
  dplyr::slice(1)

## D?rligste investering
df_worst <- df_summary %>% 
  dplyr::slice(dplyr::n())

## Mest risikable aktiv
df_risky <- df_summary %>% 
  dplyr::filter(SD == max(SD))

## Mindst risikable aktiv
df_safe <- df_summary %>% 
  dplyr::filter(SD == min(SD))

## Bedste risikojusterede afkast
df_best_sharp <- df_summary %>% 
  dplyr::filter(SharpRatio == max(SharpRatio))

## D?rligste risikojusterede afkast
df_worst_sharp <- df_summary %>% 
  dplyr::filter(SharpRatio == min(SharpRatio))


## Risiko sammenligning

## Minimum: gloale aktier min risk.

df_summary %>% 
  dplyr::filter(Ticker == "SPYI.DE") ## Globale aktier

df_summary %>% 
  dplyr::filter(Ticker == "^NDX") ## Nasdaq

## Maks: Bitcoin

df_summary %>% 
  dplyr::arrange(SD) %>% 
  dplyr::select(Investering, SD) %>% 
  as.data.frame()

## Pick some and make timeline look alike table

## Rentef?lsomhed: lille/stor, positiv/negativ

replace_na_with_lag <- function(x) {
  
  dplyr::tibble(
    x, 
    x_lag = dplyr::lag(x)
  ) %>% 
    dplyr::mutate(y = dplyr::if_else(is.na(x), x_lag, x)) %>% 
    dplyr::pull(y)
  
}

df_stock %>% 
  dplyr::filter(Ticker == "^OMX") %>% 
  dplyr::pull(Adjusted)

df_wide <- df_stock %>% 
  dplyr::select(Ticker, Date, Adjusted) %>% 
  dplyr::group_by(Ticker) %>% 
  tidyr::pivot_wider(id_cols = Date, names_from = Ticker, values_from = Adjusted) %>% 
  dplyr::mutate(dplyr::across(dplyr::everything(), replace_na_with_lag))

df_cor <- df_wide %>% 
  na.omit() %>% 
  dplyr::select(-Date) %>% 
  cor()

tbl_cor <- dplyr::tibble(
  col = rep(df_summary$Ticker, each = nrow(df_summary)), ## repeates
  row = rep(df_summary$Ticker, nrow(df_summary)), ## alternates
  cor = df_cor %>% 
  as.vector()
)

tbl_cor %>% 
  dplyr::filter(cor == min(cor))

tbl_cor %>% 
  dplyr::filter(cor != 1) %>% 
  dplyr::filter(cor == max(cor))
