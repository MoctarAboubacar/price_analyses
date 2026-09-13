# Market Price Trend Analysis — Nepal

Time series analysis of retail market prices for staple commodities in Nepal, built for price monitoring across market and commodity combinations.

There are 96 market-by-commodity combinations here, each with its own seasonality and its own missing observations. The analytical problem is less any single model than handling that structure consistently: splitting the panel into individual series, testing each for stationarity, differencing or decomposing as required, and fitting comparable models so that trends can be read across markets rather than one at a time.

Seasonality matters more than level for monitoring purposes. A price rise that matches the usual lean season pattern carries different operational meaning from the same rise occurring off-cycle, so the decomposition separating trend, seasonal and remainder components is the part that feeds into decision-making.

Where a series is too sparse to support a model, that is reported rather than filled.

## Methods

Time series decomposition, stationarity testing (`urca`, `tseries`), ARIMA-family modelling and forecasting (`forecast`), cross-market trend comparison.

## Files

| File | Purpose |
|---|---|
| `nepal-price-analysis.R` | Series construction, stationarity testing, modelling across market-commodity combinations |
| `nepal-price-analysis.Rmd` | Write-up version with narrative and figures |

## Data

The price series are not redistributed here. To run the code, place the input in `data/`:

| File | Description |
|---|---|
| `Book1.csv` | Retail price observations by market, commodity and period |

Model outputs are written to `output/`.

## Reproducing

Paths resolve from the repository root through the `here` package.

```r
install.packages(c("tidyverse", "tidyr", "lubridate", "zoo", "forecast",
                   "tseries", "urca", "broom", "foreign", "ggthemes", "here"))

source("nepal-price-analysis.R")
```

## License

MIT — see [LICENSE](LICENSE).
