% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dw_edit_chart.R
\name{dw_edit_chart}
\alias{dw_edit_chart}
\title{Edits a existing Datawrapper chart}
\usage{
dw_edit_chart(
  chart_id,
  api_key = "environment",
  title = NULL,
  intro = NULL,
  annotate = NULL,
  byline = NULL,
  type = NULL,
  source_name = NULL,
  source_url = NULL,
  folderId = NULL,
  axes = list(),
  data = list(),
  visualize = list(),
  describe = list(),
  publish = list(),
  ...
)
}
\arguments{
\item{chart_id}{Required. A Datawrapper-chart-id as character string, usually a five character combination of digits and letters, e.g. "aBcDe". Or a \strong{dw_chart}-object.}

\item{api_key}{Required. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.}

\item{title}{Optional. Adds a title to the plot.}

\item{intro}{Optional. Adds an intro below the title.}

\item{annotate}{Optional. Adds a annotation below the plot.}

\item{byline}{Optional. Adds the name of the chart's creator.}

\item{type}{Optional. Changes the type of the chart. See \href{https://developer.datawrapper.de/docs/chart-types}{the documentation} for the different types or the Chart Type section below.}

\item{source_name}{Optional. Adds a source name to the plot.}

\item{source_url}{Optional. Adds a URL to the source name (displayed only, if source name specified). Include http(s):// before URL.}

\item{folderId}{Optional. Moves the chart to the specified folder (by folder-id, which can be found using \code{\link{dw_list_folders}}).}

\item{axes}{Optional. A list which specifies the axes.}

\item{data}{Optional. A list. Add separate arguments for the data. See \href{https://developer.datawrapper.de/docs/chart-properties}{the documentation} for details.}

\item{visualize}{Optional. A list. Add separate arguments for the visualization. See \href{https://developer.datawrapper.de/docs/chart-properties}{the documentation} for details.}

\item{describe}{Optional. A list. Add separate arguments for the description. See \href{https://developer.datawrapper.de/docs/chart-properties}{the documentation} for details.}

\item{publish}{Optional. A list. Add separate arguments for publication. See \href{https://developer.datawrapper.de/docs/chart-properties}{the documentation} for details.}

\item{...}{Optional. Will be added as a list to the top-level of the call-body. Use with caution, as it may overwrite some values defined earlier.}
}
\value{
A terminal message: "Chart xyz succesfully updated." - or an error message.
}
\description{
\lifecycle{maturing}
Modifies an existing Datawrapper chart.
}
\note{
This function builds a body for a API-call to the Datawrapper-API.

Check their \href{https://developer.datawrapper.de/docs}{reference guide} or \href{https://developer.datawrapper.de/reference}{API-documentation}.
}
\section{Chart Types}{

\itemize{
\item \code{d3-bars} : Bar Chart
\item \code{d3-bars-split} : Split Bars
\item \code{d3-bars-stacked} : Stacked Bars
\item \code{d3-bars-bullet} : Bullet Bars
\item \code{d3-dot-plot} : Dot Plot
\item \code{d3-range-plot} : Range Plot
\item \code{d3-arrow-plot} : Arrow Plot
\item \code{column-chart} : Column Chart
\item \code{grouped-column-chart} : Grouped Column Chart
\item \code{stacked-column-chart} : Stacked Column Chart
\item \code{d3-area} : Area Chart
\item \code{d3-lines} : Line Chart
\item \code{d3-pies} : Pie Chart
\item \code{d3-donuts} : Donut Chart
\item \code{d3-multiple-pies} : Multiple Pies
\item \code{d3-multiple-donuts} : Multiple Donuts
\item \code{d3-scatter-plot} : Scatter Plot
\item \code{election-donut-chart} : Election Donut
\item \code{tables} : Table
\item \code{d3-maps-choropleth} : Choropleth Map
\item \code{d3-maps-symbols} : Symbol Map
\item \code{locator-map} : Locator Map
}
}

\section{Chart Properties}{


Datawrapper defines the properties of a chart in the following structure.

Note: Not all possibilities are displayed, as different chart-types offer different design features.

You can find more on this structure in the \href{https://developer.datawrapper.de/docs/chart-properties}{API-Guide}.

\if{html}{\out{<div class="sourceCode">}}\preformatted{ \{
"id": "6Ba3L",
"type": "d3-scatter-plot",
"title": "A grat chart",
...

"metadata": \{

  "data": \{
    "transpose": false,
    ...
  \},

  "publish": \{
    "embed-codes": \{
      "embed-method-iframe": "<iframe title=\\"\\" aria-label=\\"Scatter Plot\\" src=\\"//datawrapper.dwcdn.net/6Ba3L/1/\\" scrolling=\\"no\\" frameborder=\\"0\\" style=\\"border: none;\\" width=\\"600\\" height=\\"294\\"></iframe>",
      ...
    \},
    "embed-width": 600,
    "chart-height": 33.859375,
    ...
  \}
\},

"annotate": \{
  "notes": "Updates from 2018 are excluded."
\},

"describe": \{
  "intro": "",
  "byline": "",
  ...
\},

"visualize": \{
  "size": "fixed",
  "rules": false,
  "shape": "fixed",
  ...
\},

"json_error": null
\},

"language": "en-US",
"externalData": null,
...
\},
"url": "/v3/charts/6Ba3L"
\}
}\if{html}{\out{</div>}}
}

\examples{

\dontrun{
dw_edit_chart("aBcDE") # uses the preset key in the .Renviron-file, no changes
}

\dontrun{
dw_edit_chart(chart_id = "a1B2Cd", api_key = "1234ABCD") # uses the specified key, no changes
}

\dontrun{
dw_edit_chart(chart_id = "a1B2Cd", title = "I'm a title",
intro = "Data showing daily results")
# changes title and intro
}

\dontrun{
dw_edit_chart(chart_id = "a1B2Cd", title = "I'm a title",
data = list("transpose" = "true"))
# transpose data
}

}
\author{
Benedict Witzenberger
}
