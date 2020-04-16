# DatawRappr <img src="man/figures/logo.png" align="right" />

The goal of DatawRappr is to provide a wrapper for Datawrapper's API to connect data from R directly with Datawrapper's charts capabilities. It uses the new [API version 3.0](https://developer.datawrapper.de/v3.0/docs).

## Key features:

* Manages and automatically retrieves the API-key locally via `datawrapper_auth()`
* Creates, deletes or publishes charts on Datawrapper
* Sends Dataframes from R directly to Datawrapper - without having to copy them in - with `dw_data_to_chart()`

All functions (except `datawrapper_auth()`) are preceded by `dw_`:

* allows test calls to the API via `dw_test_key()`
* lists all created charts: `dw_list_charts()`.
* lists all folders: `dw_list_folders()`.
* creates a new Datawrapper chart via `dw_create_chart()`
* adds data from a R-dataframe to an existing Datawrapper chart via `dw_data_to_chart()`
* retrieves (`dw_retrieve_chart_metadata()`) or edits metadata, description and visualization of an existing chart via `dw_edit_chart()`
* publishes and republishes a Datawrapper chart via `dw_publish_chart()`
* deletes an Datawrapper chart via `dw_delete_chart()`
* exports a chart as png, pdf or svg (latter two only in paid accounts) with `dw_export_chart()`

## Installation

Right now this package is experimental and only available on Github:

```{r}
# install.packages("devtools") # if not already installed on your system
devtools::install_github("munichrocker/DatawRappr")
```

## Usage

Add the package to your environment by running: 

```{r}
library(DatawRappr)
```

### Setting up the API-key

To use the API you have to create an API key on Datawrapper.

Click on **Dashboard** - **Settings** and move down to the section that says **API Access Tokens**.

Click on **Create new personal access token**, enter a name and save the token:

![](man/figures/gif_api_key.gif)

Copy the API key in the clipboard and use

```{r}
datawrapper_auth(api_key = "12345678")
```

to save the key to our system. If a key already exists, you may add the argument `overwrite = TRUE` to `datawrapper_auth()`.

To make sure, your key is working as expected, you can run

```{r}
dw_test_key()
```

with no arguments. It will then use the saved key from the environment. If the key is correct, you will receive a response from the API with personal details about your account - a `dw_user`-object that has no further use than to check your key.

Note: If you want to see your currently saved API key, you may use the helper function `dw_get_api_key()`.

Congratulations, you're good to go!

## Under the hood

This package makes heavy use of the [`httr`](https://github.com/r-lib/httr)-package, which on itself is a wrapper of the [curl](https://cran.r-project.org/web/packages/curl/index.html)-package.

## Further Links

There is a [API-documentation](https://developer.datawrapper.de/reference) and a [Getting Started guide with examples](https://developer.datawrapper.de/docs/getting-started) from Datawrapper.

A full Package-documentation [can be found here](https://munichrocker.github.io/DatawRappr/).
