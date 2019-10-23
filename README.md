# DatawRappr

R-Library for the Datawrapper-API

Doc: https://developer.datawrapper.de/docs/getting-started

Funktionen:
* datawrapper_auth(API-key): registers API-key to R SysEnv, checks if key already exists, if yes, asks if it should be deleted
* check_key(API-Key): GET-Call to https://api.datawrapper.de/account, returns account information if right
* create_chart(title, subtitle, source_name, source_url, chart_type) creates chart, returns chart_id
* edit_chart(chart_id): allows modification of mthe cahrts metadata, also things like transpose, horizontal-header, allow list() under visualize, describe, annotate, publish
* df_to_datawrapper(df, chart_id, api_key = API-Key): uploads dataframe as csv to datawrapper-api, asks if new chart should be created (returns chart_id then), if not it needs a chart_id as argument, overwrites existing data // arguments: horizontal-header: true oder false (erste Zeile als Header) **<- signature function**
* publish_chart(chart_id): publishes chart (checks every n seconds agains the GET-API for success), returns URL and embed-code