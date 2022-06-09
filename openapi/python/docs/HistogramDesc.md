# HistogramDesc

parameters for a single-field histogram (when used as a query parameter, may be specified as \"FIELD:[log]SIZE\"); it's recommended to include fully-bounded range filters for any histogram fields

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**field** | **str** | field name in selected catalog | 
**log** | **bool** | whether to calculate the histogram using log-spaced buckets (rather than linear spacing) | [optional]  if omitted the server will use the default value of False
**size** | **int** | number of buckets to include in the histogram | [optional]  if omitted the server will use the default value of 16
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


