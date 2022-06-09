# HistogramRequest


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**fields** | [**HistogramList**](HistogramList.md) |  | 
**seed** | **int** | seed for random sample selection | [optional]  if omitted the server will use the default value of 0
**sample** | **float** | randomly select a fractional sample | [optional]  if omitted the server will use the default value of 1
**quartiles** | **str** | field name in selected catalog | [optional] 
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


