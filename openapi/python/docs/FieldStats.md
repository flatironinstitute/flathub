# FieldStats

stats for the field named by the property, depending on its type

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**min** | **float, none_type** | minimum value | [optional] 
**max** | **float, none_type** | maximum value | [optional] 
**avg** | **float, none_type** | mean value | [optional] 
**count** | **int** | number of rows with values for this field | [optional] 
**others** | **int** | number of rows with values not included in the top terms | [optional] 
**terms** | [**[TopTerm]**](TopTerm.md) | top terms in descending order of count | [optional] 
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


