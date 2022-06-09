# Bucket

histogram bucket, representing a box in the dimension space specified by the requested fields

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**key** | [**[FieldValue]**](FieldValue.md) | the minimum (left) point of this bucket, such than the bucket includes the range [key,key+size) (or [key,key*size) for log scale) | 
**count** | **int** | the number of rows with values that fall within this bucket | 
**quartiles** | [**[FieldValue]**](FieldValue.md) | if quartiles of a field were requested, includes the values of that field corresponding to the [0,25,50,75,100] percentiles ([min, first quartile, median, third quartile, max]) for rows within this bucket | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


