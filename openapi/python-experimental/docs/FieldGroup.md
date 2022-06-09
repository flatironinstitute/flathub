# FieldGroup

A single field within a catalog, or a hiearchical group of fields

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**stats** | [**FieldStats**](FieldStats.md) |  | [optional] 
**attachment** | **bool** | this is a meta field for a downloadable attachment (type boolean, indicating presence) | [optional] 
**key** | **str** | local name of field within this group | 
**name** | **str** | global unique (\&quot;variable\&quot;) name of field within the catalog | 
**store** | **bool** | true if this field is stored but not indexed, so not permitted for filtering or aggregations | [optional] 
**dict** | **str** | unique key index to global field dictionary (for compare) | [optional] 
**wildcard** | **bool** | allow wildcard prefix searching on keyword field (\&quot;xy*\&quot;) | [optional] 
**units** | **str** | display units | [optional] 
**title** | **str** | display name of the field within the group | 
**type** | [**Type**](Type.md) |  | 
**enum** | **[str]** | if present, display values as these keywords instead (integral or boolean: enum[&lt;int&gt;value]) | [optional] 
**disp** | **bool** | include field in data display by default | [optional] 
**sub** | **[FieldGroup]** | if this is present, this is a pseudo grouping field which does not exist itself, but its properties apply to its children | [optional] 
**reversed** | **bool** | display axes and ranges in reverse (high-low) | [optional] 
**base** | **str** | base storage type (floating, integral, boolean, string, void) | 
**terms** | **bool** | display dynamically as a dropdown of values | [optional] 
**scale** | **int, float** | scale factor to dict-comparable units, display  value*scale (for compare) | [optional] 
**required** | **bool** | true &#x3D; required filter; false &#x3D; top-level (default) optional filter; missing &#x3D; normal | [optional] 
**descr** | **str** | description of field within the group | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)

