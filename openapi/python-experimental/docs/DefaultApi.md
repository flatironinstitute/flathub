# openapi_client.DefaultApi

All URIs are relative to *https://flathub.flatironinstitute.org/api*

Method | HTTP request | Description
------------- | ------------- | -------------
[**attachment**](DefaultApi.md#attachment) | **GET** /{catalog}/attachment/{field}/{id} | Download a row attachment
[**attachments**](DefaultApi.md#attachments) | **GET** /{catalog}/attachments/{format} | Download attachments in bulk from matching rows for multiple fields
[**attachments1**](DefaultApi.md#attachments1) | **GET** /{catalog}/attachments/{format}/{field} | Download attachments in bulk from matching rows for single field
[**attachments1_post**](DefaultApi.md#attachments1_post) | **POST** /{catalog}/attachments/{format}/{field} | Download attachments in bulk from matching rows for single field
[**attachments_post**](DefaultApi.md#attachments_post) | **POST** /{catalog}/attachments/{format} | Download attachments in bulk from matching rows for multiple fields
[**catalog**](DefaultApi.md#catalog) | **GET** /{catalog} | Get full metadata about a specific catalog
[**count**](DefaultApi.md#count) | **GET** /{catalog}/count | Get count of matching rows (given some filters)
[**count_post**](DefaultApi.md#count_post) | **POST** /{catalog}/count | Get count of matching rows (given some filters)
[**data**](DefaultApi.md#data) | **GET** /{catalog}/data | Get a sample of raw data rows
[**data_post**](DefaultApi.md#data_post) | **POST** /{catalog}/data | Get a sample of raw data rows
[**download**](DefaultApi.md#download) | **GET** /{catalog}/data/{format} | Download raw data in bulk
[**download_post**](DefaultApi.md#download_post) | **POST** /{catalog}/data/{format} | Download raw data in bulk
[**histogram**](DefaultApi.md#histogram) | **GET** /{catalog}/histogram | Get a histogram of data across one or more fields
[**histogram_post**](DefaultApi.md#histogram_post) | **POST** /{catalog}/histogram | Get a histogram of data across one or more fields
[**schema_csv**](DefaultApi.md#schema_csv) | **GET** /{catalog}/schema.csv | Get a CSV representation of the catalog schema (no data)
[**schema_sql**](DefaultApi.md#schema_sql) | **GET** /{catalog}/schema.sql | Get a SQL representation of the catalog schema (no data)
[**stats**](DefaultApi.md#stats) | **GET** /{catalog}/stats | Get statistics about fields (given some filters)
[**stats_post**](DefaultApi.md#stats_post) | **POST** /{catalog}/stats | Get statistics about fields (given some filters)
[**top**](DefaultApi.md#top) | **GET** / | Get the list of available dataset catalogs

# **attachment**
> bool, date, datetime, dict, float, int, list, str, none_type attachment(catalogfieldid)

Download a row attachment

### Example

```python
import openapi_client
from openapi_client.api import default_api
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
        'field': "field_example",
        'id': "id_example",
    }
    try:
        # Download a row attachment
        api_response = api_instance.attachment(
            path_params=path_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->attachment: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('application/octet-stream', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 
field | FieldSchema | | 
id | IdSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

#### FieldSchema

field name in selected catalog

Type | Description | Notes
------------- | ------------- | -------------
**str** | field name in selected catalog | 

#### IdSchema

field value

Type | Description | Notes
------------- | ------------- | -------------
**str** | field value | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | attachment result

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationOctetStream, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationOctetStream

attachment file (type depends on field)

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **attachments**
> bool, date, datetime, dict, float, int, list, str, none_type attachments(catalogformat)

Download attachments in bulk from matching rows for multiple fields

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.filters import Filters
from openapi_client.model.field_list import FieldList
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
        'format': "sh",
    }
    query_params = {
    }
    try:
        # Download attachments in bulk from matching rows for multiple fields
        api_response = api_instance.attachments(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->attachments: %s\n" % e)

    # example passing only optional values
    path_params = {
        'catalog': "catalog_example",
        'format': "sh",
    }
    query_params = {
        'filters': Filters(
        seed=0,
        sample=1,
    ),
        'fields': FieldList([
        "fields_example"
    ]),
    }
    try:
        # Download attachments in bulk from matching rows for multiple fields
        api_response = api_instance.attachments(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->attachments: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('text/x-shellscript', 'text/uri-list', 'application/zip', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
filters | FiltersSchema | | optional
fields | FieldsSchema | | optional


#### FiltersSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**Filters**](Filters.md) |  | 


#### FieldsSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**FieldList**](FieldList.md) |  | 


### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 
format | FormatSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

#### FormatSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  |  must be one of ["sh", "uris", "zip", ]

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | file containing all matching attachments in the selected format

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyTextXShellscript, SchemaFor200ResponseBodyTextUriList, SchemaFor200ResponseBodyApplicationZip, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyTextXShellscript

A text shell script that downloads all the selected attachments with curl -JO, one per line

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyTextUriList

A text file listing all the URLs of the selected attachments, one per line, with a one line #comment header

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyApplicationZip

A ZIP file containing all the selected attachments

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **attachments1**
> bool, date, datetime, dict, float, int, list, str, none_type attachments1(catalogformatfield)

Download attachments in bulk from matching rows for single field

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.filters import Filters
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
        'format': "sh",
        'field': "field_example",
    }
    query_params = {
    }
    try:
        # Download attachments in bulk from matching rows for single field
        api_response = api_instance.attachments1(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->attachments1: %s\n" % e)

    # example passing only optional values
    path_params = {
        'catalog': "catalog_example",
        'format': "sh",
        'field': "field_example",
    }
    query_params = {
        'filters': Filters(
        seed=0,
        sample=1,
    ),
    }
    try:
        # Download attachments in bulk from matching rows for single field
        api_response = api_instance.attachments1(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->attachments1: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('text/x-shellscript', 'text/uri-list', 'application/zip', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
filters | FiltersSchema | | optional


#### FiltersSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**Filters**](Filters.md) |  | 


### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 
format | FormatSchema | | 
field | FieldSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

#### FormatSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  |  must be one of ["sh", "uris", "zip", ]

#### FieldSchema

field name in selected catalog

Type | Description | Notes
------------- | ------------- | -------------
**str** | field name in selected catalog | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | file containing all matching attachments in the selected format

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyTextXShellscript, SchemaFor200ResponseBodyTextUriList, SchemaFor200ResponseBodyApplicationZip, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyTextXShellscript

A text shell script that downloads all the selected attachments with curl -JO, one per line

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyTextUriList

A text file listing all the URLs of the selected attachments, one per line, with a one line #comment header

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyApplicationZip

A ZIP file containing all the selected attachments

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **attachments1_post**
> bool, date, datetime, dict, float, int, list, str, none_type attachments1_post(catalogformatfield)

Download attachments in bulk from matching rows for single field

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.filters import Filters
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
        'format': "sh",
        'field': "field_example",
    }
    try:
        # Download attachments in bulk from matching rows for single field
        api_response = api_instance.attachments1_post(
            path_params=path_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->attachments1_post: %s\n" % e)

    # example passing only optional values
    path_params = {
        'catalog': "catalog_example",
        'format': "sh",
        'field': "field_example",
    }
    body = None
    try:
        # Download attachments in bulk from matching rows for single field
        api_response = api_instance.attachments1_post(
            path_params=path_params,
            body=body,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->attachments1_post: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
path_params | RequestPathParams | |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('text/x-shellscript', 'text/uri-list', 'application/zip', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 
format | FormatSchema | | 
field | FieldSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

#### FormatSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  |  must be one of ["sh", "uris", "zip", ]

#### FieldSchema

field name in selected catalog

Type | Description | Notes
------------- | ------------- | -------------
**str** | field name in selected catalog | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | file containing all matching attachments in the selected format

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyTextXShellscript, SchemaFor200ResponseBodyTextUriList, SchemaFor200ResponseBodyApplicationZip, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyTextXShellscript

A text shell script that downloads all the selected attachments with curl -JO, one per line

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyTextUriList

A text file listing all the URLs of the selected attachments, one per line, with a one line #comment header

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyApplicationZip

A ZIP file containing all the selected attachments

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **attachments_post**
> bool, date, datetime, dict, float, int, list, str, none_type attachments_post(catalogformat)

Download attachments in bulk from matching rows for multiple fields

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.filters import Filters
from openapi_client.model.field_list import FieldList
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
        'format': "sh",
    }
    try:
        # Download attachments in bulk from matching rows for multiple fields
        api_response = api_instance.attachments_post(
            path_params=path_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->attachments_post: %s\n" % e)

    # example passing only optional values
    path_params = {
        'catalog': "catalog_example",
        'format': "sh",
    }
    body = None
    try:
        # Download attachments in bulk from matching rows for multiple fields
        api_response = api_instance.attachments_post(
            path_params=path_params,
            body=body,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->attachments_post: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
path_params | RequestPathParams | |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('text/x-shellscript', 'text/uri-list', 'application/zip', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 
format | FormatSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

#### FormatSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  |  must be one of ["sh", "uris", "zip", ]

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | file containing all matching attachments in the selected format

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyTextXShellscript, SchemaFor200ResponseBodyTextUriList, SchemaFor200ResponseBodyApplicationZip, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyTextXShellscript

A text shell script that downloads all the selected attachments with curl -JO, one per line

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyTextUriList

A text file listing all the URLs of the selected attachments, one per line, with a one line #comment header

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyApplicationZip

A ZIP file containing all the selected attachments

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **catalog**
> bool, date, datetime, dict, float, int, list, str, none_type catalog(catalog)

Get full metadata about a specific catalog

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.catalog_meta import CatalogMeta
from openapi_client.model.field_group import FieldGroup
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
    }
    try:
        # Get full metadata about a specific catalog
        api_response = api_instance.catalog(
            path_params=path_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->catalog: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | catalog result

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **count**
> int count(catalog)

Get count of matching rows (given some filters)

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.filters import Filters
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
    }
    query_params = {
    }
    try:
        # Get count of matching rows (given some filters)
        api_response = api_instance.count(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->count: %s\n" % e)

    # example passing only optional values
    path_params = {
        'catalog': "catalog_example",
    }
    query_params = {
        'filters': Filters(
        seed=0,
        sample=1,
    ),
    }
    try:
        # Get count of matching rows (given some filters)
        api_response = api_instance.count(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->count: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
filters | FiltersSchema | | optional


#### FiltersSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**Filters**](Filters.md) |  | 


### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | count result

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

number of matching rows

Type | Description | Notes
------------- | ------------- | -------------
**int** | number of matching rows | 


**int**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **count_post**
> int count_post(catalog)

Get count of matching rows (given some filters)

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.filters import Filters
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
    }
    try:
        # Get count of matching rows (given some filters)
        api_response = api_instance.count_post(
            path_params=path_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->count_post: %s\n" % e)

    # example passing only optional values
    path_params = {
        'catalog': "catalog_example",
    }
    body = None
    try:
        # Get count of matching rows (given some filters)
        api_response = api_instance.count_post(
            path_params=path_params,
            body=body,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->count_post: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
path_params | RequestPathParams | |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | count result

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

number of matching rows

Type | Description | Notes
------------- | ------------- | -------------
**int** | number of matching rows | 


**int**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **data**
> Data data(catalogcount)

Get a sample of raw data rows

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.sort import Sort
from openapi_client.model.filters import Filters
from openapi_client.model.data import Data
from openapi_client.model.field_list import FieldList
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
    }
    query_params = {
        'count': 0,
    }
    try:
        # Get a sample of raw data rows
        api_response = api_instance.data(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->data: %s\n" % e)

    # example passing only optional values
    path_params = {
        'catalog': "catalog_example",
    }
    query_params = {
        'filters': Filters(
        seed=0,
        sample=1,
    ),
        'fields': FieldList([
        "fields_example"
    ]),
        'sort': Sort([
        None
    ]),
        'count': 0,
        'offset': 0,
        'object': False,
    }
    try:
        # Get a sample of raw data rows
        api_response = api_instance.data(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->data: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
filters | FiltersSchema | | optional
fields | FieldsSchema | | optional
sort | SortSchema | | optional
count | CountSchema | | 
offset | OffsetSchema | | optional
object | ObjectSchema | | optional


#### FiltersSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**Filters**](Filters.md) |  | 


#### FieldsSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**FieldList**](FieldList.md) |  | 


#### SortSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**Sort**](Sort.md) |  | 


#### CountSchema

number of rows to return

Type | Description | Notes
------------- | ------------- | -------------
**int** | number of rows to return | 

#### OffsetSchema

start at this row offset (0 means first)

Type | Description | Notes
------------- | ------------- | -------------
**int** | start at this row offset (0 means first) | defaults to 0

#### ObjectSchema

return JSON objects instead of arrays of data

Type | Description | Notes
------------- | ------------- | -------------
**bool** | return JSON objects instead of arrays of data | defaults to False

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | selected data

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**Data**](Data.md) |  | 



[**Data**](Data.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **data_post**
> Data data_post(catalog)

Get a sample of raw data rows

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.sort import Sort
from openapi_client.model.filters import Filters
from openapi_client.model.data import Data
from openapi_client.model.field_list import FieldList
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
    }
    try:
        # Get a sample of raw data rows
        api_response = api_instance.data_post(
            path_params=path_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->data_post: %s\n" % e)

    # example passing only optional values
    path_params = {
        'catalog': "catalog_example",
    }
    body = None
    try:
        # Get a sample of raw data rows
        api_response = api_instance.data_post(
            path_params=path_params,
            body=body,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->data_post: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
path_params | RequestPathParams | |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | selected data

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**Data**](Data.md) |  | 



[**Data**](Data.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **download**
> bool, date, datetime, dict, float, int, list, str, none_type download(catalogformat)

Download raw data in bulk

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.sort import Sort
from openapi_client.model.filters import Filters
from openapi_client.model.data import Data
from openapi_client.model.field_list import FieldList
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
        'format': "ndjson",
    }
    query_params = {
    }
    try:
        # Download raw data in bulk
        api_response = api_instance.download(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->download: %s\n" % e)

    # example passing only optional values
    path_params = {
        'catalog': "catalog_example",
        'format': "ndjson",
    }
    query_params = {
        'filters': Filters(
        seed=0,
        sample=1,
    ),
        'fields': FieldList([
        "fields_example"
    ]),
        'sort': Sort([
        None
    ]),
    }
    try:
        # Download raw data in bulk
        api_response = api_instance.download(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->download: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('application/x-ndjson', 'text/csv', 'application/fits', 'application/x-npy', 'application/json', 'text/x-ecsv', 'application/gzip', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
filters | FiltersSchema | | optional
fields | FieldsSchema | | optional
sort | SortSchema | | optional


#### FiltersSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**Filters**](Filters.md) |  | 


#### FieldsSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**FieldList**](FieldList.md) |  | 


#### SortSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**Sort**](Sort.md) |  | 


### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 
format | FormatSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

#### FormatSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  |  must be one of ["ndjson", "ndjson.gz", "csv", "csv.gz", "fits", "fits.gz", "npy", "npy.gz", "json", "json.gz", "ecsv", "ecsv.gz", ]

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | file containing all matching content in the selected format

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationXNdjson, SchemaFor200ResponseBodyTextCsv, SchemaFor200ResponseBodyApplicationFits, SchemaFor200ResponseBodyApplicationXNpy, SchemaFor200ResponseBodyApplicationJson, SchemaFor200ResponseBodyTextXEcsv, SchemaFor200ResponseBodyApplicationGzip, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationXNdjson

Newline-delimited JSON data, where each line is an object of field values

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyTextCsv

Standard CSV file with a header of field names; missing values are represented by empty fields, arrays are encoded as JSON

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyApplicationFits

FITS BINTABLE binary table file containing the requested fields

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyApplicationXNpy

Numpy binary array file containing a 1-d array of structured data types representing the fields in each row; missing values are generally represented by NaN or extreme values

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyApplicationJson

JSON array data, where the first element is an array of field names and subsequent elements are arrays of values in the same order

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyTextXEcsv

Enhanced CSV file as per https://github.com/astropy/astropy-APEs/blob/main/APE6.rst, with a YAML header followed by a standard CSV file

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyApplicationGzip

compressed version of the selected format

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **download_post**
> bool, date, datetime, dict, float, int, list, str, none_type download_post(catalogformat)

Download raw data in bulk

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.sort import Sort
from openapi_client.model.filters import Filters
from openapi_client.model.data import Data
from openapi_client.model.field_list import FieldList
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
        'format': "ndjson",
    }
    try:
        # Download raw data in bulk
        api_response = api_instance.download_post(
            path_params=path_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->download_post: %s\n" % e)

    # example passing only optional values
    path_params = {
        'catalog': "catalog_example",
        'format': "ndjson",
    }
    body = None
    try:
        # Download raw data in bulk
        api_response = api_instance.download_post(
            path_params=path_params,
            body=body,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->download_post: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
path_params | RequestPathParams | |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/x-ndjson', 'text/csv', 'application/fits', 'application/x-npy', 'application/json', 'text/x-ecsv', 'application/gzip', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 
format | FormatSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

#### FormatSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  |  must be one of ["ndjson", "ndjson.gz", "csv", "csv.gz", "fits", "fits.gz", "npy", "npy.gz", "json", "json.gz", "ecsv", "ecsv.gz", ]

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | file containing all matching content in the selected format

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationXNdjson, SchemaFor200ResponseBodyTextCsv, SchemaFor200ResponseBodyApplicationFits, SchemaFor200ResponseBodyApplicationXNpy, SchemaFor200ResponseBodyApplicationJson, SchemaFor200ResponseBodyTextXEcsv, SchemaFor200ResponseBodyApplicationGzip, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationXNdjson

Newline-delimited JSON data, where each line is an object of field values

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyTextCsv

Standard CSV file with a header of field names; missing values are represented by empty fields, arrays are encoded as JSON

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyApplicationFits

FITS BINTABLE binary table file containing the requested fields

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyApplicationXNpy

Numpy binary array file containing a 1-d array of structured data types representing the fields in each row; missing values are generally represented by NaN or extreme values

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyApplicationJson

JSON array data, where the first element is an array of field names and subsequent elements are arrays of values in the same order

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyTextXEcsv

Enhanced CSV file as per https://github.com/astropy/astropy-APEs/blob/main/APE6.rst, with a YAML header followed by a standard CSV file

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

#### SchemaFor200ResponseBodyApplicationGzip

compressed version of the selected format

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **histogram**
> {str: typing.Any} histogram(catalogfields)

Get a histogram of data across one or more fields

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.field_value import FieldValue
from openapi_client.model.histogram_list import HistogramList
from openapi_client.model.filters import Filters
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
    }
    query_params = {
        'fields': HistogramList(None),
    }
    try:
        # Get a histogram of data across one or more fields
        api_response = api_instance.histogram(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->histogram: %s\n" % e)

    # example passing only optional values
    path_params = {
        'catalog': "catalog_example",
    }
    query_params = {
        'filters': Filters(
        seed=0,
        sample=1,
    ),
        'fields': HistogramList(None),
        'quartiles': "quartiles_example",
    }
    try:
        # Get a histogram of data across one or more fields
        api_response = api_instance.histogram(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->histogram: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
filters | FiltersSchema | | optional
fields | FieldsSchema | | 
quartiles | QuartilesSchema | | optional


#### FiltersSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**Filters**](Filters.md) |  | 


#### FieldsSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**HistogramList**](HistogramList.md) |  | 


#### QuartilesSchema

field name in selected catalog

Type | Description | Notes
------------- | ------------- | -------------
**str** | field name in selected catalog | 

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | histogram result

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

histogram result

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**buckets** | **[{str: typing.Any}]** |  | 
**sizes** | **[int, float]** | field order corresponds to the requested histogram fields and bucket keys | 


**{str: typing.Any}**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **histogram_post**
> {str: typing.Any} histogram_post(catalog)

Get a histogram of data across one or more fields

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.field_value import FieldValue
from openapi_client.model.histogram_list import HistogramList
from openapi_client.model.filters import Filters
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
    }
    try:
        # Get a histogram of data across one or more fields
        api_response = api_instance.histogram_post(
            path_params=path_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->histogram_post: %s\n" % e)

    # example passing only optional values
    path_params = {
        'catalog': "catalog_example",
    }
    body = None
    try:
        # Get a histogram of data across one or more fields
        api_response = api_instance.histogram_post(
            path_params=path_params,
            body=body,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->histogram_post: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
path_params | RequestPathParams | |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | histogram result

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

histogram result

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**buckets** | **[{str: typing.Any}]** |  | 
**sizes** | **[int, float]** | field order corresponds to the requested histogram fields and bucket keys | 


**{str: typing.Any}**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **schema_csv**
> bool, date, datetime, dict, float, int, list, str, none_type schema_csv(catalog)

Get a CSV representation of the catalog schema (no data)

### Example

```python
import openapi_client
from openapi_client.api import default_api
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
    }
    try:
        # Get a CSV representation of the catalog schema (no data)
        api_response = api_instance.schema_csv(
            path_params=path_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->schema_csv: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('text/csv', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | schema.csv result

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyTextCsv, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyTextCsv

CSV representation of catalog fields

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **schema_sql**
> bool, date, datetime, dict, float, int, list, str, none_type schema_sql(catalog)

Get a SQL representation of the catalog schema (no data)

### Example

```python
import openapi_client
from openapi_client.api import default_api
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
    }
    try:
        # Get a SQL representation of the catalog schema (no data)
        api_response = api_instance.schema_sql(
            path_params=path_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->schema_sql: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('application/sql', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | schema.sql result

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationSql, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationSql

CREATE TABLE SQL schema

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **stats**
> {str: (FieldStats,)} stats(catalog)

Get statistics about fields (given some filters)

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.field_stats import FieldStats
from openapi_client.model.filters import Filters
from openapi_client.model.field_list import FieldList
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
    }
    query_params = {
    }
    try:
        # Get statistics about fields (given some filters)
        api_response = api_instance.stats(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->stats: %s\n" % e)

    # example passing only optional values
    path_params = {
        'catalog': "catalog_example",
    }
    query_params = {
        'filters': Filters(
        seed=0,
        sample=1,
    ),
        'fields': FieldList([
        "fields_example"
    ]),
    }
    try:
        # Get statistics about fields (given some filters)
        api_response = api_instance.stats(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->stats: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
filters | FiltersSchema | | optional
fields | FieldsSchema | | optional


#### FiltersSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**Filters**](Filters.md) |  | 


#### FieldsSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**FieldList**](FieldList.md) |  | 


### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | stats result

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

stats

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**count** | **int** | number of matching rows | 
**any string name** | **FieldStats** | any string name can be used but the value must be the correct type | [optional]


[**{str: (FieldStats,)}**](FieldStats.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **stats_post**
> {str: (FieldStats,)} stats_post(catalog)

Get statistics about fields (given some filters)

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.field_stats import FieldStats
from openapi_client.model.filters import Filters
from openapi_client.model.field_list import FieldList
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'catalog': "catalog_example",
    }
    try:
        # Get statistics about fields (given some filters)
        api_response = api_instance.stats_post(
            path_params=path_params,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->stats_post: %s\n" % e)

    # example passing only optional values
    path_params = {
        'catalog': "catalog_example",
    }
    body = None
    try:
        # Get statistics about fields (given some filters)
        api_response = api_instance.stats_post(
            path_params=path_params,
            body=body,
        )
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->stats_post: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
path_params | RequestPathParams | |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
catalog | CatalogSchema | | 

#### CatalogSchema

catalog name

Type | Description | Notes
------------- | ------------- | -------------
**str** | catalog name | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | stats result

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

stats

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**count** | **int** | number of matching rows | 
**any string name** | **FieldStats** | any string name can be used but the value must be the correct type | [optional]


[**{str: (FieldStats,)}**](FieldStats.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **top**
> [CatalogMeta] top()

Get the list of available dataset catalogs

### Example

```python
import openapi_client
from openapi_client.api import default_api
from openapi_client.model.catalog_meta import CatalogMeta
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        # Get the list of available dataset catalogs
        api_response = api_instance.top()
        pprint(api_response)
    except openapi_client.ApiException as e:
        print("Exception when calling DefaultApi->top: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | top result

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**[CatalogMeta]** |  | 


[**[CatalogMeta]**](CatalogMeta.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

