# flathub.DefaultApi

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
> bool, date, datetime, dict, float, int, list, str, none_type attachment(catalog, field, id)

Download a row attachment

### Example


```python
import time
import flathub
from flathub.api import default_api
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    field = "field_example" # str | field name of attachment
    id = "id_example" # str | _id for row of interest

    # example passing only required values which don't have defaults set
    try:
        # Download a row attachment
        api_response = api_instance.attachment(catalog, field, id)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->attachment: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **field** | **str**| field name of attachment |
 **id** | **str**| _id for row of interest |

### Return type

**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/octet-stream


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | attachment result |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **attachments**
> bool, date, datetime, dict, float, int, list, str, none_type attachments(catalog, format)

Download attachments in bulk from matching rows for multiple fields

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.filters_value import FiltersValue
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    format = "sh" # str | 
    filters = Filters(
        key=FiltersValue(None),
    ) # Filters | filter in query string (see descriptions for non-standard formatting of range queries) (optional)
    fields = FieldList([
        "fields_example",
    ]) # FieldList | list of fields to return (optional)

    # example passing only required values which don't have defaults set
    try:
        # Download attachments in bulk from matching rows for multiple fields
        api_response = api_instance.attachments(catalog, format)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->attachments: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Download attachments in bulk from matching rows for multiple fields
        api_response = api_instance.attachments(catalog, format, filters=filters, fields=fields)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->attachments: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **format** | **str**|  |
 **filters** | [**Filters**](FiltersValue.md)| filter in query string (see descriptions for non-standard formatting of range queries) | [optional]
 **fields** | **FieldList**| list of fields to return | [optional]

### Return type

**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/x-shellscript, text/uri-list, application/zip


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | file containing all matching attachments in the selected format |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **attachments1**
> bool, date, datetime, dict, float, int, list, str, none_type attachments1(catalog, format, field)

Download attachments in bulk from matching rows for single field

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.filters_value import FiltersValue
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    format = "sh" # str | 
    field = "field_example" # str | 
    filters = Filters(
        key=FiltersValue(None),
    ) # Filters | filter in query string (see descriptions for non-standard formatting of range queries) (optional)

    # example passing only required values which don't have defaults set
    try:
        # Download attachments in bulk from matching rows for single field
        api_response = api_instance.attachments1(catalog, format, field)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->attachments1: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Download attachments in bulk from matching rows for single field
        api_response = api_instance.attachments1(catalog, format, field, filters=filters)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->attachments1: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **format** | **str**|  |
 **field** | **str**|  |
 **filters** | [**Filters**](FiltersValue.md)| filter in query string (see descriptions for non-standard formatting of range queries) | [optional]

### Return type

**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/x-shellscript, text/uri-list, application/zip


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | file containing all matching attachments in the selected format |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **attachments1_post**
> bool, date, datetime, dict, float, int, list, str, none_type attachments1_post(catalog, format, field)

Download attachments in bulk from matching rows for single field

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.attachments1_request import Attachments1Request
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    format = "sh" # str | 
    field = "field_example" # str | 
    attachments1_request = Attachments1Request(None) # Attachments1Request |  (optional)

    # example passing only required values which don't have defaults set
    try:
        # Download attachments in bulk from matching rows for single field
        api_response = api_instance.attachments1_post(catalog, format, field)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->attachments1_post: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Download attachments in bulk from matching rows for single field
        api_response = api_instance.attachments1_post(catalog, format, field, attachments1_request=attachments1_request)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->attachments1_post: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **format** | **str**|  |
 **field** | **str**|  |
 **attachments1_request** | [**Attachments1Request**](Attachments1Request.md)|  | [optional]

### Return type

**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/x-shellscript, text/uri-list, application/zip


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | file containing all matching attachments in the selected format |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **attachments_post**
> bool, date, datetime, dict, float, int, list, str, none_type attachments_post(catalog, format)

Download attachments in bulk from matching rows for multiple fields

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.attachments_request import AttachmentsRequest
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    format = "sh" # str | 
    attachments_request = AttachmentsRequest(None) # AttachmentsRequest |  (optional)

    # example passing only required values which don't have defaults set
    try:
        # Download attachments in bulk from matching rows for multiple fields
        api_response = api_instance.attachments_post(catalog, format)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->attachments_post: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Download attachments in bulk from matching rows for multiple fields
        api_response = api_instance.attachments_post(catalog, format, attachments_request=attachments_request)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->attachments_post: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **format** | **str**|  |
 **attachments_request** | [**AttachmentsRequest**](AttachmentsRequest.md)|  | [optional]

### Return type

**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/x-shellscript, text/uri-list, application/zip


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | file containing all matching attachments in the selected format |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **catalog**
> CatalogResult catalog(catalog)

Get full metadata about a specific catalog

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.catalog_result import CatalogResult
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs

    # example passing only required values which don't have defaults set
    try:
        # Get full metadata about a specific catalog
        api_response = api_instance.catalog(catalog)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->catalog: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |

### Return type

[**CatalogResult**](CatalogResult.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | catalog result |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **count**
> int count(catalog)

Get count of matching rows (given some filters)

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.filters_value import FiltersValue
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    filters = Filters(
        key=FiltersValue(None),
    ) # Filters | filter in query string (see descriptions for non-standard formatting of range queries) (optional)

    # example passing only required values which don't have defaults set
    try:
        # Get count of matching rows (given some filters)
        api_response = api_instance.count(catalog)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->count: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Get count of matching rows (given some filters)
        api_response = api_instance.count(catalog, filters=filters)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->count: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **filters** | [**Filters**](FiltersValue.md)| filter in query string (see descriptions for non-standard formatting of range queries) | [optional]

### Return type

**int**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | count result |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **count_post**
> int count_post(catalog)

Get count of matching rows (given some filters)

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.count_request import CountRequest
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    count_request = CountRequest(None) # CountRequest |  (optional)

    # example passing only required values which don't have defaults set
    try:
        # Get count of matching rows (given some filters)
        api_response = api_instance.count_post(catalog)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->count_post: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Get count of matching rows (given some filters)
        api_response = api_instance.count_post(catalog, count_request=count_request)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->count_post: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **count_request** | [**CountRequest**](CountRequest.md)|  | [optional]

### Return type

**int**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | count result |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **data**
> Data data(catalog, count)

Get a sample of raw data rows

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.data import Data
from flathub.model.filters_value import FiltersValue
from flathub.model.sort_field import SortField
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    count = 0 # int | 
    filters = Filters(
        key=FiltersValue(None),
    ) # Filters | filter in query string (see descriptions for non-standard formatting of range queries) (optional)
    fields = FieldList([
        "fields_example",
    ]) # FieldList | list of fields to return (optional)
    sort = Sort([
        SortField(None),
    ]) # Sort | how to order rows (see descriptions for non-standard formatting of sort order) (optional)
    offset = 0 # int |  (optional) if omitted the server will use the default value of 0
    object = False # bool |  (optional) if omitted the server will use the default value of False

    # example passing only required values which don't have defaults set
    try:
        # Get a sample of raw data rows
        api_response = api_instance.data(catalog, count)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->data: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Get a sample of raw data rows
        api_response = api_instance.data(catalog, count, filters=filters, fields=fields, sort=sort, offset=offset, object=object)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->data: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **count** | **int**|  |
 **filters** | [**Filters**](FiltersValue.md)| filter in query string (see descriptions for non-standard formatting of range queries) | [optional]
 **fields** | **FieldList**| list of fields to return | [optional]
 **sort** | [**Sort**](SortField.md)| how to order rows (see descriptions for non-standard formatting of sort order) | [optional]
 **offset** | **int**|  | [optional] if omitted the server will use the default value of 0
 **object** | **bool**|  | [optional] if omitted the server will use the default value of False

### Return type

[**Data**](Data.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | selected data |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **data_post**
> Data data_post(catalog)

Get a sample of raw data rows

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.data import Data
from flathub.model.data_request import DataRequest
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    data_request = DataRequest(None) # DataRequest |  (optional)

    # example passing only required values which don't have defaults set
    try:
        # Get a sample of raw data rows
        api_response = api_instance.data_post(catalog)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->data_post: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Get a sample of raw data rows
        api_response = api_instance.data_post(catalog, data_request=data_request)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->data_post: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **data_request** | [**DataRequest**](DataRequest.md)|  | [optional]

### Return type

[**Data**](Data.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | selected data |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **download**
> NdjsonData download(catalog, format)

Download raw data in bulk

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.npy_data import NpyData
from flathub.model.filters_value import FiltersValue
from flathub.model.fits_data import FitsData
from flathub.model.json_data import JsonData
from flathub.model.sort_field import SortField
from flathub.model.ecsv_data import EcsvData
from flathub.model.ndjson_data import NdjsonData
from flathub.model.gzip_data import GzipData
from flathub.model.csv_data import CsvData
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    format = "ndjson" # str | 
    filters = Filters(
        key=FiltersValue(None),
    ) # Filters | filter in query string (see descriptions for non-standard formatting of range queries) (optional)
    fields = FieldList([
        "fields_example",
    ]) # FieldList | list of fields to return (optional)
    sort = Sort([
        SortField(None),
    ]) # Sort | how to order rows (see descriptions for non-standard formatting of sort order) (optional)

    # example passing only required values which don't have defaults set
    try:
        # Download raw data in bulk
        api_response = api_instance.download(catalog, format)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->download: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Download raw data in bulk
        api_response = api_instance.download(catalog, format, filters=filters, fields=fields, sort=sort)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->download: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **format** | **str**|  |
 **filters** | [**Filters**](FiltersValue.md)| filter in query string (see descriptions for non-standard formatting of range queries) | [optional]
 **fields** | **FieldList**| list of fields to return | [optional]
 **sort** | [**Sort**](SortField.md)| how to order rows (see descriptions for non-standard formatting of sort order) | [optional]

### Return type

[**NdjsonData**](NdjsonData.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/x-ndjson, text/csv, application/fits, application/x-npy, application/json, text/x-ecsv, application/gzip


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | file containing all matching content in the selected format |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **download_post**
> NdjsonData download_post(catalog, format)

Download raw data in bulk

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.npy_data import NpyData
from flathub.model.fits_data import FitsData
from flathub.model.json_data import JsonData
from flathub.model.ecsv_data import EcsvData
from flathub.model.ndjson_data import NdjsonData
from flathub.model.gzip_data import GzipData
from flathub.model.download_request import DownloadRequest
from flathub.model.csv_data import CsvData
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    format = "ndjson" # str | 
    download_request = DownloadRequest(None) # DownloadRequest |  (optional)

    # example passing only required values which don't have defaults set
    try:
        # Download raw data in bulk
        api_response = api_instance.download_post(catalog, format)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->download_post: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Download raw data in bulk
        api_response = api_instance.download_post(catalog, format, download_request=download_request)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->download_post: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **format** | **str**|  |
 **download_request** | [**DownloadRequest**](DownloadRequest.md)|  | [optional]

### Return type

[**NdjsonData**](NdjsonData.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/x-ndjson, text/csv, application/fits, application/x-npy, application/json, text/x-ecsv, application/gzip


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | file containing all matching content in the selected format |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **histogram**
> HistogramResult histogram(catalog, fields)

Get a histogram of data across one or more fields

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.filters_value import FiltersValue
from flathub.model.histogram_result import HistogramResult
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    fields = HistogramList(None) # HistogramList | field(s) along which to calculate histograms (see descriptions for non-standard formatting of size/log)
    filters = Filters(
        key=FiltersValue(None),
    ) # Filters | filter in query string (see descriptions for non-standard formatting of range queries) (optional)
    quartiles = "quartiles_example" # str | optional field within which to calculate quartiles (optional)

    # example passing only required values which don't have defaults set
    try:
        # Get a histogram of data across one or more fields
        api_response = api_instance.histogram(catalog, fields)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->histogram: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Get a histogram of data across one or more fields
        api_response = api_instance.histogram(catalog, fields, filters=filters, quartiles=quartiles)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->histogram: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **fields** | **HistogramList**| field(s) along which to calculate histograms (see descriptions for non-standard formatting of size/log) |
 **filters** | [**Filters**](FiltersValue.md)| filter in query string (see descriptions for non-standard formatting of range queries) | [optional]
 **quartiles** | **str**| optional field within which to calculate quartiles | [optional]

### Return type

[**HistogramResult**](HistogramResult.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | histogram result |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **histogram_post**
> HistogramResult histogram_post(catalog)

Get a histogram of data across one or more fields

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.histogram_request import HistogramRequest
from flathub.model.histogram_result import HistogramResult
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    histogram_request = HistogramRequest(None) # HistogramRequest |  (optional)

    # example passing only required values which don't have defaults set
    try:
        # Get a histogram of data across one or more fields
        api_response = api_instance.histogram_post(catalog)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->histogram_post: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Get a histogram of data across one or more fields
        api_response = api_instance.histogram_post(catalog, histogram_request=histogram_request)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->histogram_post: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **histogram_request** | [**HistogramRequest**](HistogramRequest.md)|  | [optional]

### Return type

[**HistogramResult**](HistogramResult.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | histogram result |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **schema_csv**
> bool, date, datetime, dict, float, int, list, str, none_type schema_csv(catalog)

Get a CSV representation of the catalog schema (no data)

### Example


```python
import time
import flathub
from flathub.api import default_api
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs

    # example passing only required values which don't have defaults set
    try:
        # Get a CSV representation of the catalog schema (no data)
        api_response = api_instance.schema_csv(catalog)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->schema_csv: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |

### Return type

**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/csv


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | schema.csv result |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **schema_sql**
> bool, date, datetime, dict, float, int, list, str, none_type schema_sql(catalog)

Get a SQL representation of the catalog schema (no data)

### Example


```python
import time
import flathub
from flathub.api import default_api
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs

    # example passing only required values which don't have defaults set
    try:
        # Get a SQL representation of the catalog schema (no data)
        api_response = api_instance.schema_sql(catalog)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->schema_sql: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |

### Return type

**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/sql


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | schema.sql result |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **stats**
> StatsResult stats(catalog)

Get statistics about fields (given some filters)

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.filters_value import FiltersValue
from flathub.model.stats_result import StatsResult
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    filters = Filters(
        key=FiltersValue(None),
    ) # Filters | filter in query string (see descriptions for non-standard formatting of range queries) (optional)
    fields = FieldList([
        "fields_example",
    ]) # FieldList | list of fields to return (optional)

    # example passing only required values which don't have defaults set
    try:
        # Get statistics about fields (given some filters)
        api_response = api_instance.stats(catalog)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->stats: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Get statistics about fields (given some filters)
        api_response = api_instance.stats(catalog, filters=filters, fields=fields)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->stats: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **filters** | [**Filters**](FiltersValue.md)| filter in query string (see descriptions for non-standard formatting of range queries) | [optional]
 **fields** | **FieldList**| list of fields to return | [optional]

### Return type

[**StatsResult**](StatsResult.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | stats result |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **stats_post**
> StatsResult stats_post(catalog)

Get statistics about fields (given some filters)

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.stats_request import StatsRequest
from flathub.model.stats_result import StatsResult
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)
    catalog = "catalog_example" # str | catalog name from list of catalogs
    stats_request = StatsRequest(None) # StatsRequest |  (optional)

    # example passing only required values which don't have defaults set
    try:
        # Get statistics about fields (given some filters)
        api_response = api_instance.stats_post(catalog)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->stats_post: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Get statistics about fields (given some filters)
        api_response = api_instance.stats_post(catalog, stats_request=stats_request)
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->stats_post: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catalog** | **str**| catalog name from list of catalogs |
 **stats_request** | [**StatsRequest**](StatsRequest.md)|  | [optional]

### Return type

[**StatsResult**](StatsResult.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | stats result |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **top**
> [CatalogMeta] top()

Get the list of available dataset catalogs

### Example


```python
import time
import flathub
from flathub.api import default_api
from flathub.model.catalog_meta import CatalogMeta
from pprint import pprint
# Defining the host is optional and defaults to https://flathub.flatironinstitute.org/api
# See configuration.py for a list of all supported configuration parameters.
configuration = flathub.Configuration(
    host = "https://flathub.flatironinstitute.org/api"
)


# Enter a context with an instance of the API client
with flathub.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        # Get the list of available dataset catalogs
        api_response = api_instance.top()
        pprint(api_response)
    except flathub.ApiException as e:
        print("Exception when calling DefaultApi->top: %s\n" % e)
```


### Parameters
This endpoint does not need any parameter.

### Return type

[**[CatalogMeta]**](CatalogMeta.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | top result |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

