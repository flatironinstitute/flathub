# flake8: noqa

# import all models into this package
# if you have many models here with many references from one model to another this may
# raise a RecursionError
# to avoid this, import only the models that you directly need like:
# from from openapi_client.model.pet import Pet
# or import this package, but before doing it, use:
# import sys
# sys.setrecursionlimit(n)

from openapi_client.model.attachments1_request import Attachments1Request
from openapi_client.model.attachments_request import AttachmentsRequest
from openapi_client.model.attachments_request_all_of import AttachmentsRequestAllOf
from openapi_client.model.bucket import Bucket
from openapi_client.model.catalog_info import CatalogInfo
from openapi_client.model.catalog_meta import CatalogMeta
from openapi_client.model.catalog_result import CatalogResult
from openapi_client.model.count_request import CountRequest
from openapi_client.model.csv_data import CsvData
from openapi_client.model.data import Data
from openapi_client.model.data_request import DataRequest
from openapi_client.model.data_request_all_of import DataRequestAllOf
from openapi_client.model.data_row_inner import DataRowInner
from openapi_client.model.download_request import DownloadRequest
from openapi_client.model.download_request_all_of import DownloadRequestAllOf
from openapi_client.model.ecsv_data import EcsvData
from openapi_client.model.field_group import FieldGroup
from openapi_client.model.field_list import FieldList
from openapi_client.model.field_stats import FieldStats
from openapi_client.model.field_stats_one_of import FieldStatsOneOf
from openapi_client.model.field_value import FieldValue
from openapi_client.model.field_value_scalar import FieldValueScalar
from openapi_client.model.filter_range import FilterRange
from openapi_client.model.filter_wildcard import FilterWildcard
from openapi_client.model.filters import Filters
from openapi_client.model.filters_value import FiltersValue
from openapi_client.model.fits_data import FitsData
from openapi_client.model.gzip_data import GzipData
from openapi_client.model.histogram import Histogram
from openapi_client.model.histogram_desc import HistogramDesc
from openapi_client.model.histogram_list import HistogramList
from openapi_client.model.histogram_parameters import HistogramParameters
from openapi_client.model.histogram_request import HistogramRequest
from openapi_client.model.histogram_result import HistogramResult
from openapi_client.model.json_data import JsonData
from openapi_client.model.ndjson_data import NdjsonData
from openapi_client.model.npy_data import NpyData
from openapi_client.model.numeric_stats import NumericStats
from openapi_client.model.sort import Sort
from openapi_client.model.sort_descriptor import SortDescriptor
from openapi_client.model.sort_field import SortField
from openapi_client.model.stats_fields import StatsFields
from openapi_client.model.stats_request import StatsRequest
from openapi_client.model.stats_result import StatsResult
from openapi_client.model.top_term import TopTerm
from openapi_client.model.type import Type
