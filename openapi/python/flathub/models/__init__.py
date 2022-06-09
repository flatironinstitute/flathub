# flake8: noqa

# import all models into this package
# if you have many models here with many references from one model to another this may
# raise a RecursionError
# to avoid this, import only the models that you directly need like:
# from from flathub.model.pet import Pet
# or import this package, but before doing it, use:
# import sys
# sys.setrecursionlimit(n)

from flathub.model.attachments1_request import Attachments1Request
from flathub.model.attachments_request import AttachmentsRequest
from flathub.model.attachments_request_all_of import AttachmentsRequestAllOf
from flathub.model.bucket import Bucket
from flathub.model.catalog_info import CatalogInfo
from flathub.model.catalog_meta import CatalogMeta
from flathub.model.catalog_result import CatalogResult
from flathub.model.count_request import CountRequest
from flathub.model.csv_data import CsvData
from flathub.model.data import Data
from flathub.model.data_request import DataRequest
from flathub.model.data_request_all_of import DataRequestAllOf
from flathub.model.data_row_inner import DataRowInner
from flathub.model.download_request import DownloadRequest
from flathub.model.download_request_all_of import DownloadRequestAllOf
from flathub.model.ecsv_data import EcsvData
from flathub.model.field_group import FieldGroup
from flathub.model.field_list import FieldList
from flathub.model.field_stats import FieldStats
from flathub.model.field_stats_one_of import FieldStatsOneOf
from flathub.model.field_value import FieldValue
from flathub.model.field_value_scalar import FieldValueScalar
from flathub.model.filter_range import FilterRange
from flathub.model.filter_wildcard import FilterWildcard
from flathub.model.filters import Filters
from flathub.model.filters_value import FiltersValue
from flathub.model.fits_data import FitsData
from flathub.model.gzip_data import GzipData
from flathub.model.histogram import Histogram
from flathub.model.histogram_desc import HistogramDesc
from flathub.model.histogram_list import HistogramList
from flathub.model.histogram_parameters import HistogramParameters
from flathub.model.histogram_request import HistogramRequest
from flathub.model.histogram_result import HistogramResult
from flathub.model.json_data import JsonData
from flathub.model.ndjson_data import NdjsonData
from flathub.model.npy_data import NpyData
from flathub.model.numeric_stats import NumericStats
from flathub.model.sort import Sort
from flathub.model.sort_descriptor import SortDescriptor
from flathub.model.sort_field import SortField
from flathub.model.stats_fields import StatsFields
from flathub.model.stats_request import StatsRequest
from flathub.model.stats_result import StatsResult
from flathub.model.top_term import TopTerm
from flathub.model.type import Type
