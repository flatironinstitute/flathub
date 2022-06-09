"""
    FlatHUB API

    Most operations support GET and POST, either of which accepts JSON request bodies or query parameters.  In most cases, query parameters are ignored when there is a request body.  # noqa: E501

    The version of the OpenAPI document: 1.0
    Generated by: https://openapi-generator.tech
"""


import sys
import unittest

import flathub
from flathub.model.field_list import FieldList
from flathub.model.sort import Sort
globals()['FieldList'] = FieldList
globals()['Sort'] = Sort
from flathub.model.download_request_all_of import DownloadRequestAllOf


class TestDownloadRequestAllOf(unittest.TestCase):
    """DownloadRequestAllOf unit test stubs"""

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def testDownloadRequestAllOf(self):
        """Test DownloadRequestAllOf"""
        # FIXME: construct object with mandatory attributes with example values
        # model = DownloadRequestAllOf()  # noqa: E501
        pass


if __name__ == '__main__':
    unittest.main()
