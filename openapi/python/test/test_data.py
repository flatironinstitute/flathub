"""
    FlatHUB API

    Most operations support GET and POST, either of which accepts JSON request bodies or query parameters.  In most cases, query parameters are ignored when there is a request body.  # noqa: E501

    The version of the OpenAPI document: 1.0
    Generated by: https://openapi-generator.tech
"""


import sys
import unittest

import openapi_client
from openapi_client.model.data_row_inner import DataRowInner
globals()['DataRowInner'] = DataRowInner
from openapi_client.model.data import Data


class TestData(unittest.TestCase):
    """Data unit test stubs"""

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def testData(self):
        """Test Data"""
        # FIXME: construct object with mandatory attributes with example values
        # model = Data()  # noqa: E501
        pass


if __name__ == '__main__':
    unittest.main()
