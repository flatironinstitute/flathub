"""
    FlatHUB API

    Most operations support GET and POST, either of which accepts JSON request bodies or query parameters.  In most cases, query parameters are ignored when there is a request body.  # noqa: E501

    The version of the OpenAPI document: 1.0
    Generated by: https://openapi-generator.tech
"""


import sys
import unittest

import openapi_client
from openapi_client.model.histogram_desc import HistogramDesc
globals()['HistogramDesc'] = HistogramDesc
from openapi_client.model.histogram import Histogram


class TestHistogram(unittest.TestCase):
    """Histogram unit test stubs"""

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def testHistogram(self):
        """Test Histogram"""
        # FIXME: construct object with mandatory attributes with example values
        # model = Histogram()  # noqa: E501
        pass


if __name__ == '__main__':
    unittest.main()
