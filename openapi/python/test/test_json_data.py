"""
    FlatHUB API

    Most operations support GET and POST, either of which accepts JSON request bodies or query parameters.  In most cases, query parameters are ignored when there is a request body.  # noqa: E501

    The version of the OpenAPI document: 1.0
    Generated by: https://openapi-generator.tech
"""


import sys
import unittest

import flathub
from flathub.model.data import Data
globals()['Data'] = Data
from flathub.model.json_data import JsonData


class TestJsonData(unittest.TestCase):
    """JsonData unit test stubs"""

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def testJsonData(self):
        """Test JsonData"""
        # FIXME: construct object with mandatory attributes with example values
        # model = JsonData()  # noqa: E501
        pass


if __name__ == '__main__':
    unittest.main()
