# coding: utf-8

"""
Run the tests.
$ pip install nose (optional)
$ cd SwaggerPetstore-python
$ nosetests -v
"""

import time
import unittest

import swagger_client


class StoreApiTests(unittest.TestCase):

    def setUp(self):
        swagger_client.configuration.reset()

        swagger_client.configuration.api_key['api_key'] = '123456'
        swagger_client.configuration.api_key_prefix['api_key'] = 'PREFIX'
        swagger_client.configuration.username = 'test_username'
        swagger_client.configuration.password = 'test_password'

        self.store_api = swagger_client.StoreApi()

    def tearDown(self):
        # sleep 1 sec between two every 2 tests
        time.sleep(1)

    def test_get_inventory(self):
        data = self.store_api.get_inventory()
        self.assertIsNotNone(data)
        self.assertTrue(isinstance(data, dict))
