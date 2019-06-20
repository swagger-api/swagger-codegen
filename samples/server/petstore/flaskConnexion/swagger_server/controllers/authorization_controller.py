from typing import List
"""
controller generated to handled auth operation described at:
https://connexion.readthedocs.io/en/latest/security.html
"""
def check_api_key(api_key, required_scopes):
    return {'test_key': 'test_value'}

def check_bearer(token):
    return {'test_key': 'test_value'}

def check_petstore_auth(token):
    return {'scopes': ['read:pets', 'write:pets'], 'uid': 'test_value'}

def validate_scope_petstore_auth(required_scopes, token_scopes):
    return set(required_scopes).issubset(set(token_scopes))


