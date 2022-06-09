# coding: utf-8

"""
    FlatHUB API

    Most operations support GET and POST, either of which accepts JSON request bodies or query parameters.  In most cases, query parameters are ignored when there is a request body.  # noqa: E501

    The version of the OpenAPI document: 1.0
    Generated by: https://openapi-generator.tech
"""

import re  # noqa: F401
import sys  # noqa: F401
import typing  # noqa: F401
import functools  # noqa: F401

from frozendict import frozendict  # noqa: F401

import decimal  # noqa: F401
from datetime import date, datetime  # noqa: F401
from frozendict import frozendict  # noqa: F401

from openapi_client.schemas import (  # noqa: F401
    AnyTypeSchema,
    ComposedSchema,
    DictSchema,
    ListSchema,
    StrSchema,
    IntSchema,
    Int32Schema,
    Int64Schema,
    Float32Schema,
    Float64Schema,
    NumberSchema,
    UUIDSchema,
    DateSchema,
    DateTimeSchema,
    DecimalSchema,
    BoolSchema,
    BinarySchema,
    NoneSchema,
    none_type,
    Configuration,
    Unset,
    unset,
    ComposedBase,
    ListBase,
    DictBase,
    NoneBase,
    StrBase,
    IntBase,
    Int32Base,
    Int64Base,
    Float32Base,
    Float64Base,
    NumberBase,
    UUIDBase,
    DateBase,
    DateTimeBase,
    BoolBase,
    BinaryBase,
    Schema,
    _SchemaValidator,
    _SchemaTypeChecker,
    _SchemaEnumMaker
)


class Type(
    _SchemaEnumMaker(
        enum_value_to_name={
            "double": "DOUBLE",
            "float": "FLOAT",
            "half_float": "HALF_FLOAT",
            "long": "LONG",
            "unsigned_long": "UNSIGNED_LONG",
            "integer": "INTEGER",
            "short": "SHORT",
            "byte": "BYTE",
            "boolean": "BOOLEAN",
            "keyword": "KEYWORD",
            "array double": "ARRAY_DOUBLE",
            "array float": "ARRAY_FLOAT",
            "array half_float": "ARRAY_HALF_FLOAT",
            "array long": "ARRAY_LONG",
            "array unsigned_long": "ARRAY_UNSIGNED_LONG",
            "array integer": "ARRAY_INTEGER",
            "array short": "ARRAY_SHORT",
            "array byte": "ARRAY_BYTE",
            "array boolean": "ARRAY_BOOLEAN",
            "array keyword": "ARRAY_KEYWORD",
        }
    ),
    StrSchema
):
    """NOTE: This class is auto generated by OpenAPI Generator.
    Ref: https://openapi-generator.tech

    Do not edit the class manually.

    storage type
    """
    
    @classmethod
    @property
    def DOUBLE(cls):
        return cls("double")
    
    @classmethod
    @property
    def FLOAT(cls):
        return cls("float")
    
    @classmethod
    @property
    def HALF_FLOAT(cls):
        return cls("half_float")
    
    @classmethod
    @property
    def LONG(cls):
        return cls("long")
    
    @classmethod
    @property
    def UNSIGNED_LONG(cls):
        return cls("unsigned_long")
    
    @classmethod
    @property
    def INTEGER(cls):
        return cls("integer")
    
    @classmethod
    @property
    def SHORT(cls):
        return cls("short")
    
    @classmethod
    @property
    def BYTE(cls):
        return cls("byte")
    
    @classmethod
    @property
    def BOOLEAN(cls):
        return cls("boolean")
    
    @classmethod
    @property
    def KEYWORD(cls):
        return cls("keyword")
    
    @classmethod
    @property
    def ARRAY_DOUBLE(cls):
        return cls("array double")
    
    @classmethod
    @property
    def ARRAY_FLOAT(cls):
        return cls("array float")
    
    @classmethod
    @property
    def ARRAY_HALF_FLOAT(cls):
        return cls("array half_float")
    
    @classmethod
    @property
    def ARRAY_LONG(cls):
        return cls("array long")
    
    @classmethod
    @property
    def ARRAY_UNSIGNED_LONG(cls):
        return cls("array unsigned_long")
    
    @classmethod
    @property
    def ARRAY_INTEGER(cls):
        return cls("array integer")
    
    @classmethod
    @property
    def ARRAY_SHORT(cls):
        return cls("array short")
    
    @classmethod
    @property
    def ARRAY_BYTE(cls):
        return cls("array byte")
    
    @classmethod
    @property
    def ARRAY_BOOLEAN(cls):
        return cls("array boolean")
    
    @classmethod
    @property
    def ARRAY_KEYWORD(cls):
        return cls("array keyword")
