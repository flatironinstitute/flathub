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


class Sort(
    ListSchema
):
    """NOTE: This class is auto generated by OpenAPI Generator.
    Ref: https://openapi-generator.tech

    Do not edit the class manually.
    """
    
    
    class _items(
        _SchemaValidator(
            unique_items=True,
        ),
        ComposedSchema
    ):
    
        @classmethod
        @property
        @functools.cache
        def _composed_schemas(cls):
            # we need this here to make our import statements work
            # we must store _composed_schemas in here so the code is only run
            # when we invoke this method. If we kept this at the class
            # level we would get an error because the class level
            # code would be run when this module is imported, and these composed
            # classes don't exist yet because their module has not finished
            # loading
            oneOf_0 = StrSchema
            
            
            class oneOf_1(
                DictSchema
            ):
                _required_property_names = set((
                ))
                field = StrSchema
                
                
                class order(
                    _SchemaValidator(
                        regex=[{
                            'pattern': r'^[ad]*',  # noqa: E501
                        }],
                    ),
                    _SchemaEnumMaker(
                        enum_value_to_name={
                            "asc": "ASC",
                            "desc": "DESC",
                        }
                    ),
                    StrSchema
                ):
                    
                    @classmethod
                    @property
                    def ASC(cls):
                        return cls("asc")
                    
                    @classmethod
                    @property
                    def DESC(cls):
                        return cls("desc")
                _additional_properties = None
            
            
                def __new__(
                    cls,
                    *args: typing.Union[dict, frozendict, ],
                    order: typing.Union[order, Unset] = unset,
                    _configuration: typing.Optional[Configuration] = None,
                ) -> 'oneOf_1':
                    return super().__new__(
                        cls,
                        *args,
                        order=order,
                        _configuration=_configuration,
                    )
            return {
                'allOf': [
                ],
                'oneOf': [
                    oneOf_0,
                    oneOf_1,
                ],
                'anyOf': [
                ],
                'not':
                    None
            }
    
        def __new__(
            cls,
            *args: typing.Union[dict, frozendict, str, date, datetime, int, float, decimal.Decimal, None, list, tuple, bytes],
            _configuration: typing.Optional[Configuration] = None,
            **kwargs: typing.Type[Schema],
        ) -> '_items':
            return super().__new__(
                cls,
                *args,
                _configuration=_configuration,
                **kwargs,
            )
