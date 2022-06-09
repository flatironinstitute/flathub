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


class FieldGroup(
    DictSchema
):
    """NOTE: This class is auto generated by OpenAPI Generator.
    Ref: https://openapi-generator.tech

    Do not edit the class manually.

    A single field within a catalog, or a hiearchical group of fields
    """
    _required_property_names = set((
        'key',
        'name',
        'title',
        'type',
        'base',
    ))

    @classmethod
    @property
    def stats(cls) -> typing.Type['FieldStats']:
        return FieldStats
    attachment = BoolSchema
    key = StrSchema
    name = StrSchema
    store = BoolSchema
    _dict = StrSchema
    locals()['dict'] = _dict
    del locals()['_dict']
    wildcard = BoolSchema
    units = StrSchema
    title = StrSchema

    @classmethod
    @property
    def type(cls) -> typing.Type['Type']:
        return Type
    
    
    class enum(
        ListSchema
    ):
        _items = StrSchema
    disp = BoolSchema
    
    
    class sub(
        ListSchema
    ):
    
        @classmethod
        @property
        def _items(cls) -> typing.Type['FieldGroup']:
            return FieldGroup
    reversed = BoolSchema
    
    
    class base(
        _SchemaEnumMaker(
            enum_value_to_name={
                "f": "F",
                "i": "I",
                "b": "B",
                "s": "S",
                "v": "V",
            }
        ),
        StrSchema
    ):
        
        @classmethod
        @property
        def F(cls):
            return cls("f")
        
        @classmethod
        @property
        def I(cls):
            return cls("i")
        
        @classmethod
        @property
        def B(cls):
            return cls("b")
        
        @classmethod
        @property
        def S(cls):
            return cls("s")
        
        @classmethod
        @property
        def V(cls):
            return cls("v")
    terms = BoolSchema
    scale = NumberSchema
    required = BoolSchema
    descr = StrSchema
    _additional_properties = None


    def __new__(
        cls,
        *args: typing.Union[dict, frozendict, ],
        key: key,
        name: name,
        title: title,
        type: type,
        base: base,
        stats: typing.Union['FieldStats', Unset] = unset,
        attachment: typing.Union[attachment, Unset] = unset,
        store: typing.Union[store, Unset] = unset,
        wildcard: typing.Union[wildcard, Unset] = unset,
        units: typing.Union[units, Unset] = unset,
        enum: typing.Union[enum, Unset] = unset,
        disp: typing.Union[disp, Unset] = unset,
        sub: typing.Union[sub, Unset] = unset,
        reversed: typing.Union[reversed, Unset] = unset,
        terms: typing.Union[terms, Unset] = unset,
        scale: typing.Union[scale, Unset] = unset,
        required: typing.Union[required, Unset] = unset,
        descr: typing.Union[descr, Unset] = unset,
        _configuration: typing.Optional[Configuration] = None,
    ) -> 'FieldGroup':
        return super().__new__(
            cls,
            *args,
            key=key,
            name=name,
            title=title,
            type=type,
            base=base,
            stats=stats,
            attachment=attachment,
            store=store,
            wildcard=wildcard,
            units=units,
            enum=enum,
            disp=disp,
            sub=sub,
            reversed=reversed,
            terms=terms,
            scale=scale,
            required=required,
            descr=descr,
            _configuration=_configuration,
        )

from openapi_client.model.field_stats import FieldStats
from openapi_client.model.type import Type
