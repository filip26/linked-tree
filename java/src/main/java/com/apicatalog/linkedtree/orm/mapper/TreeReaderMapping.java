package com.apicatalog.linkedtree.orm.mapper;

import java.util.Map;

import com.apicatalog.linkedtree.literal.adapter.LiteralAdapter;
import com.apicatalog.linkedtree.type.TypeAdapter;

public record TreeReaderMapping(
        Map<Class<?>, TypeAdapter> typeAdapters,
        Map<String, TypeAdapter> fragmentAdapters,
        Map<String, LiteralAdapter> literalAdapters) {

    public static TreeReaderMappingBuilder createBuilder() {
        return new TreeReaderMappingBuilder().defaults();
    }

    public static TreeReaderMappingBuilder createGenericBuilder() {
        return new TreeReaderMappingBuilder();
    }
}
