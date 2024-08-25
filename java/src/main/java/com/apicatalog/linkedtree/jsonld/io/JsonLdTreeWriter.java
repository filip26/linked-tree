package com.apicatalog.linkedtree.jsonld.io;

import java.io.StringReader;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.json.JsonDecimal;
import com.apicatalog.linkedtree.json.JsonInteger;
import com.apicatalog.linkedtree.json.JsonLiteral;
import com.apicatalog.linkedtree.json.JsonScalar;
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.lang.LangString;
import com.apicatalog.linkedtree.literal.NumericValue;
import com.apicatalog.linkedtree.rdf.RdfConstants;
import com.apicatalog.linkedtree.xsd.XsdConstants;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;
import jakarta.json.stream.JsonParser;

public class JsonLdTreeWriter {

    public JsonArray writeExpanded(LinkedTree tree) {

        final JsonArrayBuilder builder = Json.createArrayBuilder();

        for (final LinkedNode fragment : tree.nodes()) {
            builder.add(writeNode(fragment));
        }

        return builder.build();
    }

    JsonObject writeTree(LinkedTree tree) {

        final JsonObjectBuilder builder = Json.createObjectBuilder()
                .add(JsonLdKeyword.GRAPH, writeExpanded(tree));

        writeFragment(tree, builder);

        return builder.build();
    }

    JsonObjectBuilder writeFragment(final LinkedFragment fragment, JsonObjectBuilder builder) {

        if (fragment.id() != null) {
            builder.add("@id", fragment.id().uri().toString());
        }

        if (fragment.type() != null && !fragment.type().isEmpty()) {
            builder.add("@type", Json.createArrayBuilder(fragment.type()));
        }

        if (fragment.pi() != null
                && fragment.pi() instanceof JsonLdPi meta) {
            meta.write(builder);
        }

        for (final String term : fragment.terms()) {
            builder.add(term, writeContainer(fragment.property(term)));
        }

        return builder;
    }

    JsonValue writeContainer(final LinkedContainer container) {

        final JsonArrayBuilder array = Json.createArrayBuilder();

        for (final LinkedNode node : container.nodes()) {
            array.add(writeNode(node));
        }

        if (LinkedContainer.Type.OrderedList.equals(container.containerType())) {
            return Json.createObjectBuilder()
                    .add(JsonLdKeyword.LIST, array)
                    .build();

        }
        return array.build();

    }

    JsonValue writeNode(LinkedNode data) {

        if (data == null) {
            return JsonValue.NULL;
        }

        if (data.isTree()) {
            return writeTree(data.asTree());
        }
        if (data.isContainer()) {
            return writeContainer(data.asContainer());
        }
        if (data.isFragment()) {
            return writeFragment(data.asFragment(), Json.createObjectBuilder()).build();
        }
        if (data.isLiteral()) {
            return writeLiteral(data.asLiteral());
        }

        throw new IllegalStateException();
    }

    JsonValue writeLiteral(LinkedLiteral literal) {

        final JsonObjectBuilder result = Json.createObjectBuilder();

        JsonValue convertedValue = null;

        String type = null;

        if (literal.datatype() != null) {

            // 2.4.1.
            if (XsdConstants.STRING.equals(literal.datatype())) {
                convertedValue = Json.createValue(literal.lexicalValue());

            } else if (literal instanceof JsonScalar jsonScalar) {
                convertedValue = jsonScalar.jsonValue();

            } else if (XsdConstants.BOOLEAN.equals(literal.datatype())) {

                if ("true".equalsIgnoreCase(literal.lexicalValue())) {

                    convertedValue = JsonValue.TRUE;

                } else if ("false".equalsIgnoreCase(literal.lexicalValue())) {

                    convertedValue = JsonValue.FALSE;

                } else {

                    type = XsdConstants.BOOLEAN;
                }

            } else if (literal instanceof JsonInteger jsonInteger) {

                convertedValue = Json.createValue(jsonInteger.integerValue());

            } else if (XsdConstants.INTEGER.equals(literal.datatype()) || XsdConstants.INT.equals(literal.datatype()) || XsdConstants.LONG.equals(literal.datatype())) {

                convertedValue = Json.createValue(Long.parseLong(literal.lexicalValue()));

            } else if (literal instanceof JsonDecimal jsonDecimal) {

                convertedValue = Json.createValue(jsonDecimal.doubleValue());

            } else if (XsdConstants.DOUBLE.equals(literal.datatype()) || XsdConstants.FLOAT.equals(literal.datatype())) {

                convertedValue = Json.createValue(Double.parseDouble(literal.lexicalValue()));

            } else if (literal instanceof NumericValue numericValue) {

                convertedValue = Json.createValue(numericValue.numberValue().doubleValue());

            } else if (literal instanceof JsonLiteral jsonLiteral) {

                convertedValue = jsonLiteral.jsonValue();
                type = JsonLdKeyword.JSON;

            } else if (RdfConstants.JSON.equals(literal.datatype())) {
                try (JsonParser parser = Json.createParser(new StringReader(literal.lexicalValue()))) {

                    parser.next();

                    convertedValue = parser.getValue();
                }
                type = JsonLdKeyword.JSON;
            }
        }

        if (type == null
                && !XsdConstants.STRING.equals(literal.datatype())
                && literal.datatype() != null) {
            type = literal.datatype();
        }

//            // 2.6.
////        } else if (RdfDirection.I18N_DATATYPE == rdfDirection
////                    && literal.datatype() != null
////                    && literal.datatype().startsWith(RdfConstants.I18N_BASE)
////                ) {
////
////            convertedValue = JsonProvider.instance().createValue(literal.getValue());
////
////            String langId = literal.datatype().substring(RdfConstants.I18N_BASE.length());
////
////            int directionIndex = langId.indexOf('_');
////
////            if (directionIndex > 1) {
////
////                result.add(Keywords.LANGUAGE, JsonProvider.instance().createValue(langId.substring(0, directionIndex)));
////                result.add(Keywords.DIRECTION, JsonProvider.instance().createValue(langId.substring(directionIndex + 1)));
////
////            } else if (directionIndex == 0) {
////
////                result.add(Keywords.DIRECTION, JsonProvider.instance().createValue(langId.substring(1)));
////
////            } else  if (directionIndex == -1) {
////
////                result.add(Keywords.LANGUAGE, JsonProvider.instance().createValue(langId));
////            }
////
//        } else if (literal.language() != null) {
//
//            result.add(Keywords.LANGUAGE, Json.createValue(literal.language()));
//
//        } else if (literal.datatype() != null
//                && !XsdConstants.STRING.equals(literal.datatype())) {
//
//            type = literal.datatype();
//        }

        if (literal instanceof LangString langString
                && langString.language() != null) {
            result.add(JsonLdKeyword.LANGUAGE, Json.createValue(langString.language()));
        }

        result.add(JsonLdKeyword.VALUE, (convertedValue != null)
                ? convertedValue
                : Json.createValue(literal.lexicalValue()));

        if (type != null) {
            result.add(JsonLdKeyword.TYPE, Json.createValue(type));
        }

        if (literal.pi() != null
                && literal.pi() instanceof JsonLdPi meta) {
            meta.write(result);
        }

        return result.build();
    }
}