package com.apicatalog.linkedtree.jsonld.io;

import java.io.StringReader;
import java.util.Collection;
import java.util.Collections;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.json.JsonDecimal;
import com.apicatalog.linkedtree.json.JsonInteger;
import com.apicatalog.linkedtree.json.JsonLiteral;
import com.apicatalog.linkedtree.json.JsonScalar;
import com.apicatalog.linkedtree.json.pi.JsonObjectWrite;
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.lang.LangString;
import com.apicatalog.linkedtree.literal.NumericValue;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
import com.apicatalog.linkedtree.rdf.RdfVocab;
import com.apicatalog.linkedtree.xsd.XsdVocab;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;
import jakarta.json.stream.JsonParser;

public class JsonLdTreeWriter {

    public static JsonValue node(LinkedNode node) {
        return (new JsonLdTreeWriter()).writeNode(node, Collections.emptyList());
    }

    public static JsonObject fragment(LinkedFragment fragment) {
        return (new JsonLdTreeWriter()).writeFragment(fragment);
    }

    public JsonArray write(LinkedTree tree) {
        if (tree.id() != null
                || !tree.type().isEmpty()
                || !tree.asTree().terms().isEmpty()) {
            return Json.createArrayBuilder()
                    .add(writeTree(tree)).build();
        }
        return writeTreeNodes(tree);
    }

    public JsonObject writeFragment(LinkedFragment fragment) {
        return writeFragment(
                fragment,
                Collections.emptyList(),
                Json.createObjectBuilder()).build();
    }

    JsonArray writeTreeNodes(LinkedTree tree) {

        final JsonArrayBuilder builder = Json.createArrayBuilder();

        int processingOrder = 1;

        for (final LinkedNode fragment : tree) {
            builder.add(writeNode(fragment, tree.pi(processingOrder++)));
        }

        return builder.build();
    }

    JsonObject writeTree(LinkedTree tree) {

        final JsonObjectBuilder builder = Json.createObjectBuilder()
                .add(JsonLdKeyword.GRAPH, writeTreeNodes(tree));

        writeFragment(tree, tree.pi(0), builder);

        return builder.build();
    }

    JsonObjectBuilder writeFragment(final LinkedFragment fragment, final Collection<ProcessingInstruction> ops, JsonObjectBuilder builder) {

        if (fragment.id() != null) {
            builder.add("@id", fragment.id().uri().toString());
        }

        if (fragment.type() != null && !fragment.type().isEmpty()) {
            builder.add("@type", Json.createArrayBuilder(fragment.type().stream().toList()));
        }

        ops.stream()
                .filter(JsonObjectWrite.class::isInstance)
                .map(JsonObjectWrite.class::cast)
                .forEach(pi -> pi.write(builder));

        for (final String term : fragment.terms()) {
            builder.add(term, writeValues(fragment.container(term)));
        }

        return builder;
    }

    JsonValue writeContainer(final LinkedContainer container) {

        JsonArrayBuilder array = Json.createArrayBuilder();

        int processingOrder = 1;

        for (final LinkedNode node : container) {
            array.add(writeNode(node, container.pi(processingOrder++)));
        }

        if (LinkedContainer.ContainerType.OrderedList.equals(container.containerType())) {
//            array = Json.createArrayBuilder()
//                    .add(
            return Json.createObjectBuilder()
                    .add(JsonLdKeyword.LIST, array).build();
            // );
        }
        if (container.isTree()) {
            return writeTree(container.asTree());
        }

        return array.build();

    }

    JsonValue writeValues(final LinkedContainer container) {

        JsonArrayBuilder array = Json.createArrayBuilder();

        int processingOrder = 1;

        for (final LinkedNode node : container) {
            array.add(writeNode(node, container.pi(processingOrder++)));
        }

        if (LinkedContainer.ContainerType.OrderedList.equals(container.containerType())) {
            array = Json.createArrayBuilder()
                    .add(Json.createObjectBuilder()
                            .add(JsonLdKeyword.LIST, array));
        }
        if (container.isTree()) {
            array = Json.createArrayBuilder()
                    .add(writeTree(container.asTree()));
        }

        return array.build();

    }

    JsonValue writeNode(LinkedNode data, Collection<ProcessingInstruction> ops) {

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
            return writeFragment(data.asFragment(), ops, Json.createObjectBuilder()).build();
        }
        if (data.isLiteral()) {
            return writeLiteral(data.asLiteral(), ops);
        }

        throw new IllegalStateException();
    }

    JsonValue writeLiteral(LinkedLiteral literal, Collection<ProcessingInstruction> ops) {

        final JsonObjectBuilder result = Json.createObjectBuilder();

        JsonValue convertedValue = null;

        String type = null;

        if (literal.datatype() != null) {

            // 2.4.1.
            if (XsdVocab.STRING.equals(literal.datatype())) {
                convertedValue = Json.createValue(literal.lexicalValue());

            } else if (XsdVocab.BOOLEAN.equals(literal.datatype())) {

                if ("true".equalsIgnoreCase(literal.lexicalValue())) {

                    convertedValue = JsonValue.TRUE;

                } else if ("false".equalsIgnoreCase(literal.lexicalValue())) {

                    convertedValue = JsonValue.FALSE;

                } else {

                    type = XsdVocab.BOOLEAN;
                }

            } else if (literal instanceof JsonScalar jsonScalar) {
                convertedValue = jsonScalar.jsonValue();
                type = literal.datatype();

            } else if (XsdVocab.INTEGER.equals(literal.datatype()) 
                    || XsdVocab.INT.equals(literal.datatype()) 
                    || XsdVocab.LONG.equals(literal.datatype())) {

                convertedValue = Json.createValue(Long.parseLong(literal.lexicalValue()));

            } else if (literal instanceof JsonInteger jsonInteger) {

                convertedValue = Json.createValue(jsonInteger.integerValue());
                type = literal.datatype();

            } else if (XsdVocab.DOUBLE.equals(literal.datatype()) || XsdVocab.FLOAT.equals(literal.datatype())) {

                convertedValue = Json.createValue(Double.parseDouble(literal.lexicalValue()));

            } else if (literal instanceof JsonDecimal jsonDecimal) {

                convertedValue = Json.createValue(jsonDecimal.doubleValue());
                type = jsonDecimal.datatype();
                
            } else if (literal instanceof NumericValue numericValue) {

                convertedValue = Json.createValue(numericValue.numberValue().doubleValue());
                type = numericValue.datatype();

            } else if (literal instanceof JsonLiteral jsonLiteral) {

                convertedValue = jsonLiteral.jsonValue();
                type = JsonLdKeyword.JSON;

            } else if (RdfVocab.JSON.equals(literal.datatype())) {
                try (JsonParser parser = Json.createParser(new StringReader(literal.lexicalValue()))) {

                    parser.next();

                    convertedValue = parser.getValue();
                }
                type = JsonLdKeyword.JSON;

            } else {
                type = literal.datatype();
            }
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

        if (literal instanceof LangString langString) {
            if (langString.language() != null) {
                result.add(JsonLdKeyword.LANGUAGE, Json.createValue(langString.language()));
            }
            if (langString.direction() != null) {
                result.add(JsonLdKeyword.DIRECTION, Json.createValue(langString.direction().name().toLowerCase()));
            }
        }

        result.add(JsonLdKeyword.VALUE, (convertedValue != null)
                ? convertedValue
                : Json.createValue(literal.lexicalValue()));

        if (type != null) {
            result.add(JsonLdKeyword.TYPE, Json.createValue(type));
        }

        ops.stream()
                .filter(JsonObjectWrite.class::isInstance)
                .map(JsonObjectWrite.class::cast)
                .forEach(pi -> pi.write(result));

        return result.build();
    }
}
