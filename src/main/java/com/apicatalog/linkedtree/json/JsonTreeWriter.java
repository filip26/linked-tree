package com.apicatalog.linkedtree.json;

import java.io.StringReader;

import com.apicatalog.linkedtree.LinkedData;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;
import jakarta.json.stream.JsonParser;

public class JsonTreeWriter {

    public JsonArray write(LinkedTree tree) {

        final JsonArrayBuilder builder = Json.createArrayBuilder();

        for (final LinkedFragment fragment : tree.fragments()) {
            builder.add(writeFragment(fragment));
        }

        return builder.build();
    }

    public JsonObject writeFragment(final LinkedFragment fragment) {

        final JsonObjectBuilder builder = Json.createObjectBuilder();

        if (fragment.id() != null) {
            builder.add("@id", fragment.id().uri().toString());
        }

        if (fragment.type() != null) {
            builder.add("@type", Json.createArrayBuilder(fragment.type()));
        }

        for (final String term : fragment.terms()) {

            final JsonArrayBuilder termValues = Json.createArrayBuilder();

            for (final LinkedData value : fragment.values(term)) {
                termValues.add(writeData(value));
            }

            builder.add(term, termValues);
        }

        return builder.build();
    }

    public JsonValue writeData(LinkedData data) {

        if (data == null) {
            return JsonValue.NULL;
        }

        if (data.isFragment()) {
            return writeFragment(data.asFragment());
        }

        if (data.isLiteral()) {
            return writeLiteral(data.asLiteral());
        }

        if (data.isTree()) {
            return write(data.asTree());
        }

        throw new IllegalStateException();
    }

    public JsonValue writeLiteral(LinkedLiteral literal) {

//        final JsonObjectBuilder builder = Json.createObjectBuilder();
//
//        builder.add("@value", writeLiteralValue(value));
//        if (value.datatype() != null && !XsdConstants.STRING.equals(value.datatype())) {
//            builder.add("@type", value.datatype());
//        }        
//        if (value.language() != null) {
//            builder.add("@language", value.language());
//        }
//        return builder.build();
//    }
//    
//    protected static JsonValue writeLiteralValue(LinkedLiteral literal) {
//        
        final JsonObjectBuilder result = Json.createObjectBuilder();

        JsonValue convertedValue = null;

        String type = null;

        if (literal.datatype() != null) {

            // 2.4.1.
            if (XsdConstants.STRING.equals(literal.datatype())) {
                convertedValue = Json.createValue(literal.value());

                // 2.4.2.
            } else if (XsdConstants.BOOLEAN.equals(literal.datatype())) {

                if ("true".equalsIgnoreCase(literal.value())) {

                    convertedValue = JsonValue.TRUE;

                } else if ("false".equalsIgnoreCase(literal.value())) {

                    convertedValue = JsonValue.FALSE;

                } else {

                    type = XsdConstants.BOOLEAN;
                }

                // 2.4.3.
            } else if (XsdConstants.INTEGER.equals(literal.datatype()) || XsdConstants.INT.equals(literal.datatype()) || XsdConstants.LONG.equals(literal.datatype())) {

                convertedValue = Json.createValue(Long.parseLong(literal.value()));

            } else if (XsdConstants.DOUBLE.equals(literal.datatype()) || XsdConstants.FLOAT.equals(literal.datatype())) {

                convertedValue = Json.createValue(Double.parseDouble(literal.value()));

            } else if (literal.datatype() != null) {

                type = literal.datatype();
            }
        }

//        if (literal.datatype() != null
//                && RdfConstants.JSON.equals(literal.datatype())) {
//
//            try (JsonParser parser = Json.createParser(new StringReader(literal.value()))) {
//
//                parser.next();
//
//                convertedValue = parser.getValue();
//                type = "@json";
//
//            } catch (Exception e) {
////                throw new JsonLdError(JsonLdErrorCode.INVALID_JSON_LITERAL, e);
//            }
//
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

        if (literal.language() != null) {
            result.add(Keywords.LANGUAGE, Json.createValue(literal.language()));
        }

        result.add(Keywords.VALUE, (convertedValue != null)
                ? convertedValue
                : Json.createValue(literal.value()));

        if (type != null) {
            result.add(Keywords.TYPE, Json.createValue(type));
        }

        return result.build();
    }
}
