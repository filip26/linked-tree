package com.apicatalog.linkedtree.json;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Locale;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href=
 *      "https://tools.ietf.org/html/draft-rundgren-json-canonicalization-scheme-17">JSON
 *      Canonicalization Scheme (JCS)</a>
 *
 */
public final class JsonCanonicalizer {

    private static final DecimalFormat eFormatBigDecimal = new DecimalFormat("0E00", new DecimalFormatSymbols(Locale.ENGLISH));

    private static final DecimalFormat eFormat = new DecimalFormat("0.#######", new DecimalFormatSymbols(Locale.ENGLISH));

    private JsonCanonicalizer() {
    }

    public static final String canonicalize(final JsonValue value) {

        final StringWriter writer = new StringWriter();

        try {

            canonicalize(value, writer);

        } catch (IOException e) {
            // ignore
        }

        return writer.toString();
    }

    private static final void canonicalize(final JsonValue value, final Writer writer) throws IOException {

        if (JsonUtils.isNull(value)) {
            writer.write("null");

        } else if (JsonUtils.isScalar(value)) {

            if (JsonUtils.isNumber(value)) {

                canonicalizeNumber((JsonNumber) value, writer);

            } else {

                writer.write(value.toString());
            }

        } else if (JsonUtils.isArray(value)) {

            canonicalizeArray(value.asJsonArray(), writer);

        } else if (JsonUtils.isObject(value)) {

            canonicalizeObject(value.asJsonObject(), writer);

        }
    }

    private static final void canonicalizeNumber(final JsonNumber number, final Writer writer) throws IOException {

        String numberString;

        if (number.bigDecimalValue().compareTo(BigDecimal.ZERO) == 0) {

            numberString = "0";

        } else if (number.bigDecimalValue().compareTo(BigDecimal.ONE.movePointRight(21)) >= 0) {

            numberString = eFormatBigDecimal.format(number.bigDecimalValue()).replace("E", "e+");

        } else if (number.bigDecimalValue().compareTo(BigDecimal.ONE.movePointLeft(21)) <= 0) {

            numberString = eFormatBigDecimal.format(number.bigDecimalValue()).toLowerCase();

        } else {

            numberString = eFormat.format(number.bigDecimalValue());
        }

        writer.write(numberString);
    }

    private static final void canonicalizeArray(final JsonArray value, final Writer writer) throws IOException {
        boolean next = false;

        writer.write("[");

        for (JsonValue item : value.asJsonArray()) {

            if (next) {
                writer.write(",");
            }

            canonicalize(item, writer);

            next = true;
        }

        writer.write("]");

    }

    private static final void canonicalizeObject(final JsonObject value, final Writer writer) throws IOException {
        boolean next = false;

        writer.write("{");

        for (String propertyName : index(value.keySet())) {

            if (next) {
                writer.write(",");
            }

            writer.write("\"");
            writer.write(escape(propertyName));
            writer.write("\":");

            JsonValue propertyValue = value.get(propertyName);

            canonicalize(propertyValue, writer);

            next = true;
        }

        writer.write("}");
    }

    static final String escape(String value) {

        final StringBuilder escaped = new StringBuilder();

        int[] codePoints = value.codePoints().toArray();

        for (int ch : codePoints) {

            if (ch == 0x9) {
                escaped.append("\\t");

            } else if (ch == 0x8) {
                escaped.append("\\b");

            } else if (ch == 0xa) {
                escaped.append("\\n");

            } else if (ch == 0xd) {
                escaped.append("\\r");

            } else if (ch == 0xc) {
                escaped.append("\\f");

            } else if (ch == '"') {
                escaped.append("\\\"");

            } else if (ch == '\\') {
                escaped.append("\\\\");

            } else if (ch >= 0x0 && ch <= 0x1f || ch == 0x7f) {
                escaped.append(String.format("\\u%04x", ch));

            } else {
                escaped.appendCodePoint(ch);
            }
        }
        return escaped.toString();
    }

    static final Collection<String> index(final Collection<String> keys) {

        if (keys == null || keys.isEmpty()) {
            return Collections.emptyList();
        }

        final ArrayList<String> sorted = new ArrayList<>(keys);
        Collections.sort(sorted);
        return sorted;
    }
}
