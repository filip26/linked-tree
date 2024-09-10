package com.apicatalog.linkedtree.jsonld;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URISyntaxException;
import java.util.Collections;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTreeError;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;
import com.apicatalog.linkedtree.writer.NodeDebugWriter;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

@DisplayName("JsonLd @graph Tests")
@TestMethodOrder(OrderAnnotation.class)
class JsonLdGraphTest {

    static JsonLdTreeReader READER = JsonLdTreeReader
            .create()
            .build();

    @Test
    void level1Graph() throws IOException, URISyntaxException, LinkedTreeError {

        JsonArray input = resource("custom/signed-vc-1.jsonld");

        var tree = READER.read(input);
NodeDebugWriter.printToStdout(tree);
        assertEquals(1, tree.subtrees().size());
        assertEquals(0, tree.subtrees().iterator().next().subtrees().size());

        assertNull(tree.root());
        
        assertTrue(tree.nodes()
                .stream()
                .map(LinkedNode::root)
                .allMatch(tree::equals));

        var proof = tree.singleFragment()
                .property("https://w3id.org/security#proof")
//                .asContainer()
//                .single()
                .asTree();

        assertTrue(proof
                .nodes()
                .stream()
                .map(LinkedNode::root)
                .allMatch(proof::equals));
    }

    static final JsonArray resource(String name) throws IOException, URISyntaxException {
        try (var reader = Json.createReader(JsonLdKeyword.class.getResourceAsStream(name))) {
            return reader.readArray();
        }
    }

    static final boolean compareJson(final String testCase, final JsonStructure result, final JsonStructure expected) {

        if (JsonLdComparison.equals(expected, result)) {
            return true;
        }

        write(testCase, result, expected, null);

        fail("Expected " + expected + ", but was" + result);
        return false;
    }

    static void write(final String testCase, final JsonStructure result, final JsonStructure expected, Exception error) {
        final StringWriter stringWriter = new StringWriter();

        try (final PrintWriter writer = new PrintWriter(stringWriter)) {
            writer.println("Test " + testCase);

            final JsonWriterFactory writerFactory = Json.createWriterFactory(Collections.singletonMap(JsonGenerator.PRETTY_PRINTING, true));

            if (expected != null) {
                write(writer, writerFactory, "Expected", expected);
                writer.println();

//            } else if (testCase.expectErrorCode != null) {
//                writer.println("Expected: " + testCase.expectErrorCode);
            }

            if (result != null) {
                write(writer, writerFactory, "Actual", result);
                writer.println();
            }
            if (error != null) {
                writer.println("Actual: ");
                error.printStackTrace(writer);
            }
        }

        System.out.println(stringWriter.toString());
    }

    static final void write(final PrintWriter writer, final JsonWriterFactory writerFactory, final String name, final JsonValue result) {

        writer.println(name + ":");

        final StringWriter out = new StringWriter();

        try (final JsonWriter jsonWriter = writerFactory.createWriter(out)) {
            jsonWriter.write(result);
        }

        writer.write(out.toString());
        writer.println();
    }

}
