package com.apicatalog.linkedtree.jakarta;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeFalse;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.jsonld.JsonTreeReader;
import com.apicatalog.linkedtree.jsonld.JsonTreeWriter;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

@DisplayName("Jakarta Test Suite")
@TestMethodOrder(OrderAnnotation.class)
class JakartaTest {

    static JsonTreeReader READER = new JsonTreeReader();
    static JsonTreeWriter WRITER = new JsonTreeWriter();
    
    @DisplayName("Read/Write")
    @ParameterizedTest(name = "{0}")
    @MethodSource({ "expandedResources" })
    void readWrite(String name, JsonArray input) {
    
        // skip rdf types
        assumeFalse(name.startsWith("0031") || name.startsWith("0061"));
        
        // @included is not supported yet
        assumeFalse(name.startsWith("in0"));
        
        var tree = READER.readExpanded(input);
        
        assertNotNull(tree);

        var output = WRITER.write(tree);
        
        assertNotNull(output);

        assertTrue(compareJson(name, output, input));
    }

    static final Stream<Object[]> expandedResources() throws IOException, URISyntaxException {
        return Files.walk(Paths.get(LinkedFragment.class.getResource("").toURI()), 1)
                .filter(name -> name.toString().endsWith("out.jsonld"))
                .sorted()
                .map(path -> {
                    try (var reader = Json.createReader(LinkedFragment.class.getResourceAsStream(path.getFileName().toString()))) {
                        return new Object[] { path.getFileName().toString(), reader.readArray() };
                    }
                });
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
