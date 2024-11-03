package com.apicatalog.linkedtree.jsonld;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeFalse;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.linkedtree.TestUtils;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeWriter;
import com.apicatalog.linkedtree.orm.mapper.TreeReaderMapping;
import com.apicatalog.linkedtree.test.Base64ByteArray;

import jakarta.json.JsonArray;

@DisplayName("JsonLd Test Suite")
@TestMethodOrder(OrderAnnotation.class)
class JsonLdReadWriteTest {

    static JsonLdTreeReader READER = JsonLdTreeReader.of(
            TreeReaderMapping.createBuilder()
                    .with(Base64ByteArray.typeAdapter())
                    .build());

    static JsonLdTreeWriter WRITER = new JsonLdTreeWriter();

    @DisplayName("Read/Write")
    @ParameterizedTest(name = "{0}")
    @MethodSource({ "expandedResources", "literalResources" })
    void readWrite(String name, JsonArray input) throws TreeBuilderError {

        // skip JsonNull
        assumeFalse(name.startsWith("0122"));

        var tree = READER.read(input);

        assertNotNull(tree);

        var output = WRITER.write(tree);

        assertNotNull(output);

        assertTrue(TestUtils.compareJson(name, tree, output, input));
    }

    static final Stream<Object[]> expandedResources() throws IOException, URISyntaxException {
        return TestUtils.resources("jsonld/expansion", "-out.jsonld");
    }

    static final Stream<Object[]> literalResources() throws IOException, URISyntaxException {
        return TestUtils.resources("jsonld/custom", ".jsonld");
    }
}
