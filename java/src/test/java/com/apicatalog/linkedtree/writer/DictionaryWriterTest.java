package com.apicatalog.linkedtree.writer;

import static org.junit.jupiter.api.Assertions.assertNotNull;

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

import jakarta.json.JsonArray;

@TestMethodOrder(OrderAnnotation.class)
class DictionaryWriterTest {

    static JsonLdTreeReader READER = JsonLdTreeReader.generic();

    @DisplayName("Read/Write")
    @ParameterizedTest(name = "{0}")
    @MethodSource({ "resources" })
    void readWrite(String name, JsonArray input) throws TreeBuilderError {

        var tree = READER.read(input);
        assertNotNull(tree);

        DictionaryWriter.writeToStdOut(tree);

//        assertTrue(compareJson(name, tree, output, input));
    }

    static final Stream<Object[]> resources() throws IOException, URISyntaxException {
        return TestUtils.resources("ltd", ".jsonld");
    }
}
