package com.apicatalog.linkedtree.jsonld;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URISyntaxException;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import com.apicatalog.linkedtree.TestUtils;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;

import jakarta.json.JsonArray;

@DisplayName("JsonLd Links")
@TestMethodOrder(OrderAnnotation.class)
class JsonLdLinkTest {

    static JsonLdTreeReader READER = JsonLdTreeReader.generic();

    @Test
    void singleRootLink() throws IOException, URISyntaxException, TreeBuilderError {

        JsonArray input = TestUtils.resource("jsonld/custom/base64-1.jsonld");

        var tree = READER.read(input);

        assertEquals(1, tree.links().size());

        for (var link : tree.links()) {
            assertEquals(1, link.refs().size());
            assertTrue(link.target().isFragment());
        }
    }

    @Test
    void signedVcLinks() throws IOException, URISyntaxException, TreeBuilderError {

        JsonArray input = TestUtils.resource("jsonld/custom/signed-vc-1.jsonld");

        var tree = READER.read(input);

        assertEquals(7, tree.links().size());

        for (var link : tree.links()) {
            assertEquals(1, link.refs().size());
            assertTrue(link.target().isFragment());
        }
    }

}
