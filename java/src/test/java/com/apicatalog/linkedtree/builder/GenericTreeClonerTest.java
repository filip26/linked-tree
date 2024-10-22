package com.apicatalog.linkedtree.builder;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.TestUtils;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeWriter;
import com.apicatalog.linkedtree.traversal.NodeSelector.TraversalPolicy;

import jakarta.json.JsonArray;

@TestMethodOrder(OrderAnnotation.class)
class GenericTreeClonerTest {

    static JsonLdTreeReader READER = JsonLdTreeReader.generic();

    static JsonLdTreeWriter WRITER = new JsonLdTreeWriter();

    @Test
    void genericClone() throws IOException, URISyntaxException, TreeBuilderError {

        JsonArray input = TestUtils.resource("jsonld/custom/signed-vc-1.jsonld");

        LinkedTree tree = READER.read(
                List.of("https://www.w3.org/2018/credentials/v1",
                        "https://w3id.org/security/data-integrity/v2"),
                input);

        assertNotNull(tree);

        GenericTreeCloner builder = new GenericTreeCloner(tree);
        var clone = builder.deepClone(
                (node, indexOrder, indexTerm, depth) -> "https://w3id.org/security#proof".equals(indexTerm)
                        ? TraversalPolicy.Drop
                        : TraversalPolicy.Accept);

        assertNotNull(clone);

        JsonArray out = WRITER.write(clone);

        JsonArray expected = TestUtils.resource("jsonld/custom/unsigned-vc-1.jsonld");

        assertTrue(TestUtils.compareJson(null, out, expected));
    }
}
