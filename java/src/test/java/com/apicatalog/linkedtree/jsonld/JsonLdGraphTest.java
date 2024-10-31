package com.apicatalog.linkedtree.jsonld;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URISyntaxException;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.TestUtils;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;

import jakarta.json.JsonArray;

@DisplayName("JsonLd @graph Tests")
@TestMethodOrder(OrderAnnotation.class)
class JsonLdGraphTest {

    static JsonLdTreeReader READER = JsonLdTreeReader.generic();

    @Test
    void level1Graph() throws IOException, URISyntaxException, TreeBuilderError {

        JsonArray input = TestUtils.resource("jsonld/custom/signed-vc-1.jsonld");

        var tree = READER.read(input);

        assertEquals(1, tree.subtrees().size());
        assertEquals(0, tree.subtrees().iterator().next().subtrees().size());

        assertNull(tree.root());
        
        assertTrue(tree.nodes()
                .stream()
                .map(LinkedNode::asFragment)
                .map(LinkedFragment::root)
                .allMatch(tree::equals));

        var proof = tree.fragment()
                .container("https://w3id.org/security#proof")
//                .asContainer()
//                .single()
                .asTree();

        assertTrue(proof.nodes()
                .stream()
                .map(LinkedNode::asFragment)
                .map(LinkedFragment::root)
                .allMatch(proof::equals));
    }
}
