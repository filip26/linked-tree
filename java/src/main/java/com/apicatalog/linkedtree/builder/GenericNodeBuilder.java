package com.apicatalog.linkedtree.builder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
import com.apicatalog.linkedtree.primitive.GenericContainer;
import com.apicatalog.linkedtree.primitive.GenericFragment;
import com.apicatalog.linkedtree.primitive.GenericTree;
import com.apicatalog.linkedtree.traversal.DepthFirstSearch;
import com.apicatalog.linkedtree.traversal.NodeConsumer;
import com.apicatalog.linkedtree.traversal.NodeConsumer.IndexScope;

public class GenericNodeBuilder implements NodeConsumer {

    protected Map<String, Link> linkMap;
    
    protected final LinkedNode source;
    protected LinkedNode clone;
    
    public GenericNodeBuilder(LinkedNode source) {
        this.source = source;
//        this.linkMap = new HashMap<>(source.links().size());
        this.clone = null;
    }

    @Override
    public void accept(LinkedNode node, IndexScope indexType, int indexOrder, String indexTerm, int depth) {
        // TODO Auto-generated method stub
        
    }
    
    public LinkedNode deepClone() {
        DepthFirstSearch.postOrder(source, this);
        return clone;
    }
    
    LinkedNode deepClone(LinkedNode source, Supplier<LinkedTree> rootSupplier) {

        if (source.isTree()) {
            return deepClone(source.asTree(), rootSupplier);

        } else if (source.isContainer()) {
            return deepClone(source.asContainer(), rootSupplier);

        } else if (source.isFragment()) {
            return deepClone(source.asFragment(), rootSupplier);

        } else if (source.isLiteral()) {
            return deepClone(source.asLiteral(), rootSupplier);
        }
        throw new IllegalStateException();
    }

    LinkedLiteral deepClone(LinkedLiteral source) {
        return source;
    }

    LinkedFragment deepClone(LinkedFragment source, Supplier<LinkedTree> rootSupplier) {
        return new GenericFragment(
                source.id(),
                source.type(),
                properties(source),
                rootSupplier);
    }

    LinkedContainer deepClone(LinkedContainer source, Supplier<LinkedTree> rootSupplier) {

        final Collection<LinkedNode> nodes = new ArrayList<>(source.size());
        final Map<Integer, Collection<ProcessingInstruction>> ops = new HashMap<>();

        int processingOrder = 1;

        for (final LinkedNode node : source) {
            nodes.add(deepClone(node, rootSupplier));

            var pi = source.pi(processingOrder);
            if (pi != null) {
                ops.put(processingOrder, pi);
            }
            processingOrder++;
        }

        // borrow pi about the container itself
        ops.put(0, source.pi(0));

        return new GenericContainer(
                source.containerType(),
                nodes,
                rootSupplier,
                () -> ops);
    }

    LinkedTree deepClone(LinkedTree source, Supplier<LinkedTree> rootSupplier) {

        Collection<LinkedTree> subtrees = new ArrayList<>(source.subtrees().size());
        Map<Integer, Collection<ProcessingInstruction>> ops = new HashMap<>();
        
        Map<String, LinkedContainer> entries = new LinkedHashMap<>(source.terms().size());
        
        Collection<LinkedNode> nodes = new ArrayList<>(source.nodes().size());
        
        
        
        
        return new GenericTree(
                source.id(),
                source.type(),
                entries,
                nodes,
                linkMap,
                subtrees,
                rootSupplier,
                ops);
    }

    protected static final Map<String, LinkedContainer> properties(LinkedFragment fragment) {
        return fragment != null && fragment.terms().size() > 0
                ? fragment.terms().stream()
                        .collect(Collectors.toMap(
                                Function.identity(),
                                fragment::property))
                : Collections.emptyMap();
    }
}
