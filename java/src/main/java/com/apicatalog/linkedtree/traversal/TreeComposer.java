package com.apicatalog.linkedtree.traversal;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.builder.TreeBuilderError;

public abstract class TreeComposer {

    protected Map<NodePointer, LinkedNode> injectors;
    protected LinkedNode root;
    
    protected TreeComposer(LinkedNode root) {
        this.root = root;
        this.injectors = new HashMap<>();
    }

    public TreeComposer inject(NodePointer pointer, LinkedNode node) {
        injectors.put(pointer, node);
        return this;
    }
    
    public void compose() throws TreeBuilderError {
        traversal(root, new Object[0]);
    }
    
    protected abstract void begin(LinkedNode node, Object[] path);
    protected abstract void end(LinkedNode node, Object[] path);
    
    // TODO exclude injectors and pass reduced map
    protected void traversal(
            final LinkedNode source,
            final Object[] path) throws TreeBuilderError {

        begin(source, path);
        
        if (source.isContainer()) {
            int nodeOrder = 0;
            for (final LinkedNode node : source.asContainer()) {

                var newPath = new Object[path.length + 1];
                System.arraycopy(path, 0, newPath, 0, path.length);
                newPath[path.length] = nodeOrder++;

                traversal(
                        node,
                        newPath);
            }
        }
        if (source.isFragment()) {
            for (final String property : source.asFragment().terms()) {

                var newPath = new Object[path.length + 1];
                System.arraycopy(path, 0, newPath, 0, path.length);
                newPath[path.length] = property;

                traversal(
                        source.asFragment().container(property),
                        newPath);
            }

            for (final Map.Entry<NodePointer, LinkedNode> injector : injectors.entrySet()) {
                if (injector.getKey().match(path)) {

                    var newPath = new Object[path.length + 1];
                    System.arraycopy(path, 0, newPath, 0, path.length);
                    newPath[path.length] = injector.getKey().term();

                    traversal(
                            injector.getValue(),
                            newPath);
                }
            }
        }
        end(source, path);
    }
}
