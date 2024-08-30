package com.apicatalog.linkedtree.jsonld;

import java.io.PrintWriter;
import java.util.Collection;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.link.Link;

public class NodeDebugWriter {

    protected PrintWriter writer;
    protected Integer level;
    protected boolean nl;

    public NodeDebugWriter(PrintWriter writer) {
        this.writer = writer;
        this.level = 0;
        this.nl = false;
    }

    void indent() {
        if (nl) {
            for (int i = 0; i < level; i++) {
                writer.print("  ");
            }
            nl = false;
        }
    }

    NodeDebugWriter print(String string) {
        indent();
        writer.print(string);
        return this;
    }

    NodeDebugWriter print(int integer) {
        indent();
        writer.print(integer);
        return this;
    }

    NodeDebugWriter println(String string) {
        indent();
        writer.println(string);
        nl = true;
        return this;
    }

    NodeDebugWriter println(Link link) {
        indent();
        writer.println(link);
        nl = true;
        return this;
    }

    NodeDebugWriter println(Collection<String> strings) {
        indent();
        writer.println(strings);
        nl = true;
        return this;
    }

    public void print(LinkedNode node) {

        print("class: ")
                .println(node.getClass().getSimpleName());
//        level++;

        if (node.isTree()) {
            print("container: ")
                    .println(node.asContainer().containerType().toString());

            printFragment(node.asFragment());
            printContainer(node.asContainer());

        } else if (node.isContainer()) {
            print("container: ")
                    .println(node.asContainer().containerType().toString());

            printContainer(node.asContainer());

        } else if (node.isFragment()) {
            printFragment(node.asFragment());

        } else if (node.isLiteral()) {
            printLiteral(node.asLiteral());
        }
    }

    void printContainer(LinkedContainer container) {
//        if (container.size() == 1) {
//            print(container.single());
//            return;
//        }
        container.nodes().forEach(node -> {
            print("- ");
            level++;
            print(node);
            level--;
        });
    }

    void printFragment(LinkedFragment fragment) {

        print("id: ")
                .println(fragment.id());
        print("type: ")
                .println(fragment.type());

        for (String term : fragment.terms()) {
            println(term + ": ");
            level++;
            print(fragment.property(term));
            level--;
        }
    }

    void printLiteral(LinkedLiteral literal) {
        print("type: ").println(literal.datatype());
        print("value: ").println(literal.lexicalValue());
    }

}
