package com.apicatalog.linkedtree.writer;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collection;
import java.util.Collections;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.lang.LangString;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public class DebugNodeWriter {

    protected PrintWriter writer;
    protected Integer level;
    protected boolean nl;

    public DebugNodeWriter(PrintWriter writer) {
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

    DebugNodeWriter print(String string) {
        indent();
        writer.print(string);
        return this;
    }

    DebugNodeWriter print(int integer) {
        indent();
        writer.print(integer);
        return this;
    }

    DebugNodeWriter println(String string) {
        indent();
        writer.println(string);
        nl = true;
        return this;
    }

    DebugNodeWriter println(Link link) {
        indent();
        writer.println(link);
        nl = true;
        return this;
    }

    DebugNodeWriter println(Collection<String> strings) {
        indent();
        writer.println(strings);
        nl = true;
        return this;
    }

    public void print(LinkedNode node) {
        print(node, Collections.emptyList());
    }

    public void print(LinkedNode node, Collection<ProcessingInstruction> ops) {

//        level++;

        if (node.isTree()) {
//            print("class: ")
//                    .println(node.asTree().cast().getClass().getSimpleName());
            print("container: ")
                    .println(node.asContainer().containerType().toString());

            printFragment(node.asFragment(), node.asContainer().pi(0));
            printContainer(node.asContainer(), ops);

        } else if (node.isContainer()) {
            print("class: ")
                    .println(node.getClass().getSimpleName());
            print("container: ")
                    .println(node.asContainer().containerType().toString());

            printContainer(node.asContainer(), ops);

        } else if (node.isFragment()) {
//            print("class: ")
//                    .println(node.asFragment().cast().getClass().getSimpleName());

            printFragment(node.asFragment(), ops);

        } else if (node.isLiteral()) {
            print("class: ")
                    .println(node.asLiteral().cast().getClass().getSimpleName());
            printLiteral(node.asLiteral(), ops);
        }
    }

    void printContainer(LinkedContainer container, Collection<ProcessingInstruction> ops) {
//        if (container.size() == 1) {
//            print(container.single());
//            return;
//        }
        int order = 1;

        for (LinkedNode node : container) {
            print("- ");
            level++;
            print(node, container.pi(order++));
            level--;
        }
    }

    void printFragment(LinkedFragment fragment, Collection<ProcessingInstruction> ops) {

        if (fragment.id() != null) {
            print("id: ")
                    .println(fragment.id());
        }
        if (fragment.type() != null && !fragment.type().isEmpty()) {
            print("type: ")
                    .println(fragment.type().stream().toList());
        }
        if (ops != null && !ops.isEmpty()) {
            print("pi: ")
                    .println(ops.toString());
        }
        for (String term : fragment.terms()) {
            println(term + ": ");
            level++;
            print(fragment.container(term));
            level--;
        }

    }

    void printLiteral(LinkedLiteral literal, Collection<ProcessingInstruction> ops) {

        print("datatype: ").println(literal.datatype());
        print("value: ").println(literal.lexicalValue());
        if (literal instanceof LangString langString) {
            if (langString.language() != null) {
                print("language: ").println(langString.language());
            }
            if (langString.direction() != null) {
                print("direction: ").println(langString.direction().toString());
            }
        }
        if (ops != null && !ops.isEmpty()) {
            print("pi: ")
                    .println(ops.toString());
        }

    }

    public static void writeToStdOut(LinkedNode node) {
        var s = new StringWriter();
        new DebugNodeWriter(new PrintWriter(s)).print(node);
        System.out.println(s);
    }
    
}
