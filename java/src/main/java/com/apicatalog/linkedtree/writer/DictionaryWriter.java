package com.apicatalog.linkedtree.writer;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collection;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.lang.LangStringLiteral;
import com.apicatalog.linkedtree.link.Link;

public class DictionaryWriter {

    protected PrintWriter writer;
    protected Integer level;
    protected boolean nl;

    public DictionaryWriter(PrintWriter writer) {
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

    DictionaryWriter print(String string) {
        indent();
        writer.print(string);
        return this;
    }

    DictionaryWriter print(int integer) {
        indent();
        writer.print(integer);
        return this;
    }

    DictionaryWriter println(String string) {
        indent();
        writer.println(string);
        nl = true;
        return this;
    }

    DictionaryWriter println(Link link) {
        indent();
        writer.println(link);
        nl = true;
        return this;
    }

    DictionaryWriter println(Collection<String> strings) {
        indent();
        writer.println(strings);
        nl = true;
        return this;
    }

    public void print(LinkedNode node) {
        if (node.isTree()) {
            printFragment(node.asFragment());
            printContainer(node.asContainer());

        } else if (node.isContainer()) {
            printContainer(node.asContainer());

        } else if (node.isFragment()) {
            printFragment(node.asFragment());

        } else if (node.isLiteral()) {
            printLiteral(node.asLiteral());
        }
    }

    void printContainer(LinkedContainer container) {
        for (LinkedNode node : container) {
            print("- ");
            level++;
            print(node);
            level--;
        }
    }

    void printFragment(LinkedFragment fragment) {

        if (fragment.id() != null) {
            print("id: ")
                    .println(fragment.id());
        }
        if (fragment.type() != null && !fragment.type().isEmpty()) {
            print("type: ")
                    .println(fragment.type().stream().toList());
        }
        for (String term : fragment.terms()) {
            println(term + ": ");
            level++;
            print(fragment.container(term));
            level--;
        }

    }

    void printLiteral(LinkedLiteral literal) {

        print("datatype: ").println(literal.datatype());
        print("value: ").println(literal.lexicalValue());
        if (literal instanceof LangStringLiteral langString) {
            if (langString.language() != null) {
                print("language: ").println(langString.language());
            }
            if (langString.direction() != null) {
                print("direction: ").println(langString.direction().toString());
            }
        }
    }

    public static void writeToStdOut(LinkedNode node) {
        var s = new StringWriter();
        new DictionaryWriter(new PrintWriter(s)).print(node);
        System.out.println(s);
    }

}
