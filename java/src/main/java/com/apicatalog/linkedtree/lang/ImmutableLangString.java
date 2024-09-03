package com.apicatalog.linkedtree.lang;

import java.util.function.Supplier;

import com.apicatalog.linkedtree.LinkedTree;

public record ImmutableLangString(
        String lexicalValue,
        String language,
        LanguageDirectionType direction,
        Supplier<LinkedTree> rootSupplier
        ) implements LangString {
    
    @Override
    public LinkedTree root() {
        return rootSupplier != null 
                ? rootSupplier.get()
                : null;
    }
}
