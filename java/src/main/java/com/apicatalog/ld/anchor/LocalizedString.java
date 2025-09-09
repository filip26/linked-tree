package com.apicatalog.ld.anchor;

public interface LocalizedString extends Literal {

    public enum LanguageDirection {
        LTR,
        RTL,
        NULL
    }

    @Override
    default java.lang.String datatype() {
        if (direction() != null) {
            return "http://www.w3.org/1999/02/22-rdf-syntax-ns#dirLangString";
        }
        return "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString";
    }
    
    /**
     * An language tag. Must not be null.
     *
     * @return language tag 
     */
    String language();

    /**
     * Optional.
     * @return
     */
    LanguageDirection direction();

    /**
     * Localized value
     * @return
     */
    String localizedValue();
}
