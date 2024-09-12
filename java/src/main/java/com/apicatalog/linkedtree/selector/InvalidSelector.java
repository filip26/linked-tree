package com.apicatalog.linkedtree.selector;

public class InvalidSelector extends Exception {

    private static final long serialVersionUID = -3090034676289710498L;
    
    protected String term;

    public InvalidSelector() {
        this(null);
    }

    public InvalidSelector(String term) {
        super();
        this.term = term;
    }
    
    public InvalidSelector(Throwable e, String term) {
        super(e);
        this.term = term;
    }

    public String term() {
        return term;
    }
    

}
