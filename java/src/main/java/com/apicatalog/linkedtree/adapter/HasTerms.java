package com.apicatalog.linkedtree.adapter;

import com.apicatalog.linkedtree.LinkedFragment;

public class HasTerms implements NodeAdapter<LinkedFragment, Boolean> {

    @Override
    public Boolean materialize(LinkedFragment source) throws NodeAdapterError {
        return !source.terms().isEmpty();
    }

}
