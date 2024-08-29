package com.apicatalog.linkedtree.adapter;

import java.util.Collection;

import com.apicatalog.linkedtree.selector.StringValueSelector;

public interface FragmentAdapterResolver {

    LinkedFragmentAdapter adapter(
            //FragmentAdapterSelector selector
            String id, Collection<String> types, StringValueSelector stringSelector 
            );

}
