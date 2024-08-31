package com.apicatalog.linkedtree.adapter.resolver;

import java.util.Collection;

import com.apicatalog.linkedtree.adapter.LinkedFragmentAdapter;
import com.apicatalog.linkedtree.selector.StringValueSelector;

public interface FragmentAdapterResolver {

    LinkedFragmentAdapter resolve(
            //FragmentAdapterSelector selector
            String id, Collection<String> types, StringValueSelector stringSelector 
            );

}
