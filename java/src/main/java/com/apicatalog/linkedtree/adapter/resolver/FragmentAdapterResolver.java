package com.apicatalog.linkedtree.adapter.resolver;

import java.util.Collection;

import com.apicatalog.linkedtree.adapter.LinkedFragmentAdapter;

@Deprecated
public interface FragmentAdapterResolver {

    LinkedFragmentAdapter resolve(
            //FragmentAdapterSelector selector
            String id, Collection<String> types
            );

}
