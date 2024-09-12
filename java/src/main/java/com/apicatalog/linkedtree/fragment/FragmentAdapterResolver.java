package com.apicatalog.linkedtree.fragment;

import java.util.Collection;

@Deprecated
public interface FragmentAdapterResolver {

    LinkedFragmentAdapter resolve(
            //FragmentAdapterSelector selector
            String id, Collection<String> types
            );

}
