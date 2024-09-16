package com.apicatalog.linkedtree.type;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapter;

public interface TypeAdapter extends NodeAdapter<LinkedFragment, Object>  {

    Class<?> typeInterface();
}
