package com.apicatalog.linkedtree.type;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.Adapter;

public interface TypeAdapter extends Adapter<LinkedFragment, Object>  {

    Class<?> typeInterface();
}
