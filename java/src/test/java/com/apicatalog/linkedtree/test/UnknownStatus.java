package com.apicatalog.linkedtree.test;

import com.apicatalog.linkedtree.LinkedNode;

public record UnknownStatus(
        LinkedNode source
        ) implements Status {

}
