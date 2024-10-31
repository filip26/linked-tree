package com.apicatalog.linkedtree.test;

import java.net.URI;

import com.apicatalog.linkedtree.LinkedNode;

public record UnknownStatus(
        LinkedNode source
        ) implements Status {

}
