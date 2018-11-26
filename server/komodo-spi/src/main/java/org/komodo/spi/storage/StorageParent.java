/*
 * Copyright Red Hat, Inc. and/or its affiliates
 * and other contributors as indicated by the @author tags and
 * the COPYRIGHT.txt file distributed with this work.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.spi.storage;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.komodo.spi.constants.StringConstants;

/**
 *
 * @param <T>
 */
public abstract class StorageParent<T> implements StringConstants {

    private final StorageParent<T> parent;

    private List<StorageNode<T>> children;

    /**
     * @param parent
     */
    public StorageParent(StorageParent<T> parent) {
        this.parent = parent;
    }

    /**
     * @return parent
     */
    public StorageParent<T> getParent() {
        return parent;
    }

    /**
     * @return true if this has children, false otherwise
     */
    public boolean hasChildren() {
        return children != null && ! children.isEmpty();
    }

    /**
     * @return children
     */
    public List<StorageNode<T>> getChildren() {
        if (children == null)
            return Collections.emptyList();

        return Collections.unmodifiableList(children);
    }

    /**
     * @param data
     * @return new storage node added as child with the given data
     */
    public StorageNode<T> addChild(T data) {
        if (children == null)
            children = new ArrayList<>();

        StorageNode<T> child = new StorageNode<T>(this, data);
        children.add(child);
        return child;
    }

    public abstract String getPath();

    public String printTree() {
        StringBuffer buf = new StringBuffer();

        buf.append(toString()).append(NEW_LINE);
        for (StorageNode<T> child : getChildren())
            buf.append(child.printTree());
        
        return buf.toString();
    }

    @Override
    public String toString() {
        return getPath();
    }
}
