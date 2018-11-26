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
package org.komodo.spi.query;

/**
     * Enumerator of types of operation
     */
public enum Operation {
    /** Represents UNION of two queries */
    UNION,
    /** Represents intersection of two queries */
    INTERSECT,
    /** Represents set difference of two queries */
    EXCEPT;

    /**
     * @param name
     * @return Operation with given name
     */
    public static Operation findOperation(String name) {
        if (name == null)
            return null;

        name = name.toUpperCase();
        for (Operation op : values()) {
            if (op.name().equals(name))
                return op;
        }
        return null;
    }
}
