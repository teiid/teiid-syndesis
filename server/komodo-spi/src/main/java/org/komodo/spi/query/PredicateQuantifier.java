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
 * Predicate quantifiers
 */
public enum PredicateQuantifier {

    /** "Some" predicate quantifier (equivalent to "Any") */
    SOME,

    /** "Any" predicate quantifier (equivalent to "Some") */
    ANY,

    /** "All" predicate quantifier */
    ALL;

    /**
     * @return index of predicate
     */
    public int getQuantifier() {
        return ordinal() + 2;
    }

    /**
     * @param quantifier
     * @return {@link PredicateQuantifier} with the given quantifier index
     */
    public static PredicateQuantifier findQuantifier(int quantifier) {
        for (PredicateQuantifier pq : values()) {
            if (pq.getQuantifier() == quantifier)
                return pq;
        }

        throw new IllegalStateException();
    }

    /**
     * @param name
     * @return PredicateQuantifier with given name
     */
    public static PredicateQuantifier findPredicateQuantifier(String name) {
        if (name == null)
            return null;

        name = name.toUpperCase();
        for (PredicateQuantifier pq : values()) {
            if (pq.name().equals(name))
                return pq;
        }
        return null;
    }
}
