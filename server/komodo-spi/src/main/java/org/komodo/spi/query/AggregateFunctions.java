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

public enum AggregateFunctions {
    COUNT,
    SUM,
    AVG,
    MIN,
    MAX,
    XMLAGG,
    TEXTAGG,
    ARRAY_AGG,
    ANY,
    SOME,
    EVERY,
    STDDEV_POP,
    STDDEV_SAMP,
    VAR_POP,
    VAR_SAMP,
    RANK,
    DENSE_RANK,
    ROW_NUMBER,

    JSONARRAY_AGG,

    STRING_AGG,

    USER_DEFINED;

    /**
     * @param name
     * @return Type for given name
     */
    public static AggregateFunctions findAggregateFunction(String name) {
        if (name == null)
            return null;

        name = name.toUpperCase();
        for (AggregateFunctions fn : values()) {
            if (fn.name().equals(name))
                return fn;
        }

        return null;
    }
}
