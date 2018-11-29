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
 * Enumerator for types of parameters 
 */
public enum ParameterInfo {
    /** Constant identifying an IN parameter */
    IN,

    /** Constant identifying an OUT parameter */
    OUT,

    /** Constant identifying an INOUT parameter */
    INOUT,

    /** Constant identifying a RETURN parameter */
    RETURN_VALUE,

    /** Constant identifying a RESULT SET parameter */
    RESULT_SET;

    /**
     * Get the index of the enumerator. For compatibility
     * with existing code, the index starts at 1 rather than 0.
     * 
     * @return value of index
     */
    public int index() {
        return ordinal() + 1;
    }

    public static ParameterInfo valueOf(int type) {
        for (ParameterInfo info : ParameterInfo.values()) {
            if (info.index() == type)
                return info;
        }

        throw new IllegalArgumentException();
    }
}
