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

import org.komodo.spi.constants.StringConstants;

/**
 * Delineation of the category of join type
 */
public enum JoinTypeTypes {
    /** Represents an inner join:  a INNER JOIN b */
    JOIN_INNER(false),

    /** Represents a right outer join:  a RIGHT OUTER JOIN b */
    JOIN_RIGHT_OUTER(true),

    /** Represents a left outer join:  a LEFT OUTER JOIN b */
    JOIN_LEFT_OUTER(true),

    /** Represents a full outer join:  a FULL OUTER JOIN b */
    JOIN_FULL_OUTER(true),

    /** Represents a cross join:  a CROSS JOIN b */
    JOIN_CROSS(false),

    /** Represents a union join:  a UNION JOIN b - not used after rewrite */
    JOIN_UNION(true),

    /** internal SEMI Join type */
    JOIN_SEMI(false),

    /** internal ANTI SEMI Join type */
    JOIN_ANTI_SEMI(true);

    private final boolean outer;

    private JoinTypeTypes(boolean outer) {
        this.outer = outer;
    }

    public int getTypeCode() {
        return this.ordinal();
    }

    public boolean isOuter() {
        return this.outer;
    }

    public String toPrintStatement() {
        String name = name();
        String JOIN = "JOIN"; //$NON-NLS-1$

        name = name.substring((JOIN + StringConstants.UNDERSCORE).length());
        name = name.replaceAll(StringConstants.UNDERSCORE, StringConstants.SPACE);
        name = name + StringConstants.SPACE + JOIN;
        return name;
    }

    /**
     * @param name
     * @return Types representing the given name
     */
    public static JoinTypeTypes findType(String name) {
        if (name == null)
            return null;

        name = name.toUpperCase();
        for (JoinTypeTypes type : values()) {
            if (type.name().equals(name))
                return type;
        }

        return null;
    }

    /**
     * @param index
     * @return Types representing the given index
     */
    public static JoinTypeTypes findType(int index) {
        for (JoinTypeTypes type : values()) {
            if (type.ordinal() == index)
                return type;
        }

        return null;
    }
}
