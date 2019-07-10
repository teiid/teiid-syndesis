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
package org.komodo.relational;


@SuppressWarnings( "javadoc" )
public interface RelationalConstants {

    enum Nullable {

        NO_NULLS( "NOT NULL" ), //$NON-NLS-1$
        NULLABLE( "NULL" ), //$NON-NLS-1$
        NULLABLE_UNKNOWN( "UNKNOWN" ); //$NON-NLS-1$

        public static final Nullable DEFAULT_VALUE = NULLABLE;

        /**
         * @param value
         *        the value whose <code>Nullable</code> is being requested (can be empty)
         * @return the corresponding <code>Nullable</code> or the default value if not found
         * @see #DEFAULT_VALUE
         */
        public static Nullable fromValue( final String value ) {
            for (final Nullable nullable : values()) {
                if (nullable.value.equals(value)) {
                    return nullable;
                }
            }

            return DEFAULT_VALUE;
        }

        private final String value;

        private Nullable( final String value ) {
            this.value = value;
        }

        /**
         * @return the Teiid nullable value (never empty)
         */
        public String toValue() {
            return this.value;
        }

    }

    /**
     * The default value for the <code>datatype name</code> property. Value is {@value} .
     */
    String DEFAULT_DATATYPE_NAME = "STRING"; //$NON-NLS-1$

    /**
     * The default value for the <code>datatype length</code> property. Value is {@value} .
     */
    long DEFAULT_LENGTH = 0;

    /**
     * The default value for the <code>datatype precision</code> property. Value is {@value} .
     */
    long DEFAULT_PRECISION = 0;

    /**
     * The default value for the <code>datatype scale</code> property. Value is {@value} .
     */
    long DEFAULT_SCALE = 0;

}
