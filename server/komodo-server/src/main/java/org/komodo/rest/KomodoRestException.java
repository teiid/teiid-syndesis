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
package org.komodo.rest;

import org.komodo.spi.StringConstants;
import org.komodo.utils.StringUtils;

/**
 * An error originating from or caught by the Komodo REST application.
 */
public class KomodoRestException extends Exception implements StringConstants {

    private static final long serialVersionUID = 1L;

    /*
     * Possible for the toString() and getMessage() methods to
     * end up in an infinite loop. Simple flag to avoid this.
     */
    private boolean stopOverflow;

    /**
     * @param message
     *        the error message (can be empty)
     */
    public KomodoRestException( final String message ) {
        this( message, null );
    }

    /**
     * @param message
     *        the error message (can be empty)
     * @param cause
     *        the cause (can be <code>null</code>)
     */
    public KomodoRestException( final String message,
                                final Throwable cause ) {
        super( message, cause );
    }

    /**
     * @param cause
     *          the cause (can be <code>null</code>)
     */
    public KomodoRestException(final Throwable cause) {
        super(cause);
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer(super.toString());

        if (! stopOverflow) {
            stopOverflow = true;
            buf.append(NEW_LINE)
                    .append(StringUtils.exceptionToString(this));
            stopOverflow = false;
        }

        return buf.toString();
    }

    @Override
    public String getMessage() {
        StringBuffer buf = new StringBuffer(super.getMessage());

        if (! stopOverflow) {
            stopOverflow = true;
            buf.append(NEW_LINE)
                    .append(StringUtils.exceptionToString(this));
            stopOverflow = false;
        }

        return buf.toString();
    }
}
