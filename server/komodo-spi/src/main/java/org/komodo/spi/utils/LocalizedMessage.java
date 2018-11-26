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
package org.komodo.spi.utils;

/**
 * Represents a translated message.
 */
public class LocalizedMessage {

    private final String id;
    private final String localeCode;
    private final String message;

    /**
     * @param msgId
     *        the message identifier (cannot be empty)
     * @param msgLocale
     *        the message locale code (cannot be empty)
     * @param msgText
     *        the translated message (cannot be empty)
     */
    public LocalizedMessage( final String msgId,
                             final String msgLocale,
                             final String msgText ) {
        this.id = msgId;
        this.localeCode = msgLocale;
        this.message = msgText;
    }

    /**
     * @return the identifier (never empty)
     */
    public String getId() {
        return this.id;
    }

    /**
     * @return the locale code (never empty)
     */
    public String getLocaleCode() {
        return this.localeCode;
    }

    /**
     * @return the translated message (never empty)
     */
    public String getMessage() {
        return this.message;
    }

}
