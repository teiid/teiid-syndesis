/*************************************************************************************
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

import static org.komodo.spi.constants.StringConstants.CLOSE_ANGLE_BRACKET;
import static org.komodo.spi.constants.StringConstants.DOT;
import static org.komodo.spi.constants.StringConstants.OPEN_ANGLE_BRACKET;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Localized messages for the {@code server rest} project.
 */
public final class Messages {

    /**
     * Messages relating to errors.
     */
    public enum Error {

        /**
         * An error message indicating a repository commit did not complete in the required amount of time.
         */
        COMMIT_TIMEOUT,

        /**
         * An error indicating the the JSON entity is incomplete.
         */
        INCOMPLETE_JSON,

        /**
         * An error message indicating the Komodo Engine did not clear the repository due to an error.
         */
        KOMODO_ENGINE_CLEAR_ERROR,

        /**
         * An error message indicating the Komodo Engine did not clear the repository in the required amount of time.
         */
        KOMODO_ENGINE_CLEAR_TIMEOUT,

        /**
         * An error message indicating the Komodo Engine did not shutdown due to an error.
         */
        KOMODO_ENGINE_SHUTDOWN_ERROR,

        /**
         * An error message indicating the Komodo Engine did not shutdown in the required amount of time.
         */
        KOMODO_ENGINE_SHUTDOWN_TIMEOUT,

        /**
         * An error message indicating the Komodo Engine did not startup due to an error.
         */
        KOMODO_ENGINE_STARTUP_ERROR,

        /**
         * An error message indicating the Komodo Engine did not startup in the required amount of time.
         */
        KOMODO_ENGINE_STARTUP_TIMEOUT,

        /**
         * An error message indicating the Komodo Engine's workspace manager could not be obtained.
         */
        KOMODO_ENGINE_WORKSPACE_MGR_ERROR,

        /**
         * An error message indicating the requested resource was not found.
         */
        RESOURCE_NOT_FOUND,

        /**
         * An error message indicating a repository rollback had an error.
         */
        ROLLBACK_ERROR,

        /**
         * An error message indicating a repository rollback did not complete in the required amount of time.
         */
        ROLLBACK_TIMEOUT,

        /**
         * An error indicating the token found in a JSON representation is unknown or misplaced.
         */
        UNEXPECTED_JSON_TOKEN,
        
        /**
         * CORS Filter error
         */
        ORIGIN_NOT_ALLOWED,

        /**
         * An error response occurred
         */
        RESPONSE_ERROR;

        /**
         * {@inheritDoc}
         *
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return getEnumName( this ) + DOT + name();
        }

    }

    /**
     * General messages.
     */
    public enum General {

        /**
         * The name of the DELETE operation.
         */
        DELETE_OPERATION_NAME,

        /**
         * The name of the GET operation.
         */
        GET_OPERATION_NAME,

        /**
         * A message indicating a value is not present.
         */
        NO_VALUE;

        /**
         * {@inheritDoc}
         *
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return getEnumName( this ) + DOT + name();
        }

    }

    private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + DOT
                                              + Messages.class.getSimpleName().toLowerCase();

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    private static String getEnumName( final Enum< ? > enumValue ) {
        final String className = enumValue.getClass().getName();
        final String[] components = className.split( "\\$" ); //$NON-NLS-1$
        return components[ components.length - 1 ];
    }

    private static String getString( final Enum< ? > key ) {
        try {
            return RESOURCE_BUNDLE.getString( key.toString() );
        } catch ( final Exception e ) {
            String msg;

            if ( e instanceof NullPointerException ) {
                msg = "<No message key>"; //$NON-NLS-1$
            } else if ( e instanceof MissingResourceException ) {
                msg = OPEN_ANGLE_BRACKET + "Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + CLOSE_ANGLE_BRACKET; //$NON-NLS-1$ //$NON-NLS-2$
            } else {
                msg = e.getLocalizedMessage();
            }

            return msg;
        }
    }

    /**
     * @param key
     *        the message key (cannot be <code>null</code>)
     * @param parameters
     *        the substitution parameters (can be <code>null</code>)
     * @return the localized message (never empty)
     */
    public static String getString( final Enum< ? > key,
                                    final Object... parameters ) {
        final String text = getString( key );

        // return key if message not found
        if ( text == null ) {
            return OPEN_ANGLE_BRACKET + key.toString() + CLOSE_ANGLE_BRACKET;
        }

        // return if no parameters to format
        if ( ( parameters == null ) || ( parameters.length == 0 ) ) {
            return text;
        }

        // return formatted message
        return String.format( text, parameters );
    }

    /**
     * Don't allow construction outside of this class.
     */
    private Messages() {
        // nothing to do
    }

}
