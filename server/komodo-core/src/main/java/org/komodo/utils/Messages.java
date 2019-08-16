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
package org.komodo.utils;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.komodo.StringConstants;

/**
 *
 */
public class Messages implements StringConstants {
    private static final String BUNDLE_NAME = "org.komodo.utils.messages"; //$NON-NLS-1$

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

    @SuppressWarnings( "javadoc" )
    public enum ArgCheck {
        isNonNegativeInt,
        isNonPositiveInt,
        isNegativeInt,
        isPositiveInt,
        isStringNonZeroLength,
        isNonNull,
        isNull,
        isInstanceOf,
        isCollectionNotEmpty,
        isMapNotEmpty,
        isArrayNotEmpty,
        isNotSame,
        contains,
        containsKey,
        isPropertiesNotEmpty,
        invalidClassMessage,
        isEqual,
        isNotEqual,
        isNonNegative,
        isNonPositive,
        isNegative,
        isPositive,
        isNotZeroLength,
        isIdentical,
        isNotEmpty_Collection,
        isNotEmpty_Map,
        contains_Collection,
        contains_Map;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum StringNameValidator {
        minLengthFailure,
        nameLengthLongerThanAllowed,
        firstCharMustBeAlphabetic,
        onlyAlphaOrDigit,
        orOtherValidChars,
        nameNotNull,
        nameSameAsOtherObjects,
        sameNameCaseSensitive,
        minLengthNotExceedMaxLength,
        unableMakeNameUnique,
        charNotValidChar,
        unquotedNameWithDelimiter;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum FileUtils {
        The_name_of_the_file_may_not_be_null,
        The_file_extension_may_not_be_null,
        Unable_to_create_file_in,
        Unable_to_write_file_in,
        Unable_to_read_file_in,
        Unable_to_rename_file_in,
        Unable_to_delete_file_in,
        File_already_exists,
        Unable_to_rename,
        File_does_not_exist_1,
        Not_a_directory;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum StringUtil {
        displayable;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    private static String getEnumName(Enum<?> enumValue) {
        String className = enumValue.getClass().getName();
        String[] components = className.split("\\$"); //$NON-NLS-1$
        return components[components.length - 1];
    }

    private Messages() {
    }

    /**
     * Get message string
     *
     * @param key
     *
     * @return i18n string
     */
    private static String getString(Enum<?> key) {
        try {
            return RESOURCE_BUNDLE.getString(key.toString());
        } catch (final Exception err) {
            String msg;

            if (err instanceof NullPointerException) {
                msg = "<No message available>"; //$NON-NLS-1$
            } else if (err instanceof MissingResourceException) {
                msg = "<Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + '>'; //$NON-NLS-1$ //$NON-NLS-2$
            } else {
                msg = err.getLocalizedMessage();
            }

            return msg;
        }
    }

    /**
     * Get message string with parameters
     *
     * @param key
     * @param parameters
     *
     * @return i18n string
     */
    public static String getString(Enum<?> key, Object... parameters) {
        String text = getString(key);

        // Check the trivial cases ...
        if (text == null) {
            return '<' + key.toString() + '>';
        }
        if (parameters == null || parameters.length == 0) {
            return text;
        }

        return MessageFormat.format(text, parameters);
    }
}
