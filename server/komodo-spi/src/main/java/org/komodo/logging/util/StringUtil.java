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
package org.komodo.logging.util;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Utilities for string processing and manipulation.
 */
public class StringUtil {

    public static final String[] EMPTY_STRING_ARRAY = new String[0];
    private static final Pattern PARAMETER_COUNT_PATTERN = Pattern.compile("\\{(\\d+)\\}");

    /**
     * Create a string by substituting the parameters into all key occurrences in the supplied format. The pattern consists of
     * zero or more keys of the form <code>{n}</code>, where <code>n</code> is an integer starting at 0. Therefore, the first
     * parameter replaces all occurrences of "{0}", the second parameter replaces all occurrences of "{1}", etc.
     * <p>
     * If any parameter is null, the corresponding key is replaced with the string "null". Therefore, consider using an empty
     * string when keys are to be removed altogether.
     * </p>
     * <p>
     * If there are no parameters, this method does nothing and returns the supplied pattern as is.
     * </p>
     * 
     * @param pattern the pattern
     * @param parameters the parameters used to replace keys
     * @return the string with all keys replaced (or removed)
     */
    public static String createString( String pattern,
                                       Object... parameters ) {
        if (pattern == null)
            throw new IllegalArgumentException("pattern");

        if (parameters == null) parameters = EMPTY_STRING_ARRAY;
        Matcher matcher = PARAMETER_COUNT_PATTERN.matcher(pattern);
        StringBuffer text = new StringBuffer();
        int requiredParameterCount = 0;
        boolean err = false;
        while (matcher.find()) {
            int ndx = Integer.valueOf(matcher.group(1));
            if (requiredParameterCount <= ndx) {
                requiredParameterCount = ndx + 1;
            }
            if (ndx >= parameters.length) {
                err = true;
                matcher.appendReplacement(text, matcher.group());
            } else {
                Object parameter = parameters[ndx];

                // Automatically pretty-print arrays
                if (parameter != null && parameter.getClass().isArray()) {
                    if (parameter instanceof Object[]) {
                        parameter = Arrays.asList((Object[])parameter);
                    } else {
                        int length = Array.getLength(parameter);
                        List<Object> parameterAsList = new ArrayList<Object>(length);
                        for (int i = 0; i < length; i++) {
                            parameterAsList.add(Array.get(parameter, i));
                        }
                        parameter = parameterAsList;
                    }
                }

                matcher.appendReplacement(text, Matcher.quoteReplacement(parameter == null ? "null" : parameter.toString()));
            }
        }
        if (err || requiredParameterCount < parameters.length) {
            StringBuffer buf = new StringBuffer();
            buf.append(parameters.length)
                    .append("parameter")
                    .append(parameters.length == 1 ? "" : "s")
                    .append(" supplied, but ")
                    .append(requiredParameterCount)
                    .append("parameter")
                    .append(requiredParameterCount == 1 ? "" : "s")
                    .append(" required: ")
                    .append(pattern)
                    .append(" => ")
                    .append(text.toString());

            throw new IllegalArgumentException(buf.toString());
        }
        matcher.appendTail(text);

        return text.toString();
    }

    /**
     * Create a new string containing the specified character repeated a specific number of times.
     * 
     * @param charToRepeat the character to repeat
     * @param numberOfRepeats the number of times the character is to repeat in the result; must be greater than 0
     * @return the resulting string
     */
    public static String createString( final char charToRepeat,
                                       int numberOfRepeats ) {
        assert numberOfRepeats >= 0;
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < numberOfRepeats; ++i) {
            sb.append(charToRepeat);
        }
        return sb.toString();
    }

    /**
     * Returns true if the given string is null or represents the empty string
     * 
     * @param str the string; may be null or empty
     * @return true if the string is null or contains only whitespace
     */
    public static boolean isBlank( String str ) {
        return str == null || str.trim().isEmpty();
    }

    private StringUtil() {
        // Prevent construction
    }
}
