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
package org.komodo.utils.i18n;

/**
 * Localized messages for the {@link org.komodo.utils.i18n} package.
 */
@SuppressWarnings( "javadoc" )
public class UtilsI18n extends I18n {

    public static String missingI18Field;
    public static String missingPropertiesKey;
    public static String problemAccessingI18Field;
    public static String problemLoadingI18nClass;
    public static String problemLoadingI18nProperties;

    static {
        final UtilsI18n i18n = new UtilsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private UtilsI18n() {
        // nothing to do
    }

}
