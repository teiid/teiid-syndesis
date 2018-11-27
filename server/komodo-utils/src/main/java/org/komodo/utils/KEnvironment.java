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

import org.komodo.spi.constants.SystemConstants;

/**
 * Komodo Environment functions
 */
public class KEnvironment {

    /**
     * Check the engine data directory property has a legitimate value
     */
    public static void checkDataDirProperty() {
        // Initialize default data directory system property if necessary
        if ( ! StringUtils.isBlank( System.getProperty( SystemConstants.ENGINE_DATA_DIR ) ) )
            return;

        final String defaultValue = ( System.getProperty( "user.home", "/" ) + "/.komodo" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        System.setProperty( SystemConstants.ENGINE_DATA_DIR, defaultValue );
    }
}
