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
package org.komodo.spi.runtime.version;

/**
 * Parent marker interface for teiid instance version information
 */
public interface MetadataVersion {

    /**
     * dot
     */
    String DOT = "."; //$NON-NLS-1$

    /**
     * wildcard character used in version strings
     */
    String WILDCARD = "x"; //$NON-NLS-1$

    /**
     * zero
     */
    String ZERO = "0"; //$NON-NLS-1$

    /**
     * one
     */
    String ONE = "1"; //$NON-NLS-1$

    /**
     * two
     */
    String TWO = "2"; //$NON-NLS-1$

    /**
     * three
     */
    String THREE = "3"; //$NON-NLS-1$

    /**
     * four
     */
    String FOUR = "4"; //$NON-NLS-1$

    /**
     * five
     */
    String FIVE = "5"; //$NON-NLS-1$

    /**
     * six
     */
    String SIX = "6"; //$NON-NLS-1$

    /**
     * seven
     */
    String SEVEN = "7"; //$NON-NLS-1$

    /**
     * eight
     */
    String EIGHT = "8"; //$NON-NLS-1$

    /**
     * nine
     */
    String NINE = "9"; //$NON-NLS-1$

    /**
     * Version property constant
     */
    String VERSION_PROPERTY = "org.teiid.version"; //$NON-NLS-1$

    /**
     * @return the major version segment
     */
    String getMajor();

    /**
     * @return the minor version segment
     */
    String getMinor();

    /**
     * @return the micro version segment 
     */
    String getMicro();

    /**
     * Test whether the minor or micro segments are wildcards '*'
     * 
     * @return true if there are wildcards. false otherwise
     */
    boolean hasWildCards();

    /**
     * @param otherVersion
     * 
     * @return true if the otherVersion is considered equivalent
     */
    boolean compareTo(MetadataVersion otherVersion);

    /**
     * Is this a 7 teiid instance?
     * 
     * @return true is version is 7
     */
    boolean isSevenInstance();

    /**
     * @return the minimum version that this version could be,
     *                 eg. 8.x.x will be 8.0.0 while 8.1.x will be 8.1.0 and
     *                       8.2.1 will always be 8.2.1
     */
    MetadataVersion getMinimumVersion();

    /**
     * @return the maximum version that this version could be,
     *                 eg. 8.x.x will be 8.9.9 while 8.1.x will be 8.1.9 and
     *                       8.2.1 will always be 8.2.1
     */
    MetadataVersion getMaximumVersion();

    /**
     * Is this version greater than the given version
     *
     * Wildcards will cause the result to return false since either
     * this or otherVersion could be the greater depending on the
     * value given to the wildcard.
     *
     * @param otherVersion
     *
     * @return true if this version is greater. False otherwise.
     */
    boolean isGreaterThan(MetadataVersion otherVersion);

    /**
     * Is this version less than the given version
     *
     * Wildcards will cause the result to return false since either
     * this or otherVersion could be the lesser depending on the
     * value given to the wildcard.
     *
     * @param otherVersion
     *
     * @return true if this version is less. False otherwise.
     */
    boolean isLessThan(MetadataVersion otherVersion);

    /**
     * Convenience that delegates to {@link #compareTo(MetadataVersion)}
     * and {@link #isGreaterThan(MetadataVersion)}.
     *
     * @param otherVersion
     *
     * @return this is greater than or equal to otherVersion
     */
    boolean isGreaterThanOrEqualTo(MetadataVersion otherVersion);

    /**
     * Convenience that delegates to {@link #compareTo(MetadataVersion)}
     * and {@link #isLessThan(MetadataVersion)}.
     *
     * @param otherVersion
     *
     * @return this is less than or equal to otherVersion
     */
    boolean isLessThanOrEqualTo(MetadataVersion otherVersion);
}
