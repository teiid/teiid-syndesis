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

import org.komodo.spi.Messages;

/**
 * Metadata Server version class 
 * 
 *
 */
public class DefaultMetadataVersion implements MetadataVersion {

    private String versionString = ZERO + DOT + ZERO + DOT + ZERO;

    private final String majorVersion;

    private String minorVersion = WILDCARD;

    private String microVersion = WILDCARD;

    /**
     * Create a new instance with the given version segments
     * 
     * @param major the major version
     * @param minor the minor version
     * @param micro the micro version
     */
    public DefaultMetadataVersion(String major, String minor, String micro) {
        if (major == null)
            throw new IllegalArgumentException(Messages.getString(Messages.SPI.valueCannotBeNull, "major")); //$NON-NLS-1$

        if (minor == null)
            throw new IllegalArgumentException(Messages.getString(Messages.SPI.valueCannotBeNull, "minor")); //$NON-NLS-1$

        if (micro == null)
            throw new IllegalArgumentException(Messages.getString(Messages.SPI.valueCannotBeNull, "micro")); //$NON-NLS-1$

        this.majorVersion = major;
        this.minorVersion = minor;
        this.microVersion = micro;
        this.versionString = major + DOT + minor + DOT + micro;
    }

    /**
     * Create a new instance with the given version string
     * 
     * @param versionString the version string
     */
    public DefaultMetadataVersion(String versionString) {
        this.versionString = versionString;

        String[] tokens = versionString.split("\\."); //$NON-NLS-1$

        if (tokens.length >= 3) {
            majorVersion = tokens[0];
            minorVersion = tokens[1];
            if (tokens[2] != null) {
                int dashIndex = tokens[2].indexOf('-');
                if (dashIndex != -1 && tokens[2].length() > 0) {
                    microVersion = tokens[2].substring(0, dashIndex);
                } else {
                    microVersion = tokens[2];
                }
            }
        } else if (tokens.length == 2) {
            majorVersion = tokens[0];
            minorVersion = tokens[1];
        } else {
            majorVersion = tokens[0];
        }

        this.versionString = majorVersion + DOT + minorVersion + DOT + microVersion;
    }

    @Override
    public String toString() {
        return versionString;
    }

    @Override
    public String getMajor() {
        return majorVersion;
    }

    @Override
    public String getMinor() {
        return minorVersion;
    }

    @Override
    public String getMicro() {
        return microVersion;
    }

    @Override
    public boolean hasWildCards() {
        return majorVersion.equals(WILDCARD) || minorVersion.equals(WILDCARD) || microVersion.equals(WILDCARD);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.majorVersion == null) ? 0 : this.majorVersion.hashCode());
        result = prime * result + ((this.microVersion == null) ? 0 : this.microVersion.hashCode());
        result = prime * result + ((this.minorVersion == null) ? 0 : this.minorVersion.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        DefaultMetadataVersion other = (DefaultMetadataVersion)obj;
        if (this.majorVersion == null) {
            if (other.majorVersion != null)
                return false;
        } else if (!this.majorVersion.equals(other.majorVersion))
            return false;
        if (this.microVersion == null) {
            if (other.microVersion != null)
                return false;
        } else if (!this.microVersion.equals(other.microVersion))
            return false;
        if (this.minorVersion == null) {
            if (other.minorVersion != null)
                return false;
        } else if (!this.minorVersion.equals(other.minorVersion))
            return false;
        return true;
    }

    @Override
    public boolean compareTo(MetadataVersion otherVersion) {
        String entryMajor = otherVersion.getMajor();

        if (!getMajor().equals(entryMajor) && !getMajor().equals(WILDCARD) && !entryMajor.equals(WILDCARD))
            return false;

        String entryMinor = otherVersion.getMinor();

        if (!getMinor().equals(entryMinor) && !getMinor().equals(WILDCARD) && !entryMinor.equals(WILDCARD))
            return false;

        String entryMicro = otherVersion.getMicro();

        if (!getMicro().equals(entryMicro) && !getMicro().equals(WILDCARD) && !entryMicro.equals(WILDCARD))
            return false;

        /*
         *  Either this version or entry version contain sufficient wildcards
         *  to be considered a match
         */
        return true;
    }

    @Override
    public boolean isSevenInstance() {
        return MetadataVersion.SEVEN.equals(getMajor());
    }

    @Override
    public MetadataVersion getMinimumVersion() {
        if (!this.hasWildCards())
            return this;

        String major = getMajor().equals(WILDCARD) ? SEVEN : getMajor();
        String minor = getMinor().equals(WILDCARD) ? ZERO : getMinor();
        String micro = getMicro().equals(WILDCARD) ? ZERO : getMicro();

        return new DefaultMetadataVersion(major, minor, micro);
    }

    @Override
    public MetadataVersion getMaximumVersion() {
        if (!this.hasWildCards())
            return this;

        String major = getMajor().equals(WILDCARD) ? NINE : getMajor();
        String minor = getMinor().equals(WILDCARD) ? NINE : getMinor();
        String micro = getMicro().equals(WILDCARD) ? NINE : getMicro();

        return new DefaultMetadataVersion(major, minor, micro);
    }

    @Override
    public boolean isGreaterThan(MetadataVersion otherVersion) {
        MetadataVersion myMinVersion = getMinimumVersion();
        MetadataVersion otherMaxVersion = otherVersion.getMaximumVersion();

        int majCompResult = isOtherNumberGreaterThan(myMinVersion.getMajor(), otherMaxVersion.getMajor());
        if (majCompResult > 0)
            return true;

        int minCompResult = isOtherNumberGreaterThan(myMinVersion.getMinor(), otherMaxVersion.getMinor());
        if (majCompResult == 0 && minCompResult > 0)
            return true;

        int micCompResult = isOtherNumberGreaterThan(myMinVersion.getMicro(), otherMaxVersion.getMicro());
        if (majCompResult == 0 && minCompResult == 0 && micCompResult > 0)
            return true;

        return false;
    }

    @Override
    public boolean isLessThan(MetadataVersion otherVersion) {
        MetadataVersion myMaxVersion = getMaximumVersion();
        MetadataVersion otherMinVersion = otherVersion.getMinimumVersion();

        int majCompResult;
        try {
            int myMax = Integer.parseInt(myMaxVersion.getMajor());
            int otherMin = Integer.parseInt(otherMinVersion.getMajor());
            majCompResult = Integer.valueOf(myMax).compareTo(Integer.valueOf(otherMin));

        } catch (NumberFormatException ex) {
            // One or other is a string so compare lexographically
            majCompResult = myMaxVersion.getMajor().compareTo(otherMinVersion.getMajor());
        }

        if (majCompResult < 0)
            return true;

        int minCompResult;
        try {
            int myMax = Integer.parseInt(myMaxVersion.getMinor());
            int otherMin = Integer.parseInt(otherMinVersion.getMinor());
            minCompResult = Integer.valueOf(myMax).compareTo(Integer.valueOf(otherMin));
        } catch (NumberFormatException ex) {
            // One or other is a string so compare lexographically
            minCompResult = myMaxVersion.getMinor().compareTo(otherMinVersion.getMinor());
        }

        if (majCompResult == 0 && minCompResult < 0)
            return true;

        int micCompResult;
        try {
            int myMax = Integer.parseInt(myMaxVersion.getMicro());
            int otherMin = Integer.parseInt(otherMinVersion.getMicro());
            micCompResult = Integer.valueOf(myMax).compareTo(Integer.valueOf(otherMin));
        } catch (NumberFormatException ex) {
            // One or other is a string so compare lexographically
            micCompResult = myMaxVersion.getMicro().compareTo(otherMinVersion.getMicro());
        }

        if (majCompResult == 0 && minCompResult == 0 && micCompResult < 0)
            return true;

        return false;
    }

    @Override
    public boolean isGreaterThanOrEqualTo(MetadataVersion otherVersion) {
        return this.compareTo(otherVersion) || this.isGreaterThan(otherVersion);
    }

    @Override
    public boolean isLessThanOrEqualTo(MetadataVersion otherVersion) {
        return this.compareTo(otherVersion) || this.isLessThan(otherVersion);
    }

    private int isOtherNumberGreaterThan(String myNumber, String otherNumber) {
        int myValue = -1;
        int otherValue = -1;

        try {
            myValue = Integer.parseInt(myNumber);
        } catch (NumberFormatException e) {
            myValue = -1;
        }

        try {
            otherValue = Integer.parseInt(otherNumber);
        } catch (NumberFormatException e) {
            otherValue = -1;
        }

        if (myValue < 0 || otherValue < 0) {
            return myNumber.compareTo(otherNumber);
        } else {
            return myValue - otherValue;
        }
    }
}
