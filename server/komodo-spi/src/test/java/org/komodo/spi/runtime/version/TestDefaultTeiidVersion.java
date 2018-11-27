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
package org.komodo.spi.runtime.version;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class TestDefaultTeiidVersion {

    private MetadataVersion version(String versionId) {
        return new DefaultMetadataVersion(versionId);
    }

    /**
     * Test {@link DefaultMetadataVersion#compareTo(MetadataVersion)}
     */
    @Test
    public void testCompareTo() {
        assertTrue(version("8.0.0").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.0").compareTo(version("8.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertTrue(version("8.0.0").compareTo(version("8.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertTrue(version("8.0.0").compareTo(version("8.x.x"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.x").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertTrue(version("8.x.0").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertTrue(version("8.x.x").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("8.0.0").compareTo(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").compareTo(version("8.1.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").compareTo(version("8.x.1"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").compareTo(version("9.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("7.0.0").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.1.0").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.x.1").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("9.0.0").compareTo(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("9.1.1").compareTo(version("9.1.x")));
    }

    /**
     * Test {@link DefaultMetadataVersion#getMaximumVersion()}
     */
    @Test
    public void testGetMaximum() {
        assertEquals(version("8.0.0"), version("8.0.0").getMaximumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
        assertEquals(version("8.9.0"), version("8.x.0").getMaximumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
        assertEquals(version("8.0.9"), version("8.0.x").getMaximumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
        assertEquals(version("8.9.9"), version("8.x.x").getMaximumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * Test {@link DefaultMetadataVersion#getMinimumVersion()}
     */
    @Test
    public void testGetMinimum() {
        assertEquals(version("8.0.0"), version("8.0.0").getMinimumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
        assertEquals(version("8.0.0"), version("8.x.0").getMinimumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
        assertEquals(version("8.0.0"), version("8.0.x").getMinimumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
        assertEquals(version("8.0.0"), version("8.x.x").getMinimumVersion()); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * Test {@link DefaultMetadataVersion#isGreaterThan(MetadataVersion)}
     */
    @Test
    public void testIsGreaterThan() {
        assertFalse(version("8.0.0").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.0").isGreaterThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.0").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.0").isGreaterThan(version("7.7.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.7.0").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.0").isGreaterThan(version("7.0.1"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.1").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.0").isGreaterThan(version("7.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.x").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.0").isGreaterThan(version("7.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.x.0").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.0").isGreaterThan(version("7.x.x"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.x.x").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.x.0").isGreaterThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.0").isGreaterThan(version("8.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.x").isGreaterThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.0").isGreaterThan(version("8.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.x.x").isGreaterThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.0").isGreaterThan(version("8.x.x"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.0.x").isGreaterThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.0").isGreaterThan(version("8.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("8.x.0").isGreaterThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("7.0.0").isGreaterThan(version("8.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("8.0.x").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isGreaterThan(version("8.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("8.x.0").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isGreaterThan(version("8.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        // silly micro version should be ignored since minor versions should be enough for the comparison
        assertTrue(version("8.1.extendedmicroversionid").isGreaterThan(version("8.0.0"))); //$NON-NLS-1$//$NON-NLS-2$

        // same minor versions up until 1 and 2
        assertTrue(version("8.designer-2.0").isGreaterThan(version("8.designer-1.0"))); //$NON-NLS-1$//$NON-NLS-2$

        // Comparing 1 and 10
        assertTrue(version("8.designer-10.0").isGreaterThan(version("8.designer-1.0"))); //$NON-NLS-1$//$NON-NLS-2$

        // 20 < 18 but designer > teiidteiid
        assertTrue(version("8.teiidteiid-18.0").isGreaterThan(version("8.designer-20.0"))); //$NON-NLS-1$//$NON-NLS-2$
        assertFalse(version("8.designer-20.0").isGreaterThan(version("8.teiidteiid-18.0"))); //$NON-NLS-1$//$NON-NLS-2$

        assertTrue(version("8.11.0").isGreaterThan(version("8.8.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertTrue(version("8.10.0").isGreaterThan(version("8.8.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertTrue(version("8.10.1").isGreaterThan(version("8.10.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.10.1").isGreaterThan(version("8.11.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("8.7.1").isGreaterThan(version("8.7.x"))); //$NON-NLS-1$//$NON-NLS-2$
        assertTrue(version("8.12.4").isGreaterThan(version("8.9.0"))); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * Test {@link DefaultMetadataVersion#isLessThan(MetadataVersion)}
     */
    @Test
    public void testIsLessThan() {
        assertFalse(version("8.0.0").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.0").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isLessThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.7.0").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isLessThan(version("7.7.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.1").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isLessThan(version("7.0.1"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.x").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isLessThan(version("7.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.x.0").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isLessThan(version("7.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.x.x").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.0").isLessThan(version("7.x.x"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.0").isLessThan(version("8.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.x.0").isLessThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.0").isLessThan(version("8.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.x").isLessThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.0").isLessThan(version("8.x.x"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.x.x").isLessThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.0").isLessThan(version("8.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.x").isLessThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertTrue(version("7.0.0").isLessThan(version("8.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.x.0").isLessThan(version("7.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("8.0.0").isLessThan(version("8.0.x"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.0.x").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("8.0.0").isLessThan(version("8.x.0"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertFalse(version("8.x.0").isLessThan(version("8.0.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        // silly micro version should be ignored since minor versions should be enough for the comparison
        assertTrue(version("8.0.0").isLessThan(version("8.1.extendedmicroversionid"))); //$NON-NLS-1$//$NON-NLS-2$

        // same minor versions up until 1 and 2
        assertTrue(version("8.designer-1.0").isLessThan(version("8.designer-2.0"))); //$NON-NLS-1$//$NON-NLS-2$

        // Comparing 1 and 10
        assertTrue(version("8.designer-1.0").isLessThan(version("8.designer-10.0"))); //$NON-NLS-1$//$NON-NLS-2$

        // 20 > 18 but designer < teiidteiid
        assertTrue(version("8.designer-20.0").isLessThan(version("8.teiidteiid-18.0"))); //$NON-NLS-1$//$NON-NLS-2$
        assertFalse(version("8.teiidteiid-18.0").isLessThan(version("8.designer-20.0"))); //$NON-NLS-1$//$NON-NLS-2$

        assertTrue(version("8.7.0").isLessThan(version("8.7.1"))); //$NON-NLS-1$ //$NON-NLS-2$
        assertTrue(version("8.8.0").isLessThan(version("8.10.0"))); //$NON-NLS-1$ //$NON-NLS-2$

        assertFalse(version("8.7.1").isLessThan(version("8.7.x"))); //$NON-NLS-1$//$NON-NLS-2$

    }

    @Test
    public void testIsLessThan2() {
        DefaultMetadataVersion version = new DefaultMetadataVersion("8.12.4");
        assertTrue(version.isLessThan(version("9.0.0")));

        assertFalse(version.isLessThan(version("8.6.0")));
    }
}
