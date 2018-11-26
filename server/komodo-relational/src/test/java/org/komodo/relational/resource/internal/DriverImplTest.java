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
package org.komodo.relational.resource.internal;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import java.io.InputStream;
import java.util.Properties;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.resource.Driver;
import org.komodo.spi.repository.KomodoType;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;
import org.komodo.spi.lexicon.datavirt.DataVirtLexicon;

@SuppressWarnings( "javadoc" )
public final class DriverImplTest extends RelationalModelTest {

    private static int _contentLength;
    private static byte[] _content;

    private Driver driver;

    @BeforeClass
    public static void initContent() throws Exception {
        final InputStream contentStream = TestUtilities.mySqlDriver();
        assertNotNull( contentStream );

        _contentLength = contentStream.available();
        _content = FileUtils.write( contentStream );
    }

    @Before
    public void init() throws Exception {
        this.driver = createDriver(TestUtilities.MYSQL_DRIVER_FILENAME, null, _content);
    }

    @Test
    public void shouldHaveCorrectContent() throws Exception {
        final InputStream is = this.driver.getContent( getTransaction() );
        assertEquals( _contentLength, is.available() );

        byte[] storedContent = FileUtils.write( is );
        assertThat( storedContent.length, is( _content.length ) );
        assertThat( storedContent, is( _content ) );
    }

    @Test
    public void shouldHaveId() throws Exception {
        assertThat(this.driver.getName(getTransaction()), is(TestUtilities.MYSQL_DRIVER_FILENAME));
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat(this.driver.getPrimaryType(getTransaction()).getName(), is(DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE));
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.driver.getTypeIdentifier(getTransaction()), is(KomodoType.DRIVER));
    }

    @Test
    public void shouldExport() throws Exception {
        final long contentChkSum = TestUtilities.checksum(_content);
        final byte[] exportBytes = this.driver.export(getTransaction(), new Properties());
        final long exportChkSum = TestUtilities.checksum(exportBytes);
        assertEquals(contentChkSum, exportChkSum);

        final String suffix = this.driver.getDocumentType(getTransaction()).toString();
        assertEquals(JAR, suffix);
    }
}
