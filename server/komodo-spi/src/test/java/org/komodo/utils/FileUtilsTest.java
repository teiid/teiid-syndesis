package org.komodo.utils;

import java.io.File;
import java.io.InputStream;

import org.junit.Test;

public final class FileUtilsTest {

    @Test
    public void shouldNotFailWhenDestinationDirectoryNameIsLess3Chars() throws Exception {
        final File parent = new File( System.getProperty( "java.io.tmpdir" ) );
        final File destination = new File( parent, "a" );
        destination.mkdir();
        destination.deleteOnExit();

        final InputStream zipStream = FileUtilsTest.class.getResourceAsStream("/JoinServiceSameTableNames.zip");
        FileUtils.zipExtract( zipStream, destination );
    }

}
