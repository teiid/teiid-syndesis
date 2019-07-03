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


/**
 * This utility class provides mechanisms for computing the checksum.
 *
 * @since 8.0
 */
public class ChecksumUtils {
//
//    protected static final int BUFFER_SIZE = 1024;
//
//    /**
//     * Compute and return the checksum (using the default CRC-32 algorithm) of the contents on the specified stream. This method
//     * closes the stream upon completion.
//     * 
//     * @param stream the stream containing the contents for which the checksum is to be computed; may not be null
//     * @return the Checksum for the contents
//     * @throws AssertionError if <code>stream</code> is null
//     * @throws IOException if there is an error reading the stream
//     */
//    public static Checksum computeChecksum( InputStream stream ) throws IOException {
//        Checksum checksum = new CRC32();
//        computeChecksum(stream, checksum);
//        return checksum;
//    }
//
//    /**
//     * Compute the checksum of the contents on the specified stream using the supplied Checksum algorithm, and modify that
//     * Checksum instance with the checksum value. This method closes the stream upon completion.
//     * 
//     * @param stream the stream containing the contents for which the checksum is to be computed; may not be null
//     * @param algorithm the checksum algorithm to be used.
//     * @return the number of bytes from <code>stream</code> that were processed
//     * @throws AssertionError if <code>stream</code> or <code>algorithm</code> is null
//     * @throws IOException if there is an error reading the stream
//     */
//    public static long computeChecksum( InputStream stream,
//                                        Checksum algorithm ) throws IOException {
//        byte[] buffer = new byte[BUFFER_SIZE];
//        int n = 0;
//        long sizeInBytes = 0;
//
//        // Compute the checksum ...
//        IOException ioe = null;
//        try {
//            while ((n = stream.read(buffer)) > -1) {
//                algorithm.update(buffer, 0, n);
//                sizeInBytes += n;
//            }
//        } catch (IOException e) {
//            ioe = e;
//        } finally {
//            try {
//                stream.close();
//            } catch (IOException e) {
//                // Throw this only if there was no IOException from processing above
//                if (ioe == null) {
//                    ioe = e;
//                }
//            }
//        }
//        if (ioe != null) {
//            throw ioe;
//        }
//        return sizeInBytes;
//    }
//

    /**
     * Don't allow construction outside of this class.
     */
    private ChecksumUtils() {
        // nothing to do
    }
}
