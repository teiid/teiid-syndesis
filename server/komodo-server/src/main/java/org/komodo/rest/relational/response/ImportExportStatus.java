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
package org.komodo.rest.relational.response;

import javax.ws.rs.core.MediaType;
import org.komodo.rest.KRestEntity;

public class ImportExportStatus implements KRestEntity {

    /**
     * Label for name
     */
    public static final String NAME_LABEL = "Name"; //$NON-NLS-1$

    /**
     * Label for type
     */
    public static final String TYPE_LABEL = "type"; //$NON-NLS-1$

    /**
     * Label for downloadable
     */
    public static final String DOWNLOADABLE_LABEL = "downloadable"; //$NON-NLS-1$

    /**
     * Label for the size of the downloadable
     */
    public static final String DOWNLOADABLE_SIZE_LABEL = "downloadableSize"; //$NON-NLS-1$

    /**
     * Label for the content
     */
    public static final String CONTENT_LABEL = "content"; //$NON-NLS-1$

    /**
     * Label for the message
     */
    public static final String MESSAGE_LABEL = "message"; //$NON-NLS-1$

    /**
     * Label for the success flag
     */
    public static final String SUCCESS_LABEL = "success"; //$NON-NLS-1$

    private String name;

    private String type;

    private boolean downloadable;

    private String content;

    private String message;

    private boolean success;

    private long downloadableSize;

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    /**
     * @return has downloadable
     */
    public boolean hasDownloadable() {
        return downloadable;
    }

    /**
     * @param downloadable
     */
    public void setDownloadable(boolean downloadable) {
        this.downloadable = downloadable;
    }

    /**
     * @return content if downloadable or null
     */
    public String getContent() {
        return content;
    }

    /**
     * @param content
     */
    public void setContent(String content) {
        this.content = content;
    }

    /**
     * @return message if provided or null
     */
    public String getMessage() {
        return message;
    }

    /**
     * @param message the message
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * @return success
     */
    public boolean isSuccess() {
        return success;
    }

    /**
     * @param success
     */
    public void setSuccess(boolean success) {
        this.success = success;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public long getDownloadableSize() {
        return this.downloadableSize;
    }

    public void setDownloadableSize(long length) {
        this.downloadableSize = length;
    }
}
