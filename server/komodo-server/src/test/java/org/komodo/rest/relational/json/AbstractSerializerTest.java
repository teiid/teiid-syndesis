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
package org.komodo.rest.relational.json;

import java.net.URI;

import javax.ws.rs.core.UriBuilder;

import org.junit.Before;
import org.komodo.rest.json.JsonConstants;
import org.komodo.spi.KException;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.StringUtils;
import org.mockito.Mock;
import org.mockito.Mockito;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls" })
public abstract class AbstractSerializerTest implements JsonConstants {

    protected static final String BASE_URI_PREFIX = "http://localhost:8081/v1";

    protected static final URI MY_BASE_URI = UriBuilder.fromUri(BASE_URI_PREFIX).build();

    protected static final String WORKSPACE_DATA_PATH = "/workspace";
    protected static final String VDB_NAME = "vdb1";
    protected static final String VDB_DATA_PATH = "/workspace/vdbs/vdb1";
    protected static final String DATASERVICE_NAME = "dataservice1";
    protected static final String DATASERVICE_DATA_PATH = "/workspace/dataservices/dataservice1";
    protected static final String SEARCH = "/workspace/search?";

    @Mock
    protected UnitOfWork transaction;

    protected static String q(Object value) {
        return SPEECH_MARK + value + SPEECH_MARK;
    }

    protected static String colon() {
        return COLON + SPACE;
    }

    protected static String tab(int freq) {
        return StringUtils.tab(freq);
    }

    protected String pnl(Object value) {
        return value + NEW_LINE;
    }

    public AbstractSerializerTest() {
        super();
    }

    @Before
    public void basicInit() throws KException {
        transaction = Mockito.mock(UnitOfWork.class);
        Mockito.when(transaction.getState()).thenReturn(State.NOT_STARTED);

        UnitOfWork uow = Mockito.mock(UnitOfWork.class);
        Mockito.when(uow.getState()).thenReturn(State.NOT_STARTED);
    }

}
