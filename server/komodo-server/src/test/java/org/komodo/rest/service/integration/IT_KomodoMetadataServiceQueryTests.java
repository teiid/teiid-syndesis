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
package org.komodo.rest.service.integration;

import org.jboss.arquillian.junit.Arquillian;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.core.repository.RepositoryImpl;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import org.komodo.test.utils.TestUtilities;

@RunWith(Arquillian.class)
@SuppressWarnings( {"javadoc", "nls"} )
public class IT_KomodoMetadataServiceQueryTests extends AbstractKomodoMetadataServiceTest {

    private static final String USSTATES_TARGET = "usstates";

    private static final String STATES_QUERY = "SELECT * FROM states";

    @Override
    protected int getTestTotalInClass() {
        return 3;
    }

    @Before
    public void setup() throws Exception {
        importDataService();

        if (! getMetadataInstance().hasVdb(TestUtilities.US_STATES_VDB_NAME))
            deployDataService();

        //
        // Give the vdb time to become active
        //
        waitForVdb();
    }

    @Test
    public void shouldQueryTeiid() throws Exception {
        KomodoQueryAttribute queryAttr = new KomodoQueryAttribute();
        queryAttr.setQuery(STATES_QUERY);
        queryAttr.setTarget(USSTATES_TARGET);

        queryDataService(queryAttr, 51, "AK");
    }

    @Test
    public void shouldQueryTeiidWithLimitAndOffset() throws Exception {
        KomodoQueryAttribute queryAttr = new KomodoQueryAttribute();
        queryAttr.setQuery(STATES_QUERY);
        queryAttr.setTarget(USSTATES_TARGET);

        int offset = 5;
        int limit = 10;
        queryAttr.setLimit(limit);
        queryAttr.setOffset(offset);

        queryDataService(queryAttr, limit, "CA");
    }

    @Test
    public void shouldQueryTeiidUsingDataservice() throws Exception {
        String dsPath = RepositoryImpl.komodoWorkspacePath(null) + FORWARD_SLASH +
                                USER_NAME + FORWARD_SLASH + "UsStatesService";

        KomodoQueryAttribute queryAttr = new KomodoQueryAttribute();
        queryAttr.setQuery(STATES_QUERY);
        queryAttr.setTarget(dsPath);

        queryDataService(queryAttr, 51, "AK");
    }
}
