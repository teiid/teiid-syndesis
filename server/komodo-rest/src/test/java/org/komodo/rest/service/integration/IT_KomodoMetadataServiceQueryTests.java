/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.rest.service.integration;

import org.jboss.arquillian.junit.Arquillian;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.core.repository.RepositoryImpl;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import org.komodo.spi.constants.StringConstants;
import org.komodo.test.utils.TestUtilities;

@RunWith(Arquillian.class)
@SuppressWarnings( {"javadoc", "nls"} )
public class IT_KomodoMetadataServiceQueryTests extends AbstractKomodoMetadataServiceTest implements StringConstants {

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
