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

import java.net.URI;
import java.util.Properties;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * A VDB import that can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * <code>
 * {
 *     "id" : "MyImportVdb",
 *     "version" : "1",
 *     "importDataPolicies" : "true"
 * }
 * </code>
 * </pre>
 */
public final class RestVdbImport extends RestBasicEntity {

    /**
     * Label used to describe name
     */
    public static final String NAME_LABEL = KomodoService.protectPrefix(VdbLexicon.ImportVdb.IMPORT_VDB);

    /**
     * Label used to describe import data policies
     */
    public static final String IMPORT_POLICIES_LABEL = KomodoService.protectPrefix(VdbLexicon.ImportVdb.IMPORT_DATA_POLICIES);

    /**
     * Label used to describe version
     */
    public static final String VERSION_LABEL = KomodoService.protectPrefix(VdbLexicon.ImportVdb.VERSION);

    /**
     * And empty array of VDB imports.
     */
    public static final RestVdbImport[] NO_IMPORTS = new RestVdbImport[ 0 ];

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbImport() {
        setVersion(Vdb.DEFAULT_VERSION);
        setImportDataPolicies(VdbImport.DEFAULT_IMPORT_DATA_POLICIES);
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param vdbImport the import
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestVdbImport(URI baseUri, VdbImport vdbImport, UnitOfWork uow) throws KException {
        super(baseUri, vdbImport, uow);

        setName(vdbImport.getName(uow));
        setVersion(vdbImport.getVersion(uow));
        setImportDataPolicies(vdbImport.isImportDataPolicies(uow));

        Vdb vdb = ancestor(vdbImport, Vdb.class, uow);
        ArgCheck.isNotNull(vdb);
        String vdbName = vdb.getName(uow);

        Properties settings = getUriBuilder().createSettings(SettingNames.VDB_NAME, vdbName);
        getUriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, getUriBuilder().vdbParentUri(vdb, uow));
        getUriBuilder().addSetting(settings, SettingNames.IMPORT_NAME, getId());

        addLink(new RestLink(LinkType.SELF, getUriBuilder().vdbImportUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().vdbImportUri(LinkType.PARENT, settings)));
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        Object value = tuples.get(NAME_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @param newName
     *        the new VDB import name (can be empty)
     */
    public void setName( final String newName ) {
        tuples.put(NAME_LABEL, newName);
    }

    /**
     * @return the VDB version
     */
    public int getVersion() {
        Object value = tuples.get(VERSION_LABEL);
        return value != null ? Integer.parseInt(value.toString()) : Vdb.DEFAULT_VERSION;
    }

    /**
     * @param newVersion
     *        the new VDB import version
     */
    public void setVersion( final int newVersion ) {
        tuples.put(VERSION_LABEL, newVersion);
    }

    /**
     * @return <code>true</code> if importing data policies
     */
    public boolean isImportDataPolicies() {
        Object value = tuples.get(IMPORT_POLICIES_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : VdbImport.DEFAULT_IMPORT_DATA_POLICIES;
    }

    /**
     * @param newImportDataPolicies
     *        the new import data policies
     */
    public void setImportDataPolicies( final boolean newImportDataPolicies ) {
        tuples.put(IMPORT_POLICIES_LABEL, newImportDataPolicies);
    }
}
