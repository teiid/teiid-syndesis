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
package org.komodo.rest.relational.response;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.komodo.rest.AbstractKEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.spi.KException;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.metadata.MetadataInstance.Condition;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.version.MetadataVersion;

/**
 * A Metadata Server Status object that can be used by GSON to build a JSON document representation.
 */
public final class RestMetadataStatus extends AbstractKEntity {

    /**
     * Label used to describe whether metadata instance is available
     */
    public static final String AVAILABLE_LABEL = "available";

    /**
     * Label used to describe the version of the metadata instance
     */
    public static final String VERSION_LABEL = "version";

    /**
     * Label used to describe number of data sources
     */
    public static final String DATA_SOURCE_SIZE_LABEL = "dataSourceSize";

    /**
     * Label used to describe the names of the available data sources
     */
    public static final String DATA_SOURCE_NAMES_LABEL = "dataSourceNames";

    /**
     * Label used to describe number of data source drivers
     */
    public static final String DATA_SOURCE_DRIVER_SIZE_LABEL = "dataSourceDriverSize";

    /**
     * Label used to describe the names of the available data source drivers
     */
    public static final String DATA_SOURCE_DRIVERS_LABEL = "dataSourceDrivers";

    /**
     * Label used to describe number of translators
     */
    public static final String TRANSLATOR_SIZE_LABEL = "translatorSize";

    /**
     * Label used to describe the names of the available translators
     */
    public static final String TRANSLATOR_NAMES_LABEL = "translatorNames";

    /**
     * Label used to describe number of vdbs
     */
    public static final String VDB_SIZE_LABEL = "vdbSize";

    /**
     * Label used to describe the names of the available vdbs
     */
    public static final String VDB_NAMES_LABEL = "vdbNames";

    private List<RestConnectionDriver> sourceDrivers = new ArrayList<>();

    /**
     * Constructor for use when deserializing
     */
    public RestMetadataStatus() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri
     * @param mServer the metadata instance
     *
     * @throws KException if error occurs
     */
    public RestMetadataStatus(URI baseUri, MetadataInstance mServer) throws KException {
        super(baseUri);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().mServerStatusUri()));

        setVersion(mServer.getVersion());
        setAvailable(Condition.REACHABLE == mServer.getCondition());

        if (mServer != null) {
            Collection<TeiidDataSource> dataSources = mServer.getDataSources();
            setDataSourceSize(dataSources.size());
            setDataSourcesNames(dataSources);

            Collection<ConnectionDriver> dataSourceDrivers = mServer.getDrivers();
            setDataSourceDriverSize(dataSourceDrivers.size());
            if (dataSourceDrivers == null || dataSourceDrivers.isEmpty())
                this.sourceDrivers = Collections.emptyList();
            else {
                this.sourceDrivers = new ArrayList<>();
                for (ConnectionDriver driver : dataSourceDrivers) {
                    this.sourceDrivers.add(new RestConnectionDriver(driver));
                }
            }

            Collection<TeiidTranslator> translators = mServer.getTranslators();
            setTranslatorSize(translators.size());
            setTranslatorNames(translators);

            Collection<TeiidVdb> vdbs;
            try {
                vdbs = mServer.getVdbs();
                setVdbSize(vdbs.size());
                setVdbNames(vdbs);
            } catch (Exception e) {
                throw new KException(e);
            }
        }
    }

    public String getVersion() {
        Object versionObj = tuples.get(VERSION_LABEL);
        return versionObj != null ? versionObj.toString() : null;
    }

    protected void setVersion(MetadataVersion version) {
        tuples.put(VERSION_LABEL, version.toString());
    }

    public boolean isAvailable() {
        Object hasServer = tuples.get(AVAILABLE_LABEL);
        return hasServer != null ? Boolean.parseBoolean(hasServer.toString()) : false;
    }

    protected void setAvailable(boolean hasServer) {
        tuples.put(AVAILABLE_LABEL, hasServer);
    }

    public int getDataSourceSize() {
        Object size = tuples.get(DATA_SOURCE_SIZE_LABEL);
        return size != null ? Integer.parseInt(size.toString()) : 0;
    }

    protected void setDataSourceSize(int size) {
        tuples.put(DATA_SOURCE_SIZE_LABEL, size);
    }

    public String[] getDataSourceNames() {
        Object names = tuples.get(DATA_SOURCE_NAMES_LABEL);
        return names != null ? (String[])names : EMPTY_ARRAY;
    }

    protected void setDataSourcesNames(Collection<TeiidDataSource> dataSources) {
        List<String> names = new ArrayList<String>(dataSources.size());
        for (TeiidDataSource source : dataSources)
            names.add(source.getName());

        tuples.put(DATA_SOURCE_NAMES_LABEL, names.toArray(new String[0]));
    }

    public int getDataSourceDriverSize() {
        Object size = tuples.get(DATA_SOURCE_DRIVER_SIZE_LABEL);
        return size != null ? Integer.parseInt(size.toString()) : 0;
    }

    protected void setDataSourceDriverSize(int size) {
        tuples.put(DATA_SOURCE_DRIVER_SIZE_LABEL, size);
    }

    public List<RestConnectionDriver> getDataSourceDrivers() {
        return this.sourceDrivers;
    }

    public void setDataSourceDrivers(RestConnectionDriver[] dataSourceDrivers) {
        if (dataSourceDrivers == null || dataSourceDrivers.length == 0)
            this.sourceDrivers = Collections.emptyList();

        this.sourceDrivers = new ArrayList<>();
        for (RestConnectionDriver driver : sourceDrivers) {
            this.sourceDrivers.add(driver);
        }
    }

    public int getTranslatorSize() {
        Object size = tuples.get(TRANSLATOR_SIZE_LABEL);
        return size != null ? Integer.parseInt(size.toString()) : 0;
    }

    protected void setTranslatorSize(int size) {
        tuples.put(TRANSLATOR_SIZE_LABEL, size);
    }

    public String[] getTranslatorNames() {
        Object names = tuples.get(TRANSLATOR_NAMES_LABEL);
        return names != null ? (String[])names : EMPTY_ARRAY;
    }

    protected void setTranslatorNames(Collection<TeiidTranslator> translators) {
        List<String> names = new ArrayList<String>(translators.size());
        for (TeiidTranslator tr : translators)
            names.add(tr.getName());

        tuples.put(TRANSLATOR_NAMES_LABEL, names.toArray(new String[0]));
    }

    public int getVdbSize() {
        Object size = tuples.get(VDB_SIZE_LABEL);
        return size != null ? Integer.parseInt(size.toString()) : 0;
    }

    protected void setVdbSize(int size) {
        tuples.put(VDB_SIZE_LABEL, size);
    }

    public String[] getVdbNames() {
        Object names = tuples.get(VDB_NAMES_LABEL);
        return names != null ? (String[])names : EMPTY_ARRAY;
    }

    protected void setVdbNames(Collection<TeiidVdb> vdbs) {
        List<String> names = new ArrayList<String>(vdbs.size());
        for (TeiidVdb vdb : vdbs)
            names.add(vdb.getName());

        tuples.put(VDB_NAMES_LABEL, names.toArray(new String[0]));
    }
}
