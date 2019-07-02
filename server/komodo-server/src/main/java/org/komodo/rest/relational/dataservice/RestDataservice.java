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
package org.komodo.rest.relational.dataservice;

import java.net.URI;
import java.util.Properties;

import org.komodo.core.KomodoLexicon;
import org.komodo.openshift.BuildStatus;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A Dataservice that can be used by GSON to build a JSON document representation.
 */
public final class RestDataservice extends RestBasicEntity {

    /**
     * Label used to describe description
     */
    public static final String DESCRIPTION_LABEL = KomodoService.protectPrefix(KomodoLexicon.LibraryComponent.DESCRIPTION);

    /**
     * Label used to describe dataservice view modelName
     */
    public static final String DATASERVICE_VIEW_MODEL_LABEL = "serviceViewModel"; //$NON-NLS-1$

    /**
     * Label used to describe dataservice viewNames
     */
    public static final String DATASERVICE_VIEW_DEFINITIONS_LABEL = "serviceViewDefinitions"; //$NON-NLS-1$

    /**
     * Label used to describe dataservice vdbName
     */
    public static final String DATASERVICE_VDB_NAME_LABEL = "serviceVdbName"; //$NON-NLS-1$

    /**
     * Label used to describe dataservice vdbVersion
     */
    public static final String DATASERVICE_VDB_VERSION_LABEL = "serviceVdbVersion"; //$NON-NLS-1$

    /**
     * Label used to describe dataservice connection total
     */
    public static final String DATASERVICE_CONNECTION_TOTAL_LABEL = "connections"; //$NON-NLS-1$

    /**
     * Label used to describe dataservice published state
     */
    public static final String DATASERVICE_PUBLISHED_STATE_LABEL = "publishedState"; //$NON-NLS-1$
    
    /**
     * Label used to describe dataservice pod namespace
     */
    public static final String DATASERVICE_POD_NAMESPACE = "podNamespace"; //$NON-NLS-1$

    /**
     * Label used to describe dataservice publish pod name
     */
    public static final String DATASERVICE_PUBLISH_POD_NAME = "publishPodName"; //$NON-NLS-1$

    /**
     * Label used to describe dataservice odata host name
     */
    public static final String DATASERVICE_ODATA_HOST_NAME = "odataHostName"; //$NON-NLS-1$

    /**
     * fqn table option key
     */
    private final static String TABLE_OPTION_FQN = "teiid_rel:fqn"; //$NON-NLS-1$

    /**
     * schema model suffix
     */
    private final static String SCHEMA_MODEL_SUFFIX = "schemamodel"; //$NON-NLS-1$
    
    /**
     * Constructor for use when deserializing
     */
    public RestDataservice() {
        super();
        setkType(KomodoType.DATASERVICE);
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the vdb
     * @param dataService the dataService
     * @param exportXml whether xml should be exported
     * @param uow the transaction
     *
     * @throws KException if error occurs
     */
    public RestDataservice(URI baseUri, Dataservice dataService, boolean exportXml, UnitOfWork uow) throws KException {
        super(baseUri, dataService, uow, false);

        setDescription(dataService.getDescription(uow));

        addExecutionProperties(uow, dataService);

        Properties settings = getUriBuilder().createSettings(SettingNames.DATA_SERVICE_NAME, getId());
        URI parentUri = getUriBuilder().dataserviceParentUri(dataService, uow);
        getUriBuilder().addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, parentUri);

        Vdb serviceVdb = dataService.getServiceVdb(uow);
        if (serviceVdb != null) {
            setServiceVdbName(serviceVdb.getVdbName( uow ));
            setServiceVdbVersion(Integer.toString(serviceVdb.getVersion( uow )));
            setServiceViewModel(dataService.getServiceViewModelName(uow));
            setViewDefinitionNames(dataService.getViewDefinitionNames(uow));
        }

        Connection[] connections = dataService.getConnections(uow);
        setConnectionTotal(connections != null ? connections.length : 0);

        // Initialize the published state to NOTFOUND
        setPublishedState(BuildStatus.Status.NOTFOUND.name());

        addLink(new RestLink(LinkType.SELF, getUriBuilder().dataserviceUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().dataserviceUri(LinkType.PARENT, settings)));
        createChildLink();
        addLink(new RestLink(LinkType.VDBS, getUriBuilder().dataserviceUri(LinkType.VDBS, settings)));
        addLink(new RestLink(LinkType.CONNECTIONS, getUriBuilder().dataserviceUri(LinkType.CONNECTIONS, settings)));
    }

    /**
     * @return the VDB description (can be empty)
     */
    public String getDescription() {
        Object description = tuples.get(DESCRIPTION_LABEL);
        return description != null ? description.toString() : null;
    }

    /**
     * @param description the description to set
     */
    public void setDescription(String description) {
        tuples.put(DESCRIPTION_LABEL, description);
    }

    /**
     * @return the service view model name (can be empty)
     */
    public String getServiceViewModel() {
        Object modelName = tuples.get(DATASERVICE_VIEW_MODEL_LABEL);
        return modelName != null ? modelName.toString() : null;
    }

    /**
     * @param modelName the view model name to set
     */
    public void setServiceViewModel(String modelName) {
        tuples.put(DATASERVICE_VIEW_MODEL_LABEL, modelName);
    }

    /**
     * @return the service ViewDefinition names (can be empty)
     */
    public String[] getViewDefinitionNames() {
        return (String[])tuples.get(DATASERVICE_VIEW_DEFINITIONS_LABEL);
    }

    /**
     * @param viewDefinitionNames the service view names to set
     */
    public void setViewDefinitionNames(final String[] viewDefinitionNames) {
        tuples.put(DATASERVICE_VIEW_DEFINITIONS_LABEL, viewDefinitionNames);
    }

    /**
     * @return the service vdb name (can be empty)
     */
    public String getServiceVdbName() {
        Object serviceVdbName = tuples.get(DATASERVICE_VDB_NAME_LABEL);
        return serviceVdbName != null ? serviceVdbName.toString() : null;
    }

    /**
     * @param serviceVdbName the service vdb name to set
     */
    public void setServiceVdbName(String serviceVdbName) {
        tuples.put(DATASERVICE_VDB_NAME_LABEL, serviceVdbName);
    }

    /**
     * @return the service vdb version (can be empty)
     */
    public String getServiceVdbVersion() {
        Object version = tuples.get(DATASERVICE_VDB_VERSION_LABEL);
        return version != null ? version.toString() : "1"; //$NON-NLS-1$
    }

    /**
     * @param version the version to set
     */
    public void setServiceVdbVersion( final String version) {
        tuples.put(DATASERVICE_VDB_VERSION_LABEL, version);
    }

    /**
     * @return the number of connections (can be empty)
     */
    public int getConnectionTotal() {
        Object total = tuples.get(DATASERVICE_CONNECTION_TOTAL_LABEL);
        return total != null ? Integer.parseInt(total.toString()) : 0;
    }

    /**
     * @param total the number of connections
     */
    public void setConnectionTotal(int total) {
        tuples.put(DATASERVICE_CONNECTION_TOTAL_LABEL, total);
    }

    /**
     * @return the service published state (never empty)
     */
    public String getPublishedState() {
        Object publishedState = tuples.get(DATASERVICE_PUBLISHED_STATE_LABEL);
        return publishedState != null ? publishedState.toString() : null;
    }
    
    /**
     * @param publishedState the published state
     */
    public void setPublishedState(String publishedState) {
        tuples.put(DATASERVICE_PUBLISHED_STATE_LABEL, publishedState);
    }

    /**
     * @return the pod namespace (can be empty)
     */
    public String getPodNamespace() {
        Object podNamespace = tuples.get(DATASERVICE_POD_NAMESPACE);
        return podNamespace != null ? podNamespace.toString() : null;
    }

    /**
     * @param podNamesapce the service pod namespace to set
     */
    public void setPodNamespace(String podNamespace) {
        tuples.put(DATASERVICE_POD_NAMESPACE, podNamespace);
    }

    /**
     * @return the service pod name (can be empty)
     */
    public String getPublishPodName() {
        Object publishPodName = tuples.get(DATASERVICE_PUBLISH_POD_NAME);
        return publishPodName != null ? publishPodName.toString() : null;
    }

    /**
     * @param publishPodName the service pod name to set
     */
    public void setPublishPodName(String publishPodName) {
        tuples.put(DATASERVICE_PUBLISH_POD_NAME, publishPodName);
    }
    
    /**
     * @return the service pod name (can be empty)
     */
    public String getOdataHostName() {
        Object odataHostName = tuples.get(DATASERVICE_ODATA_HOST_NAME);
        return odataHostName != null ? odataHostName.toString() : null;
    }

    /**
     * @param publishPodName the service pod name to set
     */
    public void setOdataHostName(String odataHostName) {
        tuples.put(DATASERVICE_ODATA_HOST_NAME, odataHostName);
    }
    
}
