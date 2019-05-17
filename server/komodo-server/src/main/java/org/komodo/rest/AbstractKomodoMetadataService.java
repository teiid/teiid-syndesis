package org.komodo.rest;

import java.text.MessageFormat;
import java.util.Collection;

import org.komodo.core.KEngine;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.relational.response.metadata.RestSyndesisSourceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.KLog;

public abstract class AbstractKomodoMetadataService implements V1Constants {

//    private static final String CONNECTION_VDB_PATTERN = "{0}btlconn"; //$NON-NLS-1$
//
//    private static final String SCHEMA_MODEL_NAME_PATTERN = "{0}schemamodel"; //$NON-NLS-1$
//    private static final String SCHEMA_VDB_NAME_PATTERN = "{0}schemavdb"; //$NON-NLS-1$

    /**
     * Time to wait after deploying/undeploying an artifact from the metadata instance
     */
    public final static int DEPLOYMENT_WAIT_TIME = 10000;

	private KEngine kengine;

    protected static final KLog LOGGER = KLog.getLogger();

	public AbstractKomodoMetadataService(KEngine kengine) {
		super();
		this.kengine = kengine;
	}
	
//	private synchronized MetadataInstance getMetadataInstance() throws KException {
//        return this.kengine.getMetadataInstance();
//    }

//    private String getSchema(UnitOfWork uow, String vdbName, String modelName) throws Exception {
//        MetadataInstance mServer = getMetadataInstance();
//        return mServer.getSchema(vdbName, "1", modelName); //$NON-NLS-1$
//    }

//    private boolean hasDriver(String driverName) throws Exception {
//        boolean hasDriver = false;
//
//        try {
//            Collection<ConnectionDriver> drivers = getMetadataInstance().getDataSourceDrivers();
//            for (ConnectionDriver driver : drivers) {
//                if (driver.getName().startsWith(driverName)) {
//                    hasDriver = true;
//                    break;
//                }
//            }
//
//            return hasDriver;
//
//        } catch (KException ex) {
//            this.kengine.getErrorHandler().error(ex);
//
//            throw ex;
//        }
//    }

//    private boolean hasDynamicVdb(String vdbName) throws Exception {
//        boolean hasVdb = false;
//
//        try {
//            Collection<TeiidVdb> vdbs = getMetadataInstance().getVdbs();
//            for (TeiidVdb vdb : vdbs) {
//                if (vdb.getName().startsWith(vdbName)) {
//                    hasVdb = true;
//                    break;
//                }
//            }
//
//            return hasVdb;
//
//        } catch (KException ex) {
//            this.kengine.getErrorHandler().error(ex);
//            throw ex;
//        }
//    }

//    private boolean hasDataSource(String dataSourceName) throws Exception {
//        boolean hasDataSource = false;
//
//        try {
//            Collection<TeiidDataSource> datasources = getMetadataInstance().getDataSources();
//            for (TeiidDataSource datasource : datasources) {
//                if (datasource.getName().startsWith(dataSourceName)) {
//                    hasDataSource = true;
//                    break;
//                }
//            }
//
//            return hasDataSource;
//
//        } catch (KException ex) {
//            this.kengine.getErrorHandler().error(ex);
//            throw ex;
//        }
//    }

//    private String extractServiceVdbName(UnitOfWork uow, WorkspaceManager mgr, String dsPath) throws KException {
//    	Repository repo = this.kengine.getDefaultRepository();
//        KomodoObject dsObject = repo.getFromWorkspace(uow, dsPath);
//        if (dsObject == null)
//            return null; // Not a path in the workspace
//
//        Dataservice dService = mgr.resolve(uow, dsObject, Dataservice.class);
//        if (dService == null)
//            return null; // Not a data service
//
//        Vdb vdb = dService.getServiceVdb(uow);
//        if (vdb == null)
//            return null;
//
//        return vdb.getVdbName(uow);
//    }

    /**
     * Deploy / re-deploy a VDB to the metadata instance for the provided teiid data source.
     * @param uow the transaction
     * @param teiidSource the teiidSource
     * @return the DeployStatus from deploying the VDB
     * @throws KException
     * @throws InterruptedException
     */
//    private DeployStatus doDeploySourceVdb( final UnitOfWork uow,
//                                            TeiidDataSource teiidSource ) throws KException, InterruptedException {
//        assert( uow.getState() == State.NOT_STARTED );
//        assert( teiidSource != null );
//
//        // Get necessary info from the source
//        String sourceName = teiidSource.getName();
//        String jndiName = teiidSource.getJndiName();
//        String driverName = teiidSource.getType();
//        
//        // Name of VDB to be created is based on the source name
//        String vdbName = getWorkspaceSourceVdbName( sourceName );
//        
//        // VDB is created in the repository.  If it already exists, delete it
//        Repository repo = this.kengine.getDefaultRepository();
//        final WorkspaceManager mgr = WorkspaceManager.getInstance( repo, uow );
//        String repoPath = repo.komodoWorkspace( uow ).getAbsolutePath();
//        
//        final Vdb existingVdb = findVdb( uow, vdbName );
//
//        if ( existingVdb != null ) {
//            mgr.delete(uow, existingVdb);
//        }
//        
//        // delete schema VDB if it exists
//        final Vdb schemaVdb = findWorkspaceSchemaVdb( uow, teiidSource );
//
//        if ( schemaVdb != null ) {
//            mgr.delete( uow, schemaVdb );
//        }
//
//        // Create new VDB
//        String vdbPath = repoPath + "/" + vdbName; //$NON-NLS-1$
//        final Vdb vdb = mgr.createVdb( uow, null, vdbName, vdbPath );
//        vdb.setDescription(uow, "Vdb for source "+teiidSource); //$NON-NLS-1$
//                    
//        // Add model to the VDB
//        Model model = vdb.addModel(uow, getSchemaModelName(sourceName));
//        model.setModelType(uow, Model.Type.PHYSICAL);
//        model.setProperty(uow, "importer.TableTypes", "TABLE,VIEW"); //$NON-NLS-1$ //$NON-NLS-2$
//        model.setProperty(uow, "importer.UseQualifiedName", "true");  //$NON-NLS-1$//$NON-NLS-2$
//        model.setProperty(uow, "importer.UseCatalogName", "false");  //$NON-NLS-1$//$NON-NLS-2$
//        model.setProperty(uow, "importer.UseFullSchemaName", "false");  //$NON-NLS-1$//$NON-NLS-2$
//        if (teiidSource.getPropertyValue("schema") != null) {
//        	model.setProperty(uow, "importer.schemaPattern", teiidSource.getPropertyValue("schema"));  //$NON-NLS-1$//$NON-NLS-2$
//        }
//        
//        // Add model source to the model
//        final String modelSourceName = teiidSource.getName();
//        ModelSource modelSource = model.addSource(uow, modelSourceName);
//        modelSource.setJndiName(uow, jndiName);
//        modelSource.setTranslatorName(uow, driverName);
//        // TODO: re-implement, needed for publishing
//        // modelSource.setAssociatedConnection(uow, connection);
//        
//        // Deploy the VDB
//        DeployStatus deployStatus = vdb.deploy(uow);
//        
//        // Wait for deployment to complete
//        Thread.sleep(DEPLOYMENT_WAIT_TIME);
//        
//        return deployStatus;
//    }
    
    /**
     * Add model to the schema vdb
     * @param uow the transaction
     * @param schemaVdb the schema VDB
     * @param dataSource the teiid dataSource
     * @param schemaModelName the name for the schema model being created
     * @return the created schema model
     * @throws KException
     */
//    private Model addModelToSchemaVdb(final UnitOfWork uow, final Vdb schemaVdb, final TeiidDataSource dataSource, final String schemaModelName) throws KException {
//        // create schema model
//        Model schemaModel = schemaVdb.addModel( uow, schemaModelName );
//        
//        // Make a copy of the workspace syndesis source vdb model source under the syndesis source schema vdb model
//        final ModelSource workspaceVdbModelSource = findWorkspaceSyndesisSourceVdbModelSource( uow, dataSource );
//        if( workspaceVdbModelSource != null ) {
//            ModelSource mdlSource = schemaModel.addSource(uow, workspaceVdbModelSource.getName(uow));
//            mdlSource.setJndiName(uow, workspaceVdbModelSource.getJndiName(uow));
//            mdlSource.setTranslatorName(uow, workspaceVdbModelSource.getTranslatorName(uow));
//            // TODO: re-implement, needed for publishing
//            // mdlSource.setAssociatedConnection(uow, workspaceVdbModelSource.getOriginConnection(uow));
//        }
//        
//        return schemaModel;
//    }

    /**
     * Find the VDB in the workspace for the specified teiid dataSource
     * @param uow the transaction
     * @param dataSource the teiid data source
     * @return the workspace VDB
     * @throws KException
     */
//    private Vdb findWorkspaceSyndesisSourceVdb( final UnitOfWork uow,
//                                                final TeiidDataSource dataSource ) throws KException {
//        final String dataSourceName = dataSource.getName( );
//
//        final String wsSourceVdbName = this.getWorkspaceSourceVdbName(dataSourceName);
//
//        Vdb vdb = findVdb(uow, wsSourceVdbName);
//        return vdb;
//    }
    
    /**
     * Find the Vdb ModelSource for the workspace syndesis source VDB
     * @param uow the transaction
     * @param dataSource the teiid data source
     * @return the workspace VDB
     * @throws KException
     */
//    private ModelSource findWorkspaceSyndesisSourceVdbModelSource( final UnitOfWork uow,
//                                                                   final TeiidDataSource dataSource ) throws KException {
//        ModelSource modelSource = null;
//        
//        final Vdb vdb = findWorkspaceSyndesisSourceVdb( uow, dataSource );
//
//        if ( vdb != null ) {
//            final String dataSourceName = dataSource.getName( );
//            final String schemaModelName = getSchemaModelName( dataSourceName );
//            final Model[] models = vdb.getModels(uow, schemaModelName);
//
//            Model model = null;
//            if ( models.length != 0 ) {
//                model = models[ 0 ];
//            }
//            
//            if( model != null ) {
//                final String schemaModelSourceName = dataSource.getName();
//                final ModelSource[] modelSources = model.getSources(uow, schemaModelSourceName);
//                if ( modelSources.length != 0 ) {
//                    modelSource = modelSources[ 0 ];
//                }
//            }
//        }
//
//        return modelSource;
//    }

    /**
     * Find the schema VDB in the workspace for the specified teiid source
     * @param uow the transaction
     * @param dataSource the teiid datasource
     * @return the VDB
     * @throws KException
     */
//    private Vdb findWorkspaceSchemaVdb( final UnitOfWork uow,
//                                        final TeiidDataSource dataSource ) throws KException {
//        final String dataSourceName = dataSource.getName( );
//        final String schemaVdbName = getSchemaVdbName( dataSourceName );
//
//        Vdb vdb = findVdb(uow, schemaVdbName);
//        return vdb;
//    }
    
    /**
     * Find the schema VDB model in the workspace for the specified teiid source
     * @param uow the transaction
     * @param dataSource the teiid datasource
     * @return the Model
     * @throws KException
     */
//    private Model findSchemaModel( final UnitOfWork uow,
//                                   final TeiidDataSource dataSource ) throws KException {
//        final Vdb vdb = findWorkspaceSchemaVdb( uow, dataSource );
//
//        if ( vdb != null ) {
//            final String dataSourceName = dataSource.getName( );
//            final String schemaModelName = getSchemaModelName( dataSourceName );
//            final Model[] models = vdb.getModels( uow, schemaModelName );
//
//            if ( models.length != 0 ) {
//                return models[ 0 ];
//            }
//        }
//
//        return null;
//    }

    /**
     * Find the teiid datasource with the specified name
     * @param sourceName the source name
     * @return the teiid datasource
     * @throws KException
     */
//    private TeiidDataSource findTeiidSource( final String sourceName ) throws KException {
//        return getMetadataInstance().getDataSource(sourceName);
//    }

    /**
     * Find the deployed Vdb for the specified source
     * @param sourceName the source name
     * @return the TeiidVdb
     * @throws KException
     */
//    private TeiidVdb findDeployedVdb( final String sourceName ) throws KException {
//        final String sourceVdbName = getWorkspaceSourceVdbName( sourceName );
//        return getMetadataInstance().getVdb( sourceVdbName );
//    }


    /**
     * Generate schema VDB name, given the name of the source
     * @param sourceName the source name
     * @return the schema VDB name
     */
//    private String getSchemaVdbName( final String sourceName ) {
//        return MessageFormat.format( SCHEMA_VDB_NAME_PATTERN, sourceName.toLowerCase() );
//    }

    /**
     * Generate a workspace source vdb name, given the name of the source
     * @param sourceName the source name
     * @return the source vdb name
     */
//    private String getWorkspaceSourceVdbName( final String sourceName ) {
//        return MessageFormat.format( CONNECTION_VDB_PATTERN, sourceName.toLowerCase() );
//    }

    /**
     * Generate schema VDB model name, given the name of the source
     * @param sourceName the source name
     * @return the schema VDB model name
     */
//    private String getSchemaModelName( final String sourceName ) {
//        return MessageFormat.format( SCHEMA_MODEL_NAME_PATTERN, sourceName.toLowerCase() );
//    }


    /**
     * Generate the path for the node, given the segments and the position within the segments
     * @param iPosn the index position within the segments
     * @param segments the array of segments
     * @return the node path (segment0/segment1/etc)
     */
//    private String createSchemaNodePath(int iPosn, String[] segments) {
//        StringBuilder sb = new StringBuilder();
//        if(segments!=null && segments.length > 0) {
//            for (int i = 0; i < segments.length; i++) {
//                if(i < iPosn) {
//                    sb.append(segments[i]+"/"); //$NON-NLS-1$
//                } else {
//                    sb.append(segments[i]);
//                    break;
//                }
//            }
//        }
//        return sb.toString();
//    }

    /**
     * Split the segment apart and return the name
     * @param segment the segment (eg "table=customer")
     * @return the name (eg "customer")
     */
//    private String getSegmentName(String segment) {
//        String[] parts = segment.split(EQUALS);
//        return parts[1].trim();
//    }
//    
    /**
     * Split the segment apart and return the type
     * @param segment the segment (eg "table=customer")
     * @return the type (eg "table")
     */
//    private String getSegmentType(String segment) {
//        String[] parts = segment.split(EQUALS);
//        return parts[0].trim();
//    }
    
    /**
     * Set the schema availability for the provided RestSyndesisSourceStatus 
     * @param uow the transaction
     * @param status the RestSyndesisSourceStatus
     * @throws Exception if error occurs
     */
//    private void setSchemaStatus( final UnitOfWork uow,
//                                  final RestSyndesisSourceStatus status ) throws Exception {
//        // Name of schema vdb based on source name
//        final String srcName = status.getSourceName();
//        final String schemaVdbName = getSchemaVdbName( srcName );
//
//        // Get the workspace schema VDB
//        Vdb vdb = findVdb(uow, schemaVdbName);
//
//        // If no vdb found, then status is not set
//        if ( vdb != null ) {
//            status.setSchemaVdbName( vdb.getName(uow) );
//
//            // there should be one model
//            final String schemaModelName = getSchemaModelName( srcName );
//            final Model[] models = vdb.getModels( uow, schemaModelName );
//
//            if ( models.length > 0 ) {
//                final Model schemaModel = models[ 0 ];
//                status.setSchemaModelName( schemaModelName );
//
//                // if model has children the DDL has been sequenced
//                if ( schemaModel.hasChildren( uow ) ) {
//                    // assume sequencer ran successfully
//                    status.setSchemaState( RestSyndesisSourceStatus.EntityState.ACTIVE );
//                } else if ( schemaModel.hasProperty( uow, VdbLexicon.Model.MODEL_DEFINITION ) ) {
//                    // assume sequencer is running but could have failed
//                    status.setSchemaState( RestSyndesisSourceStatus.EntityState.LOADING );
//                }
//            } else {
//                // Since VDB and model are created in the same transaction this should never happen.
//                // Would be nice to be able to get here if we can detect the DDL sequencing failed.
//                status.setSchemaState( RestSyndesisSourceStatus.EntityState.FAILED );
//            }
//        }
//    }
    
    protected Vdb findVdb(UnitOfWork uow, String vdbName) throws KException {
        if (! getWorkspaceManager(uow).hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
            return null;
        }

        final KomodoObject kobject = getWorkspaceManager(uow).getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
        final Vdb vdb = getWorkspaceManager(uow).resolve( uow, kobject, Vdb.class );

        return vdb;
    }
    
	protected WorkspaceManager getWorkspaceManager(UnitOfWork transaction) throws KException {
		Repository repo = this.kengine.getDefaultRepository();
		return WorkspaceManager.getInstance(repo, transaction);
	}
    
    protected KEngine getKEngine() {
    	return this.kengine;
    }

}

