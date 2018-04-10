package org.komodo.rest.relational.response.metadata;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import javax.ws.rs.core.MediaType;

import org.komodo.rest.KRestEntity;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.StringUtils;

public final class RestMetadataConnectionStatus implements KRestEntity {

    /*

    {
      "connectionName": "myConnection",
      "vdbState": "MISSING" | "LOADING" | "ACTIVE" | "FAILED",
      "vdbName": "myConnectionBtlConn",
      "schemaState": "MISSING" | "LOADING" | "ACTIVE" | "FAILED",
      "schemaVdbName": "myConnectionbtlconnschemavdb",
      "schemaModelName": "myConnectionBtlConnSchemaModel",
      "errors": []
    }

    */

    public enum EntityState {

        ACTIVE,
        FAILED,
        LOADING,
        MISSING;

        /**
         * @param value the value whose <code>EntityState</code> is being requested
         * @return the entity state or {@link EntityState#MISSING} if not found
         */
        public static EntityState fromString( final String value ) {
            if ( StringUtils.isBlank( value ) ) {
                return MISSING;
            }

            if ( value.toUpperCase().equals( ACTIVE.toString() ) ) {
                return ACTIVE;
            }

            if ( value.toUpperCase().equals( FAILED.toString() ) ) {
                return FAILED;
            }

            if ( value.toUpperCase().equals( LOADING.toString() ) ) {
                return LOADING;
            }

            return MISSING;
        }
 
    }

    /**
     * The property key for the connection name.
     */
    public static final String CONNECTION_NAME = "connectionName";

    /**
     * The property key for a list of validity errors.
     */
    public static final String ERRORS = "errors";

    /**
     * The property key for the connection's workspace schema model name.
     */
    public static final String SCHEMA_MODEL_NAME = "schemaModelName";

    /**
     * The property key for the connection's workspace schema (i.e., tables, procedures, etc. are available) state.
     */
    public static final String SCHEMA_STATE = "schemaState";

    /**
     * The property key for the connection's workspace schema VDB name.
     */
    public static final String SCHEMA_VDB_NAME = "schemaVdbName";

    /**
     * The property key for the connection's server VDB name.
     */
    public static final String SERVER_VDB_NAME = "vdbName";

    /**
     * The property key for the connection's server VDB state.
     */
    public static final String SERVER_VDB_STATE = "vdbState";

    private String connectionName;
    private List< String > errors;
    private EntityState schemaState;
    private String schemaModelName;
    private String schemaVdbName;
    private String vdbName;
    private EntityState vdbState;

    /**
     * Constructor for use in deserialization.
     */
    public RestMetadataConnectionStatus() {
        // nothing to do
    }

    /**
     * Constructor for when the server VDB is missing. The schema workspace state 
     * will be {@link EntityState#MISSING} also.
     *
     * @param connectionName the connection name (cannot be <code>null</code>)
     */
    public RestMetadataConnectionStatus( final String connectionName ) {
        this.connectionName = connectionName;
    }

    /**
     * Uses the specified VDB to determine the server VDB status. The schema workspace state 
     * is set to {@link EntityState#MISSING}.
     *
     * @param connectionName the connection name (cannot be <code>null</code>)
     * @param vdb the server VDB (cannot be <code>null</code>)
     */
    public RestMetadataConnectionStatus( final String connectionName,
                                         final TeiidVdb vdb ) {
        this( connectionName );
        this.vdbName = vdb.getName();
        this.errors = vdb.getValidityErrors();

        if ( vdb.isActive() ) {
            this.vdbState = EntityState.ACTIVE;
        } else if ( vdb.hasFailed() ) {
            this.vdbState = EntityState.FAILED;
        } else if ( vdb.isLoading() ) {
            this.vdbState = EntityState.LOADING;
        }
    }

    /**
     * @param connectionName the connection name (cannot be <code>null</code>)
     * @param vdb the server VDB (cannot be <code>null</code>)
     * @param schemaState the workspace schema state (can be <code>null</code>)
     * @param schemaVdbName the workspace schema VDB name (can be empty)
     * @param schemaModelName the workspace schema model name (can be empty)
     */
    public RestMetadataConnectionStatus( final String connectionName,
                                         final TeiidVdb vdb,
                                         final EntityState schemaState,
                                         final String schemaVdbName,
                                         final String schemaModelName ) {
        this( connectionName, vdb );
        this.schemaState = schemaState;
        this.schemaVdbName = schemaVdbName;
        this.schemaModelName = schemaModelName;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object obj ) {
        if ( obj == null || !( obj instanceof RestMetadataConnectionStatus ) ) {
            return false;
        }

        final RestMetadataConnectionStatus that = ( RestMetadataConnectionStatus )obj;

        return Objects.equals( this.connectionName, that.connectionName )
               && Objects.equals( this.errors, that.errors )
               && Objects.equals( this.schemaModelName, that.schemaModelName )
               && Objects.equals(  this.schemaState, that.schemaState )
               && Objects.equals(  this.schemaVdbName, that.schemaVdbName )
               && Objects.equals( this.vdbName, that.vdbName )
               && Objects.equals( this.vdbState, that.vdbState );
    }

    /**
     * @return the connection name (can be empty)
     */
    public String getConnectionName() {
        return this.connectionName;
    }

    /**
     * @return the errors (never <code>null</code>)
     */
    public List<String> getErrors() {
        return this.errors == null ? Arrays.asList( StringUtils.EMPTY_ARRAY ) : this.errors;
    }

    /**
     * @return the schema state or {@link EntityState#MISSING} if not set
     */
    public EntityState getSchemaState() {
        return this.schemaState == null ? EntityState.MISSING : this.schemaState;
    }

    /**
     * @return the schema model name (can be empty)
     */
    public String getSchemaModelName() {
        return this.schemaModelName;
    }

    /**
     * @return the schema VDB name (can be empty)
     */
    public String getSchemaVdbName() {
        return this.schemaVdbName;
    }

    /**
     * @return the server VDB name (can be empty)
     */
    public String getServerVdbName() {
        return this.vdbName;
    }

    /**
     * @return the server VDB state or {@link EntityState#MISSING} if not set
     */
    public EntityState getServerVdbState() {
        return this.vdbState == null ? EntityState.MISSING : this.vdbState;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.KRestEntity#getXml()
     * @throws UnsupportedOperationException if called
     */
    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return Objects.hash( this.connectionName,
                             this.errors,
                             this.schemaModelName,
                             this.schemaState,
                             this.schemaVdbName,
                             this.vdbName,
                             this.vdbState );
    }

    /**
     * @param connectionName the connection name (can be empty)
     */
    public void setConnectionName( final String connectionName ) {
        this.connectionName = connectionName;
    }

    /**
     * @param errors the server VDB validity errors (can be <code>null</code>)
     */
    public void setErrors( final List< String > errors ) {
        this.errors = errors;
    }

    /**
     * @param schemaState the schema state (can be <code>null</code>)
     */
    public void setSchemaState( final EntityState schemaState ) {
        this.schemaState = schemaState == null ? EntityState.MISSING : schemaState;
    }

    /**
     * @param schemaModelName the workspace schema model name (can be empty)
     */
    public void setSchemaModelName( final String schemaModelName ) {
        this.schemaModelName = schemaModelName;
    }

    /**
     * @param schemaVdbName the workspace schema VDB name (can be empty)
     */
    public void setSchemaVdbName( final String schemaVdbName ) {
        this.schemaVdbName = schemaVdbName;
    }

    /**
     * @param vdbName the server VDB name (can be empty)
     */
    public void setServerVdbName( final String vdbName ) {
        this.vdbName = vdbName;
    }

    /**
     * @param vdbState the server VDB state (can be <code>null</code>)
     */
    public void setServerVdbState( final EntityState vdbState ) {
        this.vdbState = vdbState == null ? EntityState.MISSING : vdbState;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.KRestEntity#supports(javax.ws.rs.core.MediaType)
     */
    @Override
    public boolean supports( final MediaType mediaType ) {
        return MediaType.APPLICATION_JSON_TYPE.equals( mediaType );
    }

}
