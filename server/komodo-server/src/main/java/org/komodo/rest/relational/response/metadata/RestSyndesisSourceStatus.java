package org.komodo.rest.relational.response.metadata;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import javax.ws.rs.core.MediaType;

import org.komodo.metadata.runtime.TeiidVdb;
import org.komodo.rest.KRestEntity;
import org.komodo.utils.StringUtils;

/**
 * The Syndesis source status.  Determine the following for syndesis source
 * - does it have corresponding teiid source
 * - the corresponding teiid source vdb state, and details
 * - the corresponding schema vdb state, and details
 */
public final class RestSyndesisSourceStatus implements KRestEntity {

    /*

    {
      "sourceName": "syndesisSrcName",
      "hasTeiidSource": "true | false",
      "vdbState": "MISSING" | "LOADING" | "ACTIVE" | "FAILED",
      "vdbName": "myConnectionBtlConn",
      "schemaState": "MISSING" | "LOADING" | "ACTIVE" | "FAILED",
      "schemaVdbName": "syndesissrcnamebtlconnschemavdb",
      "schemaModelName": "syndesisSrcNameBtlConnSchemaModel",
      "errors": []
    }

    */

    /**
     * Enumeration for state
     */
    public enum EntityState {

        /**
         * Active state
         */
        ACTIVE,
        /**
         * Failed state
         */
        FAILED,
        /**
         * Loading state
         */
        LOADING,
        /**
         * Missing state
         */
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
     * The property key for the syndesis source name.
     */
    public static final String SOURCE_NAME = "sourceName"; //$NON-NLS-1$

    /**
     * The property key for has teiid source
     */
    public static final String HAS_TEIID_SOURCE = "hasTeiidSource"; //$NON-NLS-1$

    /**
     * The property key for a list of validity errors.
     */
    public static final String ERRORS = "errors"; //$NON-NLS-1$

    /**
     * The property key for the connection's workspace schema model name.
     */
    public static final String SCHEMA_MODEL_NAME = "schemaModelName"; //$NON-NLS-1$

    /**
     * The property key for the connection's workspace schema (i.e., tables, procedures, etc. are available) state.
     */
    public static final String SCHEMA_STATE = "schemaState"; //$NON-NLS-1$

    /**
     * The property key for the connection's workspace schema VDB name.
     */
    public static final String SCHEMA_VDB_NAME = "schemaVdbName"; //$NON-NLS-1$

    /**
     * The property key for the connection's server VDB name.
     */
    public static final String SERVER_VDB_NAME = "vdbName"; //$NON-NLS-1$

    /**
     * The property key for the connection's server VDB state.
     */
    public static final String SERVER_VDB_STATE = "vdbState"; //$NON-NLS-1$

    private String sourceName;
    private boolean hasTeiidSource = false;
    private List< String > errors;
    private EntityState schemaState = EntityState.MISSING;
    private String schemaModelName;
    private String vdbName;
    private EntityState vdbState = EntityState.MISSING;

    /**
     * Constructor for use in deserialization.
     */
    public RestSyndesisSourceStatus() {
        // nothing to do
    }

    /**
     * Constructor with name only.  Server VDB state is set to {@link EntityState#MISSING}.  Schema state is set to {@link EntityState#MISSING}. 
     * @param sourceName the syndesis source name (cannot be <code>null</code>)
     */
    public RestSyndesisSourceStatus( final String sourceName ) {
        this.sourceName = sourceName;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object obj ) {
        if ( obj == null || !( obj instanceof RestSyndesisSourceStatus ) ) {
            return false;
        }

        final RestSyndesisSourceStatus that = ( RestSyndesisSourceStatus )obj;

        return Objects.equals( this.sourceName, that.sourceName )
               && Objects.equals( this.hasTeiidSource, that.hasTeiidSource )
               && Objects.equals( this.errors, that.errors )
               && Objects.equals( this.schemaModelName, that.schemaModelName )
               && Objects.equals(  this.schemaState, that.schemaState )
               && Objects.equals( this.vdbName, that.vdbName )
               && Objects.equals( this.vdbState, that.vdbState );
    }

    /**
     * @return the source name (can be empty)
     */
    public String getSourceName() {
        return this.sourceName;
    }

    /**
     * @return 'true' if has corresponding teiid source
     */
    public boolean hasTeiidSource() {
        return this.hasTeiidSource;
    }

    /**
     * @return the errors (never <code>null</code>)
     */
    public List<String> getErrors() {
        return this.errors == null ? Arrays.asList( EMPTY_ARRAY ) : this.errors;
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
     * @return the server VDB name (can be empty)
     */
    public String getVdbName() {
        return this.vdbName;
    }

    /**
     * @return the server VDB state or {@link EntityState#MISSING} if not set
     */
    public EntityState getVdbState() {
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
        return Objects.hash( this.sourceName,
                             this.hasTeiidSource,
                             this.errors,
                             this.schemaModelName,
                             this.schemaState,
                             this.vdbName,
                             this.vdbState );
    }

    /**
     * @param sourceName the source name (can be empty)
     */
    public void setSourceName( final String sourceName ) {
        this.sourceName = sourceName;
    }

    /**
     * @param hasTeiidSrc 'true' if has teiid source
     */
    public void setHasTeiidSource( final boolean hasTeiidSrc ) {
        this.hasTeiidSource = hasTeiidSrc;
    }

    /**
     * Set status teiid vdb details
     * @param teiidVdb the teiid VDB
     */
    public void setTeiidVdbDetails( final TeiidVdb teiidVdb ) {
        this.vdbName = teiidVdb.getName();
        this.errors = teiidVdb.getValidityErrors();

        if ( teiidVdb.isActive() ) {
            this.vdbState = EntityState.ACTIVE;
        } else if ( teiidVdb.hasFailed() ) {
            this.vdbState = EntityState.FAILED;
        } else if ( teiidVdb.isLoading() ) {
            this.vdbState = EntityState.LOADING;
        }
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

    public void setSchemaState( final String state) {
        this.schemaState = schemaState == null ? EntityState.MISSING : EntityState.valueOf(state);
    }
    
    /**
     * @param schemaModelName the workspace schema model name (can be empty)
     */
    public void setSchemaModelName( final String schemaModelName ) {
        this.schemaModelName = schemaModelName;
    }

    /**
     * @param vdbName the server VDB name (can be empty)
     */
    public void setVdbName( final String vdbName ) {
        this.vdbName = vdbName;
    }

    /**
     * @param vdbState the server VDB state (can be <code>null</code>)
     */
    public void setVdbState( final EntityState vdbState ) {
        this.vdbState = vdbState == null ? EntityState.MISSING : vdbState;
    }

    public void setVdbState( final String state ) {
        this.vdbState = vdbState == null ? EntityState.MISSING : EntityState.valueOf(state);
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
