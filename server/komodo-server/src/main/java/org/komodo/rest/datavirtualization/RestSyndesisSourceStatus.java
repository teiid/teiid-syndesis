package org.komodo.rest.datavirtualization;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import org.komodo.metadata.TeiidVdb;
import org.komodo.rest.V1Constants;
import org.komodo.utils.StringUtils;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

/**
 * The Syndesis source status.  Determine the following for syndesis source
 * - does it have corresponding teiid source
 * - the corresponding teiid source vdb state, and details
 * - the corresponding schema vdb state, and details
 */
@JsonSerialize(as = RestSyndesisSourceStatus.class)
@JsonInclude(Include.NON_NULL)
public final class RestSyndesisSourceStatus implements V1Constants {

    /*

    {
      "sourceName": "syndesisSrcName",
      "hasTeiidSource": "true | false",
      "vdbState": "MISSING" | "LOADING" | "ACTIVE" | "FAILED",
      "vdbName": "myConnectionBtlConn",
      "schemaState": "MISSING" | "LOADING" | "ACTIVE" | "FAILED",
      "schemaVdbName": "syndesissrcnamebtlconnschemavdb",
      "id": "syndesis source / source schema id",
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

    private String sourceName;
    private boolean hasTeiidSource = false;
    private List< String > errors;
    private EntityState schemaState = EntityState.MISSING;
    private String id;
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
               && Objects.equals( this.id, that.id )
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
    public boolean getHasTeiidSource() {
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
    public String getId() {
        return this.id;
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
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return Objects.hash( this.sourceName,
                             this.hasTeiidSource,
                             this.errors,
                             this.id,
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

    public void setId(String id) {
        this.id = id;
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

}
