package org.komodo.rest.relational.json;

import static org.komodo.rest.Messages.Error.INCOMPLETE_JSON;
import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import java.util.Arrays;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.metadata.RestSyndesisSourceStatus;
import org.komodo.rest.relational.response.metadata.RestSyndesisSourceStatus.EntityState;
import org.komodo.utils.StringUtils;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A serializer for {@link RestSyndesisSourceStatus} objects.
 */
public final class SyndesisSourceStatusSerializer extends TypeAdapter< RestSyndesisSourceStatus > {

    private void checkComplete( final RestSyndesisSourceStatus status ) throws IOException {
        // per documentation
        assert( status.getSchemaState() != null );
        assert( status.getVdbState() != null );
        assert( status.getErrors() != null );

        // should always have a connection name
        if ( StringUtils.isBlank( status.getSourceName() ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, getClass().getSimpleName() ) );
        }
    }

    /**
     * {@inheritDoc}
     *
	 * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
	 */
	@Override
	public RestSyndesisSourceStatus read( final JsonReader in ) throws IOException {
		final RestSyndesisSourceStatus status = new RestSyndesisSourceStatus();
		in.beginObject();

		while ( in.hasNext() ) {
			final String name = in.nextName();

			if ( RestSyndesisSourceStatus.SOURCE_NAME.equals( name ) ) {
				status.setSourceName( in.nextString() );
            } else if ( RestSyndesisSourceStatus.HAS_TEIID_SOURCE.equals( name ) ) {
                status.setHasTeiidSource( in.nextBoolean() );
            } else if ( RestSyndesisSourceStatus.ERRORS.equals( name ) ) {
                final String[] errors = BUILDER.fromJson( in, String[].class );
                status.setErrors( Arrays.asList( errors ) );
            } else if ( RestSyndesisSourceStatus.SCHEMA_MODEL_NAME.equals( name ) ) {
                status.setSchemaModelName( in.nextString() );
            } else if ( RestSyndesisSourceStatus.SCHEMA_STATE.equals( name ) ) {
                status.setSchemaState( EntityState.fromString( in.nextString() ) );
            } else if ( RestSyndesisSourceStatus.SCHEMA_VDB_NAME.equals( name ) ) {
                status.setSchemaVdbName( in.nextString() );
            } else if ( RestSyndesisSourceStatus.SERVER_VDB_NAME.equals( name ) ) {
                status.setVdbName( in.nextString() );
            } else if ( RestSyndesisSourceStatus.SERVER_VDB_STATE.equals( name ) ) {
                status.setVdbState( EntityState.fromString( in.nextString() ) );
			} else {
				throw new IOException( Messages.getString( Messages.Error.UNEXPECTED_JSON_TOKEN, name ) );
			}
		}

		checkComplete( status );

		in.endObject();
		return status;
	}

    /**
     * {@inheritDoc}
     *
	 * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
	 */
	@Override
	public void write( final JsonWriter out,
                       final RestSyndesisSourceStatus status ) throws IOException {
        checkComplete( status );
		out.beginObject();

		// source name
		out.name( RestSyndesisSourceStatus.SOURCE_NAME );
		out.value( status.getSourceName() );

        Boolean hasSrc = status.hasTeiidSource();
        out.name(RestSyndesisSourceStatus.HAS_TEIID_SOURCE);
        out.value(hasSrc);

        // server VDB state
        out.name( RestSyndesisSourceStatus.SERVER_VDB_STATE );
        out.value( status.getVdbState().toString() );

        // workspace schema state
        out.name( RestSyndesisSourceStatus.SCHEMA_STATE );
        out.value( status.getSchemaState().toString() );

        // errors
        out.name( RestSyndesisSourceStatus.ERRORS );
        out.beginArray();

        if ( !status.getErrors().isEmpty() ) {
            for ( final String err: status.getErrors() ) {
                out.value( err );
            }
        }

        out.endArray();

        // schema model name
        if ( !StringUtils.isBlank( status.getSchemaModelName() ) ) {
            out.name( RestSyndesisSourceStatus.SCHEMA_MODEL_NAME );
            out.value( status.getSchemaModelName() );
        }

        // schema VDB name
        if ( !StringUtils.isBlank( status.getSchemaVdbName() ) ) {
            out.name( RestSyndesisSourceStatus.SCHEMA_VDB_NAME );
            out.value( status.getSchemaVdbName() );
        }

        // server VDB name
        if ( !StringUtils.isBlank( status.getVdbName() ) ) {
            out.name( RestSyndesisSourceStatus.SERVER_VDB_NAME );
            out.value( status.getVdbName() );
        }

		out.endObject();
	}

}
