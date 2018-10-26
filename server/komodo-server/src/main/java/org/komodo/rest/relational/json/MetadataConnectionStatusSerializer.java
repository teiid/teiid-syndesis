package org.komodo.rest.relational.json;

import static org.komodo.rest.Messages.Error.INCOMPLETE_JSON;
import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;

import java.io.IOException;
import java.util.Arrays;

import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.metadata.RestMetadataConnectionStatus;
import org.komodo.rest.relational.response.metadata.RestMetadataConnectionStatus.EntityState;
import org.komodo.utils.StringUtils;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A serializer for {@link RestMetadataConnectionStatus} objects.
 */
public final class MetadataConnectionStatusSerializer extends TypeAdapter< RestMetadataConnectionStatus > {

    private void checkComplete( final RestMetadataConnectionStatus status ) throws IOException {
        // per documentation
        assert( status.getSchemaState() != null );
        assert( status.getServerVdbState() != null );
        assert( status.getErrors() != null );

        // should always have a connection name
        if ( StringUtils.isBlank( status.getConnectionName() ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, getClass().getSimpleName() ) );
        }
    }

    /**
     * {@inheritDoc}
     *
	 * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
	 */
	@Override
	public RestMetadataConnectionStatus read( final JsonReader in ) throws IOException {
		final RestMetadataConnectionStatus status = new RestMetadataConnectionStatus();
		in.beginObject();

		while ( in.hasNext() ) {
			final String name = in.nextName();

			if ( RestMetadataConnectionStatus.CONNECTION_NAME.equals( name ) ) {
				status.setConnectionName( in.nextString() );
            } else if ( RestMetadataConnectionStatus.ERRORS.equals( name ) ) {
                final String[] errors = BUILDER.fromJson( in, String[].class );
                status.setErrors( Arrays.asList( errors ) );
            } else if ( RestMetadataConnectionStatus.SCHEMA_MODEL_NAME.equals( name ) ) {
                status.setSchemaModelName( in.nextString() );
            } else if ( RestMetadataConnectionStatus.SCHEMA_STATE.equals( name ) ) {
                status.setSchemaState( EntityState.fromString( in.nextString() ) );
            } else if ( RestMetadataConnectionStatus.SCHEMA_VDB_NAME.equals( name ) ) {
                status.setSchemaVdbName( in.nextString() );
            } else if ( RestMetadataConnectionStatus.SERVER_VDB_NAME.equals( name ) ) {
                status.setServerVdbName( in.nextString() );
            } else if ( RestMetadataConnectionStatus.SERVER_VDB_STATE.equals( name ) ) {
                status.setServerVdbState( EntityState.fromString( in.nextString() ) );
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
                       final RestMetadataConnectionStatus status ) throws IOException {
        checkComplete( status );
		out.beginObject();

		// connection name
		out.name( RestMetadataConnectionStatus.CONNECTION_NAME );
		out.value( status.getConnectionName() );

        // server VDB state
        out.name( RestMetadataConnectionStatus.SERVER_VDB_STATE );
        out.value( status.getServerVdbState().toString() );

        // workspace schema state
        out.name( RestMetadataConnectionStatus.SCHEMA_STATE );
        out.value( status.getSchemaState().toString() );

        // errors
        out.name( RestMetadataConnectionStatus.ERRORS );
        out.beginArray();

        if ( !status.getErrors().isEmpty() ) {
            for ( final String err: status.getErrors() ) {
                out.value( err );
            }
        }

        out.endArray();

        // schema model name
        if ( !StringUtils.isBlank( status.getSchemaModelName() ) ) {
            out.name( RestMetadataConnectionStatus.SCHEMA_MODEL_NAME );
            out.value( status.getSchemaModelName() );
        }

        // schema VDB name
        if ( !StringUtils.isBlank( status.getSchemaVdbName() ) ) {
            out.name( RestMetadataConnectionStatus.SCHEMA_VDB_NAME );
            out.value( status.getSchemaVdbName() );
        }

        // server VDB name
        if ( !StringUtils.isBlank( status.getServerVdbName() ) ) {
            out.name( RestMetadataConnectionStatus.SERVER_VDB_NAME );
            out.value( status.getServerVdbName() );
        }

		out.endObject();
	}

}
